
# SETUP -------------------------------------------------------------------

library(tidycensus)
library(tidyverse)
library(sf)
library(janitor)
library(scales)
library(patchwork) 

options(tigris_class = 'sf')

g <- glimpse



# GET DATA: DECENNIAL POPULATION ------------------------------------------


# Counties & Places in Washington

vars_1990 <- tidycensus::load_variables(year = "1990",dataset = "sf1") 

vars_1990 %>% filter(str_detect(name,"^P")) # "Total" 

totalpop_variable_1990 <- vars_1990 %>% filter(str_detect(label,"^Total\\sPersons$")) %>% pull(name)  # "P0010001"


vars_2000 <- tidycensus::load_variables(year = "2000",dataset = "sf1")

vars_2000 %>% filter(str_detect(name,"^P"))  # "Population:Total [1"

totalpop_variable_2000 <- vars_2000 %>% filter(str_detect(label,"^Population:Total")) %>% pull(name)  # "P001001"


vars_2010 <- tidycensus::load_variables(year = "2010",dataset = "sf1")

vars_2010 %>% filter(str_detect(name,"^P")) # "Total"

totalpop_variable_2010 <- vars_2010 %>% 
  filter(concept %in% "TOTAL POPULATION") %>% pull(name)  # "P001001"


vars_2018 <- load_variables(year = "2018",dataset = "acs1")

totalpop_variable_2018 <- vars_2018 %>% 
  filter(str_detect(concept,"^TOTAL\\sPOPULATION$")) %>% 
  pull(name)  # "B01003_001"



decennial_year_var <- tibble(year = c(1990L,
                                      2000L,
                                      2010L),
                             variables = c(totalpop_variable_1990, 
                                           totalpop_variable_2000,
                                           totalpop_variable_2010),
)


params_decennial <- expand_grid(geography = c("county","place"),
                                year = decennial_year_var$year) %>% 
  left_join(decennial_year_var, by = "year")


get_decennial_data <- function(geography, year, variables){ 
  dat <- get_decennial(geography = geography, state = 53, variables = variables, year = year)  
  dat <- mutate(dat, 
                year = year,
                geography = geography)
  return(dat)
}

decennial_total_pop <- pmap_dfr(params_decennial, get_decennial_data) 

params_acs <- expand_grid(year = 2018L,
                          variable = totalpop_variable_2018,
                          geography = c("county","place"))

get_acs_data <- function(geography, year, variables){ 
  dat <- get_acs(geography = geography, variables = variables, year = year,
                 state = 53, survey = "acs1")  
  dat <- mutate(dat, 
                year = year,
                geography = geography, 
                value = estimate)
  dat <- select(dat, 
                -moe,
                -estimate)
  return(dat)
}

acs_total_pop <- pmap_dfr(params_acs, get_acs_data) 


total_pop <- bind_rows(decennial_total_pop,
                       acs_total_pop)

# Write the data

write_csv(total_pop,path = "data/wa-population-counties-places-1990-2000-2010-2018.csv")



# PREPARE DATA ------------------------------------------------------------

dat <- read_csv("data/wa-population-counties-places-1990-2000-2010-2018.csv")

# Check the names (should be 4 – one for each endyear)

dat %>% count(NAME) %>% count(n)  # 1's and 2's

replace_pattern <- c("city", 
                     "CDP",
                     ",") %>% paste0(collapse = "|")

dat_clean <- dat %>% 
  arrange(NAME) %>% 
  mutate(NAME = str_replace(NAME, "\\sWashington","")) %>% 
  mutate(NAME = str_trim(str_replace_all(NAME, replace_pattern,""))) %>% 
  add_count(NAME) %>% 
  filter(n >= 4L) %>% 
  select(-n) 

dat_kc_minus_seattle <- dat_clean %>% 
  mutate(NAME = if_else(NAME %in% "King County", "King County (excl. Seattle)",NAME)) %>% 
  mutate(seattle = case_when(
    NAME %in% "Seattle" ~ value,
    TRUE ~ NA_real_
  )) %>% 
  fill(seattle, .direction = "downup") %>% 
  mutate(value = case_when(
    NAME %in% "King County (excl. Seattle)" ~ value - seattle,
    TRUE ~ value
  )) %>% 
  select(-seattle)

dat_pct <- dat_kc_minus_seattle %>% 
  filter(NAME %in% "Seattle" | geography %in% "county") %>%  
  select(-variable) %>%  # necessary to make the pivot_wider work
  mutate(year = str_c("year_",year)) %>% 
  tidyr::pivot_wider(
    names_from = year,
    values_from = value
  ) %>% 
  mutate(PCT_1990_2018 = (year_2018/year_1990) - 1,
         RANK_PCT_1990_2018 = dense_rank(PCT_1990_2018),
         LABEL_PCT_1990_2018 = scales::percent(PCT_1990_2018)) %>%  
  pivot_longer(
    cols = starts_with("year"),
    names_to = "year",
    values_to = "value"
  ) %>%  
  mutate(year = str_remove(year, "year_")) %>% 
  arrange(NAME,year) %>% 
  group_by(NAME) %>% 
  mutate(PCT_GROWTH = value/lag(value)-1, 
         PCT_GROWTH_LABEL = percent(PCT_GROWTH,1),
         PCT_GROWTH_LABEL = str_c(lag(year),"–",year,":\n+", PCT_GROWTH_LABEL),
         YEAR = factor(year)) %>%  
  ungroup()


dat_top10_abs <- dat_pct %>%  
  group_by(NAME) %>% 
  mutate(MAX_POP = max(value)) %>% 
  ungroup() %>% 
  mutate(RANK_MAX_POP = dplyr::dense_rank(MAX_POP)) %>% 
  arrange(desc(MAX_POP), NAME, geography, year) %>% 
  filter(RANK_MAX_POP >= max(RANK_MAX_POP) - 9L) 


dat_top10_pct <- dat_pct %>% 
  arrange(desc(RANK_PCT_1990_2018), NAME, geography) %>% 
  filter(RANK_PCT_1990_2018 >= max(RANK_PCT_1990_2018) - 9L)
  



# PREPARE PLOT THEME ------------------------------------------------------

line_plot_theme <- theme_void()

bar_plot_theme <- theme_minimal() + 
  theme(plot.caption = element_text(hjust=0, size=rel(1.5)),
        legend.position = "none", 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x.bottom = element_line(color = "black"),
        axis.ticks.x.bottom = element_line(color = "black"),
        axis.title = element_blank())

create_combined_plot <- function(target_county, all_counties_min_pct, all_counties_max_pct, all_counties_max_abs, title, .abs_or_pct = c("abs","pct")){
  
  # browser()
  
  if(! .abs_or_pct %in% c(c("abs","pct"))){stop("The `.abs_or_pct` parameter must be either 'abs' or 'pct'.")}
  
  if(.abs_or_pct %in% "abs"){
    
    col_fill_values <- c("1990" = "#000000",
                         "2000" = "#000000",
                         "2010" = "#000000",
                         "2018" = "#EF4B54")

    col_size <-  0L
    col_color <-  "#FFFFFF"  # doesn't matter because size = 0
    col_label_color <- "#FFFFFF" 
    
    line_1990_2018_color <- "#000000"
    line_1990_2018_size <- 1
    
    label_1990_2018_fill <- "#FFFFFF"
    label_1990_2018_color <- "#000000"
    label_1990_2018_fontface <- "plain"
    
    line_others_color <- "#000000"
    line_others_size <- 1
    
    label_others_fill <- "#FFFFFF"
    label_others_color <- "#000000"
    label_others_fontface <- "plain"
    
  }
  
  if(.abs_or_pct %in% "pct"){
    col_fill_values <- c("1990" = "#FFFFFF",
                         "2000" = "#FFFFFF",
                         "2010" = "#FFFFFF",
                         "2018" = "#FFFFFF")
    col_size <-  1L
    col_color <-  "#000000"
    col_label_color <- "#000000" 
    
    line_1990_2018_color <- "#EF4B54"
    line_1990_2018_size <- 1.5
    
    label_1990_2018_fill <- "#EF4B54"
    label_1990_2018_color <- "#FFFFFF"
    label_1990_2018_fontface <- "bold"
    
    line_others_color <- "#000000"
    line_others_size <- 1.5
    
    label_others_fill <- "#000000"
    label_others_color <- "#FFFFFF"
    label_others_fontface <- "bold"
    
    
  }
  
  bar_label_y <- 1e5
  
  nudge_y <- all_counties_max_abs*0.2
  
  county_max <- max(target_county$value,na.rm = TRUE) + nudge_y
  
  pct_growth_scaled_floor <- (3 * nudge_y) + county_max
  
  pct_growth_scaled_ceiling <- pct_growth_scaled_floor +(all_counties_max_abs*0.5)
  
  target_county <- target_county %>% 
    mutate(PCT_GROWTH_SCALED = rescale(x = PCT_GROWTH, from = c(all_counties_min_pct, all_counties_max_pct), to = c(pct_growth_scaled_floor, pct_growth_scaled_ceiling)),
           PCT_GROWTH_LABEL_1990_2018 = case_when(
             as.character(YEAR) %in% "2018" ~  str_c("1990–2018:\n+",LABEL_PCT_1990_2018),
             TRUE ~ NA_character_
           ),
           LINE_Y = case_when(
             as.character(YEAR) %in% "1990" ~ value + nudge_y,
             as.character(YEAR) %in% "2018" ~ value + nudge_y,
             TRUE ~ NA_real_
           ))
  
  
  target_county_1990_2018 <- filter(target_county, as.character(YEAR) %in% c("1990","2018"))  
  
  
  plot_bar <- ggplot(data = target_county,
                     aes(x = factor(year), y = value)) 
  plot_bar <- plot_bar + geom_col(aes(fill = factor(year)), size = col_size, color = col_color) 
  
  plot_bar <- plot_bar + geom_label(aes(x = factor(year), label = comma(value), fill = factor(year)), y = bar_label_y,  color = col_label_color, label.size = NA)
  
  plot_bar <- plot_bar + geom_line(data = target_county_1990_2018,
                                   aes(x = as.numeric(YEAR), y = LINE_Y), 
                                   group = 1, 
                                   color = line_1990_2018_color, 
                                   size = line_1990_2018_size, 
                                   linetype = 3)
  
  plot_bar <- plot_bar + geom_label(data = target_county_1990_2018,
                                    aes(x = as.numeric(YEAR), y = LINE_Y,label = PCT_GROWTH_LABEL_1990_2018), 
                                    fill = label_1990_2018_fill, 
                                    colour = label_1990_2018_color,
                                    fontface = label_1990_2018_fontface)
  
  plot_bar <- plot_bar + geom_line(data = target_county,
                                   aes(x = as.numeric(YEAR), y = PCT_GROWTH_SCALED), 
                                   group = 1, 
                                   color = line_others_color, 
                                   size = line_others_size, 
                                   linetype = 3)
  
  plot_bar <- plot_bar + geom_label(data = target_county,
                                    aes(x = as.numeric(YEAR), y = PCT_GROWTH_SCALED, label = PCT_GROWTH_LABEL),
                                    fill = label_others_fill, 
                                    colour = label_others_color,
                                    fontface = label_others_fontface)
  
  plot_bar <- plot_bar +  scale_y_continuous(labels = scales::comma,
                                             limits = c(0,
                                                        all_counties_max_abs*2))
  
  plot_bar <- plot_bar + 
    scale_fill_manual(values = col_fill_values) +
    labs(caption = title) +
    bar_plot_theme
  
  
  return(plot_bar)
}

# CREATE PLOT LIST:  TOP 10 POP. GROWTH (ABSOLUTE) ------------------------


county_list <- dat_top10_abs %>% 
  group_by(NAME) %>% 
  group_split() 

params_abs <- tibble(target_county = county_list,
                 all_counties_min_pct = min(dat_pct$PCT_GROWTH,na.rm =TRUE)*0.9,
                 all_counties_max_pct = max(dat_pct$PCT_GROWTH,na.rm =TRUE)*1.2,
                 all_counties_max_abs = max(dat_pct$value, na.rm = TRUE)) %>% 
  mutate(title = map_chr(target_county,~ unique(pluck(.x,"NAME"))),
         rank = map_int(target_county, ~unique(pluck(.x, "RANK_MAX_POP")))) %>% 
  arrange(desc(rank)) %>% 
  mutate(title = str_c(row_number(),". ",title)) %>% 
  select(-rank)
  

params_abs <- params_abs %>% prepend(c(.abs_or_pct = "abs"))

plot_list_abs <- pmap(params_abs, create_combined_plot)




# CREATE PLOT LIST:  TOP 10 POP. GROWTH (PCT) -----------------------------

county_list <- dat_top10_pct %>% 
  group_by(NAME) %>% 
  group_split() 

params_pct <- tibble(target_county = county_list,
                 all_counties_min_pct = min(dat_pct$PCT_GROWTH,na.rm =TRUE)*0.9,
                 all_counties_max_pct = max(dat_pct$PCT_GROWTH,na.rm =TRUE)*1.2,
                 all_counties_max_abs = max(dat_pct$value, na.rm = TRUE)) %>% 
  mutate(title = map_chr(target_county,~ unique(pluck(.x,"NAME"))),
         rank = map_int(target_county, ~unique(pluck(.x, "RANK_PCT_1990_2018")))) %>% 
  arrange(desc(rank)) %>% 
  mutate(title = str_c(row_number(),". ",title)) %>% 
  select(-rank)
  

params_pct <- params_pct %>% prepend(c(.abs_or_pct = "pct"))

plot_list_pct <- pmap(params_pct, create_combined_plot)

