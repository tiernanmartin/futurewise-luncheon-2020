# MAKE PLOT: POP GROWTH ---------------------------------------------------

dat_top10_abs_ready <- dat_top10_abs %>% 
  mutate(NAME_FCT = fct_reorder(factor(NAME),MAX_POP),
         YEAR_FCT = fct_rev(factor(year))) %>% 
  mutate(YEAR_LABEL = fct_rev(factor(case_when(
    MAX_POP_RANK == max(MAX_POP_RANK) ~ as.character(year),
    TRUE ~ NA_character_
  ))))


plot_pop <- ggplot(data = dat_top10_abs_ready, aes(x = NAME_FCT, y = value, fill = YEAR_FCT))
plot_pop <- plot_pop + geom_col()
plot_pop <- plot_pop + geom_text(aes(label = YEAR_LABEL, color = factor(YEAR_LABEL)), position = position_stack(vjust = 0.5))
plot_pop <- plot_pop + geom_hline(yintercept = 0) 
# plot_pop <- plot_pop + geom_text(aes(NAME_FCT_TOT, y = POP_2017),hjust = 0, nudge_y = 5e3) 
plot_pop <- plot_pop + scale_y_continuous(limits = c(0,8e6),
                                          breaks = c(1e5,5e5, seq(from = 1e6, to = 8e6, by = 1e6)),
                                          labels = comma) 
plot_pop <- plot_pop + coord_flip() 
plot_pop <- plot_pop + scale_fill_manual(labels = c("2018", "2010", "2000", "1990"),
                                         values = rev(c("#FCBBA1", "#FC9272", "#FB6A4A", "#EF4B54")),
                                         guide = guide_legend(title = NULL,
                                                              reverse = TRUE,direction = "vertical"))
plot_pop <- plot_pop + scale_color_manual(labels = c("2018", "2010", "2000", "1990"),
                                          values = rev(c("#000000", "#000000", "#000000", "#ffffff")))
plot_pop <- plot_pop + theme_minimal() 
plot_pop <- plot_pop + theme(legend.position = "none",
                             # legend.position = c(.99, .05),
                             # legend.justification = c("right", "bottom"),
                             # legend.box.just = "right",
                             # legend.margin = margin(6, 6, 6, 6),
                             panel.grid = element_line(linetype = 2),
                             # axis.line.y.left = element_line(color = "black"),
                             axis.line.x.bottom = element_line(color = "black"),
                             axis.ticks.x.bottom = element_line(color = "black"),
                             axis.title = element_blank())
plot_pop


# MAKE PLOT: PCT POP GROWTH -----------------------------------------------

dat_top10_pct_ready <- dat_top10_pct %>% 
  filter(! year %in% "1990_2018") %>%  
  mutate(NAME_FCT = fct_reorder(factor(NAME),RANK_1990_2018),
         YEAR_FCT = fct_rev(factor(year))) %>% 
  mutate(YEAR_LABEL = fct_rev(factor(case_when(
    RANK_1990_2018 == max(RANK_1990_2018) ~ str_replace(as.character(year),"_","–"),
    TRUE ~ NA_character_
  )))) 

plot_pop_pct <- ggplot(data = dat_top10_pct_ready, aes(x = NAME_FCT, y = value, fill = YEAR_FCT))
plot_pop_pct <- plot_pop_pct + geom_col(position = "dodge")
plot_pop_pct <- plot_pop_pct + geom_text(aes(label = YEAR_LABEL, color = factor(YEAR_LABEL)), hjust = 1, vjust = 0.5, position = position_dodge(width = 1))
plot_pop_pct <- plot_pop_pct + geom_hline(yintercept = 0) 
# plot_pop_pct <- plot_pop_pct + geom_text(aes(NAME_FCT_TOT, y = POP_2017),hjust = 0, nudge_y = 5e3) 
plot_pop_pct <- plot_pop_pct + scale_y_continuous(limits = c(0,1.5),
                                                  # breaks = c(1e5,5e5, seq(from = 1e6, to = 8e6, by = 1e6)),
                                                  labels = percent) 
plot_pop_pct <- plot_pop_pct + coord_flip()
plot_pop_pct <- plot_pop_pct + scale_fill_manual(labels = c("2010_2018", "2000_2010", "1990_2000"),
                                                 values = rev(c("#FC9272", "#FB6A4A", "#EF4B54")),
                                                 guide = guide_legend(title = NULL,
                                                                      reverse = TRUE,direction = "vertical"))
plot_pop_pct <- plot_pop_pct + scale_color_manual(labels = c("2010_2018", "2000_2010", "1990_2000"),
                                                  values = rev(c( "#000000", "#000000", "#ffffff")))
plot_pop_pct <- plot_pop_pct + theme_minimal() 
plot_pop_pct <- plot_pop_pct + theme(legend.position = "none",
                                     # legend.position = c(.99, .05),
                                     # legend.justification = c("right", "bottom"),
                                     # legend.box.just = "right",
                                     # legend.margin = margin(6, 6, 6, 6),
                                     panel.grid = element_line(linetype = 2),
                                     # axis.line.y.left = element_line(color = "black"),
                                     axis.line.x.bottom = element_line(color = "black"),
                                     axis.ticks.x.bottom = element_line(color = "black"),
                                     axis.title = element_blank())
plot_pop_pct






tmp_kc <- tmp %>% 
  filter(NAME %in% "King County (excl. Seattle)") %>% 
  mutate(YEAR = factor(year))


plot_line_kc <- tmp_kc %>% 
  ggplot(aes(x = YEAR, y = PCT_GROWTH, group = 1, label = PCT_GROWTH_LABEL)) +
  geom_point() +
  geom_line()+
  geom_label(fill = "black",colour = "white", fontface = "bold") +
  scale_y_continuous(limits = c(min(tmp$PCT_GROWTH,na.rm =TRUE),
                                max(tmp$PCT_GROWTH,na.rm =TRUE))) +
  labs(title = "King County (excl. Seattle)") +
  line_plot_theme


plot_bar_kc <- tm_kc %>% 
  ggplot(aes(x = factor(year), y = value)) +
  geom_col() +  
  scale_y_continuous(labels = scales::comma,
                     limits = c(0,
                                max(tmp$value,na.rm =TRUE))) +  
  bar_plot_theme


tmp_sea <- tmp %>% 
  filter(NAME %in% "Seattle") %>% 
  mutate(YEAR = factor(year))


plot_line_sea <- tmp_sea %>% 
  ggplot(aes(x = YEAR, y = PCT_GROWTH, group = 1, label = PCT_GROWTH_LABEL)) +
  geom_point() +
  geom_line()+
  geom_label(fill = "black",colour = "white", fontface = "bold") +
  scale_y_continuous(limits = c(min(tmp$PCT_GROWTH,na.rm =TRUE),
                                max(tmp$PCT_GROWTH,na.rm =TRUE))) +
  labs(title = "Seattle") +
  line_plot_theme 


plot_bar_sea <- tmp_sea %>% 
  ggplot(aes(x = factor(year), y = value)) +
  geom_col() + 
  scale_y_continuous(labels = scales::comma,
                     limits = c(0,
                                max(tmp$value,na.rm =TRUE))) + 
  bar_plot_theme
  


p1 <- (plot_line_kc / plot_bar_kc)

p2 <- (plot_line_sea / plot_bar_sea) 

{p1} / {p2} + plot_layout(ncol = 1)


({(plot_line_kc / plot_bar_kc) + plot_layout(heights = c(1,3), ncol = 1) + plot_annotation(title = "King County (excl. Seattle)")} / {(plot_line_sea / plot_bar_sea) +plot_layout(heights = c(1,3),ncol = 1) + plot_annotation(title = "Seattle")}) + plot_layout(ncol = 1)


