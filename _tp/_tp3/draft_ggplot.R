#visualisation 

ggplot(data = nov_1984, aes(x = category,
                            y = prop)) +
  geom_col(fill = "#3d5a80") +
  scale_x_discrete(limits = c("civil_rights", "crime", "energy", "prov_local" ,"government_ops", "sstc", "macroeconomics")) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, by = 10)) +
  coord_flip() +
  labs(x = "Category",
       y = "Proportion (%)",
       title = "Top categories by proportion in Brian Mulroney's speeches", 
       subtitle = "November 1984, 33rd Parliament") +
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(color = "black", size = .2), 
        panel.grid.minor.y = element_line(color = "grey70", size = .1), 
        axis.line = element_line(color = "black"),
        plot.title = element_text(size = 14, family = "Times New Roman"), 
        text = element_text(face = "plain", size = 14, family = "Times New Roman"))

## Saving the graph 

graph_mulroney <- ggplot(data = nov_1984, aes(x = category,
                                              y = prop)) +
  geom_col(fill = "#3d5a80") +
  scale_x_discrete(limits = c("civil_rights", "crime", "energy", "prov_local" ,"government_ops", "sstc", "macroeconomics")) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, by = 10)) +
  coord_flip() +
  labs(x = "Category",
       y = "Proportion (%)",
       title = "Top categories by proportion in Brian Mulroney's speeches", 
       subtitle = "November 1984, 33rd Parliament") +
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(color = "black", size = .2), 
        panel.grid.minor.y = element_line(color = "grey70", size = .1), 
        axis.line = element_line(color = "black"),
        plot.title = element_text(size = 14, family = "Arial"), 
        text = element_text(face = "plain", size = 14, family = "Arial"))


save(graph_mulroney, file = "~/Dropbox/fas_1001_Zhuk/_tp/_tp3/fig/graph_mulroney.Rda")

ggsave(graph_mulroney, 
       filename = "~/Dropbox/fas_1001_Zhuk/_tp/_tp3/fig/final/graph_mulroney.png",
       dpi = 320, 
       bg = "white",
       units = "cm",
       height = 10, 
       width = 20)