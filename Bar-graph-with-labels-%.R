pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')
multiple_line_cases_df <- read.csv("Nirbhaya_funding.csv")
  



cases_time_series <-  ggplot(multiple_line_cases_df, aes(x=Year, y =gb))+ 
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  coord_flip()+
  theme(panel.grid.major.x = element_line(color="#cbcbcb"), 
        panel.grid.major.y=element_blank())+
  scale_y_continuous(labels = scales::comma)+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = 0, colour = "black")+
  theme(panel.grid.major.y = element_line())+
  bbc_style()+
  reith_style()+
  labs(title= "The size of gender budget has remained\nbelow 6%",
       subtitle = "Share of gender budget as the percentage of total\nbudget ")
  
cases_time_series

labelled.bars <- cases_time_series +
  geom_label(aes(x = Year, y = gb, label = gb),
             hjust = 1, 
             vjust = 0.5, 
             colour = "white", 
             fill = NA, 
             label.size = NA, 
             family="Helvetica", 
             size = 6)

labelled.bars

finalise_plot(plot_name = labelled.bars,
              source = "Source: Indian budget documents",
              width_pixels = 640,
              height_pixels = 450)


