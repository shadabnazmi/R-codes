pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot','bbplot2')
multiple_line_cases_df <- read.csv("Nirbhaya_funding.csv")




cases_time_series <-  ggplot(multiple_line_cases_df, aes(x=Year, y =gb, group =1 ))+ 
  geom_line(colour = "#1380A1", size = 1) +
  theme(panel.grid.major.x = element_line(color="#cbcbcb"), 
        panel.grid.major.y=element_blank())+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_x_discrete(breaks = c("2009-10", "2014-15","2019-20"))+
  geom_hline(yintercept = 0, colour = "black")+
  theme(panel.grid.major.y = element_line())+
  bbc_style()+
  reith_style()+
  labs(title= "The size of gender budget has remained\nbelow 6%",
       subtitle = "Share of gender budget as the percentage of total\nbudget ")

cases_time_series



finalise_plot(plot_name = cases_time_series,
              source = "Source: Indian budget documents",
              save_filepath = "gender_budget-nc.png",
              width_pixels = 640,
              height_pixels = 500)


