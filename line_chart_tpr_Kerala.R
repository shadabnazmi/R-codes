pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png',
               'grid', 'ggpubr', 'scales',
               'bbplot2')


multiple_line_df <- read.csv("kerala_tpr.csv")
multiple_line_df$Date <- as.Date(multiple_line_df$Date, format = "%d/%m/%y")

#Make plot
multiple_line <- ggplot(multiple_line_df, aes(x = Date, y = tpr)) +
  geom_line(size = 1, colour = "#FAAB18")+

  scale_y_continuous(expand = c(0,0), labels = function(x) paste0(x, "%"), limits = c(0, 29))+
  bbc_style() +
  reith_style()+
  all_reith_underneath()+
  scale_x_date(breaks = date_breaks("6 months"),
               labels = date_format("%b"))+
  labs(title="Test positivity rate in Kerala",
       subtitle = "Rolling seven-day averages")
multiple_line

finalise_plot(plot_name = multiple_line,
              save_filepath = "tpr_kerala-nc.png",
              source = "Source: Indian Ministry of Health and Family Welfare, data to 1 Aug",
              width_pixels = 640,
              height_pixels = 380)
