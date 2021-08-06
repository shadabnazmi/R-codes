pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png',
               'grid', 'ggpubr', 'scales',
               'bbplot2')

#Prepare data
grouped_bar_df <- read.csv("disinvestment_India .csv") %>%
  select(year, type, count) %>%
  spread(year, count) %>%
  head(5) %>%
  gather(key = year, 
         value = count,
         -type) 

#Make plot
grouped_bars <- ggplot(grouped_bar_df, 
                       aes(x = year, 
                           y = count, 
                           fill = as.factor(type), label = (count, 0) +
  geom_bar(stat="identity", position="dodge") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
   scale_fill_manual(values = c("#1380A1", "#FAAB18")) +
  labs(title="How often has India met its disinvestment\ntarget?", subtitle = "All figures in USD billion")+
  ylim(0,15)+
  bbc_style() +
  reith_style()+
  all_reith_underneath()

grouped_bars

finalise_plot(plot_name = grouped_bars,
              source = "Source: Department of Investment & Public Asset Management",
              width_pixels = 640,
              height_pixels = 450)


