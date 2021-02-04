library(ggplot2)
library(dplyr)
library(bbplot2)
library(lubridate)


facet <- read.csv("cases.csv", header = TRUE,na.strings = 999.99)
str(facet)

facet <- facet %>%
  mutate(Date= as.Date(Date, format = "%d/%m/%y"))



#Make plot
facet_plot <- ggplot() +
  geom_area(data = facet, aes(x = Date, y = Count, fill = type)) +
  scale_fill_manual(values = c( "#1380A1","#BB1919")) + 
  facet_wrap( ~ type, ncol = 1, scales = "free") + 
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "none") +
  theme(legend.title = element_blank())+
  labs(title = "Daily cases and deaths are dropping",
       subtitle = "Rolling seven-day averages")+
  bbc_style()+
  reith_style()

facet_plot

finalise_plot(plot_name = facet_plot,
              source = "Source: Indian Ministry of Health and Family Welfare, data to 1 Feb",
              save_filepath = "cases_daily.png",
              width_pixels = 640,
              height_pixels = 650)