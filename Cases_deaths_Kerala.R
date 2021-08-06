library(ggplot2)
library(dplyr)
library(bbplot2)
library(lubridate)
library(scales)


facet <- read.csv("kerala_updated.csv")
str(facet)
facet <- facet %>%
  mutate(Date= as.Date(Date, format = "%d/%m/%y"))

facet_plot_01 <- ggplot() +
  geom_area(data = facet, aes(x = Date, y = Count, fill = type)) +
  scale_fill_manual(values = c( "#1380A1","#BB1919")) + 
  facet_wrap( ~ type, ncol = 1, scales = "free") + 
  scale_y_continuous(expand =c(0,0), labels = scales::comma)+
  theme(legend.title = element_blank())+
  labs(title = "Daily cases and deaths in Kerala",
       subtitle = "Rolling seven-day averages, Jan-Aug 2021")+
  scale_x_date(breaks = date_breaks("2 months"),
               labels = date_format("%b"))+
  bbc_style()+
  reith_style()

facet_plot_01

facet_plot_without_lab <- facet_plot_01 +theme(legend.position = "none") 


finalise_plot(plot_name = facet_plot_without_lab,
              source = "Source: Indian Ministry of Health and Family Welfare, data to 1 Aug",
              save_filepath = "kerala_updated-nc.png",
              width_pixels = 640,
              height_pixels = 480)