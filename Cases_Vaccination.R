library(ggplot2)
library(dplyr)
library(bbplot2)
library(lubridate)
library(scales)

facet <- read.csv("cases_vacc_01.csv")
str(facet)

facet <- facet %>%
  mutate(Date= as.Date(Date, format = "%d/%m/%y"))



multiple_line <- ggplot() +
  geom_area(data =facet, aes(x = Date, y =count, fill = type)) +
  scale_fill_manual(values = c( "#1380A1","#983302")) + 
  scale_y_continuous(expand = c(0, 0), labels = scales::comma)+
  facet_wrap( ~ type, ncol = 1, scales = "free") + 
  reith_style()+
  scale_x_date(breaks = date_breaks("months"),
               labels = date_format("%b"))+
  labs(title="India’s Covid cases are on the rise as\nvaccination lags", subtitle = "Rolling seven-day averages")
multiple_line

facet_plot_without_lab <- multiple_line +theme(legend.position = "none") 


finalise_plot(plot_name = facet_plot_without_lab,
              source = "Source: Ministry of Health and Family Welfare, data to 13 May",
              save_filepath = "vaccination_cases-nc.png",
              width_pixels = 640,
              height_pixels = 600)