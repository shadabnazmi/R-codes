library(ggplot2)
library(dplyr)
library(bbplot2)
library(lubridate)


facet <- read.csv("states.csv", header = TRUE,na.strings = 999.99)
str(facet)

facet <- facet %>%
  mutate(Date= as.Date(Date, format = "%d/%m/%y"))%>%
  filter(State == "Maharashtra" | State == "Andhra Pradesh" | State == "Delhi" | State == "Karnataka" | State == "Tamil Nadu" | State == "Karnataka" | State == "Uttar Pradesh")%>%
  filter(type == "Confirmed")
  


#Make plot
facet_plot <- ggplot() +
  geom_area(data = facet, aes(x = Date, y = Count, fill = State)) +
  scale_fill_manual(values = c( "#1380A1","#1380A1","#1380A1","#1380A1","#1380A1","#1380A1")) + 
  facet_wrap( ~ State, ncol = 2, scales = "free") + 
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  labs(title = "Daily cases in selected Indian states",
       subtitle = "Rolling seven-day averages")+
  bbc_style()+
  reith_style()

facet_final <- facet_plot+theme(legend.position = "none") 
facet_final



finalise_plot(plot_name = facet_final,
              source = "Source: Indian Ministry of Health and Family Welfare, data to 1 Feb",
              save_filepath = "states_daily.png",
              width_pixels = 640,
              height_pixels = 650)