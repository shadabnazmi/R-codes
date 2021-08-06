library(ggplot2)
library(dplyr)
library(bbplot2)
library(lubridate)
library(scales)
library(forcats)

facet <- read.csv("Rt.csv")
str(facet)


multiple_line <- ggplot(data =facet, aes(x = fct_reorder(State,Rt),
                                         y =Rt))+
  coord_flip()+
  geom_bar(stat="identity", 
          position="identity", 
          fill="#1380A1") +
                          reith_style()+
                          labs(title="States with highest and lowest effective\nreproduction number (Rt)")
                        multiple_line
                        
  facet_plot_without_lab <- multiple_line +theme(legend.position = "none")
                        
                        
                        finalise_plot(plot_name = facet_plot_without_lab,
                                      source = "Source: Indian Ministry of Health and Family Welfare, data to 18 May",
                                      save_filepath = "Rt.png",
                                      width_pixels = 640,
                                      height_pixels = 480)