##1. Load Packages ----
library("magick")
library(tidytuesdayR)
library(tidyverse)
library(patchwork)
library(showtext)
#library(sysfonts)
library(ggradar)

showtext_auto()

# 2. Load Data------
tidy_week = 29
tidy_year = 2022
tidy_source = "NEBR"
tuesdata <- tidytuesdayR::tt_load(tidy_year, week = tidy_week)
tech <- tuesdata$technology

#Energy Types
electricty_production <- unique(tech$variable)[grep("elec_",unique(tech$variable))]
fossil <- electricty_production[c(1,3,6)]
renewable <- electricty_production[c(4,7,8,9)]

tech$Type <- ifelse(tech$variable %in% fossil, "Fossil Fuel",
                    ifelse(tech$variable %in% renewable, "Renewable",
                           ifelse(tech$variable =="elec_nuc", "Nuclear","none")))

#Energy Type Usage across G7 countries
G7_countries <- c("DEU", "FRA", "GBR", "ITA", "ESP","USA","CAN")


#My Aesthetics----
background_fill = "ivory"
my_text_colour = "black"

#My Fonts
font_add_google("Play", family = "play") #https://fonts.google.com
font_add_google("Poiret One", family = "poiretone")

main_font = "play"
text_box_font = "poiretone"


my_theme <- theme(
  plot.background = element_rect(fill=background_fill,colour = background_fill),
  panel.background = element_rect(fill=background_fill,colour=background_fill),
  legend.position = "bottom",
  plot.margin = unit(c(1,1,1,1), "cm"),
  legend.background = element_rect(fill=background_fill),
  legend.title = element_text(family = main_font,color=my_text_colour),
  legend.key = element_rect(fill=background_fill,colour=background_fill),
  legend.box.background = element_rect(fill=background_fill, colour=background_fill),
  plot.title = element_text(family = main_font, size=22,hjust=0.5,color=my_text_colour,face="bold"),
  plot.subtitle = element_text(family = text_box_font, size=14,hjust=0.5,color=my_text_colour),
  plot.caption = element_text(colour=my_text_colour,size=10),
  legend.text = element_text(colour=my_text_colour,family = main_font),
)


make_radar_data <- function(data, select_year){
  radar_data <- data %>%
    filter(Type != "none" & iso3c %in% G7_countries & year==select_year) %>%
    select(country= iso3c, energy_type = variable, value=value)  %>%
    pivot_wider(names_from = energy_type, values_from = value) %>%
    rowwise() %>% mutate(solar_wind_other = sum(c_across(elec_renew_other:elec_wind))) %>% #combine wind, solar, others together
    ungroup() %>% select(c(1:6,10))%>%
    rowwise() %>% mutate(Total = sum(c_across(elec_coal:solar_wind_other))) %>%
    ungroup() %>%
    mutate(across(elec_coal:solar_wind_other, ~ . / Total)) %>%
    select(c(1:7)) 
  
  colnames(radar_data) <- c("Country","Coal","Gas","Hydro","Nuclear","Oil","Solar&Wind")
  return(radar_data)
}

make_radar_plot <- function(plot_data, year){
  ggradar(plot_data,
          group.point.size = 1,
          font.radar=main_font,
          grid.min = 0,
          grid.mid = 0.5,
          grid.max = 0.80,
          values.radar = c("", "50%", "80%"))+
    labs(title=year,
         subtitle = "Changing energy consumption across G7 countries 2000-2020.",
         caption=paste0("@Rosie_Griffiths | #TidyTuesday Week ",tidy_week," ",tidy_year," | Source: ",tidy_source))+
    my_theme
}

year = 2020
G7_energy_usage_2020 <- make_radar_data(data=tech,select_year = year)
radar_2020 <- make_radar_plot(G7_energy_usage_2020, year=year)

#View Plot
radar_2020

dir.create("Downloads/radar_plot")
#Make plot for each year
for(year in c(2000:2020)){
  tmp <- make_radar_data(data=tech,select_year = year)
  radar_plot <- make_radar_plot(tmp, year=year)
  ggsave(plot = radar_plot, 
         filename = paste0("Downloads/radar_plot/",year,".png"), 
         width=7,height=7,
         device = "png")
}

## list file names and read in
imgs <- list.files("Downloads/radar_plot/", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 1)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "Downloads/radar_plot.gif")

image_write(image = img_animated,
            path = "Documents/GitHub/TidyTuesday/EnergyRadarPlot.gif")

