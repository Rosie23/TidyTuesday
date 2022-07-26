##1. Load Packages ----
library(tidytuesdayR)
library(tidyverse)
library(ggbump)
library(ggflags)
library(RColorBrewer)
library(countrycode)
library(patchwork)
library(showtext)
library(sf)
library(rnaturalearth)
library(viridis)
#library(sysfonts)

showtext_auto()

# 2. Load Data------
tuesdata <- tidytuesdayR::tt_load(2022, week = 29)
tech <- tuesdata$technology

#Examine data
info_df <- tech %>%
  select(variable, label, group, category) %>% unique()

#My Lists----
#Energy Types
electricty_production <- unique(tech$variable)[grep("elec_",unique(tech$variable))]
fossil <- electricty_production[c(1,3,6)]
renewable <- electricty_production[c(4,7,8,9)]

#Get List of European countries
european_countries = codelist[codelist$continent =="Europe",]
european_countries <- european_countries$iso3c
european_countries <- european_countries[complete.cases(european_countries)]
european_countries

#Top Electricity Producing European Countries
df <- tech %>%
  filter(group=="Production" & variable == "elec_cons" & iso3c %in% european_countries & year == 2000) %>%
  group_by(iso3c) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value))

top_12_countries <- df$iso3c[1:12]
top_12_countries

#Make data to plot ----
#Greenest Countries 
tech$GreenEnergy <- ifelse(tech$variable %in% fossil, "No",
                    ifelse(tech$variable %in% c(renewable,"elec_nuc"), "Yes","none"))

tech$Type <- ifelse(tech$variable %in% fossil, "Fossil Fuel",
                    ifelse(tech$variable %in% renewable, "Renewable",
                           ifelse(tech$variable =="elec_nuc", "Nuclear","none")))

get_renewable_perc <- function(values){
  perc <- values[2]/sum(values)
  return(perc)
}

percentage_green_2020 <- tech %>%
  filter(GreenEnergy != "none" & iso3c %in% european_countries & year == 2020) %>%
  group_by(iso3c,GreenEnergy) %>% 
  summarise(value = sum(value))%>%
  arrange(desc(iso3c))%>%
  group_by(iso3c) %>%
  summarise(perc = get_renewable_perc(value))

percentage_green_2020

#Ranked Data
make_ranked_data <- function(selected_year){
  ranked_data <- tech %>%
    filter(GreenEnergy != "none" & iso3c %in% top_12_countries & year == selected_year) %>%
    group_by(iso3c, GreenEnergy) %>% 
    summarise(value = sum(value))%>%
    arrange(desc(iso3c))%>%
    group_by(iso3c) %>% 
    summarise(perc = get_renewable_perc(value)) %>% 
    mutate(rank = rank(perc, ties.method = "random"))%>% 
    ungroup()
  return(ranked_data)
}

countries_ranked <- data.frame(iso3c=as.character(),perc=as.numeric(),rank=as.integer())

for (year in c(2000,2010,2020)) {
  tmp <- make_ranked_data(year)
  tmp$year <- year
  countries_ranked <- rbind(countries_ranked,tmp)
}
countries_ranked

#Get country code for ggflag
country_2_letters <- countrycode(countries_ranked$iso3c %>% unique() %>% sort(),
                                 origin = "iso3c",destination = "genc2c") %>% 
  tolower() %>% set_names(countries_ranked$iso3c %>% unique() %>% sort())

countries_ranked <- countries_ranked %>% 
  mutate(country_code = country_2_letters[iso3c])

head(percentage_green_2020)

#Get Map data
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
Europe_energy <- merge(Europe,percentage_green_2020, by.x="iso_a3",by.y="iso3c",all = TRUE)

#My Aesthetics----
background_fill = "azure"
my_text_colour = "darkslateblue"

#My Fonts
font_add_google("Play", family = "play") #https://fonts.google.com
font_add_google("Poiret One", family = "poiretone")

main_font = "play"
text_box_font = "poiretone"

my_cols <- c("firebrick","forestgreen","gold")
names(my_cols) <- c("Fossil Fuel","Renewable","Nuclear")

my_theme <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_text(family = main_font,color=my_text_colour),
  plot.background = element_rect(fill=background_fill),
  panel.background = element_rect(fill=background_fill),
  panel.grid = element_blank(),
  legend.position = "bottom",
  legend.box="horizontal",
  plot.margin = margin(1, 1, 1, 1, unit = "line"),
  legend.background = element_rect(fill=background_fill),
  legend.title = element_text(family = main_font,color=my_text_colour),
  legend.key.width = unit(1,"cm"),
  plot.title = element_text(family = main_font, size=18,hjust=0.5,color=my_text_colour),
  plot.subtitle = element_text(family = text_box_font,size=14,hjust=0.5,color=my_text_colour),
  plot.caption = element_text(colour=my_text_colour),
  legend.text = element_text(colour=my_text_colour,family = main_font),
)

#Make Plots----
ranked_plot <- ggplot(countries_ranked,aes(x=year, y=rank, group = iso3c,colour=iso3c,fill=iso3c)) +
  geom_bump(aes(smooth = 10), size= 1.5, lineend = "round")+
  scale_color_brewer(palette="Paired")+scale_fill_brewer(palette = "Paired")+
  geom_flag(data = countries_ranked %>% filter(year == min(year)), 
            aes(country = country_code),size = 8,color = "black") +
  geom_flag(data = countries_ranked %>% filter(year == max(year)), 
            aes(country = country_code),size = 8,color = "black")+
  labs(title="Ranked over the Years",
       x="Year",y="Rank")+
  my_theme+theme(legend.position = "none",
                 axis.text.x = element_text(family = "play",color=my_text_colour))

map_plot <- ggplot(Europe_energy, aes(fill=perc)) +
  labs(fill='% Green Energy')+
  geom_sf() +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE)+
  scale_fill_viridis(labels=scales::percent)+
  labs(title="% Green Energy in 2020
       across Europe")+
  my_theme

top_12_across_year <- tech %>%
  filter(Type != "none" & iso3c %in% top_12_countries & year >1999) %>%
  group_by(iso3c, year,Type) %>% 
  summarise(value = sum(value))

top_12_across_year$Country <- countrycode(top_12_across_year$iso3c,origin = "iso3c",destination = "country.name")

line_plot <- ggplot(top_12_across_year, aes(x=year, y=value,group=Type,colour=Type))+
  geom_line(size=1.5)+
  facet_wrap("~Country", labeller = label_wrap_gen(width=10),scales="free_y")+
  scale_color_manual(values=my_cols)+
  labs(y="Energy Production (TWH)",
       x="Year",
       title="A Shift to Renewables",
       subtitle = "Change from fossil fuels to renewable energy sources across the
       top 12 European countries with the largest energy output")+
  my_theme+
  theme(axis.text.x = element_text(family = "play",angle=45,size=10,hjust = 0.9,color=my_text_colour),
        axis.text.y = element_text(family = "play",size=8,color=my_text_colour),
        strip.background =element_rect(fill=background_fill),
        legend.position = "bottom",
        legend.key=element_blank(),
        strip.text = element_text(colour = 'black', family="play",size=12,color=my_text_colour))


#Add image
#my_image <- readPNG("Documents/GitHub/TidyTuesday/Logo.png", native = TRUE)


patchwork_plots <- map_plot+ranked_plot+line_plot


layout <- c(
  area(t = 1, l = 1, b = 6, r = 6),
  area(t = 1, l = 7, b = 6, r = 9),
  area(t = 7, l = 1, b = 11, r = 9)
)

plot(layout)

patchwork_plots+
  plot_layout(design = layout)+
  #inset_element(p = my_image,
  #              left = 0.5,
  #              bottom = 0.55,
  #              right = 0.95,
  #              top = 0.95)+
  plot_annotation(title = 'Green Energy Production Across Europe',
                  subtitle = "Percentage of Total Energy Production from Green energy
       sources (Wind, Solar, Nuclear, Hydro) across Europe",
                  theme =  my_theme & theme(plot.title = element_text(size = 34,family = "play",
                                                                      face="bold",color=my_text_colour),
                                            plot.subtitle = element_text(size = 16,family = text_box_font,color=my_text_colour)),
                  caption = "@Rosie_Griffiths | #TidyTuesday Week 29 2022 | Source: NBER")

ggsave(filename = "Documents/GitHub/TidyTuesday/EnergyPlot.png",width = 10, height = 12.5)  
ggsave(filename = "Downloads/EnergyPlot.png", width = 10, height = 12.5)  

