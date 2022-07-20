library(tidytuesdayR)
library(tidyverse)
library(ggbump)
library(ggflags)
library(countrycode)
library(patchwork)
library(directlabels)
library(RColorBrewer)
library(gridExtra)
library(showtext)

#tuesdata <- tidytuesdayR::tt_load(2022, week = 28)
flights <- tuesdata$flights
flights$STATE_NAME[flights$STATE_NAME == "Türkiye"] <- "Turkey"

#Get Country Codes
country_2_letters <- countrycode(flights$STATE_NAME %>% unique() %>% sort(),
                                 origin = "country.name",
                                 destination = "genc2c") %>% 
  tolower() %>% 
  set_names(flights$STATE_NAME %>% unique() %>% sort())


#Get Top 12 Countries
country_flights <- flights %>% 
  select(state = STATE_NAME, flights = FLT_TOT_1) %>% 
  group_by(state) %>% 
  summarise(flights = sum(flights)) %>%
  arrange(desc(flights))

top_12_country_flights <-country_flights[1:12,]
top_12_states <- as.character(top_12_country_flights$state)

#My Aesthetics
my_cols <- brewer.pal(12,"Paired")
my_cols[11] <- "#D8CC13" #change pale yellow to darker yellow

names(my_cols) <- top_12_states

font_add_google("Montserrat", family = "montserrat") #https://fonts.google.com
font_add_google("Poiret One", family = "poiretone")
font_add_google("Michroma", family = "michroma")

showtext_auto()


my_theme <-   theme(legend.position = "none",
                    plot.background = element_rect(fill=background_colour,colour = background_colour),
                    panel.background = element_rect(fill = background_colour, colour = background_colour),
                    panel.border = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(), 
                    axis.ticks = element_blank(),
                    plot.caption = element_text(colour=axis_colour,family="poiretone"),
                    plot.margin = margin(1, 1, 1, 1, unit = "line"),
                    plot.title = element_text(colour = axis_colour,face = "bold",hjust=0.5,size=16,family="montserrat"),
                    axis.title = element_text(colour = axis_colour,face="bold"),
                    plot.subtitle = element_text(colour = axis_colour,hjust=0.5,face = "bold",size=12,family="poiretone")
)

###Percentage Difference Plots####
get_percentage_diff <- function(values){
  if(length(values)!=2){
    print("Wrong number of values")
    break
  }
  val1 <- values[1]
  val2 <- values[2]
  perc_diff <- (val2/val1)*100
  return(perc_diff)
}

country_flights_2019_2020_month_top12 <- flights %>% 
  select(year=YEAR, month=MONTH_NUM, state = STATE_NAME, flights = FLT_TOT_1) %>% 
  filter(year %in% c(2019,2020) & state %in% top_12_states) %>%
  group_by(month, year, state) %>% 
  summarise(flights = sum(flights)) %>% 
  group_by(month, state) %>% 
  summarise(flights = get_percentage_diff(flights))

country_flights_2019_2021_month_top12 <- flights %>% 
  select(year=YEAR, month=MONTH_NUM, state = STATE_NAME, flights = FLT_TOT_1) %>% 
  filter(year %in% c(2019,2021) & state %in% top_12_states) %>%
  group_by(month,year, state) %>% 
  summarise(flights = sum(flights)) %>% 
  group_by(month, state) %>% 
  summarise(flights = get_percentage_diff(flights))

country_flights_2019_2022_month_top12 <- flights %>% 
  select(year=YEAR, month=MONTH_NUM, state = STATE_NAME, flights = FLT_TOT_1) %>% 
  filter(year %in% c(2019,2022) & state %in% top_12_states, month %in% c("01","02","03","04","05")) %>%
  group_by(month,year, state) %>% 
  summarise(flights = sum(flights)) %>% 
  group_by(month, state) %>% 
  summarise(flights = get_percentage_diff(flights))

country_flights_2019_2022_month_top12$month_combined <- as.numeric(country_flights_2019_2022_month_top12$month)+24
country_flights_2019_2021_month_top12$month_combined <- as.numeric(country_flights_2019_2021_month_top12$month)+12
country_flights_2019_2020_month_top12$month_combined <- as.numeric(country_flights_2019_2020_month_top12$month)

combined_data <- bind_rows(country_flights_2019_2021_month_top12,
                           country_flights_2019_2020_month_top12,
                           country_flights_2019_2022_month_top12)


combined_data$month_combined <- factor(combined_data$month_combined, levels = (1:29))

#Add country codes
combined_data <- combined_data %>% 
  mutate(team_2_letters = country_2_letters[state])


recover_plot <-ggplot(combined_data, aes(x=month_combined,y=flights, colour=state, group=state))+
  geom_line(size=1.5)+
  geom_dl(aes(label=state),method="last.qp",size=14)+
  scale_y_continuous(breaks = c(25,50,75,100))+
  scale_x_discrete(breaks = c(1:29),labels=c(rep(months,2),months[1:5]))+
  expand_limits(x = 34)+
  scale_colour_manual(values=my_cols)+
  scale_fill_manual(values=my_cols)+
  
  annotate("text",label="2020",x=6,y=110,colour=label_colour,size=4,fontface="bold")+
  annotate("text",label="2021",x=18,y=110,colour=label_colour,size=4,fontface="bold")+
  annotate("text",label="2022",x=27,y=110,colour=label_colour,size=4,fontface="bold")+
  
  geom_vline(xintercept = 12.5,colour="grey",size=1,alpha=0.5,linetype="longdash")+
  geom_vline(xintercept = 24.5,colour="grey",size=1,alpha=0.5,linetype="longdash")+
  
  
  labs(y="% Change compared to 2019\nof total flights taken",
       x="Month",
       title="Return to Normal",
       subtitle = "Percentage change in total flights for the top 12 busiest european
       countries in 2020-2021 compared to their pre-pandemic levels")+
  my_theme + 
  theme(axis.line= element_line(colour = axis_colour),
        axis.text.y = element_text(colour = axis_colour),
        axis.text.x = element_text(colour = axis_colour,angle=45,vjust=0.7))

recover_plot

####Ranked Plots######
maked_ranked_data <- function(date){
  ranked_data <- flights %>%
    select(state = STATE_NAME, day=FLT_DATE, flights = FLT_TOT_1) %>%
    filter(state %in% top_12_states & day == date) %>%
    group_by(day,state) %>% 
    summarise(flights = sum(flights)) %>% 
    group_by(day) %>% 
    mutate(rank = rank(flights, ties.method = "random"))%>% 
    ungroup()
  return(ranked_data)
}

countries_ranked <- data.frame(day=as.character(),state=as.character(),
                               flights=as.numeric(),rank=as.integer())

for (day in c("01","07","14","21","29")) {
  date <- paste0("2020-03-",day)
  tmp <- maked_ranked_data(date)
  countries_ranked <- rbind(countries_ranked,tmp)
}

#Get Day of Month
countries_ranked$day <- format(as.Date(countries_ranked$day,format="%Y-%m-%d"), format = "%d")

#Add country codes
countries_ranked <- countries_ranked %>% 
  mutate(team_2_letters = country_2_letters[state])

ranked_plot <- ggplot(countries_ranked,aes(x=day, y=rank, group = state, color = state, fill = state)) +
  geom_bump(aes(smooth = 10), size= 1.5, lineend = "round")+
  
  geom_flag(data = countries_ranked %>% filter(day == min(day)), 
            aes(country = team_2_letters),
            size = 8,
            color = "black") +
  geom_flag(data = countries_ranked %>% filter(day == max(day)), 
            aes(country = team_2_letters),
            size = 8,
            color = "black")+
  
  scale_colour_manual(values=my_cols)+
  scale_fill_manual(values=my_cols)+
  
  labs(y="",
       x="March 2020",
       title="Pandemic Lockdown",
       subtitle = "Top ranked europrean countries by total number of flights during March 2020")+
  my_theme + 
  theme(axis.line= element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = axis_colour,vjust=0.7))

#Combine Plots with Patchwork

patchwork_plots <- ranked_plot/recover_plot

patchwork_plots+
  plot_annotation(title = 'Post-Pandemic Air Travel Levels in\nthe Busiest European Countries',
                  theme =  my_theme & theme(plot.title = element_text(size = 22,family = "michroma")),
                  caption = "@Rosie_Griffiths | #TidyTuesday Week 28 2022 | Source: Eurocontrol")


ggsave(filename = "Documents/GitHub/TidyTuesday/FlightPlot.png", width = 8, height = 10)  




