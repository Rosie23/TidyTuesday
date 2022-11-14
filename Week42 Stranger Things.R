#Load Data and Packages
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load(2022, week = 42)
episodes <- tuesdata$episodes
dialogue <- tuesdata$stranger_things_all_dialogue

#Get Stranger Things Font
library(showtext)
font_add(family="brain-james", regular = 'OPTIBrianJamesBoldCond.otf')
showtext_auto()

#Stranger Things Theme
stranger_things_theme <- function(main_col='#ff1515', background="black", my_font="brain-james"){
  theme_minimal()+
  theme(
  strip.background = element_blank(),
  strip.text = element_text(family = my_font, colour = main_col),
  panel.background = element_rect(fill = background),
  plot.background = element_rect(fill = background),
  legend.background = element_rect(fill=background),
  legend.key = element_rect(fill=background),
  legend.text = element_text(family = my_font, colour = main_col),
  panel.grid = element_blank(),
  axis.line=element_blank(),
  axis.title = element_text(family = my_font, colour = main_col),
  axis.text.y = element_blank(),
  axis.text.x = element_text(family = my_font, colour = main_col),
  axis.ticks = element_blank(),
  plot.title = element_text(family = my_font, colour = main_col, hjust = 0.5, vjust=1, size=18),
  plot.subtitle = element_text(family = my_font, colour = main_col,hjust = 0.5, size = 14),
  plot.caption = element_text(family = my_font, colour = main_col),
  legend.position = 'bottom'
  )
}

## Monsters of Stranger Things
monsters <- c("Demogorgon|demogorgon|Demodogs|demodogs", "Mind Flayer|Mind flayer|mind flayer", "Vecna|vecna")
names(monsters) <- c('Demogorgon', "Mind Flayer","Vecna")

monster_cols <- c("#ff1515",'#3a5fe5', '#40A156')
names(monster_cols) <- names(monsters)

season_labels <- paste('Season',1:max(dialogue$season))
names(season_labels) <- 1:max(dialogue$season)

monsters_say_what <- dialogue %>%
  mutate(Word1 = str_count(dialogue, pattern=monsters[1]),
         Word2 = str_count(dialogue, pattern=monsters[2]),
         Word3 = str_count(dialogue, pattern=monsters[3])) %>%
  pivot_longer(cols=Word1:Word3, values_to = 'Occurances',names_to='Type') %>%
  mutate(Type = case_when(Type == 'Word1' ~ names(monsters)[1],
                          Type == 'Word2' ~ names(monsters)[2],
                          Type == 'Word3' ~ names(monsters)[3])) %>%
  drop_na(Occurances) %>%
  select(season, episode, Type, Occurances, start_time) %>%
  filter(Occurances != 0) %>%
  uncount(weights = Occurances)


## Heroes of Strangers things
heroes <- c("Eleven ", "Hopper", "Mike", "Steve")
names(heroes) <- heroes

hero_cols <- c("#ff1515",'#3a5fe5', '#40A156',"#F69D2C")
names(hero_cols) <- names(heroes)

heros_say_what <- dialogue %>%
  mutate(Word1 = str_count(stage_direction, pattern=heroes[1]),
         Word2 = str_count(stage_direction, pattern=heroes[2]),
         Word3 = str_count(stage_direction, pattern=heroes[3]),
         Word4 = str_count(stage_direction, pattern=heroes[4])) %>%
  pivot_longer(cols=Word1:Word4, values_to = 'Occurances',names_to='Type') %>%
  mutate(Type = case_when(Type == 'Word1' ~ names(heroes)[1],
                          Type == 'Word2' ~ names(heroes)[2],
                          Type == 'Word3' ~ names(heroes)[3],
                          Type == 'Word4' ~ names(heroes)[4])) %>%
  drop_na(Occurances) %>%
  select(season, episode, Type, Occurances, start_time) %>%
  filter(Occurances != 0) %>%
  uncount(weights = Occurances)


## Make Plots
make_rug_plot <- function(df, word_cols, plot_title){
  rug_plot <- ggplot(df, aes(start_time, color=Type))+geom_rug(length = unit(1, "npc"))+
    scale_color_manual(values=word_cols)+
    stranger_things_theme()+
    facet_grid(rows=vars(episode), cols=vars(season), scales="free_x",switch='y',
               labeller = labeller(season = season_labels))+
    labs(title=plot_title)+
    ylab("Episode")+xlab("")+theme(axis.text.x = element_blank(), legend.position = 'none')
}

make_density_plot <- function(df, word_cols){
  density_plot = ggplot(df, aes(start_time, fill=Type, col=Type))+geom_density(alpha=0.5)+
    scale_fill_manual(values=word_cols)+scale_colour_manual(values=word_cols)+
    stranger_things_theme()+theme(strip.text = element_blank())+
    scale_x_time(breaks = scales::breaks_width("20 min"), labels=scales::time_format(format='%H:%M'))+
    facet_grid(cols=vars(season), scales="free_x")+
    xlab("Episode Duration")+ylab('')
}

make_freq_plot <- function(df, word_cols){
  hist_plot = ggplot(df, aes(start_time, fill=Type, col=Type))+
    geom_freqpoly(binwidth=600)+
    scale_fill_manual(values=word_cols)+scale_colour_manual(values=word_cols)+
    stranger_things_theme()+theme(strip.text = element_blank(), axis.text.x = element_text(size=5, angle=45))+
    scale_x_time(breaks = scales::breaks_width("20 min"), labels=scales::time_format(format='%H:%M'))+
    facet_grid(cols=vars(season), scales="free_x")+
    xlab("Episode Duration")+ylab('')
}


hero_rug_plot <- make_rug_plot(heros_say_what, hero_cols, "Heroes")
monster_rug_plot <- make_rug_plot(monsters_say_what, monster_cols, "Monsters")
hero_freq_plot <- make_freq_plot(heros_say_what, hero_cols)
monster_freq_plot <- make_freq_plot(monsters_say_what, monster_cols)


## Bring all plots together
library(patchwork)

patchwork_plots <- hero_rug_plot + monster_rug_plot + hero_freq_plot + monster_freq_plot

layout <- c(
  area(t = 1, l = 1, b = 7, r = 5),
  area(t = 1, l = 6, b = 7, r = 10),
  area(t = 8, l = 1, b = 10, r = 5),
  area(t = 8, l = 6, b = 10, r = 10)
)

plot(layout)

my_plot <- patchwork_plots+
  plot_layout(design = layout)+
  #inset_element(p = my_image,
  #              left = 0.5,
  #              bottom = 0.55,
  #              right = 0.95,
  #              top = 0.95)+
  plot_annotation(title = 'STRANGER THINGS',
                  subtitle = "Frequency of when characeters appear during Stranger Things",
                  theme =  stranger_things_theme()+theme(plot.title = element_text(size=40)),
                  caption = "@Rosie_Griffiths | #TidyTuesday Week 42 2022 | Source: 8flix.com")

ggsave(my_plot, filename = "Stranger_Things_plot4.png", width=9, height=8)

#ggsave(plot=hero_plot, filename="Stranger Things Heroes.png")
#ggsave(plot=monster_plot, filename="Stranger Things Monsters.png")

