#Installing packages
install.packages("tidyverse")
install.packages("devtools")
install.packages("remotes")
##remotes::install_version("SDMTools", "1.1-221") Doesn't work
devtools::install_github("statsbomb/SDMTools")
devtools::install_github("statsbomb/StatsBombR")

#Library
library(tidyverse)
library(StatsBombR)

#Pulling Free Data 
Comp <- FreeCompetitions() %>% #%>% takes the output of the FreeCompetitions() function and passes it as the first argument to the next function in the sequence.
filter(competition_id==37 & season_name=="2020/2021") #grabs the data I want
Matches <- FreeMatches(Comp) 
FAWSL <- free_allevents(MatchesDF = Matches, Parallel = T)  #Created a dataframe
FAWSL = allclean(FAWSL) #Extract loads of Information like x/y coordinates

View(FAWSL)


##-------------------------------------------Data Use Case 1: Goals and Shots------------------------------------------##
shots_goals = FAWSL %>% 
  group_by(team.name)%>% #groups data by team
  summarise(shots=sum(type.name=="Shot",na.rm=TRUE),
            #summarise takes whatever operations we give it and produces a new separate table, usually used after group_by
            #shots: telling it to create a new column called 'shots' that sums up all the rows under the 'type.name' column that contain the word "shot" na.rm = TRUE tells it to ignore any NAs within the column
  goals=sum(shot.outcome.name=="Goal",na.rm=TRUE))

#For per game basis
shots_goals = StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE)/n_distinct(match_id),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)/n_distinct(match_id))
#adding in 'n_district(match_id)' means that we are dividing the number of shots/goals by each distinct (or unique) instance of a match, for every team we are diving the numbers per game


##-----------------------------------------Data Use Case 2: From Data to a Chart---------------------------------------------------------##
library(ggplot2)

ggplot(data = shots_goals, 
       aes(x=reorder(team.name,shots),y=shots))+
  geom_bar(stat="identity",width = 0.5)+ #format as a bar chart
  labs(y="Shots")+ #relabels the shot axis
  theme(axis.title.y=element_blank())+ #removes the title for axis
  scale_y_continuous(expand=c(0,0))+ #cut down the spaces between the bars and the edge of the plot
  coord_flip()+ #flips the plot, with the bars now going horizontally instead
  theme_SB() #StatsBomb's own visual aesthetic



##----------------------------------------Data Use Case 3: Player Shots Per 90-----------------------------------------------------------##
player_shots = FAWSL%>%
  group_by(player.name,player.id)%>%
  summarise(shots = sum(type.name=="Shot",na.rm=TRUE))

player_minutes = get.minutesplayed(FAWSL)

player_minutes = player_minutes%>%
  group_by(player.id)%>%
  summarise(minutes=sum(MinutesPlayed))

player_shots = left_join(player_shots,player_minutes) #left_join allows us to combine our shots table and our minutes table, with the player.id acting as a reference point
player_shots = player_shots%>%mutate(nineties=minutes/90) #mutate is a dplyr function that creates a new column
player_shots = player_shots%>%mutate(shots_per90 = shots/nineties)



##----------------------------------------Data Use Case 4: Plotting Passes----------------------------------------------##
devtools::install_github("FCrSTATS/SBpitch") #Pre-made pitch visualisation

#Purpose: To plot Fran Kirby's completed passes into the box for the 20/21 FAWSL

library(SBpitch)

passes=FAWSL%>%
  filter(type.name=="Pass"&is.na(pass.outcome.name)&
           player.id==4641)%>%
  filter(pass.end_location.x>=102 & pass.end_location.y<=62 &
           pass.end_location.y>18) #Filtering to passes within the box

#Visuals
create_Pitch()+
  geom_segment(data=passes, aes(x=location.x ,y=location.y,
                                xend=pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", size=0.5, colour = "#000000", arrow =
                 arrow(length=unit(0.07, "inches"), ends="last", type = "open"))+ #Creates an arrow from one point to an end point
  labs(title = "Fran Kirby, Completed Box Passes", subtitle = "WSL, 2020-21")+ #creates a title and subtitle
  scale_y_reverse()+ #reverses the y axis, else data would be plotted on the wrong side
  coord_fixed(ratio=105/100) #fixes the plot to a certain aspect ratio of your choice

#ggplot 2 cheatsheet: https://raw.githubusercontent.com/rstudio/cheatsheets/master/pngs/data-visualization.png


#Other useful packages
#1. soccermatics
devtools::install_github("jogall/soccermatics")
library(soccermatics)
#Link: https://github.com/JoGall/soccermatics


#2. ggsoccer
install.packages("ggsoccer")
#or
remotes::install_github("torvaney/ggsoccer")

#3. ggrepel: useful for when you're having issues with overlapping labels on a chart
#Link: https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html

#4. gganimate: create animated charts
#Link: https://github.com/thomasp85/gganimate



##---------------------------------------------Data Use Case 5--------------------------------------------------##

library(tidyverse)
library(StatsBombR)
xGA = FAWSL %>%
  filter(type.name=="Shot") %>% #filtering the data to just "Shots", as they are the only event with xG events
  select(shot.key_pass_id, xGA = shot.statsbomb_xg) 
#Select() allows you to choose which columns you want to, well
#select, from your daata, as not all are always necessary - especially with
#big datasets. First we are selecting the shot.key_pass_id column, which
#is a variable attached to shots that is just the ID of the pass that created
#the shot. You can also rename columns within select() which is what we
#are doing with xGA = shot.statsbomb_xg. This is so that, when we join it
#with the passes, it already has the correct name.

shot_assists = left_join(FAWSL, xGA, by = c("id" = "shot.key_pass_id"))%>% # lef_join() lets you combine the columns from two diferent DFs by using two columns within either side of the join as reference keys.
  select(team.name, player.name, player.id, type.name, pass.shot_assist, pass.goal_assist, xGA)%>%
  filter(pass.shot_assist==TRUE|pass.goal_assist==TRUE)

##Part2
player_xGA = shot_assists %>%
  group_by(player.name, player.id, team.name) %>%
  summarise(xGA = sum(xGA, na.rm = TRUE)) #1 Grouping by player and summing their total xGA for the season.


player_xG = FAWSL %>%
  filter(type.name=="Shot") %>%
  filter(shot.type.name!="Penalty" | is.na(shot.type.name)) %>%
  group_by(player.name, player.id, team.name) %>%
  summarise(xG = sum(shot.statsbomb_xg, na.rm = TRUE)) %>%
  left_join(player_xGA) %>%
  mutate(xG_xGA = sum(xG+xGA, na.rm =TRUE) ) #2 Filtering out penalties and summing each player's xG, then joining with
#the xGA and adding the two together to get a third combined column.


player_minutes = get.minutesplayed(FAWSL)

player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed)) #3 Getting minutes played for each player. If you went through the earlier data use cases in this guide you will have done this already.


player_xG_xGA = left_join(player_xG, player_minutes) %>%
  mutate(nineties = minutes/90,
         xG_90 = round(xG/nineties, 2),
         xGA_90 = round(xGA/nineties,2),
         xG_xGA90 = round(xG_xGA/nineties,2) ) #4 Joining the xG/xGA to the minutes, creating the 90s and dividing each stat by the 90s to get xG per 90 etc.


chart = player_xG_xGA %>%
  ungroup() %>%
  filter(minutes>=600) %>%
  top_n(n = 15, w = xG_xGA90) 

chart<-chart %>%
  select(1, 9:10)%>%
  pivot_longer(-player.name, names_to = "variable", values_to = "value") %>%
  filter(variable=="xG_90" | variable=="xGA_90") #make chart neater


##GGplot
ggplot(chart, aes(x =reorder(player.name, value), y = value, fill=fct_rev(variable))) + #1
  geom_bar(stat="identity", colour="white")+
  labs(title = "Expected Goal Contribution", subtitle = "FA Women's Super League, 2020-21",
       x="", y="Per 90", caption ="Minimum 600 minutes\nNPxG = Value of shots taken (no penalties)\nxG assisted = Value of shots assisted")+
  theme(axis.text.y = element_text(size=14, color="#333333", family="Source Sans Pro"),
        axis.title = element_text(size=14, color="#333333", family="Source Sans Pro"),
        axis.text.x = element_text(size=14, color="#333333", family="Source Sans Pro"),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour ="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title=element_text(size=24, color="#333333", family="Source Sans Pro" , face="bold"),
        plot.subtitle=element_text(size=18, color="#333333", family="Source Sans Pro", face="bold"),
        plot.caption=element_text(color="#333333", family="Source Sans Pro", size =10),
        text=element_text(family="Source Sans Pro"),
        legend.title=element_blank(),
        legend.text = element_text(size=14, color="#333333", family="Source Sans Pro"),
        legend.position = "bottom") + #2
  scale_fill_manual(values=c("#3371AC", "#DC2228"), labels = c( "xG Assisted","NPxG")) + #3
  scale_y_continuous(expand = c(0, 0), limits= c(0,max(chart$value) + 0.3)) + #4
  coord_flip()+ #5
  guides(fill = guide_legend(reverse = TRUE)) #6



##-------------------------------------------Data Use Case 6: Heatmaps-------------------------------------------------##



heatmap = FAWSL %>%mutate(location.x = ifelse(location.x>120, 120, location.x),
                           location.y = ifelse(location.y>80, 80, location.y),
                           location.x = ifelse(location.x<0, 0, location.x),
                           location.y = ifelse(location.y<0, 0, location.y))

heatmap$xbin <- cut(heatmap$location.x, breaks = seq(from=0, to=120, by = 20),include.lowest=TRUE )
heatmap$ybin <- cut(heatmap$location.y, breaks = seq(from=0, to=80, by = 20),include.lowest=TRUE)



heatmap = heatmap%>%
  filter(type.name=="Pressure" | duel.type.name=="Tackle" |
           type.name=="Foul Committed" | type.name=="Interception" |
           type.name=="Block" ) %>%
  group_by(team.name) %>%
  mutate(total_DA = n()) %>%
  group_by(team.name, xbin, ybin) %>%
  summarise(total_DA = max(total_DA),
            bin_DA = n(),
            bin_pct = bin_DA/total_DA,
            location.x = median(location.x),
            location.y = median(location.y)) %>%
  group_by(xbin, ybin) %>%
  mutate(league_ave = mean(bin_pct)) %>%
  group_by(team.name, xbin, ybin) %>%
  mutate(diff_vs_ave = bin_pct - league_ave) 


library(grid)
defensiveactivitycolors <- c("#dc2429", "#dc2329", "#df272d", "#df3238", "#e14348", "#e44d51",
                                      "#e35256", "#e76266", "#e9777b", "#ec8589", "#ec898d", "#ef9195",
                                      "#ef9ea1", "#f0a6a9", "#f2abae", "#f4b9bc", "#f8d1d2", "#f9e0e2",
                                      "#f7e1e3", "#f5e2e4", "#d4d5d8", "#d1d3d8", "#cdd2d6", "#c8cdd3", "#c0c7cd",
                                      "#b9c0c8", "#b5bcc3", "#909ba5", "#8f9aa5", "#818c98", "#798590",
                                      "#697785", "#526173", "#435367", "#3a4b60", "#2e4257", "#1d3048",
                                      "#11263e", "#11273e", "#0d233a", "#020c16") #colors used on heatmap
                                      


ggplot(data= heatmap, aes(x = location.x, y = location.y, fill = diff_vs_ave, group =diff_vs_ave)) +
  geom_bin2d(binwidth = c(20, 20), position = "identity", alpha = 0.9) + #2 : 'geom_bin2d' is what will create the heatmap itself
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  annotate("point", x = 12 , y = 40, colour = "white", size = 1.05) +
  annotate("point", x = 108 , y = 40, colour = "white", size = 1.05) +
  annotate("path", colour = "white", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  annotate("point", x = 60 , y = 40, colour = "white", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
  annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") + #Everything up to here is what is drawing the pitch. 
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=22,family="Source Sans Pro"),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5,
                                  family="Source Sans Pro", colour = "black", hjust = 0.5),
        legend.direction = "vertical",
        axis.ticks=element_blank(),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Source Sans Pro")) + #4
  scale_y_reverse() + #5: Reversing the y axis so the pitch is the correct way round along that axis
  scale_fill_gradientn(colours = defensiveactivitycolors, trans = "reverse", labels =
                         scales::percent_format(accuracy = 1), limits = c(0.03, -0.03)) + #6
  labs(title = "Where Do Teams Defend vs League Average?", subtitle = "FA Women's Super
League, 2020/21") + #7
  coord_fixed(ratio = 95/100)+
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="last",
                                                 length=unit(2.55,"mm")), gp=gpar(col="black", fill=NA, lwd=2.2)),
                    xmin=25, xmax = 95, ymin = -83, ymax = -83) + #9
  facet_wrap(~team.name)+ #10
  guides(fill = guide_legend(reverse = TRUE))#11



##---------------------------------------------------Data Use Case 7: Shot Maps-------------------------------------------------##

shots = FAWSL %>%
  filter(type.name=="Shot" & (shot.type.name!="Penalty" | is.na(shot.type.name)) & player.name=="Samantha May Kerr") #1

shotmapxgcolors <- c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960", "#FCDC5F",
                              "#F5B94D", "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608", "#BF0000", "#7F0000", "#5F0000") 
                              

ggplot() +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  # add penalty spot right
  annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
  annotate("path", colour = "black", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  # add centre spot
  annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_point(data = shots, aes(x = location.x, y = location.y, fill = shot.statsbomb_xg, shape = shot.body_part.name),
             size = 6, alpha = 0.8) + 
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.position = "top",
        legend.title=element_text(size=22,family="Source Sans Pro"),
        legend.text=element_text(size=20,family="Source Sans Pro"),
        legend.margin = margin(c(20, 10, -85, 50)),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5, family="Source Sans
Pro", colour = "black", hjust = 0.5),
        legend.direction = "horizontal",
        axis.ticks=element_blank(),
        aspect.ratio = c(65/100),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Source Sans Pro")) +
  labs(title = "Sam Kerr, Shot Map", subtitle = "FA Women's Super League, 2020/21") + #4
  scale_fill_gradientn(colours = shotmapxgcolors, limit = c(0,0.8), oob=scales::squish, name = "Expected Goals
Value") +
  scale_shape_manual(values = c("Head" = 21, "Right Foot" = 23, "Left Foot" = 24), name ="") + #6
  guides(fill = guide_colourbar(title.position = "top"),
         shape = guide_legend(override.aes = list(size = 7, fill = "black"))) + #7
  coord_flip(xlim = c(85, 125)) 



