#put in packages 
library(tidyverse)
library(dplyr)
library(tidyr)

#choose the stats that I want
nbasalaries <- NBA_Salaries
nbastats <- NBA_Stats_2018_2019
NBA <- merge(nbastats, nbasalaries, by.x="Player", by.y="Player") #used Player twice because that is the only common variable#
nba <- select(, 'Player','Pos','G','GS','FG.','X3P.','X3PA','FG','TRB','AST','STL','BLK','TOV','PTS')
#calculate average points per game

nba$PTS<-as.numeric(as.character(nba$PTS))
nba$G<-as.numeric(as.character(nba$G))
nba <- mutate(nba,avg_point=PTS / G)

#filter the data and build my proto pitch
top<-function(x){
  x<-filter(x,as.numeric(as.character(X3PA))>200&as.numeric(as.character(X3P.))>0.35&as.numeric(as.character(FG))>0.45&avg_point>15)
}
pitch<-top(nba)
pitch<-mutate(pitch,)

#transform columns into numeric in order to compute
for (i in 3:ncol(pitch)) pitch[,i]<-as.numeric(as.character(pitch[,i]))

#calculate average stats
pitch<-mutate(pitch,avg_trb=TRB/G)
pitch<-mutate(pitch,avg_ast=AST/G)
pitch<-mutate(pitch,avg_stl=STL/G)
pitch<-mutate(pitch,avg_blk=BLK/G)
pitch<-mutate(pitch,avg_tov=TOV/G)

#rank players according to different avg_stats by ascnding order
pitch<-pitch[order(pitch$avg_point),]
pitch$point_score<-1:nrow(pitch)
pitch<-pitch[order(pitch$avg_trb),]
pitch$trb_score<-1:nrow(pitch)
pitch<-pitch[order(pitch$avg_ast),]
pitch$ast_score<-1:nrow(pitch)
pitch<-pitch[order(pitch$avg_stl),]
pitch$stl_score<-1:nrow(pitch)
pitch<-pitch[order(pitch$avg_blk),]
pitch$blk_score<-1:nrow(pitch)
pitch<-pitch[order(pitch$avg_tov),]
pitch$tov_score<-1:nrow(pitch)

#find the total score of each player
pitch<-mutate(pitch,total_score=point_score+trb_score+ast_score+stl_score+
                blk_score-tov_score)

#assign salary to pitch and find the top cost-effective players
salary<-select(NBA,c("X2019.20","Player"))
final_pitch<-left_join(pitch,salary,by="Player")
final_pitch<-distinct(final_pitch,Player,.keep_all = TRUE)
final_pitch<-rename(final_pitch,"Salary"="X2019.20")
final_pitch$Salary<-as.numeric(gsub("[$,]","",final_pitch$Salary))
final_pitch<-mutate(final_pitch,cost_effective=total_score/Salary)
fin_pitch<-fin_pitch[order(-fin_pitch$cost_effective),]

#pick 12 players and the total salary should be under 120 million salary cap
fin_pitch$index<-1:nrow(fin_pitch)
as.array(fin_pitch$index)
team<- function(x){
  sa <- 0 
  for (i in 1:nrow(x)){
    sa = sa + x[i,2]
    if (sa<=120000000){
      print(paste("I pick",x[i,1]))
    } else{
      break
    }
  }
  print(paste("The total salary is ",sa)) 
}
team(fin_pitch)
max(fin_pitch[1:11,2])
#Since my salary cap exceeds 12million on my 12th player, I will drop Nikola 
#Vucevic,who has the highest salary among top 11 players, and add Zack Lavine 
#and JJ. Reddick to my team. 