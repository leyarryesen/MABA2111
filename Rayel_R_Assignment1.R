# R Assignment 1
# Rosario Rayel
# 11/7/2018

#1.WHO dataset
WHO<-read.csv('WHO.csv')

head(WHO,5)

#D. Find the country with the lowest literacy rate.
literacy_min <- min(WHO$LiteracyRate,na.rm = TRUE)

row_lowest_literacy <- subset(WHO, LiteracyRate==literacy_min)

print(row_lowest_literacy$Country)


#E. Find the richest country in Europe based on GNI.
europe<-subset(WHO,Region=='Europe')

max_gni_europe<-max(europe$GNI,na.rm = TRUE)

row_with_max_gni_europe<-subset(europe, GNI==max_gni_europe)

print(row_with_max_gni_europe$Country)


#F. The mean life expectancy of countries in Africa.
Africa<-subset(WHO,Region=='Africa')

mean_life_expectancy_Africa<-mean(Africa$LifeExpectancy,na.rm = TRUE)

mean_life_expectancy_Africa

#G. Find the number of countries with population over 10M
Countries_with_population_over_10M<-subset(WHO,Population>10000)

dim(Countries_with_population_over_10M)[1]


#H. Find names of the country (top 5) in the Americas with the highest child mortality rate.
Americas<-subset(WHO,Region=='Americas')

index_ordered_childmortality_Americas<-order(Americas$ChildMortality,decreasing = TRUE)

Americas_ordered_by_childmortality<-Americas[index_ordered_childmortality_Americas,]

Top_5_childmortality_Americas_head<-head(Americas_ordered_by_childmortality,5)

Top_5_childmortality_Americas_indexing<-Americas_ordered_by_childmortality[1:5,]

Top_5_childmortality_Americas_indexing$Country


#2. The NBA Historical Performance Dataset
install.packages("readxl")

library('readxl')

historical_nba<-read_excel('Historical NBA Performance.xlsx')

#A. Find the year the Chicago Bulls has its highest winning percentage
bulls<-subset(historical_nba,Team =='Bulls')
bulls_highest_winpct<-max(bulls$'Winning Percentage')
bulls_row_highest_winpct<-subset(bulls,'Winning Percentage'==bulls_highest_winpct)
bulls_row_highest_winpct$Year

#B. Find the teams with an even win-loss record(i.e. the teams whose recorded win pct for the year are 0.500).
teams_even<-subset(historical_nba,'Winning Percentage'==0.5)
teams_even

#3. The season stats dataset
season_stats<-read.csv('Seasons_Stats.csv')

#A. Find the player with the highest three point attempt rate.
highest_x3p<-max(Season_Stats$X3P.,na.rm = TRUE)
players_with_highest_x3p<-subset(Season_Stats,X3P.==highest_x3p)
players_with_highest_x3p

#B. Find the players with the highest free throw rate in season.
highest_ftr<-max(season_stats$FT.,na.rm = TRUE)
subset(season_stats,FT.==highest_ftr)


#C. Find the year The King recorded his highest number of points.
LBJ<-subset(season_stats,Player=='LeBron James')
LBJ_max_pts<-max(LBJ$PTS,na.rm = TRUE)
subset(LBJ,PTS==LBJ_max_pts)$Year

#D. The year the GOAT scored his most number of points.
MJ<-subset(season_stats,Player=='Michael Jordan*')
subset(MJ,PTS==max(MJ$PTS))$Year

#E. Get Kobe's PER when his MP(minutes played) is the lowest.
Kobe<-subset(season_stats,Player=='Kobe Bryant')
subset(Kobe, MP== min(Kobe$MP))$PER

#4. The National Universities Rankings
# Load the dataframe
universities<-read.csv('National Universities Rankings.csv')

#A. Find the university with the most number of undergrad.
print(universities[which.max(universities$Undergrad.Enrollment),]$Name)


#B. Find the average tuition of the top ten universities.
top_10<-universities[order(universities$Rank),][1:10,]
top_10$tuition_no_dollar<- gsub (pattern = "\\$|\\,",replacement = "",top_10$Tuition.and.fees)
mean(as.numeric(top_10$tuition_no_dollar))

     