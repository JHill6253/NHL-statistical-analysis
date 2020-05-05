# Hill and Billings Script
# Final Project 
# Date: 4/23/2020
# Description: We importing and cleaning NHL hockey team Statistics to help betters/Sports books set or bet on Odds for different future or team bets
# Source for this data is: https://www.hockey-reference.com/leagues   
# We are creating models based on AvAge, W and L, GF, GA, SOS, SO
#Questions will have 10 most likely to win Playoffs , Trends in overall league performance, areas where worst 5 avg team  are weakests .

#------------ Data Import from CSV------------#
statsTeam.2019<- read.csv('NHLDataTeam.csv',skip =1,header =TRUE)
statsTeam.2018<- read.csv('NHLDataTeam2018.csv',skip=1,header =TRUE)
statsTeam.2017<- read.csv('NHLDataTeam2017.csv',skip=1,header =TRUE)
#------------End of Data Import from CSV------------#


#------------ Data minipulation------------#
#Selecting only the data we would like 
library(dplyr)
statsTeam.2019<-statsTeam.2019 %>%
    select(Rk,X,AvAge,W,L, GF,GA,SOS,SO)
statsTeam.2018<-statsTeam.2018 %>%
    select(Rk,X, AvAge,W,L, GF,GA,SOS,SO)
statsTeam.2017<-statsTeam.2017 %>%
    select(Rk,X,AvAge,W,L, GF,GA,SOS,SO)
#Renaming variables so they are independant 
statsTeam.2019<- statsTeam.2019 %>%
    rename(RK2019=Rk,X=X ,AvAge2019=AvAge,W2019=W,L2019=L, GF2019=GF,GA2019=GA,
        SOS2019=SOS,SO2019=SO)

statsTeam.2018<- statsTeam.2018 %>%
    rename(RK2018=Rk,X=X ,AvAge2018=AvAge,W2018=W,L2018=L, GF2018=GF,GA2018=GA,
        SOS2018=SOS,SO2018=SO)

statsTeam.2017<- statsTeam.2017 %>%
    rename(RK2017=Rk,X=X ,AvAge2017=AvAge,W2017=W,L2017=L, GF2017=GF,GA2017=GA,
        SOS2017=SOS,SO2017=SO)

# merging datasets
statsTeam.2017thru2018 <- merge(statsTeam.2017,statsTeam.2018, by = 'X', all= T)
statsTeam.2017thru2019 <- merge(statsTeam.2017thru2018,statsTeam.2019, by = 'X', all= T)

# Averages 
teamAvg<- statsTeam.2017thru2019 %>%
    select(X,RK2017,RK2018,RK2019,AvAge2017,AvAge2018,AvAge2019,W2017,W2018,W2019,L2017,L2018,L2019,GF2017,GF2018,GF2019,GA2019,GA2017,GA2018,SOS2017,SOS2018,SOS2019,SO2017,SO2018,SO2019)
#Averages and PlyfOdds higher number means likely hood to be in Playoffs
teamAvg <- teamAvg %>%
    group_by(X) %>%
    summarize(ARnk=round(sum(RK2017,RK2018,RK2019, na.rm= TRUE)/3, 7),AAVEAge=round(sum(AvAge2017,AvAge2018,AvAge2019,na.rm= TRUE)/3, 7),AW=round(sum(W2017,W2018,W2019,na.rm= TRUE)/3, 7),AL=round(sum(L2017,L2018,L2019,na.rm= TRUE)/3, 7),
    AGF=round(sum(GF2017,GF2018,GF2019,na.rm= TRUE)/3, 7),
    AGA=round(sum(GA2017,GA2018,GA2019,na.rm= TRUE)/3, 7),ASOS=round(sum(SOS2017,SOS2018,SO2019,na.rm= TRUE)/3, 7),ASO=round(sum(SO2017,SO2018,SO2019,na.rm= TRUE)/3, 7),PlyfOdds= round(sum(AAVEAge,AW)/AL)*sum(ASOS,ASO))

    Top10toplyf<-filter(teamAvg,PlyfOdds>=23)
#------------ End of Data Minipulation------------#

# NOTE: Graphics and models are used to show knowlege of use. Actual Graphs 
# and Models created on Tableau

#----------Graphics and Models--------------#  

#Top 10 Teams
library(ggplot2)

ggplot(Top10toplyf,aes(x=X,y=PlyfOdds)) +
    geom_bar(stat = 'identity')

# Linear regression model for Shutouts and plyoff odds 

plot(teamAvg$ASO,teamAvg$PlyfOdds)

cor(teamAvg$ASO,teamAvg$PlyfOdds)

reg<-lm(PlyfOdds~ASO, data= teamAvg)

abline(reg)

summary(reg)

names(reg)

reg$fitted
plot(teamAvg$ASO,reg$fitted)
reg$coef[1]+reg$coef[2]*5.5


predict(reg,list(ASO=9))

#----------End of Graphics and Models--------------#  

write.csv(teamAvg,'teamAvgs.csv')