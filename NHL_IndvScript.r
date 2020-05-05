# Hill and Billings Script
# Final Project 
# Date: 4/23/2020
# Description: We importing and cleaning NHL hockey individual Statistics based on three years of
#   data provided by poolexpert.com to create models to help betters choose players for fantasy leagues and choosing future/prop bets on players 
# Analysis includes: Odds of winning MVP, Best player per position, AVG of three year.

#------------ Data Cleaning for analysis------------#

#Loading Files

stats.2019<- read.csv('NHLDataSkaters.csv')
stats.2018<- read.csv('NHLDataSkaters2018.csv')
stats.2017<- read.csv('NHLDataSkaters2017.csv')

#Selecting only the data we would like 
library(dplyr)
stats.2019<-stats.2019 %>%
    select(Name, Pos, GP,G,A,PIM,PP,PPA,OTG,GW,Hat.,S)
stats.2018<-stats.2018 %>%
    select(Name, Pos,GP,G,A,PIM,PP,PPA,OTG,GW,Hat.,S)
stats.2017<-stats.2017 %>%
    select(Name, Pos, GP,G,A,PIM,PP,PPA,OTG,GW,Hat.,S)
#Renaming variables so they are independant 
stats.2019<- stats.2019 %>%
    rename(Name=Name, Pos= Pos, GP.2019=GP,G.2019=G,A.2019=A,
        PIM.2019=PIM,PP.2019=PP,PPA.2019=PPA,OTG.2019=OTG,
        GW.2019=GW,Hat.2019=Hat.,S.2019=S)

stats.2018<- stats.2018 %>%
    rename(Name=Name, Pos=Pos, GP.2018=GP,G.2018=G,A.2018=A,
        PIM.2018=PIM,PP.2018=PP,PPA.2018=PPA,OTG.2018=OTG,
        GW.2018=GW,Hat.2018=Hat.,S.2018=S)

stats.2017<- stats.2017 %>%
    rename(Name=Name, Pos=Pos, GP.2017=GP,G.2017=G,A.2017=A,
        PIM.2017=PIM,PP.2017=PP,PPA.2017=PPA,OTG.2017=OTG,
        GW.2017=GW,Hat.2017=Hat.,S.2017=S)

# merging datasets
stats.2017thru2018 <- merge(stats.2017,stats.2018, by = c('Name','Pos'), all= T)
stats.2017thru2019 <- merge(stats.2017thru2018,stats.2019, by = c('Name','Pos'), all= T)
#----------- End of Cleaning for analysis------------#


#----------Corelations and relationships--------------#

#PerGame Odds for single game prop bets and Overall ODDS of winning MVP
    statAvgs <- stats.2017thru2019 %>%
        select(Name,Pos,GP.2017,GP.2018,GP.2019,G.2017,G.2018,G.2019,A.2017,A.2018,A.2019,PIM.2017,PIM.2018,PIM.2019,
        OTG.2017,OTG.2018,OTG.2019,GW.2017,GW.2018,GW.2019,Hat.2017,Hat.2018,Hat.2019)
    #Per game stats 
    statAvgs<- statAvgs %>%
    group_by(Name,Pos) %>%
    summarize(TGP = sum(GP.2017,GP.2018,GP.2019, na.rm= TRUE),AG=sum(G.2019,G.2018,G.2019, na.rm=TRUE)/TGP,AA=sum(A.2019,A.2018,A.2019, na.rm=TRUE)/TGP,
    APIM=sum(PIM.2019,PIM.2018,PIM.2019, na.rm=TRUE)/TGP,AOTG=sum(OTG.2019,OTG.2018,OTG.2019, na.rm=TRUE)/TGP,AGW=sum(GW.2019,GW.2018,GW.2019, na.rm=TRUE)/TGP,
    AHat=sum(Hat.2019,Hat.2018,Hat.2019, na.rm=TRUE)/TGP,TPIM=sum(PIM.2019,PIM.2018,PIM.2019, na.rm=TRUE))
    # Odds of winning MVP
    MVP_Odds <- statAvgs %>%
        select(Name,Pos,TGP,AG,APIM,AA,AOTG,AGW,AHat)

    MVP_Odds<- filter(MVP_Odds,TGP >= 50)

    MVP_Odds <- MVP_Odds %>%
    group_by(Name,Pos) %>%
    summarize(Odds=sum(AHat*25,AGW,AOTG,AA, na.rm= TRUE)-(APIM*.05),TGP,AG,APIM,AA,AOTG,AGW,AHat)



#Best players per position 
    # use filter to show players above certain threshold based on MVP ODDS
    Centers<- filter(MVP_Odds,Pos == 'C')
    Centers<- filter(Centers, Odds>=.969)
    RWS<- filter(MVP_Odds,Pos=='RW')
    RWS <- filter(RWS,Odds>=0.827642276)
    LWS<- filter(MVP_Odds,Pos=='LW')
    LWS <- filter(LWS,Odds>=0.655801105)
    D<- filter(MVP_Odds,Pos=='D')
    D <- filter(D,Odds>=0.7222222)
    HatOddsgto<- filter(MVP_Odds,AHat>.0001)
    
    CentersRW<-rbind(Centers,RWS)
    LWD<-rbind(LWS,D)
    Top_Odds<-rbind(CentersRW,LWD)
    
    
    #Trends 

        
#----------End Corelations and relationships--------------#  

# NOTE: Graphics and models are used to show knowlege of use. Actual Graphs 
# and Models created on Tableau

#----------Graphics and Models--------------#  

#This code was not Used to create Models  
# Pos vs Odds of MVP
library(ggplot2)

ggplot(MVP_Odds,aes(x=Pos,y=Odds)) +
    geom_bar(stat = 'identity')


#MVP VS Hatrick Odds
Hat2MVP<-plot(HatOddsgto$AHat,HatOddsgto$Odds, main="Hat trick impact on MVP odds", xlab='Hat tricks',ylab='MVP Odds')

cor(HatOddsgto$AHat,HatOddsgto$Odds)

HatOdds<-lm(HatOddsgto$AHat~HatOddsgto$Odds )

abline(HatOdds)

summary(HatOdds)



# What position scores most overtime goals 



#Gamesplayeds impact on PIM


GvsPIMPlot<-plot(statAvgs$APIM,statAvgs$TGP, main="Games played impact on PIM", xlab='Games Played',ylab='PIM')

cor(statAvgs$TGP,statAvgs$APIM)

LMPIMvsG<-lm(statAvgs$APIM~statAvgs$TGP )

abline(LMPIMvsG)

summary(LMPIMvsG)


#Export of data sets for Tablaue analysis

write.csv(MVP_Odds,'MVP_Odds.csv')






#----------End Graphics and Models--------------#  