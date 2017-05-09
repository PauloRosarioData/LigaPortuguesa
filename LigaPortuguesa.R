library(reshape)
library("EloRating")
library(dplyr)


#loads the files locally


fixtures<- read.csv("Calendario.csv")
teams<- read.csv("EquipasPortuguesas.csv")
results<- read.csv("P1.csv")

#Computes the current league standings

results$HomePoints <- ifelse(results$FTR == "H", 3,ifelse(results$FTR == "D", 1,0))
results$AwayPoints <- ifelse(results$FTR == "A", 3,ifelse(results$FTR == "D", 1,0))




HomeStats <- as.data.frame(summarise(group_by(results, HomeTeam), HomeScored=sum(FTHG),
                                     HomeSuffered=sum(FTAG), HomePoints=sum(HomePoints) ))


AwayStats <- as.data.frame(summarise(group_by(results, AwayTeam), AwayScored=sum(FTAG),
                                     AwaySuffered=sum(FTHG), AwayPoints=sum(AwayPoints) ))



teams$Points <- HomeStats$HomePoints + AwayStats$AwayPoints
teams$Scored <- HomeStats$HomeScored+ AwayStats$AwayScored
teams$Suffered <- HomeStats$HomeSuffered+ AwayStats$AwaySuffered


#Loads the elo list from the webiste clubelo.com

for(i in 1:dim(teams))
{

Date = Sys.Date()


api = paste("http://api.clubelo.com/",Date, sep = "")
mydata = read.csv(api)
Club = toString(teams$Club[i])
Rating = mydata[mydata$Club == Club,]$Elo

teams$Elo[i] = Rating
elorankings <- teams

myvars <- c("Club", "Elo")
elorankings <- teams[myvars]

print(Club)
print(i)
}


#Creating tables where the results are going to be stored simulation


Champion <- c()
Second <- c()
Third <- c()
Fourth <- c()
Fifth <- c()
sexto <- c()
septimo <- c()
oitavo <- c()
nono <- c()
decimo <- c()
onze <- c()
doze <- c()
treze <- c()
catorze<- c()
quinze <- c()
desazzeis <- c()
Penultimo<- c()
Ultimo <- c()
EmpateA3 <- 0
EmpateA2 <- 0
Standings <- teams

fixtures$HScored <- 0
fixtures$AScored <- 0

ll <- Sys.time()

#"""generates the EXPECTED goals for the remaining games"""




for(p in 1:dim(fixtures))
{
  fixtures$HElo[p] <- teams[teams$Original == toString(fixtures$Home[p]),]$Elo
  fixtures$AElo[p] <- teams[teams$Original == toString(fixtures$Away[p]),]$Elo
  
  fixtures$HomeProbability <- winprob( fixtures$HElo + 87, fixtures$AElo)
  fixtures$AwayProbability <- 1 -  fixtures$HomeProbability
  
  fixtures$HG[p] <- ifelse(fixtures$HomeProbability[p] < 0.5,
                           0.2 + 1.1*sqrt( fixtures$HomeProbability[p]/0.5),
                           1.69 / (1.12*sqrt(2 -fixtures$HomeProbability[p]/0.5)+0.18))
  
  fixtures$AG[p] <- ifelse(fixtures$HomeProbability[p] < 0.8,
                           -0.96 + 1/(0.1+0.44*sqrt((fixtures$HomeProbability[p]+0.1)/0.9)),
                           0.72*sqrt((1 - fixtures$AwayProbability[p])/0.3)+0.3)
  
}



for(z in 1:10000)
{



  Standings <- teams
  
  for(q in 1:dim(fixtures))
  {
    fixtures$HScored[q] <- rpois(1, lambda = fixtures$HG[q] )
    fixtures$AScored[q] <- rpois(1, lambda = fixtures$AG[q] )
    
  }
  
  
  
  #"""generates the points for the groupstages"""
  
  fixtures$HomePoints <- 0
  fixtures$HomePoints <- ifelse(fixtures$HScored == fixtures$AScored, 1, fixtures$HomePoints)
  fixtures$HomePoints <- ifelse(fixtures$HScored > fixtures$AScored, 3, fixtures$HomePoints)
  
  fixtures$AwayPoints <- 3
  fixtures$AwayPoints <- ifelse(fixtures$HScored == fixtures$AScored, 1, fixtures$AwayPoints)
  fixtures$AwayPoints <- ifelse(fixtures$HScored > fixtures$AScored, 0, fixtures$AwayPoints)
  
  HomeStats <- as.data.frame(summarise(group_by(fixtures, Home), HomeScored=sum(HScored),
                                       HomeSuffered=sum(AScored), HomePoints=sum(HomePoints) ))
  HomeStats$Team <- HomeStats$Home
  
  AwayStats <- as.data.frame(summarise(group_by(fixtures, Away), AwayScored=sum(AScored),
                                       AwaySuffered=sum(HScored), AwayPoints=sum(AwayPoints) ))
  AwayStats$Team <- AwayStats$Away
  
  Standings$Points <- Standings$Points + HomeStats$HomePoints + AwayStats$AwayPoints
  Standings$Scored <- Standings$Scored + HomeStats$HomeScored+ AwayStats$AwayScored
  Standings$Suffered <- Standings$Suffered + HomeStats$HomeSuffered+ AwayStats$AwaySuffered
  Standings$GolAverage <- Standings$Scored - Standings$Suffered
  
  Classificacao <- Standings[with(Standings, order(-Points, -GolAverage)), ]
  
  Champion <- c(Champion, toString(Classificacao$Original[1]))
  Second <- c(Second, toString(Classificacao$Original[2]))
  Third <- c(Third, toString(Classificacao$Original[3]))
  Fourth <- c(Fourth, toString(Classificacao$Original[4]))
  Fifth <- c(Fifth, toString(Classificacao$Original[5]))
  sexto <- c(sexto, toString(Classificacao$Original[6]))
  septimo <- c(septimo, toString(Classificacao$Original[7]))
  oitavo <- c(oitavo, toString(Classificacao$Original[8]))
  nono <- c(nono, toString(Classificacao$Original[9]))
  decimo <- c(decimo, toString(Classificacao$Original[10]))
  onze <- c(onze, toString(Classificacao$Original[11]))
  doze <- c(doze, toString(Classificacao$Original[12]))
  treze <- c(treze, toString(Classificacao$Original[13]))
  catorze<- c(catorze, toString(Classificacao$Original[14]))
  quinze <- c(quinze, toString(Classificacao$Original[15]))
  desazzeis <- c(desazzeis, toString(Classificacao$Original[16]))
  Penultimo<- c(Penultimo, toString(Classificacao$Original[17]))
  Ultimo <- c(Ultimo, toString(Classificacao$Original[18]))
  EmpateA3 <- ifelse(Classificacao$Points[1]==Classificacao$Points[3],1,0) + EmpateA3
  EmpateA2 <- ifelse(Classificacao$Points[1]==Classificacao$Points[2],1,0) + EmpateA2


print("SimulatioN")
print(z)


}

pp <- Sys.time()

pp - ll

finalrecord[1:1,]

qwerty <- finalrecord[1:1,]




Champion <- (table(Champion))
Second <- (table(Second))
Third <- (table(Third))
Fourth <- (table(Fourth))
Fifth <- (table(Fifth))
sexto <- (table(sexto))
septimo <- (table(septimo))
oitavo <- (table(oitavo))
nono <- (table(nono))
decimo <- (table(decimo))
onze <- (table(onze))
doze <- (table(doze))
treze <- (table(treze))
catorze <- (table(catorze))
quinze <- (table(quinze))
desazzeis <- (table(desazzeis))
Penultimo <- (table(Penultimo))
Ultimo <- (table(Ultimo))




write.csv(Champion, "Champion.csv")
write.csv(Second, "Second.csv")
write.csv(Third, "Third.csv")
write.csv(Fourth, "Fourth.csv")
write.csv(Fifth, "Fifth.csv")
write.csv(sexto, "sexto.csv")
write.csv(septimo, "septimo.csv")
write.csv(oitavo, "oitavo.csv")
write.csv(nono, "nono.csv")
write.csv(decimo, "decimo.csv")
write.csv(onze, "onze.csv")
write.csv(doze, "doze.csv")
write.csv(treze, "treze.csv")
write.csv(catorze, "catorze.csv")
write.csv(quinze, "quinze.csv")
write.csv(desazzeis, "desazzeis.csv")
write.csv(Penultimo, "Penultimo.csv")
write.csv(Ultimo, "Ultimo.csv")

