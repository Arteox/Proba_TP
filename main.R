library(randtoolbox)
source('generateurs.R')
options(digits=5)

sVN <- 926
sMT <- 77
sR <- 331
Nsimu <- 1000
Nrepet <- 1


############################################################
##  Section 2
############################################################

vn <- VonNeumann(Nsimu,Nrepet,sVN)
mt <- MersenneTwister(Nsimu,Nrepet,sMT)
rnd <- Randu(Nsimu,sR,Nrepet)
std <- StandardMinimal(Nsimu,sR,Nrepet)

par(mfrow=c(1,1))
#hist(mt[,1],xlab='',main='Mersenne Twister')
hist(vn[,1],xlab='',main='Von Neumann')
#hist(rnd[,1],xlab='',main='Randu')
hist(std[,1],xlab='',main='Standard Minimal')

par(mfrow=c(1,2))
plot(mt[1:(Nsimu-1),1],mt[2:Nsimu,1],xlab='MT(i)', ylab='MT(i+1)', main='Mersenne Twister')
plot(vn[1:(Nsimu-1),1],vn[2:Nsimu,1],xlab='VN(i)', ylab='VN(i+1)', main='Von Neumann')


# Sequence de bits pour les tests
(bit_mt <- binary(mt[1,1]))

###########################################################
##  Test de fréquence Monobit
###########################################################
#nb a 14 pour VN, a 31 pour rnd et std et 32 pour mt
frequencyMT <- 0
frequencyVN <- 0
frequencyRND <- 0
frequencySTD <- 0
for (i in 51:150){
  vn <- VonNeumann(Nsimu,Nrepet,i)
  mt <- MersenneTwister(Nsimu,Nrepet,i)
  rnd <- Randu(Nsimu,i,Nrepet)
  std <- StandardMinimal(Nsimu,i,Nrepet)
  frequencyMT <- frequencyMT + Frequency(mt,32)
  frequencyVN <- frequencyVN + Frequency(vn,14)
  frequencyRND <- frequencyRND + Frequency(rnd,31)
  frequencySTD <- frequencySTD + Frequency(std,31)
}
frequencyMT <- frequencyMT/100
frequencyVN <- frequencyVN/100
frequencyRND <- frequencyRND/100
frequencySTD<- frequencySTD/100
print(frequencyMT) 
print(frequencyVN)
print(frequencySTD)
print(frequencyRND)

###########################################################
##  Test des runs
###########################################################
#test avec une valeur predefinie
valeur_predef = 619 #cette valeur ne valide pas le pretest
print(Runs(valeur_predef,10)) #valeur attendue : à 0.1472

#test avec les generateurs
runsMT <- 0
runsVN <- 0
runsRND <- 0
runsSTD <- 0
for (i in 51:150){
  vn <- VonNeumann(Nsimu,Nrepet,i)
  mt <- MersenneTwister(Nsimu,Nrepet,i)
  rnd <- Randu(Nsimu,i,Nrepet)
  std <- StandardMinimal(Nsimu,i,Nrepet)
  runsMT <- runsMT + Runs(mt,32)
  runsVN <- runsVN + Runs(vn,14)
  runsRND <- runsRND + Runs(rnd,31)
  runsSTD <- runsSTD + Runs(std,31)
}
print(runsMT/100)
print(runsVN/100)
print(runsRND/100)
print(runsSTD/100)

###########################################################
##  Test d'ordre
###########################################################
vn <- VonNeumann(Nsimu,4,sVN)
print(OrderTest(vn,4))

orderMT <- 0
orderVN <- 0
orderRND <- 0
orderSTD <- 0
for (i in 951:1050){
  vn <- VonNeumann(Nsimu,4,i)
  mt <- MersenneTwister(Nsimu,4,i)
  rnd <- Randu(Nsimu,i,4)
  std <- StandardMinimal(Nsimu,i,4)
  orderMT <- orderMT + OrderTest(mt,4)
  orderVN <- orderVN + OrderTest(vn,4)
  orderRND <- orderRND + OrderTest(rnd,4)
  orderSTD <- orderSTD + OrderTest(std,4)
}
print(orderMT/100)
print(orderVN/100) #resultat bizarre car vn a souvent des valeurs avec que des 0 en fonction de la seed
print(orderRND/100)
print(orderSTD/100)

###########################################################
##  Files d'attente
###########################################################
source('files.R')
options(digits=5)

#question 6
liste <- FileMM1(8,12,10)
#question 7
plot(EvolClients(liste),type='s')

#application
lambda <- 14/60
mu <- 15/60
D <- 60*1200

liste <- FileMM1(lambda,mu,D)
evol <- EvolClients(liste)
plot(evol,type='s')

#question 8
#calcul du nombre moyen de clients et du temps de presence moyen
moy_nb_clients <- 0
moy_tps_presence <- 0

for (i in 1:(nrow(evol))){
  
  #nb moyen de clients
  if (i>1){
    moy_nb_clients <- moy_nb_clients + (-evol[i-1,1]+evol[i,1]) * evol[i-1,2]
  }
  
  #tps de presence moyen
  if (i <= length(liste$dep)){
    moy_tps_presence <- moy_tps_presence + liste$dep[i]-liste$arr[i]
  }
  
}
moy_nb_clients <- moy_nb_clients/D
moy_tps_presence <- moy_tps_presence/length(liste$dep)

print(moy_nb_clients)
print(moy_tps_presence)


