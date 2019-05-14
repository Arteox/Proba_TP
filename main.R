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
print(Frequency(mt,32)) #nb a 14 pour VN, a 31 pour rdn et std et 32 pour mt
print(Frequency(vn,14))
print(Frequency(rnd,31))
print(Frequency(std,31))

###########################################################
##  Test des runs
###########################################################
#test avec une valeur predefinie
valeur_predef = 619 #cette valeur ne valide pas le pretest
print(Runs(valeur_predef,10)) #valeur attendue : à 0.1472

#test avec les generateurs
print(Runs(mt,32))
print(Runs(rnd,32))
print(Runs(std,32))
print(Runs(vn,32))

###########################################################
##  Test d'ordre
###########################################################
vn <- VonNeumann(Nsimu,4,sVN)
mt <- MersenneTwister(Nsimu,4,sMT)
rnd <- Randu(Nsimu,sR,4)
std <- StandardMinimal(Nsimu,sR,4)
print(OrderTest(mt,4))
print(OrderTest(vn,4))
print(OrderTest(rnd,4))
print(OrderTest(std,4))