library(randtoolbox)
source('generateurs.R')

sVN <- 9271
sMT <- 2508
sR <- 3316
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

print(Runs(rnd,32))
