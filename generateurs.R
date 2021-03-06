VonNeumann <- function(n, p=1, graine)
{
  x <-  rep(graine,n*p+1)
  for(i in 2:(n*p+1))
  {
    numbers <- strsplit(format(x[i-1]^2,scientific=FALSE),'')[[1]]
    while(length(numbers)>4){ 
      numbers <- numbers[2:(length(numbers)-1)] 
    }
    x[i] <- as.numeric(numbers)%*%(10^seq(length(numbers)-1,0,-1))
  }
  x <- matrix(x[2:(n*p+1)],nrow=n,ncol=p)
  return(x)
}


MersenneTwister <- function(n, p=1, graine)
{
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(x)
}

Randu <- function(k,graine,p=1)
{
  a <- 65539
  b <- 0 #On pourrait l'enlever, comme il vaut 0 et ne varie jamais, mais par défaut de cohérence, nous le laissons.
  m <- 2^31
  s <- graine
  M <- rep(graine,k*p+1)
  for(i in 1:(k*p))
  {
      s <- (a*s+b)%%m
      M[i] <- s
  }
  M<-matrix(M,nrow=k,ncol=p)
  return (M)
}

StandardMinimal <- function(k,graine,p=1)
{
  a <- 16807
  b <- 0 #On pourrait l'enlever, comme il vaut 0 et ne varie jamais, mais par défaut de cohérence, nous le laissons.
  m <- 2^31-1
  s <- graine
  M <- rep(graine,k*p+1)
  for(i in 1:(k*p))
  {
    s <- (a*s+b)%%m
    M[i] <- s
  }
  M<-matrix(M,nrow=k,ncol=p)
  return (M)
}

binary <- function(x)
{
  if((x<2^31)&(x>=0))
    return( as.integer(rev(intToBits(as.integer(x)))) )
  else{
    if((x<2^32)&(x>0))
      return( c(1,binary(x-2^31)[2:32]) )
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}

Frequency <- function(x,nb)
{
  Sn <- 0
  for(i in 1:length(x))
  {
    bin <- binary(x[i]);
    for(i in (33-nb):32)
    {
      bin[i] <- 2*bin[i]-1
    }
    Sn <- sum(bin[(33-nb):32])+Sn
  }
  Sobs <- abs(Sn)/(sqrt(nb*length(x)))
  pValeur <- 2*(1-pnorm(Sobs))
  return(pValeur)
}

Runs <- function(x,nb)
{
  #calcul de pi
  pi <- 0
  for (j in 1:length(x)){
    bin <- binary(x[j])
    for (i in (33-nb):32){
      pi <- pi + bin[i]
    }
  }
  pi <- pi/(length(x)*nb)
  #print("valeur de pi")
  #print(pi)
  
  #verification que le resultat du pretest soit correct
  to <- 2/sqrt(nb)
  if (abs(pi-0.5)>=to){
    #print('pretest correct')
    return(0) #pValeur vaut 0 dans ce cas
  }
  
  #si pretest incorrect, on fait un deuxieme test
  else{
    #on initialise a 1 car Vobs = somme des bits+1
    Vobs <- 1
    
    for (j in 1:length(x)){
      lastBit <- 0
      bin <- binary(x[j])
      
      #on parcourt chaque bit
      for (i in (33-nb):32){
        
        #pas le premier bit ni le dernier et bit actuel different du suivant
        if (i >(33-nb) && i < 32 && bin[i]!=bin[i+1]){
          Vobs <- Vobs + 1 # r vaut 1
        }
        #si c'est le dernier bit
        else if (i == 32 ){
          lastBit <- bin[i] # on sauvegarde ce bit pour une future comparaison
        }
        #si c'est le premier bit et pas de la premiere valeur
        else if (i == (33-nb) && j>1){
          #comparaison avec le last bit du mot precedent
          if (lastBit != bin[i]){
            Vobs <- Vobs +1
          }
          #comparaison avec le bit suivant
          if (bin[i] != bin[i+1]){
            Vobs <- Vobs+1
          }
        }
        #premier bit de la premiere valeur
        else if (i==(33-nb)){
          #comparaison avec le bit suivant
          if (bin[i] != bin[i+1]){
            Vobs <- Vobs+1
          }
        }
      }
    }
    
    #print("valeur de Vobs")
    #print(Vobs)
    
    #nombre total de bits
    n = nb*length(x)
    
    #calcul de la pValeur
    pValeur <- 2*(1-pnorm(abs(Vobs-2*n*pi*(1-pi))/(2*sqrt(n)*pi*(1-pi))))
    
    # if (pValeur < 0.01){
    #   print("la sequence n'est pas aleatoire")
    # }
    # else {
    #   print("la sequence est aleatoire")
    # }
    return(pValeur)
  }
}

OrderTest <- function(x,D){
  mini <- min(x)
  maxi <- max(x)
  for (i in 1:length(x)){
    x[i] <- (x[i]-mini)/(maxi-mini)
  }
  pVal = order.test(x,d=D,echo=FALSE)$p.value
  # if (pVal < 0.01){
  #   print("ce n'est pas une loi uniforme")
  # }
  # else {
  #   print("c'est une loi uniforme")
  # }
  return(pVal)
}