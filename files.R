FileMM1<-function(lambda, mu, D){
  
  #calcul des lois de temps d'arrivÃ©e et dÃ©part
 
  #init des variables
  tpsArrivee <- 0
  arrivee <- c()
  depart <- c()

  
  #on remplit le vecteur arrivee
  while(tpsArrivee < D){
    duree_arrivee =  rexp(1,lambda)
    arrivee <- c(arrivee, tpsArrivee)
    tpsArrivee <- tpsArrivee + duree_arrivee
  }
  
  #on remplit le vecteur depart
  #on insere le premier en dehors de la boucle
  
  duree_depart = rexp(1,mu)
  
  
  depart <- c(depart, duree_depart + duree_arrivee)
  tpsDepart <- duree_depart + duree_arrivee
  maxArr <- length(arrivee)
  index_arrivee <- 2
  while(tpsDepart < D){
    
    duree_depart = rexp(1,mu)
    
    
    #si personne devant dans la file d'attente
    if (arrivee[index_arrivee] > tpsDepart){
      tpsDepart <- arrivee[index_arrivee] + duree_depart
    }
    #si qqun devant dans dans la file d'attente
    else {
      tpsDepart <- tpsDepart + duree_depart
    }
    if (tpsDepart < D ){
      #on verifie qu'on ne depasse pas le nombre de personnes arrivees
      if (index_arrivee < maxArr){
        index_arrivee <- index_arrivee + 1
        depart <- c(depart, tpsDepart)
      }
      
    }
  }

  liste <- list(arr=arrivee, dep=depart)

  return(liste)
}

EvolClients <- function(liste){
  
  matTempsClient <- matrix(nrow=0,ncol=2)
  indArr <- 1
  indDep <- 1
  nbPers <- 0
  maxIndArr <- length(liste$arr)
  maxIndDep <- length(liste$dep)
  reachedMaxArr <- FALSE
  reachedMaxDep <- FALSE
  
  #on remplit une matrice qui contient la paire{temps; nb de clients}
  #on ajoute une paire Ã  chaque fois qu'un client entre ou sort
  
  for (i in 1:(length(liste$arr)+length(liste$dep))){
    toAppend <- c()
    
    #si un client arrive et qu'on n'a pas parcouru toute la liste d'arrivee OU que toute la liste de depart a été parcourue
    if ((liste$arr[indArr] < liste$dep[indDep] && !reachedMaxArr) || reachedMaxDep) {
      
      nbPers <- nbPers + 1
      toAppend <- c(toAppend, liste$arr[indArr])
      toAppend <- c(toAppend, nbPers)
      matTempsClient <- rbind(matTempsClient,toAppend)
      if (indArr < maxIndArr){
        
        indArr <- indArr + 1
      }
      else if (indArr == maxIndArr){
        reachedMaxArr <- TRUE
      }
    }
    
    #si un client sort et que la liste de depart n'a pas ete entierement parcourue
    else if (!reachedMaxDep){
      nbPers <- nbPers - 1
      toAppend <- c(toAppend, liste$dep[indDep])
      toAppend <- c(toAppend, nbPers)
      matTempsClient <- rbind(matTempsClient,toAppend)
      if (indDep < maxIndDep){
        
        indDep <- indDep + 1
      }
      else if (indDep == maxIndDep){
        
        reachedMaxDep <- TRUE
      }
      
    }
  }
  
  return(matTempsClient)
}