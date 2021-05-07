#### Affecter filtre pour enlever les candidats ou users pas significatif (autre fonction)

DLA <- function (
  input,  # a data.frame
  ID, 
  calib, 
  ngram=2, 
  wordsinrow=FALSE, 
  docincol = FALSE
  ){
  
  #docsincol TRUE si les id/users sont la première colonne dans ce cas faut faire autrement
  #wordsinrow TRUE  si les mots sont la première ligne
  #On a considerer que les documents et features sont les noms des lignes colonnes
  
  data<- input
  data_target_name<-data[,ID]#nom du target
  data<-data[, !(colnames(data) %in% c(ID)), drop = FALSE]#on enleve le nom du target du data
  
  #on sépare le data en deux data, un avec les individus qui servent à calibrer (data_target) et l'autre le reste
  
  data_users<-data[-calib,]
  rownames(data_users)[0] <- "words" #on donne un nom a la première ligne
  data_target<-data[calib,]
  

  
  
###### trouvons les beta à fixer
  
  data_target<-t(data_target) #on transpose pour faire le wordfish et avoir un FDM
  words<-rownames(data_target) # les mots sont les nomes des lignes
  
  wf_out <- wordfish(data_target, fixtwo = FALSE, dir = c(1, 2), wordsincol = FALSE, tol = 1e-04)
  omega <- wf_out$documents[, "omega"]
  beta <- wf_out$words[, "b"]
  psi <- wf_out$words[, "psi"]

### associer les mots et leurs poids respectifs  
  
  word_df <- data.frame(words,beta)
  
#### fixer les poids
  data_users<-t(data_users)#on transpose pour pouvoir assembler les data
 
  #on les assemble
  
  wordcountdata_users_weighted <- merge(data_users,word_df,by.x = " words" ,by.y = "words")
  
  L <- dim(wordcountdata_users_weighted)[2] # nombre de colonnes
  
  words_weighted    <- wordcountdata_users_weighted[,0] #les mots
  TM_users_weighted <- wordcountdata_users_weighted[,1:(L-1)]#les occurences
  beta_weighted     <- wordcountdata_users_weighted[,L]#les betas

#on refait le wordfish avec les betas fixés
    
    sum(rowSums(TM_users_weighted > 0) == 0)
    sum(colSums(TM_users_weighted > 0) == 0)
    dim(TM)

    wf2_out <- wordfish2(beta_weighted,TM_users_weighted,fixtwo=FALSE,dir=c(1,2),wordsincol=FALSE,tol=1e-4)
    


    beta    <- beta_weighted
    opinions  <- wf2_out$documents[,'omega']
    words_weighted_df<-data.frame(words_weighted,beta_weighted)
    opinions_df<-data.frame(rownames(data_users)[0],opinions)

}

#charger les deux csv et les fusuinner pour tester la fonction plus qu'a charger le csv tm2 gram


setwd("C:/Users/fredo/OneDrive/Documents/Stage/CNRS/Stage/Code/Code Projet")
pol<-read.csv2("DFM1.csv")
users<-read.csv2("DFM2.csv")
colnames(pol[1])
data_comb<-merge(pol,users,all.x =TRUE,all.y=TRUE )
data_comb1<-data_comb[1]

calib_test<-data_comb[1:297]
data_comb=t(data_comb)

#random letter our crrer un random dfm
