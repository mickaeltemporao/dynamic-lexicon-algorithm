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
  (data_target_name)
  data_without_target<-data[, !(colnames(data) %in% c(ID)), drop = FALSE]#on enleve le nom du target du data
  
  #on sépare le data en deux data, un avec les individus qui servent à calibrer (data_target) et l'autre le reste
  
  data_users<-data_without_target[-calib,]
  print(data_users)
  #rownames(data_users)[0] <- "word" #on donne un nom a la première ligne
  data_target<-data_without_target[calib,]
  print(data_target)
  

  
  
###### trouvons les beta à fixer
  
  t_data_target<-t(data_target) #on transpose pour faire le wordfish et avoir un FDM
  t_data_target<-as.data.frame(t_data_target)
  words<-rownames(t_data_target) # les mots sont les nomes des lignes
  
  wf_out <- wordfish(t_data_target, fixtwo = FALSE, dir = c(1, 2), wordsincol = FALSE, tol = 1e-04)
  omega <- wf_out$documents[, "omega"]
  print(omega)
  beta <- wf_out$words[, "b"]
  psi <- wf_out$words[, "psi"]
  print(psi)

### associer les mots et leurs poids respectifs  
  
  word_df <- data.frame(words,beta)
  print(word_df)
  
#### fixer les poids
  t_data_users<-t(data_users)#on transpose pour pouvoir assembler les data
  t_data_users<-as.data.frame(t_data_users)
  
  #on les assemble aux mots
  
  t_data_users[,"weight"]<-word_df[,2]
  t_data_users[,"words"]<-word_df[,1]
  wordcountdata_users_weighted <- t_data_users
  
  L <- dim(wordcountdata_users_weighted)[2] # nombre de colonnes
  
  words_weighted    <- wordcountdata_users_weighted[,L] #les mots
  TM_users_weighted <- wordcountdata_users_weighted[,1:(L-2)]#les occurences
  beta_weighted     <- wordcountdata_users_weighted[,L-1]#les betas

#on refait le wordfish avec les betas fixés
  
  
  #on refait le wordfish avec les betas fixés
  
  sum(rowSums(TM_users_weighted > 0) == 0)
  sum(colSums(TM_users_weighted > 0) == 0)
  dim(TM_users_weighted)
    
   

    wf2_out <- wordfish2(beta_weighted,TM_users_weighted,fixtwo=FALSE,dir=c(1,2),wordsincol=FALSE,tol=1e-4)
    


    beta    <- beta_weighted
    print(beta)
    opinions  <- wf2_out$documents[,'omega']
    words_weighted_df<-data.frame(words_weighted,beta_weighted)
    print(words_weighted_df)
    #opinions_df<-data.frame(rownames(data_users)[0],opinions)
    opinions_df<-data.frame(rownames(data_users),opinions)
    return(opinions_df)

}



