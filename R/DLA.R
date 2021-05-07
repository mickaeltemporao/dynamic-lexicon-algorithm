#######
# si c'est un DFM faut le transposer pour avoir un FDM

DLA <- function (input, ID, calib, ngram=2, wordsinrow=FALSE, docincol = FALSE){
  #docsincol TRUE si les id/users sont la première colonne dans ce cas faut faire autrement
  #wordsinrow TRUE  si les mots sont la première ligne
  #On a considerer que les documents et features sont les noms des lignes colonnes
  
  data<- input
  data_target_name<-data[,"ID"]
  data<-data[, !(colnames(data) %in% c("ID")), drop = FALSE]
  data_users<-data[-calib,]
  colnames(data_users)[1] <- "words"
  data_target<-data[calib,]
  
  #### Affecter filtre pour enlever les candidats ou users pas significatif
  
  
###### trouvons les beta à fixer
  data_target<-t(data_target)
  words<-rownames(data_target)
  wf_out <- wordfish(data_target, fixtwo = FALSE, dir = c(1, 2), wordsincol = FALSE, tol = 1e-04)
  omega <- wf_out$documents[, "omega"]
  beta <- wf_out$words[, "b"]
  psi <- wf_out$words[, "psi"]

### associer les mots et leurs poids respectifs  
  word_df <- data.frame(words,beta)
  
#### fixer les poids
  wordcountdata_users_weighted <- merge(data_users,word_df,by.x = " words" ,by.y = "words")
  L <- dim(wordcountdata_users_weighted)[2]
  words_weighted    <- wordcountdata_users_weighted[,1]
  TM_users_weighted <- wordcountdata_users_weighted[,2:(L-1)]
  beta_weighted     <- wordcountdata_users_weighted[,L]

  
####
  # 3) Suppress columns with zeros
  non_zero_idx <- (colSums(TM_users_weighted != 0) > min_words_users)
  TM_users_weighted <- TM_users_weighted[, non_zero_idx]
  tweeters_users_weighted <- tweeters_users[,'Twitter_ID']
  tweeters_users_weighted <- tweeters_users_weighted[non_zero_idx]
  
  # 3bis) Suppress lines with zeros
  kept_words_users_weighted <- (rowSums(TM_users_weighted > 0) > 0)
  TM_users_weighted <- TM_users_weighted[kept_words_users_weighted,]
  words_weighted     <- words_weighted[kept_words_users_weighted]
  beta_weighted      <- beta_weighted[kept_words_users_weighted]
  
  
  # 4) Compute opinion for each user
  if(wf_mode == 'wgt'){
    opinions <- (t(as.matrix(beta_weighted)) %*% as.matrix(TM_users_weighted))
    opinions <- opinions[1,] / as.matrix(colSums(TM_users_weighted))
    opinions <- as.numeric(opinions)
  }else if(wf_mode == 'wgt2'){
    sum(rowSums(TM_users_weighted > 0) == 0)
    sum(colSums(TM_users_weighted > 0) == 0)
    dim(TM)
    wf2_out <- wordfish2(beta_weighted,TM_users_weighted,fixtwo=FALSE,dir=c(1,2),wordsincol=FALSE,tol=1e-4)
    opinions  <- wf2_out$documents[,'omega']
  }
  
  # 5) Adapt names to save convention
  tweeters <- tweeters_users_weighted
  omega    <- opinions
  words    <- words_weighted
  beta     <- beta_weighted
  
  # 6) Define a TM matrix
  TM <- TM_users_weighted
  
  
}


wf_out <- wordfish(TM, fixtwo = FALSE, dir = c(1, 2), wordsincol = FALSE, tol = 1e-04)
omega <- wf_out$documents[, "omega"]
beta <- wf_out$words[, "b"]
psi <- wf_out$words[, "psi"]