#' Calibrate function
#'
#' Calibrate the weight of words to applicate on the users
#'
#' @param input
#' @param ID
#' @param calib
#' @param ngram
#' @param wordsinrow
#' @param docincol
#' @param complet
#'
#' @return
#' @import dplyr
#' @import magrittr
#' @export
#'
#' @examples
calibrate<- function (
  input,  # a data.frame avec lignes/users et col/text
  complet = T, #T ou F si T data avec calib et users
 # ID,     #si T faut enlever col avec differenciation
  calib, # si c'est T faut diff
  ngram=2,
  wordsinrow=FALSE,
  docincol = FALSE
){

  #docsincol TRUE si les id/users sont la première colonne dans ce cas faut faire autrement
  #wordsinrow TRUE  si les mots sont la première ligne
  #On a considerer que les documents et features sont les noms des lignes colonnes

  if (complet ==  F){
    data<- input

    ###### trouvons les beta à fixer

    t_data<-t(data) #on transpose pour faire le wordfish et avoir un FDM
    t_data<-as.data.frame(t_data)

    words<-rownames(t_data) # les mots sont les nomes des lignes
    target_name<-colnames(t_data)

    #On supprime les mots qui ont étés prononcés zéro fois par les target

    zero_word <- (rowSums(t_data>0) > 0)

    t_data_without_zero <- t_data[zero_word, ]
    words_kept <- words[zero_word]

    zero_docs <- (colSums(t_data_without_zero) > 0)
    t_data_without_zero <- t_data_without_zero[, zero_docs]
    target_kept <- target_name[zero_docs]


    sum(rowSums(t_data_without_zero> 0) == 0)
    sum(colSums(t_data_without_zero > 0) == 0)
    dim(t_data_without_zero)

    wf_out_data <- wordfish(t_data_without_zero, fixtwo = FALSE, dir = c(1, 2), wordsincol = FALSE, tol = 1e-04)

    omega <- wf_out_data$documents[, "omega"]
    print(omega)
    beta <- wf_out_data$words[, "b"]
    psi <- wf_out_data$words[, "psi"]
    print(psi)

    ### associer les mots et leurs poids respectifs

    word_df <- data.frame(words_kept,beta)
    word_top<-word_df[sort(abs(word_df$beta),decreasing=T,index.return=T)[[2]],][1:6,]
    print(word_df)
    return(list(word_df=word_df,words_kept=words_kept,word_top=word_top))

  }
  else{
 # data_target_name<-data[,ID]#nom du target

  #data_without_target<-data[, !(colnames(data) %in% c(ID)), drop = FALSE]#on enleve le nom du target du data

  #on sépare le data en deux data, un avec les individus qui servent à calibrer (data_target) et l'autre le reste
  data<-input
  data_users<-data[-calib,]

  #rownames(data_users)[0] <- "word" #on donne un nom a la première ligne
  data_target<-data[calib,]





  ###### trouvons les beta à fixer

  t_data_target<-t(data_target) #on transpose pour faire le wordfish et avoir un FDM
  t_data_target<-as.data.frame(t_data_target)

  words<-rownames(t_data_target) # les mots sont les nomes des lignes
  target_name<-colnames(t_data_target)

  #On supprime les mots qui ont étés prononcés zéro fois par les target

  zero_word <- (rowSums(t_data_target>0) > 0)

  t_data_target_without_zero <- t_data_target[zero_word, ]
  words_kept <- words[zero_word]

  zero_docs <- (colSums(t_data_target_without_zero) > 0)
  t_data_target_without_zero <- t_data_target_without_zero[, zero_docs]
  target_kept <- target_name[zero_docs]


  sum(rowSums(t_data_target_without_zero> 0) == 0)
  sum(colSums(t_data_target_without_zero > 0) == 0)
  dim(t_data_target_without_zero)

  wf_out <- wordfish(t_data_target_without_zero, fixtwo = FALSE, dir = c(1, 2), wordsincol = FALSE, tol = 1e-04)

  omega <- wf_out$documents[, "omega"]
  print(omega)
  beta <- wf_out$words[, "b"]
  psi <- wf_out$words[, "psi"]
  opini<-wf_out$documents[,'omega']
  print(psi)

  ### associer les mots et leurs poids respectifs

  word_df <- data.frame(words_kept,beta)
  word_top<-word_df[sort(abs(word_df$beta),decreasing=T,index.return=T)[[2]],][1:6,]
  print(word_df)
  return(list(word_df=word_df,data_users=data_users,word_top=word_top,words_kept=words_kept,opinions=opini))
  }

}





