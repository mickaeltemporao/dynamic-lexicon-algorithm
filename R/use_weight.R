#' Use weight function
#'
#' Prediction on the users
#'
#' @param input a data.frame with the users avec lignes/users et col/text
#' @param word  words kept calculating in the calibrate function
#' @param df word/weight data calculate in the calibrate function
#' @param wordsinrow fix
#' @param docincol fix
#'
#' @return
#' @export
#'
#' @examples
#' w <- data_fixture()
#' x<-calibrate(w[[1]],complet=T,c(1,2,3))
#' use_weight(x[[2]],rownames(x[[1]]),x[[1]])
#' remove(w)
#' remove(x)
use_weight <- function (
  input,  # a data.frame avec lignes/users et col/text
  words,   # words kept calculating in the last function
  df,     #word/weight data
  wordsinrow=FALSE,
  docincol = FALSE
){

  data_users <- input
  words_kept <- words
  word_df <- df

  #### fixer les poids

  t_data_users <- t(data_users)#on transpose pour pouvoir assembler les data
  t_data_users <- as.data.frame(t_data_users)

  t_data_users_kept <- t_data_users#on garde que les mots utilisés par les calibreurs
  users_name <- colnames(t_data_users_kept)

  #On enlève ceux qui ont étés utilisées zéro fois par tout le monde

  zero_word1 <- (rowSums(t_data_users_kept>0) > 0)

  t_data_users_without_zero <- t_data_users_kept[zero_word1, ]

  words_kept1 <- words_kept[zero_word1]

  zero_docs1 <- (colSums(t_data_users_without_zero) > 0)
  t_data_users_without_zero <- t_data_users_without_zero[, zero_docs1]
  users_kept1 <- users_name[zero_docs1]


  #on les assemble aux mots

  word_df<-word_df[zero_word1,]
  t_data_users_without_zero[,"weight"] <- word_df[,2]
  t_data_users_without_zero[,"words"] <- word_df[,1]




  wordcountdata_users_weighted <- t_data_users_without_zero

  L <- dim(wordcountdata_users_weighted)[2] # nombre de colonnes

  words_weighted    <- wordcountdata_users_weighted[,L] #les mots
  TM_users_weighted <- wordcountdata_users_weighted[,1:(L-2)]#les occurences
  beta_weighted     <- wordcountdata_users_weighted[,L-1]#les betas

  #on refait le wordfish avec les betas fixés


  sum(rowSums(TM_users_weighted > 0) == 0)
  sum(colSums(TM_users_weighted > 0) == 0)
  dim(TM_users_weighted)



  wf2_out <- wordfish2(beta_weighted,TM_users_weighted,fixtwo=FALSE,dir=c(1,2),wordsincol=FALSE,tol=1e-4)



  beta    <- beta_weighted
  opinions  <- wf2_out$documents[,'omega']
  words_weighted_df <- data.frame(words_weighted,beta_weighted)
  #opinions_df<-data.frame(rownames(data_users)[0],opinions)
  opinions_df <- data.frame(users_kept1,opinions)
  colnames(opinions_df) <- c("users","opinions")

  cat("Finished \n")
  return(list(opinions_df=opinions_df,words_weighted_df=words_weighted_df))

}
