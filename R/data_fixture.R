
#' Make the data fixture with the df validation
#'
#' @return
#' @import quanteda
#' @import magrittr
#' @export
#'
#' @examples
#' data_fixture()
data_fixture <- function(){


  x<-data.frame()

  for (i in 1:100){
    x[1,i]<-paste0("words",i)
  }

  # On crée un data users avec des occurences aléatoires

  a<-data.frame() # colonnes des users

  for (i in 1:50){
    a[i,1]<-paste0("users",i)
  }

  b<-c(rpois(50, 0.5)) # occurences

  data_users <- data.frame(a,b)
  data_users <- as.data.frame(data_users)

  for(i in 3:101){
    data_users[,i] <- round(abs(c(rpois(50, 0.5)))) # on rajoute autant qu'il y a de mots
  }

  rownames(data_users)<-data_users[,1]

  data_users<-data_users[,2:101]

  colnames(data_users) <- x # on mets les mêmes noms pour fusionner


  dfm_fixture <- data_users# data finale

  remove(a)
  remove(b)
  remove(i)
  remove(data_users)
  remove(x)



########### Creation du Data frame validation

  df_validation <- data.frame(rownames(dfm_fixture))
  colnames(df_validation) <- "users_id"
  df_validation[,2] <- rnorm(50,5,2)
  rownames(df_validation)<-df_validation[,1]



#########weight le data

  calib_vector<-sample(rownames(dfm_fixture),10)


  dfm_pour_filtre<-dfm_fixture[calib_vector,]
  dfm_pour_filtre<-t(dfm_pour_filtre)
  dfm_pour_filtre<-as.data.frame(dfm_pour_filtre)

  ###filtre pour enlever les zero

  zero_word <- (rowSums(dfm_pour_filtre>0) > 0)

  dfm_pour_filtre <- dfm_pour_filtre[zero_word, ]

  zero_docs <- (colSums(dfm_pour_filtre) > 0)
  dfm_filtre <- dfm_pour_filtre[, zero_docs]



  sum(rowSums(dfm_filtre> 0) == 0)
  sum(colSums(dfm_filtre > 0) == 0)

  dfm_filtre<-t(dfm_filtre)
  dfm_filtre<-as.data.frame(dfm_filtre)

  dfm_fixture<-dfm_fixture[,colnames(dfm_filtre)]

  words_choose<-sample(colnames(dfm_filtre),15)

#mean<-c(15,20,13,10,18)

for (i in calib_vector){
  for(j in words_choose){
  dfm_fixture[i,j]=rpois(1, df_validation[i,2])
  }
}

cat("Finished \n")

  return(list(dfm_fixture=dfm_fixture,df_validation=df_validation,calib_vector,words_choose))

}




