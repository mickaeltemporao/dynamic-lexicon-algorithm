
#' Make the data fixture with the df validation
#'
#' @return
#' @import quanteda
#' @import magrittr
#' @export
#'
#' @examples
#' w <- data_fixture()
#' dfm <- w[[1]]
#' val <- w[[2]]
data_fixture <- function(){


  library(quanteda)
  library(magrittr)
  ########## Creation du DFM


  #On crée un data avec les politiciens, ici on prends des mots des présidents américains lors de leurs discours

  data_pol <- data_corpus_inaugural %>% corpus_subset(Year > 2011) %>%tokens()
  data_pol  <-dfm(data_pol)# on transforme en dfm
  data_pol <- dfm_keep(data_pol, min_nchar = 2)#on garde que les mots de plus de 2 lettres
  data_pol <- dfm_remove(data_pol, pattern = stopwords("en"))#on enleve les mots communs (ponctuation, et ..)
  data_pol <- convert(data_pol, to = "data.frame")
  rownames(data_pol) <- data_pol[,1]
  data_pol <- data_pol[,-1]


  dim(data_pol) # 1343 features

  # On crée un data users avec des occurences aléatoires

  a<-c((1:100)) # colonnes des users


  b<-abs(c(rnorm(100, mean=0.4, sd=3))) # occurences
  b<-round(b)

  data_users <- data.frame(a,b)
  data_users <- data_users[,-1]
  data_users <- as.data.frame(data_users)

  for(i in 2:dim(data_pol)[2]){
    data_users[,i] <- round(abs(c(rnorm(100, mean=0.4, sd=3)))) # on rajoute autant qu'il y a de mots
  }

  colnames(data_users) <- colnames(data_pol) # on mets les mêmes noms pour fusionner

  data <- rbind(data_pol, data_users)

  data[,dim(data)[2]+1] <- 1



  for(i in 1:dim(data)[1]){
    if(i<4){
      data[i,dim(data)[2]]=1
    }
    else{data[i,dim(data)[2]]=0}
  }

  data[,dim(data)[2]]

  colnames(data)[dim(data)[2]] <- "ID"


  data <- data[4:103,1:1343]

  dfm_fixture <- data# data finale






  ########### Creation du Data frame validation

  df_validation < -data.frame(rownames(dfm_fixture))
  colnames(df_validation) <- "users_id"
  for(i in 1:dim(df_validation)[1]){
    df_validation[i,2] <- rnorm(1, mean=0.4, sd=3)
  }


  for (j in 1:dim(dfm_fixture)[1]){
     df_validation[j,2]=dfm_fixture[j,"technologies"]*(-1.5)+df_validation[j,2]
     df_validation[j,2]=dfm_fixture[j,"democracy"]*(-3)+df_validation[j,2]
     df_validation[j,2]=dfm_fixture[j,"patriotism"]*3+df_validation[j,2]
     df_validation[j,2]=dfm_fixture[j,"terrorism"]*1.5+df_validation[j,2]
  }




  return(list(dfm_fixture=dfm_fixture,df_validation=df_validation))

}



