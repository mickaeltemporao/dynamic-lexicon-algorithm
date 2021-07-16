#' Optimisation algo_10 function
#'
#' @param input a dfm avec lignes/users et col/text
#' @param vector the return of algo_10 function
#' @param time_running the max time of running the users want in second
#' @param input2 a df with users and target (df validation)
#' @param name_users the name of col users in the df validation (in "")
#'
#' @return
#' @export
#'
#' @examples
opti_10 <- function (
  input,
  input2,
  name_users,
  vector,
  time_running=300
){


df_validation<-input2

x4<-rownames(input[-vector,])
x4<-as.numeric(x4)
X3<-sample(x4, size = 0.4*length(x4))

a <- seq(1,252)
data_cor3_opti <- data.frame(a)
colnames(data_cor3_opti)[1] <- "results"
remove(a)


niter<-0

for(j in 1){
  for(i in X3){

  X_replace<-as.numeric(insert(vector,j,i))

  x <- calibrate(input,complet=T,X_replace)


  #use weight on the other

  y <- use_weight(x[[2]],rownames(x[[1]]),x[[1]])


  ## Data frame des opinions de tous

  library(dplyr)
  require(rpart)
  opinions_df <- rbind(x[[3]], y[[1]])
  opinions_df$users <- as.numeric(opinions_df$users)
  opinions_df$opinions <- as.numeric(opinions_df$opinions)


  ###Validation

  opinions_df_arrange <- arrange(opinions_df,users)

  df_validation_arrange <- arrange(df_validation,name_users)

  op_match <- merge(opinions_df_arrange,df_validation_arrange,by.x = "users",by.y = name_users)


  validation_metrics <- cor(op_match$opinions,op_match[,3])


  if((validation_metrics>0.2)){ # si le score est meilleur alors on garde et on passe a l'indice suivant
niter<-niter+1
for(w in 1:length(X_replace)){
  data_cor3_opti[niter,w]<-X_replace[w]
  colnames(data_cor3_opti)[w]<-paste0("donnée de calib",w)
}
data_cor3_opti[niter,length(X_replace)+1]<-validation_metrics
colnames(data_cor3_opti)[length(X_replace)+1]<-"score"
break

}
}
}

for(j in 2:length(vector)){
  start_time = Sys.time()
  for(i in X3){

    X_replace<-as.numeric(insert(vector,j,i))


    x <- calibrate(input,complet=T,X_replace)


    #use weight on the other

    y <- use_weight(x[[2]],rownames(x[[1]]),x[[1]])


    ## Data frame des opinions de tous

    library(dplyr)
    require(rpart)
    opinions_df <- rbind(x[[3]], y[[1]])
    opinions_df$users <- as.numeric(opinions_df$users)
    opinions_df$opinions <- as.numeric(opinions_df$opinions)


    ###Validation

    opinions_df_arrange <- arrange(opinions_df,users)

    df_validation_arrange <- arrange(df_validation,name_users)

    op_match <- merge(opinions_df_arrange,df_validation_arrange,by.x = "users",by.y = name_users)


    validation_metrics <- cor(op_match$opinions,op_match[,3])

    #si le score est meilleur alors on garde et on passe a l'indice suivant
    if((validation_metrics>abs(data_cor3_opti[niter,length(X_replace)+1]))){

      niter<-niter+1
      for(w in 1:length(X_replace)){
        data_cor3_opti[niter,w]<-X_replace[w]
        colnames(data_cor3_opti)[w]<-paste0("donnée de calib",w)
      }
      data_cor3_opti[niter,length(X_replace)+1]<-validation_metrics
      colnames(data_cor3_opti)[length(X_replace)+1]<-"score"
      break

    }


  }

  end_time = Sys.time()

  if(as.numeric(difftime(end_time,start_time,units = "sec"))>ceiling((time_running/length(vector)))){
    cat("algo stopped time out")
    break}
}

data_cor3_opti <- arrange(data_cor3_opti,desc(data_cor3_opti$score))
data_score<-data_cor3_opti

return(data_score)
}





