



#' ALGO_10 function
#'
#' find the best sample for calibration
#'
#' @param input
#' @param input2
#' @param name_users
#' @param name_val
#'
#' @return
#' @export
#'
#' @examples


algo_10<- function (
    input,  # a dfm avec lignes/users et col/text
    input2,   # a df with users and target (df validation)
    name_users,     # the name of col users in the df validation
    name_val  # the name of col score in the df validation
  ){



data<-input
data_validation<-input2

data_without_na <- subset(data,!is.na(data))

###########Optimisation

a<-seq(1,10)
data_cor<-data.frame(a)
colnames(data_cor)[1]<-"numéro du sample"
remove(a)



for (i in 1:10){

  X1<-sample(nrow(data_without_na), size=round(0.01*(dim(data_without_na)[1])))

  for(j in 1:length(X1)){
    data_cor[i,j+1]<-X1[j]
    colnames(data_cor)[j+1]<-"donnée de calib"
  }


  x<-calibrate(data_without_na,complet=T,X1)

  data_users<-x[[2]]
  word_df<-x[[1]]
  opini_target<-x[[3]]

  #use weight on the other

  y<-use_weight(data_users,rownames(word_df),word_df)
  opini_users<-y[[1]]
  word_wei<-y[[2]]

  ## Data frame des opinions de tous

  library(dplyr)
  opinions_df<-rbind(opini_target, opini_users)
  opinions_df$users<-as.numeric(opinions_df$users)
  opinions_df$opinions<-as.numeric(opinions_df$opinions)


  ###Validation

  opinions_df_arrange<-arrange(opinions_df,users)

  df_validation_arrange<-arrange(data_validation,name_users)

  op_match<-merge(opinions_df_arrange,df_validation_arrange,by.x = "users",by.y = "name_users")

  validation_metrics<-cor(op_match$opinions,op_match$name_val)


  data_cor[i,length(X1)+2]<-validation_metrics
  colnames(data_cor)[length(X1)+2]<-"validation_score"

}


return(list(op_match=op_match,data_cor=data_cor))
}
