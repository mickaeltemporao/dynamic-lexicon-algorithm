#' ALGO_10 function
#'
#' find the best sample for calibration
#'
#' @param input a dfm avec lignes/users et col/text
#' @param input2 a df with users and target (df validation)
#' @param name_users the name of col users in the df validation (in "")
#'
#' @return
#' data frame with the validation score of the 10 sample vector calib and
#' @export
#'
#' @examples
#'W<-data_fixure()
#'dfm<-W[[1]]
#'val<-W[[2]]
#'remove(W)
#'S<-algo_10(dfm,val,"users_id")

algo_10<- function (
    input,  # a dfm avec lignes/users et col/text
    input2,   # a df with users and target (df validation)
    name_users    # the name of col users in the df validation

  ){



data<-input
data_validation<-input2
name_user<-name_users



###########Optimisation

a<-seq(1,10)
data_cor<-data.frame(a)
colnames(data_cor)[1]<-"numéro du sample"
remove(a)



for (i in 1:10){

  X1<-sample(nrow(data), size=round(0.03*(dim(data)[1])))

  for(j in 1:length(X1)){
    data_cor[i,j+1]<-X1[j]
    colnames(data_cor)[j+1]<-"donnée de calib"
  }


  x<-calibrate(data,complet=T,X1)

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

  df_validation_arrange<-arrange(data_validation,name_user)

  op_match<-merge(opinions_df_arrange,df_validation_arrange,by.x = "users",by.y = name_users)

  validation_metrics<-cor(op_match$opinions,op_match[,3])


  data_cor[i,length(X1)+2]<-validation_metrics
  colnames(data_cor)[length(X1)+2]<-"validation_score"

  data_cor[i,length(X1)+3]<-x2y(op_match$opinions,op_match[,3])[2]
  colnames(data_cor)[length(X1)+3]<-"validation_score_x2y"

}


return(data_cor=data_cor)
}
