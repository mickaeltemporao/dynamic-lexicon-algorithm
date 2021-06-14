
#' Validation function
#'
#' @param input_1 the data frame with the score calculate
#' @param input_2 the real score
#' @param name_user_1 the name of the col of the users-id in the df calculate
#' @param name_user_2 the name of the col of the users-id in the df validation
#'
#' @return
#' @import corrplot
#' @export
#'
#' @examples
#'  w <- data_fixture()
#'  dfm <- w[[1]]
#'  df_val<-w[[2]]
#'
#'  x<-calibrate(dfm,complet=T,c(1,2,3))
#'  data_users <- x[[2]]
#'  word_df <- x[[1]]
#'  opini_target <- x[[3]]
#'
#'  y <- use_weight(data_users,rownames(word_df),word_df)
#'  opini_users <- y[[1]]
#'  word_wei <- y[[2]]
#'
#'  opinions_df <- rbind(opini_target, opini_users)
#'  opinions_df$users <- as.numeric(opinions_df$users)
#'  opinions_df$opinions <- as.numeric(opinions_df$opinions)
#'
#'  s<-validation(opinions_df,df_val,"users","users_id")
validation <- function(input_1,input_2,name_user_1,name_user_2){

  data_estimation<-input_1
  data_vpl<-input_2
  mat_ecart_relatif<-data.frame()

  opinions_df_arrange <- arrange(data_estimation,name_user_1)

  df_validation_arrange <- arrange(data_vpl,name_user_2)

  op_match <- merge(opinions_df_arrange,df_validation_arrange,by.x = name_user_1,by.y = name_user_2)

  coeff_corr<-cor(op_match[,2],op_match[,3],method="pearson")

  for(i in 1:nrow(op_match)){
    mat_ecart_relatif[i,1]<-(op_match[i,3] - op_match[i,2])/op_match[i,3]

  }


  average_efficiency<-mean(mat_ecart_relatif[,1])

  x2y<-x2y(op_match[,2],op_match[,3])[[2]]

  return(list(average_efficiency,coeff_corr,x2y))

}

