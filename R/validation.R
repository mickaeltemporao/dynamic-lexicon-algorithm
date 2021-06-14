
#' Validation functioon
#'
#'
#'
#' @param input_1 the data frame with the score calculate
#' @param input_2 the real score
#'
#' @return
#' @import corrplot
#' @export
#'
#' @examples
validation<-function(input_1,input_2){

  data_estimation<-input_1
  data_vpl<-input_2


  coeff_corr<-cor(data_estimation,data_vpl,method=c("Pearson"))

  for(i in 1:nrow(data_estimation)){
    mat_ecart_relatif[i,1]<-(data_vpl[i,1] - data_estimation[i,1])/data_vpl[i,1]

  }


  average_efficiency<-colSums(mat_ecart_relatif[,1])/nrow(mat_ecart_relatif)

  return(list(average_efficiency,coeff_corr))

}

