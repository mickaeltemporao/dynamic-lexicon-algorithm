
# Algo _10 sur les données réelles ---------------

library(DLA)


x <- algo_10_review(TM2gram_test_without_na,
                    op_without_na,
                    "Vpl_ID")

algo_10_return<-x$data_cor

best_algo_10_return<-algo_10_return[1,2:267]

## a partir de la

best_vector<-as.character(best_algo_10_return)

y<-opti_10(TM2gram_test_without_na,
           op_without_na,
           "Vpl_ID",
           best_vector,
           runtime=600)



