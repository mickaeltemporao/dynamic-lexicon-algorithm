# Dynamic-Lexicon-Algorithm


## About

This R package is designed to estimate a target using analyse and weight of words.

![Flowchart_DLA](https://user-images.githubusercontent.com/83821244/123930107-b844cf00-d98f-11eb-9f48-c951951dca79.jpg)


## How to install

From GitHub for the latest development version:

```
devtools::install_github("mickaeltemporao/dynamic-lexicon-algorithm") 

```

## How to use

### From a dataset

You can run the dfm_generation() function to create a Documents-Features-Matrix from a data with users and their text (tweets fro example) like this :

```
 Tweet <- read.csv(file="ex_tweets.csv",encoding = 'UTF-8')
 Tweet <- subset(Tweet, select = c("user_id","text"))
 DFM_test <- dfm_generation(Tweet,2,1,"fr")

```

### Estimate parameters

When you have a dfm you can run calibrate() function to estimate the weights of words using the wordfish() function :


```
 w <- data_fixture()
   dfm <- w[[1]]
 
 x<-calibrate(dfm,complet=T,c(1,2,3))
   data_users <- x[[2]]
   word_df <- x[[1]]
   opini_target <- x[[3]]

```

### Predict target

Then use the weights calculate previously on the others users with the function named use_weights().
It will calulate the target choose :

```
 w <- data_fixture()
   dfm <- w[[1]]

 x<-calibrate(dfm,complet=T,c(1,2,3))
   data_users <- x[[2]]
   word_df <- x[[1]]
   opini_target <- x[[3]]
 
 y <- use_weight(data_users,rownames(word_df),word_df)
   opini_users <- y[[1]]
   word_wei <- y[[2]]

```

Now you have a data frame with the weight of words and a data frame with the prediction 
of the users.

### Validation

You can now run the validation with the data prediction and the real opinion's data.

```
  w <- data_fixture()
  dfm <- w[[1]]
  df_val<-w[[2]]

  x<-calibrate(dfm,complet=T,c(1,2,3))
  data_users <- x[[2]]
  word_df <- x[[1]]
  opini_target <- x[[3]]

  y <- use_weight(data_users,rownames(word_df),word_df)
  opini_users <- y[[1]]
  word_wei <- y[[2]]

  opinions_df <- rbind(opini_target, opini_users)
  opinions_df$users <- as.numeric(opinions_df$users)
  opinions_df$opinions <- as.numeric(opinions_df$opinions)

  s<-validation(opinions_df,df_val,"users","users_id")

```

### Optimisation

If you don't have the vector calibration you can run the function algo_10_review who the best vector with 4 sample : (see "hyper_param_opti.Rmd" for the xplication of hyper-parameter choose)

```
w<-data_fixture()
dfm<-w[[1]]
val<-w[[2]]
remove(w)
s<-algo_10_review(dfm,val,"users_id")

```
Then you can run the function opti_10() who take the best calib vector return in the last function algo_10_review() et run an optimization algorithm. For the moment the optimisation algorithm we use is the one explained in "idee_opti3.Rmd", as it is the fastest and most efficient. 

Example :

```
x<-s[[1]]
opti_10(dfm_fixture,x,300)
```




