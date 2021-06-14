### Vigilant-Octo-Invention

## About

This R package is designed to estimate a target using analyse and weight of words.

## How to install

From GitHub for the latest development version:

```
devtools::install_github("mickaeltemporao/dynamic-lexicon-algorithm") 

```

## How to use

# From a dataset

You can run the dfm_generation() function to create a Documents-Features-Matrix from a data with users and their text (tweets fro example) like this :

```
 Tweet <- read.csv(file="ex_tweets.csv",encoding = 'UTF-8')
 Tweet <- subset(Tweet, select = c("user_id","text"))
 DFM_test <- dfm_generation(Tweet,2,1,"fr")

```

# Estimate parameters

When you have a dfm you can run calibrate() function to estimate the weights of words using the wordfish() function :


```
 w <- data_fixture()
   dfm <- w[[1]]
 
 x<-calibrate(dfm,complet=T,c(1,2,3))
   data_users <- x[[2]]
   word_df <- x[[1]]
   opini_target <- x[[3]]

```

# Predict target

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

# Validation

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

# Optimisation

If you don't have the vector calibration you can run the function algo_10 who the best vector with 10 sample :

```
w<-data_fixture()
dfm<-w[[1]]
val<-w[[2]]
remove(w)
s<-algo_10(dfm,val,"users_id")

```


