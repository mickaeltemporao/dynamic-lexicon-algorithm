# vigilant-octo-invention
Dynamic Optimization Algorithm

This package is designed to estimate a target use weight of words.

In order to do this, you can take a data-frame with at least the users ids and the text used, and run the dfm generation function called dfm_generation. This function constructs a Documents-Features-Matrix.
Then, you can now estimate the weights of words with the function calibrate, who take the dfm and estimate the words.
Then use the weights on the others users.
Now you have a data frame with the weight of words and a data frame with the prediction 
of the users.
You can now run the validation with the data prediction and the real opinion's data.

``` 
Z<-read.csv(file="data/ex_tweets.csv",encoding = 'UTF-8')

Z<-subset(Z, select = c("user_id","text"))

X1<-DLA::dfm_generation(Z,2,1,"fr")

X2<-DLA::calibrate(X1,complet = T,c(1,2))

data_users<-X2[[2]]
word_df<-X2[[1]]
top<-X2[[3]]
words<-X2[[4]]
opini_target<-X2[[5]]

Y<-DLA::use_weight(X1,words,word_df)
opini_df<-Y[[1]]


```
