# vigilant-octo-invention
Dynamic Optimization Algorithm

Take a Data-frame with users id and text, and run the dfm generation function.
Then take the dfm and run the estimate function who takes the dfm, a vector with the rows 
of the calibration, and the col with the id.
Now you have a data frame with the weight of words and a data frame with the prediction 
of the users.
You can now run the validation with the data prediction and the real opinion's data.

``` 
Tweet<-read.csv(file="ex_tweets.csv",encoding = 'UTF-8')
Tweet<-subset(Tweet, select = c("user_id","text"))
DFM_test<-dfm_generation(Tweet,2,1,"fr")

List_test<-Estimate_prediction(DFM_test,"user_id",c(1,2,3))


```
