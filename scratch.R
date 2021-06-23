library(readr)

##########Chargement des données

TM2gram_bis <- read_table2("C:/Users/fredo/OneDrive/Documents/Stage/CNRS/Stage/Code/Code Projet Mickael/data/raw/text/2014-nzl-new-zealand/TM2gram.csv",
                           col_names = FALSE)

link_bis <- read_csv("C:/Users/fredo/OneDrive/Documents/Stage/CNRS/Stage/Code/Code Projet Mickael/data/raw/text/2014-nzl-new-zealand/link.csv")

vpl <- read_csv("C:/Users/fredo/OneDrive/Documents/Stage/CNRS/Stage/Code/Code Projet Mickael/data/raw/vpl/2014-nzl-new-zealand-extended.csv")

#####Transformation pour avoir dfm et vpl

TM2gram_bis <-as.data.frame(TM2gram_bis)

rownames(TM2gram_bis)<-TM2gram_bis[,1]

TM2gram_bis<-TM2gram_bis[,-1]

colnames(TM2gram_bis)<-link_bis$VPL_ID


TM2gram_bis<-t(TM2gram_bis)


TM2gram_bis <-as.data.frame(TM2gram_bis)

a<-link_bis$VPL_ID

TM2gram_bis[,20754]<-a

TM2gram_test<-merge(TM2gram_bis,vpl, by.x="V20754", by.y="VPL_ID")



#sum(is.na(TM2gram_test$selfPlacementRaw))

#TM2gram_test <- subset(TM2gram_test,
#                    !is.na(TM2gram_test$selfPlacementRaw))

TM2gram_test<-TM2gram_test[1:2000,]#Warnong : error optim complete.cases()

#b<-TM2gram_test$selfPlacementRaw

Vpl_kept<-TM2gram_test[,20755:20785]
rownames(Vpl_kept)<-Vpl_kept[,1]
Vpl_kept<-Vpl_kept[,-1]

TM2gram_test<-TM2gram_test[,1:20754]
rownames(TM2gram_test)<-TM2gram_test[,1]

c<-TM2gram_test[,1]

TM2gram_test<-TM2gram_test[,-1]


op<-data.frame(c)
colnames(op)[1]<-"Vpl_ID"
#colnames(op)[2]<-"placement"


remove(TM2gram_bis)
remove(vpl)
remove(link_bis)
remove(a)
remove(b)
remove(c)


####calculer ideologie



# Factor Analysis by maximum likelihood

fact <- psych::fa(Vpl_kept, fm='ml', digits=2)
scores<-fact$scores

op[,2]<-scores

colnames(op)[2]<-"opinions calculees"

op_without_na <- subset(op,
                        !is.na(op$`opinions calculees`))

colnames(op_without_na)[2]<-"opinions calculees"

remove(scores)
remove(fact)
remove(Vpl_kept)
remove(op)

TM2gram_test[,20754]<-rownames(TM2gram_test)

TM2gram_test_without_na<-merge(TM2gram_test,op_without_na, by.x="V20754", by.y="Vpl_ID")

TM2gram_test_without_na<-TM2gram_test_without_na[,1:20754]
rownames(TM2gram_test_without_na)<-TM2gram_test_without_na[,1]
TM2gram_test_without_na<-TM2gram_test_without_na[,-1]
remove(TM2gram_test)

###########Optimisation

a<-seq(1,10)
data_cor<-data.frame(a)
colnames(data_cor)[1]<-"numéro du sample"
remove(a)

library(DLA)

for (i in 1:10){

  X1<-sample(nrow(TM2gram_test_without_na), size=round(0.01*(dim(TM2gram_test_without_na)[1])))

  for(j in 1:length(X1)){
    data_cor[i,j+1]<-X1[j]
    colnames(data_cor)[j+1]<-"donnée de calib"
  }


  x<-calibrate(TM2gram_test_without_na,complet=T,X1)

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

  op_without_na<-arrange(op_without_na,op_without_na$Vpl_ID) ###check numeric pour arrange doncr crééer

  op_match<-merge(opinions_df_arrange,op_without_na,by.x = "users",by.y = "Vpl_ID")



  validation_metrics<-cor(op_match$opinions,op_match$`opinions calculees`)


  data_cor[i,length(X1)+2]<-validation_metrics<-cor(op_match$opinions,op_match$`opinions calculees`)
  colnames(data_cor)[length(X1)+2]<-"validation_score"

}




#####Optimize data cor

###Essai de l'autre metrique

x2y(op_match$opinions,op_match$`opinions calculees`)
#################

b<-data_cor$validation_score
data_cor<-data_cor[,1:14]
data_cor[,15]<-b
data_cor[,15]<-as.numeric(data_cor[,15])
data_cor_arrange<-arrange(data_cor,data_cor$V15)[9:10,]

vecteur_calib<-data_cor_arrange[,2:14]
a<-data_cor_arrange[1,]
a<-t(a)
a<-a[2:14]
b<-data_cor_arrange[2,]
b<-t(b)
b<-b[2:14]

vecteur_calib<-c(a,b)

Z<-combn(vecteur_calib,13)

Z1<-Z[,sort(sample.int(10400600, 50))]
Z1<-t(Z1)
Z1<-as.data.frame(Z1)




for (i in 1:dim(Z1)[1]){


  X1<-as.integer(Z1[i,])

  for(j in 1:length(X1)){
    data_cor[i,j+1]<-X1[j]
    colnames(data_cor)[j+1]<-"donnée de calib"
  }


  x<-calibrate(TM2gram_test_without_na,complet=T,X1)

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

  op_without_na<-arrange(op_without_na,op_without_na$Vpl_ID) ###check numeric pour arrange doncr crééer

  op_match<-merge(opinions_df_arrange,op_without_na,by.x = "users",by.y = "Vpl_ID")



  validation_metrics<-cor(op_match$opinions,op_match$`opinions calculees`)


  data_cor[i,length(X1)+2]<-cor(op_match$opinions,op_match$`opinions calculees`)
  colnames(data_cor)[length(X1)+2]<-"validation_score_corr"

  data_cor[i,length(X1)+3]<-x2y(op_match$opinions,op_match$`opinions calculees`)
  colnames(data_cor)[length(X1)+3]<-"validation_score_x2y"
}



# plot point

library(ggplot2)

a<-data_cor$validation_score
b<-data_cor$`numéro du sample`


ggplot(data_cor, aes(y=data_cor$validation_score,x=data_cor$`numéro du sample`))+ geom_point()

ggplot(data_cor, aes(y=users,x=opinions,color=opinions))+
  geom_segment(aes(x = 0, y = users, xend = opinions, yend = users)) +
  geom_point()





############

a<-1:10
b<-letters[1:5]
expand.grid(a,b)## combinaison


W<-data_fixure()
dfm<-W[[1]]
val<-W[[2]]
remove(W)

S<-algo_10(dfm,val,"users_id")

g<-as.numeric(S[4,2:4])

D<-calibrate(dfm,complet=T,g)

data_users<-D[[2]]
word_df<-D[[1]]
opini_target<-D[[3]]


y<-use_weight(data_users,rownames(word_df),word_df)
opini_users<-y[[1]]
word_wei<-y[[2]]

example("algo_10")
example("data_fixure")




#####################Web scraping data fr

library(rvest)
library(tidyverse)
#### read page

url<-read_html("https://www.politiquemedia.com/classement-twitter.html")


###### on prends toutes les pages

page<-url%>%  html_nodes(".invisible") %>% html_attr("href")

### cree data frame

data_poll_fr<-data.frame(1,1,1,1)
colnames(data_poll_fr)<-c("Nom","Age","Parti","Twitter")

for(i in 1:length(url%>%html_nodes(".invisible.classement-twitter-nom") %>%html_text())){

  ### on rentre dans la première page

  url_test<-read_html(paste0("https://www.politiquemedia.com/" ,page[i]))

  ### on prends le nom

  data_poll_fr[i,1]<-url_test%>%html_nodes("h1") %>%html_text()

  ##on prends l'age


  data_poll_fr[i,2]<-url_test%>%html_nodes(".naissance-personnalite") %>%html_text()

  if(str_count(data_poll_fr[i,2],"")==42){data_poll_fr[i,5]="F"}
  else{data_poll_fr[i,5]="M"}


  ### on prends le parti

  data_poll_fr[i,3]<-url_test%>%html_nodes("span.parti-personnalite") %>%html_text()


  ### on prends les comptes twitter

  data_poll_fr[i,4]<-(url_test%>%html_nodes("a.invisible") %>%html_attr("href"))[2]

}
remove(i)
remove(page)
remove(url)
remove(url_test)

colnames(data_poll_fr)[5]<-"Sexe"

#####Cleaning


data_poll_fr[,1]<-gsub("Ã©", "é", data_poll_fr[,1])
data_poll_fr[,1]<-gsub("Ã§", "ç", data_poll_fr[,1])

data_poll_fr[,3]<-gsub("Ã©", "é", data_poll_fr[,3])
data_poll_fr[,3]<-gsub("Ã§", "ç", data_poll_fr[,3])

data_poll_fr[,2]<-str_sub(data_poll_fr[,2],16,27)
data_poll_fr[,2]<-gsub(" ", "", data_poll_fr[,2])
data_poll_fr[,2]<-str_sub(data_poll_fr[,2],1,10)

data_poll_fr[,4]<-str_sub(data_poll_fr[,4],21,100)
data_poll_fr[,4]<-paste0("@",data_poll_fr[,4])

###################optimisation

####idee 1

z<-c(calib_vector,10,3,7)

calib_vector_opti<-combn(z,10)
calib_vector_opti<-calib_vector_opti[,sort(sample.int(286, 50))]
calib_vector_opti<-t(calib_vector_opti)
calib_vector_opti<-as.data.frame(calib_vector_opti)

a <- seq(1,50)
data_cor <- data.frame(a)
colnames(data_cor)[1] <- "numéro du sample"
remove(a)


for (i in 1:50){


  X1<-calib_vector_opti[i,]
  X1<-as.numeric(X1)

  for(j in 1:length(X1)){
    data_cor[i,j+1]<-X1[j]
    colnames(data_cor)[j+1]<-paste0("donnée de calib",j)
  }


  x <- calibrate(dfm_fixture,complet=T,X1)

  data_users <- x[[2]]
  word_df <- x[[1]]
  opini_target <- x[[3]]

  #use weight on the other

  y <- use_weight(data_users,rownames(word_df),word_df)
  opini_users <- y[[1]]
  word_wei <- y[[2]]

  ## Data frame des opinions de tous

  library(dplyr)
  require(rpart)
  opinions_df <- rbind(opini_target, opini_users)
  opinions_df$users <- as.numeric(opinions_df$users)
  opinions_df$opinions <- as.numeric(opinions_df$opinions)


  ###Validation

  opinions_df_arrange <- arrange(opinions_df,users)

  df_validation_arrange <- arrange(df_validation,users_id)

  op_match <- merge(opinions_df_arrange,df_validation_arrange,by.x = "users",by.y = "users_id")


  validation_metrics <- cor(op_match$opinions,op_match[,3])


  data_cor[i,length(X1)+2] <- validation_metrics
  colnames(data_cor)[length(X1)+2] <- "validation_score"

  data_cor[i,length(X1)+3] <- x2y(op_match$opinions,op_match[,3])[2]
  colnames(data_cor)[length(X1)+3] <- "validation_score_x2y"

}


##### idée 2

X1<-sample(calib_vector,7)
X2<-sample(calib_vector,7)
W1<-combn(X1,5)
W1<-t(W1)
W1<-as.data.frame(W1)

W2<-combn(X2,5)
W2<-t(W2)
W2<-as.data.frame(W2)


a <- seq(1,50)
data_cor <- data.frame(a)
colnames(data_cor)[1] <- "numéro du sample"
remove(a)


for (i in 1:50){


  X1<-calib_vector_opti[i,]
  X1<-as.numeric(X1)

  for(j in 1:length(X1)){
    data_cor[i,j+1]<-X1[j]
    colnames(data_cor)[j+1]<-paste0("donnée de calib",j)
  }


  x <- calibrate(dfm_fixture,complet=T,X1)

  data_users <- x[[2]]
  word_df <- x[[1]]
  opini_target <- x[[3]]

  #use weight on the other

  y <- use_weight(data_users,rownames(word_df),word_df)
  opini_users <- y[[1]]
  word_wei <- y[[2]]

  ## Data frame des opinions de tous

  library(dplyr)
  require(rpart)
  opinions_df <- rbind(opini_target, opini_users)
  opinions_df$users <- as.numeric(opinions_df$users)
  opinions_df$opinions <- as.numeric(opinions_df$opinions)


  ###Validation

  opinions_df_arrange <- arrange(opinions_df,users)

  df_validation_arrange <- arrange(df_validation,users_id)

  op_match <- merge(opinions_df_arrange,df_validation_arrange,by.x = "users",by.y = "users_id")


  validation_metrics <- cor(op_match$opinions,op_match[,3])


  data_cor[i,length(X1)+2] <- validation_metrics
  colnames(data_cor)[length(X1)+2] <- "validation_score"

  data_cor[i,length(X1)+3] <- x2y(op_match$opinions,op_match[,3])[2]
  colnames(data_cor)[length(X1)+3] <- "validation_score_x2y"

}
