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
expand.grid(a,b)


