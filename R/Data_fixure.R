
make_data_fixure<-function(nb_users){

library(quanteda)

########## Creation du DFM  
  
  
#On crée un data avec les politiciens, ici on prends des mots des présidents américains lors de leurs discours

data_pol<-data_corpus_inaugural %>% corpus_subset(Year > 2011) %>%tokens()
data_pol<-dfm(data_pol)# on transforme en dfm
data_pol<-dfm_keep(data_pol, min_nchar = 2)#on garde que les mots de plus de 2 lettres
data_pol<-dfm_remove(data_pol, pattern = stopwords("en"))#on enleve les mots communs (ponctuation, et ..)
data_pol<-as.data.frame(data_pol)
rownames(data_pol)<-data_pol[,1]
data_pol<-data_pol[,-1]


dim(data_pol) # 1343 features

# On crée un data users avec des occurences aléatoires

a<-c((1:nb_users)) # colonnes des users


b<-abs(c(rnorm(nb_users, mean=0.4, sd=3))) # occurences
b<-round(b)

data_users<-data.frame(a,b)
data_users<-data_users[,-1]
data_users<-as.data.frame(data_users)

for(i in 2:dim(data_pol)[2]){
  data_users[,i]<-round(abs(c(rnorm(nb_users, mean=0.4, sd=3)))) # on rajoute autant qu'il y a de mots
}

colnames(data_users)<-colnames(data_pol) # on mets les mêmes noms pour fusionner

data<-rbind(data_pol, data_users)

data[,dim(data)[2]+1]<-1



for(i in 1:dim(data)[1]){
  if(i<4){
    data[i,dim(data)[2]]=1
  }
else{data[i,dim(data)[2]]=0}
  }

data[,dim(data)[2]]

colnames(data)[dim(data)[2]] <- "ID"


class(colnames(data))

dfm_fixure<-data# data finale


saveRDS(dfm_fixure,"data/data_fixure.rds", row.names = TRUE)



########### Creation du Data frame validation

df_validation<-data.frame(rownames(data_users))
colnames(df_validation)<-'users_id'
for(i in 1:dim(df_validation)[1]){
  df_validation[i,2]<-rnorm(1, mean=0.4, sd=3)
}

saveRDS(df_validation,"data/df_validation.rds", row.names = TRUE)

return(list(dfm_fixure=dfm_fixure,df_validation=df_validation))

}

make_data_fixure(1000)
