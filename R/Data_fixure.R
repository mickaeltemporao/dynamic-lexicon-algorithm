library(quanteda)

#On crée un data avec les politiciens, ici on prends des mots des présidents américains lors de leurs discours

data_pol<-data_corpus_inaugural %>% corpus_subset(Year > 2011) %>%tokens()
data_pol<-dfm(data_pol)# on transforme en dfm
data_pol<-dfm_keep(data_pol, min_nchar = 2)#on garde que les mots de plus de 2 lettres
data_pol<-dfm_remove(data_pol, pattern = stopwords("en"))#on enleve les mots communs (ponctuation, et ..)
data_pol<-as.data.frame(data_pol)
rownames(data_pol)<-data_pol[,1]
data_pol<-data_pol[,-1]

#data_pol[4,]<-data_pol[3,]
#data_pol[3,]<-data_pol[2,]
#data_pol[2,]<-data_pol[1,]
#data_pol[1,]<-colnames(data_pol)
#data_pol[1,1]<-"users/words"

dim(data_pol) # 1343 features

# On crée un data users avec des occurences aléatoires

a<-c((1:1000)) # colonnes des users


b<-abs(c(rnorm(1000, mean=0.4, sd=3))) # occurences
b<-round(b)

data_users<-data.frame(a,b)
data_users<-data_users[,-1]
data_users<-as.data.frame(data_users)

for(i in 2:1343){
  data_users[,i]<-round(abs(c(rnorm(1000, mean=0.4, sd=3)))) # on rajoute autant qu'il y a de mots
}

colnames(data_users)<-colnames(data_pol) # on mets les mêmes noms pour fusionner

data<-rbind(data_pol, data_users)

data[,1344]<-1

for(i in 1:1003){
  if(i<4){
    data[i,1344]=1
  }
else{data[i,1344]=0}
  }

data[,1344]

colnames(data)[1344] <- "ID"


class(colnames(data))

db<-data# data finale

plot(density(as.numeric(data[1,c(1:600)])))#loi de poisson

write.csv(db,"C:/Users/fredo/OneDrive/Documents/Stage/CNRS/Stage/Code/Code Projet/vigilant-octo-invention\\Data_fixure.csv", row.names = TRUE)
