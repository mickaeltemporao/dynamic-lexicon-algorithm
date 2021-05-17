###### POUR L'ESSAI

setwd("C:/Users/fredo/OneDrive/Documents/Stage/CNRS/Stage/Code")

Tweet<-read.csv('Ex_Tweets.csv',encoding = 'UTF-8')#on charge le csv des tweets

Tweet<-subset(Tweet, select = c("user_id","text"))#on garde que les tweets des user et leurs id



dfm_generation<-function(
  input, #2 col (text & users )
  text,  #numéro de colonne ou il y a le text
  doc,   #numéro de colonne ou il y a les ids
  lg  # "en", "fr" etc..
  ){
  
library(quanteda)
library(dplyr)



#stemming
  #stopwords option
  #as.numeric(Sys.time()-start)
  #sys.time()



# On créée un toks avec les textes pour le dfm

tok<-tokens(input[,text], remove_punct = TRUE,remove_symbols=TRUE,remove_numbers=TRUE,remove_url=TRUE,remove_separators=TRUE)

df<-dfm(tok)# On le convertit en dfm

#On enleve les mots inutiles (ex mots de liaisons ou autres)

dfstop <- dfm_select(df, pattern = stopwords(lg), selection = "remove")

#On garde que les mots plus long que 3

dfstop<-dfm_keep(dfstop, min_nchar = 3)

# FILTER  : On garde que les mots cités au moins 5 fois 
# a word needs to belong to enough users 

dfstop<- dfm_trim(dfstop, min_termfreq = 5)

#Maintenant on fusionne les tweets des mêmes id

#comme ca  

#df<-as.data.frame(dfstop) 
#df<-df[,-1]#on enleve la colonne id
#a<-Tweet$user_id
#df[,1491]<-a
#colnames(df)[1491]<-"user_id"

#dfM<-aggregate(df[,1:1490], by=list(user_id=df$user_id), FUN=sum)

# ou comme ca (plus rapide)

DFM <- dfm_group(dfstop, groups = Tweet$user_id)
DFM<-as.data.frame(DFM)

cat("Finished \n")

return(list(DFM = DFM))
}

dfm_generation(Tweet,2,1,"fr")
DFM
