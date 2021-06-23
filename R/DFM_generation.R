#'  dfm_generation
#'
#'  Création d'une Documents-Features-Matrix à partir d'un data frame de
#'  texte et d'users
#'
#' @param input un data frame avec au moins une colonne représentant l'ID
#' de l'user et une colonne représentant le text/Tweet
#' @param text le numéro de la colonne du text/Tweet
#' @param doc le  numéro de la colonne ID
#' @param lg le language des Tweets (une suele langue)
#'
#'
#' @return la fonction renvoie le dfm
#'
#' @import dplyr
#' @import quanteda
#' @example
#' Tweet <- read.csv(file="ex_tweets.csv",encoding = 'UTF-8')
#' Tweet <- subset(Tweet, select = c("user_id","text"))
#' dfm_generation(Tweet,2,1,"fr")
#' remove(Tweet)
#' @export
dfm_generation <- function(
  input, #2 col (text & users )
  text,  #numéro de colonne ou il y a le text
  doc,   #numéro de colonne ou il y a les ids
  lg  # "en", "fr" etc..
){


library(quanteda)


  # On créée un toks avec les textes pour le dfm

  tok <- tokens(input[,text], remove_punct = TRUE,remove_symbols=TRUE,remove_numbers=TRUE,remove_url=TRUE,remove_separators=TRUE)

  df <- dfm(tok)# On le convertit en dfm

  #On enleve les mots inutiles (ex mots de liaisons ou autres)

  dfstop <- dfm_select(df, pattern = stopwords(lg), selection = "remove")

  #On garde que les mots plus long que 3

  dfstop  <-dfm_keep(dfstop, min_nchar = 3)

  # FILTER  : On garde que les mots cités au moins 5 fois
  # a word needs to belong to enough users

  dfstop <- dfm_trim(dfstop, min_termfreq = 5)

  #Maintenant on fusionne les tweets des mêmes id

  #comme ca

  #df<-as.data.frame(dfstop)
  #df<-df[,-1]#on enleve la colonne id
  #a<-Tweet$user_id
  #df[,1491]<-a
  #colnames(df)[1491]<-"user_id"

  #dfM<-aggregate(df[,1:1490], by=list(user_id=df$user_id), FUN=sum)

  # ou comme ca (plus rapide)

  DFM <- dfm_group(dfstop, groups = input[,doc])
  DFM <- convert(DFM, to = "data.frame")
  rownames(DFM)<-DFM[,1]

  DFM <- DFM[,-1]



  cat("Finished \n")

  return(DFM)
}

