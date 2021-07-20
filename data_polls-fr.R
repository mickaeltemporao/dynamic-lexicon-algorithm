
library(rvest)
library(tidyverse)

# Read page --------------

url <- read_html("https://www.politiquemedia.com/classement-twitter.html")


# On prends toutes les pages --------------

page <- url %>%

  html_nodes(".invisible") %>%
  html_attr("href")



# Creation data frame -----------------

data_poll_fr <- data.frame(1,1,1,1)

colnames(data_poll_fr) <- c("nom","age","parti","twitter")


# Boucle pour rentrer dans toutes les pages -------------


for (i in 1:length (url %>%
                  html_nodes(".invisible.classement-twitter-nom") %>%
                  html_text() )
    ) {

  # on rentre dans la première page

  url_test <- read_html( paste0("https://www.politiquemedia.com/" , page[i]) )

  # on prends le nom

  data_poll_fr[i,1] <- url_test %>%
                html_nodes("h1") %>%
                html_text()

  # on prends l'age


  data_poll_fr[i,2] <- url_test %>%
                html_nodes(".naissance-personnalite") %>%
                html_text()

  # si née alors femme sinon homme

  if (str_count (data_poll_fr[i,2] , "" ) == 42 ) {
    data_poll_fr[i,5] = "F"
    }
  else {
    data_poll_fr[i,5] = "M"
    }


  # on prends le parti

  data_poll_fr[i,3] <- url_test %>%
                html_nodes("span.parti-personnalite") %>%
                html_text()


  # on prends les comptes twitter

  data_poll_fr[i,4] <- ( url_test %>%
                           html_nodes("a.invisible") %>%
                           html_attr("href") )[2]

}

# Removing objects ------------

remove(i)
remove(page)
remove(url)
remove(url_test)


# Add names --------------

colnames(data_poll_fr)[5]<-"Sexe"



# Cleaning -------------


data_poll_fr[, 1] <- gsub( "Ã©" , "é" , data_poll_fr[, 1] )
data_poll_fr[, 1] <- gsub( "Ã§", "ç" , data_poll_fr[, 1] )

data_poll_fr[, 3] <- gsub( "Ã©" , "é" , data_poll_fr[, 3] )
data_poll_fr[, 3] <- gsub( "Ã§" , "ç" , data_poll_fr[, 3] )

data_poll_fr[, 2] <- str_sub( data_poll_fr[, 2] , 16 , 27 )
data_poll_fr[, 2] <- gsub( " " , "" , data_poll_fr[, 2] )
data_poll_fr[, 2] <- str_sub( data_poll_fr[, 2] , 1 , 10 )

data_poll_fr[, 4] <- str_sub( data_poll_fr[, 4] , 21 , 100 )
data_poll_fr[, 4] <- paste0( "@" , data_poll_fr[, 4] )


save( data_poll_fr , file = "data/data_polls_fr.rda" )
