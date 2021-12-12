library(tidyverse)
library(tidyr)
library(magrittr)
library(stringr)
library(zoo)


netflix <- read.csv("netflix-rotten-tomatoes-metacritic-imdb.csv", header = TRUE)

netflix <- netflix %>% filter(Netflix.Release.Date >= "2018-01-01")
netflix <- netflix[, c(1:20,25)]
netflix$Release.Date <- gsub("[ ]","-",netflix[,19])

netflix$Release.Date <- as.Date(netflix$Release.Dat,"%d-%b-%Y")
netflix$Netflix.Release.Date <- as.Date(netflix$Netflix.Release.Date,"%Y-%m-%d")

#netflix$Netflix.Release.Date <- as.yearmon(netflix$Netflix.Release.Date,"%Y-%m")

netflix <- netflix %>% filter(Release.Date >= "2018-01-01")
netflix <- subset(netflix,IMDb.Score!="NA")
netflix1 <- subset(netflix,IMDb.Score!="NA")

for (i in 1:nrow(netflix[is.na(netflix$Rotten.Tomatoes.Score),])) {
  netflix[is.na(netflix$Rotten.Tomatoes.Score),]$Rotten.Tomatoes.Score <- rep("0", nrow(netflix[is.na(netflix$Rotten.Tomatoes.Score),]))
}
for (i in 1:nrow(netflix[is.na(netflix$Metacritic.Score),])) {
  netflix[is.na(netflix$Metacritic.Score),]$Metacritic.Score <- rep("0", nrow(netflix[is.na(netflix$Metacritic.Score),]))
}
for (i in 1:nrow(netflix[is.na(netflix$Awards.Received),])) {
  netflix[is.na(netflix$Awards.Received),]$Awards.Received <- rep("0", nrow(netflix[is.na(netflix$Awards.Received),]))
}



#Genre
netflix$num.Genre <- str_count(netflix$Genre,pattern = ",")+1

netflix %<>% separate(Genre,c("Major.Genre","Sec.Genre"), sep=",")
for (i in 1:nrow(netflix[is.na(netflix$Sec.Genre),])) {
  netflix[is.na(netflix$Sec.Genre),]$Sec.Genre <- rep("", nrow(netflix[is.na(netflix$Sec.Genre),]))
}

for (i in 1:nrow(netflix[is.na(netflix$Sec.Genre),])) {
  netflix[is.na(netflix$Sec.Genre),]$Sec.Genre <- rep("", nrow(netflix[is.na(netflix$Sec.Genre),]))
}


for (i in 1:nrow(netflix[is.na(netflix$Awards.Nominated.For),])) {
  netflix[is.na(netflix$Awards.Nominated.For),]$Awards.Nominated.For <- rep("", nrow(netflix[is.na(netflix$Awards.Nominated.For),]))
}

#tags
netflix$num.tags <- str_count(netflix$Tags,pattern = ",")+1

#country
netflix$num.country <- str_count(netflix$Country.Availability,pattern = ",")+1

#runtime
netflix$Runtime_1hour <- ifelse(netflix$Runtime=="1-2 hour"|
                                  netflix$Runtime=="> 2 hrs", 1,0)

#Languages
netflix$num.Lang <- str_count(netflix$Languages,pattern = ",")+1
netflix %<>% separate(Languages,c("Major.Lang","Sec.Lang"), sep=",")
for (i in 1:nrow(netflix[is.na(netflix$Sec.Lang),])) {
  netflix[is.na(netflix$Sec.Lang),]$Sec.Lang <- rep("", nrow(netflix[is.na(netflix$Sec.Lang),]))
}

netflix$Major.Genre[netflix$Major.Genre==""] <- NA
netflix$Major.Genre[netflix$Major.Lang==""] <- NA
netflix$Awards.Nominated.For[netflix$Awards.Nominated.For==""] <- "0"

netflix <- subset(netflix,IMDb.Score!="NA")


netflix$Rotten.Tomatoes.Score <- as.numeric(netflix$Rotten.Tomatoes.Score)
netflix$Metacritic.Score <- as.numeric(netflix$Metacritic.Score)
netflix$Awards.Received <- as.numeric(netflix$Awards.Received)
netflix$Awards.Nominated.For <- as.numeric(netflix$Awards.Nominated.For)
netflix<- na.omit(netflix)
netflix1 <- netflix[,c(1,2,7,15:19,23:28)]
netflix2 <- netflix1[,c(4:14)]


