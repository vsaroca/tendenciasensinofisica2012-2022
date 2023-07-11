library(dplyr)
library(tidyr)
library(tidyverse)
library(mlaibr)
library(readr)
library(stringr)
library(tidytext)

#Set Workign Directory
#setwd("/Users/vsaroca/Documents/Production/Physics Publication SR")

#Dataset Geral
dsgeral <- read_ris ("dsgeral.ris", fields = NULL)
dsgeral <- spread_ris(dsgeral, multi_sep = " # ")

dsgeral <- select(dsgeral, id, UR, TI, PY, KW, AB)

dsgeral <- separate_wider_delim (dsgeral, AB, delim = " # ", names = c("ABEN", "ABPT"),
                            too_few = "align_start", too_many = "merge")
dsgeral$ABPT <- gsub("Resumo","", as.character(dsgeral$ABPT))
dsgeral$ABPT <- gsub(":","", as.character(dsgeral$ABPT))
dsgeral$ABPT <- gsub("<p>","", as.character(dsgeral$ABPT))
dsgeral$ABPT <- gsub("Abstract","", as.character(dsgeral$ABPT))
dsgeral$ABPT <- str_trim (dsgeral$ABPT, "left")

#Puxando entradas de abstracts sem resumo em inglês
dsgeral$ABPT[287] <- dsgeral$ABEN[287] 
dsgeral$ABPT[292] <- dsgeral$ABEN[292] 

#Invertendo entrada de abstract que posicionou portugues em primeiro lugar
dsgeral$ABPT[18] <- dsgeral$ABEN[18]
dsgeral$ABPT[26] <- dsgeral$ABEN[26]

#Dataset Quant
dsquant <- read_ris ("dsquant.ris", fields = NULL)
dsquant <- spread_ris(dsquant, multi_sep = " # ")

dsquant <- select(dsquant, id, UR, TI, PY, KW, AB)

dsquant <- separate_wider_delim (dsquant, AB, delim = " # ", names = c("ABEN", "ABPT"),
                                  too_few = "align_start", too_many = "merge")
dsquant$ABPT <- gsub("Resumo","", as.character(dsquant$ABPT))
dsquant$ABPT <- gsub(":","", as.character(dsquant$ABPT))
dsquant$ABPT <- gsub("<p>","", as.character(dsquant$ABPT))
dsquant$ABPT <- str_trim (dsquant$ABPT, "left")


##Data Consolidation

#Publications Per Year PY
 
PG <- dsgeral %>% count(PY)
PQ <- dsquant %>% count(PY)
PTotal <- merge(x = PG, y = PQ, by = "PY", all.x = TRUE)
colnames(PTotal) <- c("Year", "PG", "PQ")

##Content Analisys

#Merge Title, Keywords and ABPT

dsgm <- dsgeral
dsgm$CA <- paste (dsgm$TI, dsgm$KW)
dsgm <- select(dsgm, id, PY, CA)

dsqm <- dsquant
dsqm$CA <- paste (dsqm$TI, dsqm$KW)
dsqm <- select(dsqm, id, PY, CA)

#Word Analysis DataSets

CAdsgm <-
dsgm %>%  unnest_tokens(word, CA)
CAdsgm <- CAdsgm %>% anti_join(get_stopwords(language = "pt", source = "stopwords-iso"))
CAdsgm <- CAdsgm %>% anti_join(get_stopwords(language = "en"))
CAdsgm <- CAdsgm %>% group_by(PY)
CAdsgmresultados <- CAdsgm  %>%  count(word, sort = TRUE)
CAdsgmresultados <- CAdsgmresultados %>% filter(n<9)





#Export CSVs

write_csv(dsgeral, "dsgeral.csv", col_names = TRUE)
write_csv(dsquant, "dsquant.csv", col_names = TRUE)
write_csv(PTotal, "PTotal.csv", col_names = TRUE)
write_csv(CAdsgmresultados, "CAdsgmresultados.csv", col_names = TRUE)
write_csv(CAdsqmresultados, "CAdsqmresultados.csv", col_names = TRUE)


