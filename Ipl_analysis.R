imdb<-read.csv("movie_metadata.csv",stringsAsFactors = F)
summary(imdb)
colSums(is.na(imdb))
str(imdb)
library(dplyr)
library(ggplot2)
View(imdb)


proper_feature_names <- function(input_table){
  
  #--------------------------------------------
  # This function normalizes the column names.
  # INPUT -- Table with messed up column names.
  # OUTPUT -- Table with proper column names.
  #--------------------------------------------
  
  colnames(input_table) <- tolower(colnames(input_table))
  
  colnames(input_table) <- gsub('([[:punct:]])|\\s+','_',colnames(input_table))
  
  while (any(grepl("__",colnames(input_table),fixed = TRUE)) == TRUE){
    colnames(input_table) <- gsub("__","_",colnames(input_table),fixed = TRUE) 
  }
  
  colnames(input_table) <- gsub("\\*$", "",colnames(input_table))
  
  return(input_table)
}

imdb<-proper_feature_names(imdb)
names(imdb)
##############################
##Director with highest number of IMDB rating
director<-imdb%>%
  group_by(director_name)%>%
  summarise(score=mean(imdb_score))%>%
  arrange(desc(score))

