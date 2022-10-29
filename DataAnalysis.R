#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("skimr")
library(skimr)
#install.packages("DataExplorer")
library(DataExplorer)
#install.packages("ggpubr")
library(ggpubr)
# install.packages(dlookr)
# install.packages("flextable")
library(dlookr)
library(flextable)
#install.packages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud2")
library("wordcloud2")
#install.packages("tm")
library(tm)
# install.packages("caret")
library(caret)
library(lubridate)
library(data.table)
library(tidytext)
# library(SnowballC)
library(e1071)





fkj <- read.csv("fake_job_postings.csv")
fkj
summary(fkj)
colnames(fkj)
str(fkj)
fkj %>%
  View()


length(unique(fkj$job_id))

for(i in 1:ncol(fkj)) {       # for-loop over columns
  print(names(fkj)[i])
  print(length(unique(fkj[ , i])))
}

#Finding total Missing values each column  have
for(i in 1:ncol(fkj)) {       
  print(names(fkj)[i])
  #print(sum(is.na(fkj[ , i])))
  print(sum(fkj[ , i] == ''))
}
#sum(fkj$benefits == '')


plot_str(fkj, type = c("diagonal", "radial"), print_network = TRUE)

head(fkj)

skim(fkj)
#Finding Data Quality
plot_intro(fkj, title = "Data Quality")

#Plotting the missing data
plot_missing(fkj, title = "Missing data")

#Plot Salary Range
fkj %>% group_by(salary_range) %>% summarize(Freq = n()) %>% arrange(desc(Freq)) %>% slice(2:21) %>% 
  ggplot(aes(x = salary_range, y = Freq)) + geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  theme_bw() + geom_text(aes(label=round(Freq,0)), vjust=2) + theme(axis.text.x=element_text(size=10, angle=90,hjust=0.5,vjust=1)) + 
  labs(title = "Salary Range Data with Count Plot",
       x = "Salary Range $",
       y = "Number of values")

fkj_title_grouped <- fkj
#Group with respect to Title of Job
fkj_title_grouped %>% group_by(title) %>% summarize(Freq = n()) %>% arrange(desc(Freq))

Fraudulent %>% group_by(title) %>% summarize(Freq = n()) %>% arrange(desc(Freq)) %>%  slice(1:10) %>%
  ggplot(aes(x=reorder(title, -Freq), y = Freq)) +
  geom_segment( aes(x=reorder(title, -Freq), xend=reorder(title, -Freq), y=0, yend=Freq), color="black") +
  geom_point( color="green", size=2, alpha=1) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) + 
  theme_bw() + 
  labs(title = "Top 10 jobs listed",
       x = "Job Title",
       y = "Count") + geom_text(aes(label=round(Freq,0)), vjust=-0.8)

fkj_location_grouped <- fkj
Fraudulent %>% group_by(location) %>% summarize(Freq = n()) %>% arrange(desc(Freq)) %>%  slice(1:10) %>%
  ggplot(aes(x=reorder(location, -Freq), y = Freq)) +
  geom_segment( aes(x=reorder(location, -Freq), xend=reorder(location, -Freq), y=0, yend=Freq), color="black") +
  geom_point( color="green", size=2, alpha=1) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) + 
  theme_bw() + 
  labs(location = "Top 10 Locations listed",
       x = "Job location",
       y = "Count") + geom_text(aes(label=round(Freq,0)), vjust=-0.8)

Fraudulent %>% group_by(department) %>% summarize(Freq = n()) %>% arrange(desc(Freq)) %>%  slice(1:10) %>%
  ggplot(aes(x=reorder(department, -Freq), y = Freq)) +
  geom_segment( aes(x=reorder(department, -Freq), xend=reorder(department, -Freq), y=0, yend=Freq), color="black") +
  geom_point( color="green", size=2, alpha=1) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) + 
  theme_bw() + 
  labs(department = "Top 10 departments listed",
       x = "Job department",
       y = "Count") + geom_text(aes(label=round(Freq,0)), vjust=-0.8)


data_location_clean <- gsub("(.*),.*", "\\1", Fraudulent$location)
data_location_clean_countries <- gsub("(.*),.*", "\\1", data_location_clean)
data_location_clean_countries <- data.frame(Country = data_location_clean_countries)


data_location_clean_countries %>% group_by(Country) %>% summarize(Freq= n()) %>% arrange(desc(Freq)) %>% slice(1:5,7:11) %>%
  ggplot(aes(x= reorder(Country, -Freq), y = Freq)) + geom_bar(stat = "identity", color = "black", fill = "steelblue") + theme_bw() +
  labs(title = "Top 10 countries with number of Fraudulent jobs listed",
       x = "Job Country",
       y = "Count") +
  geom_text(aes(label=round(Freq,0)), vjust=-0.5) + theme(axis.text.x=element_text(size=10, angle=0,hjust=0.5,vjust=1))



Fraudulent$telecommuting <- if_else(Fraudulent$telecommuting == 1, "Required", "Not Required")
Fraudulent %>% group_by(telecommuting) %>% summarize(Freq = n()) %>% arrange(desc(Freq)) %>% 
  ggplot(aes(x = reorder(telecommuting, -Freq), y = Freq)) + geom_bar(stat = "identity", fill = "steelblue", color = "black") + 
  theme_bw() + labs(title = "Fraudulent Jobs that require telecommuting",
                    x = "Telecommuting",
                    y = "Count") +
  geom_text(aes(label=round(Freq,0)), vjust= - 0.5) + theme(axis.text.x=element_text(size=10, angle=90,hjust=0.5,vjust=1))




Fraudulent %>% group_by(employment_type) %>% summarize(Freq = n()) %>% arrange(desc(Freq)) %>% slice(1,3:6) %>%
  ggplot(aes(x = reorder(employment_type, -Freq), y = Freq, fill = employment_type)) + geom_bar(stat = "identity",  color = "black") + 
  theme_bw() + labs(title = "Fraudulent Employment types",
                    x = "Type",
                    y = "Count",
                    fill = "Employment Type") +
  geom_text(aes(label=round(Freq,0)), vjust= - 0.5) + theme(axis.text.x=element_text(size=10, angle=90,hjust=0.5,vjust=1))



Fraudulent %>% group_by(required_experience) %>% summarize(Freq = n()) %>% arrange(desc(Freq)) %>% slice(2:8) %>%
  ggplot(aes(x = reorder(required_experience, -Freq), y = Freq, fill = required_experience)) + geom_bar(stat = "identity", color = "black") + 
  theme_bw() + labs(title = "Experience required",
                    x = "Required Experience in Fraudulent Posts",
                    y = "Count",
                    fill = "Required Experience") +
  geom_text(aes(label=round(Freq,0)), vjust= -0.2) + theme(axis.text.x=element_text(size=10, angle=90,hjust=0.5,vjust=1))




Non_Fraudulent %>% group_by(required_education) %>% summarize(Freq = n()) %>% arrange(desc(Freq)) %>% slice(2:14) %>%
  ggplot(aes(x=reorder(required_education, -Freq), y = Freq)) +
  geom_segment( aes(x=reorder(required_education, -Freq), xend=reorder(required_education, -Freq), y=0, yend=Freq), color="skyblue") +
  geom_point( color="steelblue", size=2, alpha=1) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) +
  theme_bw() + labs(title = "Education required",
                    x = "Education",
                    y = "Count",
                    fill = "Education") + 
  geom_text(aes(label=round(Freq,0)), vjust=-0.6)



Fraudulent %>% group_by(industry) %>% summarize(Freq = n()) %>% arrange(desc(Freq)) %>% slice(2:11) %>%
  ggplot(aes(x = reorder(industry, -Freq), y = Freq)) + geom_bar(stat = "identity", color = "black", fill = "purple") + 
  theme_bw() + labs(title = "Top 10 industries with Frudulent Post",
                    x = "Industry",
                    y = "Count",
                    fill = "Education") +
  geom_text(aes(label=round(Freq,0)), vjust= -0.2) + theme(axis.text.x=element_text(size=10, angle=90,hjust=0.5,vjust=1))


fkj %>% group_by(fraudulent) %>%  ggplot(aes(fraudulent, group = fraudulent)) + 
  geom_bar(aes(fill = fraudulent), stat = "count") + 
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  geom_text(aes(label=..count..),stat='count',position=position_stack(vjust=0.5), color = "white") + 
  ggtitle("Fraudulent vs. Non Fraudulent Jobs") + xlab("Fraud Flag") + ylab("Job Count") + theme_bw()

#2nd Column department with most missing values analysis
fkj %>% group_by(fkj$department) %>% summarize(Freq = n()) %>% arrange(desc(Freq))

fkj_dept <- fkj$department

fkj_dept <- fkj[!fkj$department == "", ]
fkj_deptt <- fkj_dept$department
fkj_deptt
fkj_deptt %>% group_by(fkj_deptt) %>% summarize(Freq = n()) %>% arrange(desc(Freq))
barplot(table(fkj_deptt),
        main="Departments",
        xlab="Departments",
        ylab="Count",
        border="black",
        col="blue",
        density=100
)

fkj_dept <- fkj %>%
  select(department)
fkj_dept %>%
  count(department, sort = TRUE)
  View()

#Fraudulent Data Analysis
Fraudulent <- fkj[ which(fkj$fraudulent== 1), ]
Non_Fraudulent <- fkj[ which(fkj$fraudulent== 0), ]


#Columns to Analyse as Word Clouds
#1. Title
#3 Versions All Data, Fraudulent Data, Non-Fraudulent Data
#Data Selection Containing Only Text data
title_all <-fkj$title
title_f <-Fraudulent$title
title_nf <- Non_Fraudulent$title

#Creating Data Courps
title_all_C <- Corpus(VectorSource(title_all))
title_f_C <- Corpus(VectorSource(title_f))
title_nf_C <- Corpus(VectorSource(title_nf))

#Cleaning the data
title_all_C <- title_all_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
title_all_C <- tm_map(title_all_C, content_transformer(tolower))
title_all_C <- tm_map(title_all_C, removeWords, stopwords("english"))


title_f_C <- title_f_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
title_f_C <- tm_map(title_f_C, content_transformer(tolower))
title_f_C <- tm_map(title_f_C, removeWords, stopwords("english"))



title_nf_C <- title_nf_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
title_nf_C <- tm_map(title_nf_C, content_transformer(tolower))
title_nf_C <- tm_map(title_nf_C, removeWords, stopwords("english"))


#Creating Document Term Matrix, 
#and its data frame so that the first column will represent word 2nd will represent frequency
title_all_dtm <- TermDocumentMatrix(title_all_C) 
title_all_matrix <- as.matrix(title_all_dtm) 
title_all_words <- sort(rowSums(title_all_matrix),decreasing=TRUE) 
title_all_df <- data.frame(words = names(title_all_words),freq=title_all_words)


title_f_dtm <- TermDocumentMatrix(title_f_C) 
title_f_matrix <- as.matrix(title_f_dtm) 
title_f_words <- sort(rowSums(title_f_matrix),decreasing=TRUE) 
title_f_df <- data.frame(word = names(title_f_words),freq=title_f_words)


title_nf_dtm <- TermDocumentMatrix(title_nf_C) 
title_nf_matrix <- as.matrix(title_nf_dtm) 
title_nf_words <- sort(rowSums(title_nf_matrix),decreasing=TRUE) 
title_nf_df <- data.frame(word = names(title_nf_words),freq=title_nf_words)

#Generating the Word Cloud from DataFrames
set.seed(1234)
wordcloud(words = title_all_df$word, freq = title_all_df$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = title_f_df$word, freq = title_f_df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = title_nf_df$word, freq = title_nf_df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
##################################################################################
#Word cloud of location variable
#Data Selection Containing Only Text data
location_all <-fkj$location
location_f <-Fraudulent$location
location_nf <- Non_Fraudulent$location

#Creating Data Courps
location_all_C <- Corpus(VectorSource(location_all))
location_f_C <- Corpus(VectorSource(location_f))
location_nf_C <- Corpus(VectorSource(location_nf))

#Cleaning the data
location_all_C <- location_all_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
location_all_C <- tm_map(location_all_C, content_transformer(tolower))
location_all_C <- tm_map(location_all_C, removeWords, stopwords("english"))


location_f_C <- location_f_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
location_f_C <- tm_map(location_f_C, content_transformer(tolower))
location_f_C <- tm_map(location_f_C, removeWords, stopwords("english"))



location_nf_C <- location_nf_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
location_nf_C <- tm_map(location_nf_C, content_transformer(tolower))
location_nf_C <- tm_map(location_nf_C, removeWords, stopwords("english"))


#Creating Document Term Matrix, 
#and its data frame so that the first column will represent word 2nd will represent frequency
location_all_dtm <- TermDocumentMatrix(location_all_C) 
location_all_matrix <- as.matrix(location_all_dtm) 
location_all_words <- sort(rowSums(location_all_matrix),decreasing=TRUE) 
location_all_df <- data.frame(words = names(location_all_words),freq=location_all_words)


location_f_dtm <- TermDocumentMatrix(location_f_C) 
location_f_matrix <- as.matrix(location_f_dtm) 
location_f_words <- sort(rowSums(location_f_matrix),decreasing=TRUE) 
location_f_df <- data.frame(word = names(location_f_words),freq=location_f_words)


location_nf_dtm <- TermDocumentMatrix(location_nf_C) 
location_nf_matrix <- as.matrix(location_nf_dtm) 
location_nf_words <- sort(rowSums(location_nf_matrix),decreasing=TRUE) 
location_nf_df <- data.frame(word = names(location_nf_words),freq=location_nf_words)

#Generating the Word Cloud from DataFrames
set.seed(1234)
wordcloud(words = location_all_df$word, freq = location_all_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = location_f_df$word, freq = location_f_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = location_nf_df$word, freq = location_nf_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

##################################################################################
#Word cloud of department variable
#Data Selection Containing Only Text data
department_all <-fkj$department
department_f <-Fraudulent$department
department_nf <- Non_Fraudulent$department

#Creating Data Courps
department_all_C <- Corpus(VectorSource(department_all))
department_f_C <- Corpus(VectorSource(department_f))
department_nf_C <- Corpus(VectorSource(department_nf))

#Cleaning the data
department_all_C <- department_all_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
department_all_C <- tm_map(department_all_C, content_transformer(tolower))
department_all_C <- tm_map(department_all_C, removeWords, stopwords("english"))


department_f_C <- department_f_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
department_f_C <- tm_map(department_f_C, content_transformer(tolower))
department_f_C <- tm_map(department_f_C, removeWords, stopwords("english"))



department_nf_C <- department_nf_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
department_nf_C <- tm_map(department_nf_C, content_transformer(tolower))
department_nf_C <- tm_map(department_nf_C, removeWords, stopwords("english"))


#Creating Document Term Matrix, 
#and its data frame so that the first column will represent word 2nd will represent frequency
department_all_dtm <- TermDocumentMatrix(department_all_C) 
department_all_matrix <- as.matrix(department_all_dtm) 
department_all_words <- sort(rowSums(department_all_matrix),decreasing=TRUE) 
department_all_df <- data.frame(words = names(department_all_words),freq=department_all_words)


department_f_dtm <- TermDocumentMatrix(department_f_C) 
department_f_matrix <- as.matrix(department_f_dtm) 
department_f_words <- sort(rowSums(department_f_matrix),decreasing=TRUE) 
department_f_df <- data.frame(word = names(department_f_words),freq=department_f_words)


department_nf_dtm <- TermDocumentMatrix(department_nf_C) 
department_nf_matrix <- as.matrix(department_nf_dtm) 
department_nf_words <- sort(rowSums(department_nf_matrix),decreasing=TRUE) 
department_nf_df <- data.frame(word = names(department_nf_words),freq=department_nf_words)

#Generating the Word Cloud from DataFrames
set.seed(1234)
wordcloud(words = department_all_df$word, freq = department_all_df$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = department_f_df$word, freq = department_f_df$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = department_nf_df$word, freq = department_nf_df$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

##################################################################################
#Word cloud of company_profile variable
#Data Selection Containing Only Text data
company_profile_all <-fkj$company_profile
company_profile_f <-Fraudulent$company_profile
company_profile_nf <- Non_Fraudulent$company_profile

#Creating Data Courps
company_profile_all_C <- Corpus(VectorSource(company_profile_all))
company_profile_f_C <- Corpus(VectorSource(company_profile_f))
company_profile_nf_C <- Corpus(VectorSource(company_profile_nf))

#Cleaning the data
company_profile_all_C <- company_profile_all_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
company_profile_all_C <- tm_map(company_profile_all_C, content_transformer(tolower))
company_profile_all_C <- tm_map(company_profile_all_C, removeWords, stopwords("english"))


company_profile_f_C <- company_profile_f_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
company_profile_f_C <- tm_map(company_profile_f_C, content_transformer(tolower))
company_profile_f_C <- tm_map(company_profile_f_C, removeWords, stopwords("english"))



company_profile_nf_C <- company_profile_nf_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
company_profile_nf_C <- tm_map(company_profile_nf_C, content_transformer(tolower))
company_profile_nf_C <- tm_map(company_profile_nf_C, removeWords, stopwords("english"))


#Creating Document Term Matrix, 
#and its data frame so that the first column will represent word 2nd will represent frequency
company_profile_all_dtm <- TermDocumentMatrix(company_profile_all_C) 
company_profile_all_matrix <- as.matrix(company_profile_all_dtm) 
company_profile_all_words <- sort(rowSums(company_profile_all_matrix),decreasing=TRUE) 
company_profile_all_df <- data.frame(words = names(company_profile_all_words),freq=company_profile_all_words)


company_profile_f_dtm <- TermDocumentMatrix(company_profile_f_C) 
company_profile_f_matrix <- as.matrix(company_profile_f_dtm) 
company_profile_f_words <- sort(rowSums(company_profile_f_matrix),decreasing=TRUE) 
company_profile_f_df <- data.frame(word = names(company_profile_f_words),freq=company_profile_f_words)

#gc()
company_profile_nf_dtm <- TermDocumentMatrix(company_profile_nf_C) 
company_profile_nf_matrix <- as.matrix(company_profile_nf_dtm) 
company_profile_nf_words <- sort(rowSums(company_profile_nf_matrix),decreasing=TRUE) 
company_profile_nf_df <- data.frame(word = names(company_profile_nf_words),freq=company_profile_nf_words)

#Generating the Word Cloud from DataFrames
set.seed(1234)
wordcloud(words = company_profile_all_df$word, freq = company_profile_all_df$freq, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = company_profile_f_df$word, freq = company_profile_f_df$freq, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = company_profile_nf_df$word, freq = company_profile_nf_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
##################################################################################
#Word cloud of description variable
#Data Selection Containing Only Text data
description_all <-fkj$description
description_all <- sample(fkj$description, 1000)
description_f <-Fraudulent$description
description_nf <- Non_Fraudulent$description
description_nf <- sample(Non_Fraudulent$description, 2000)

#Creating Data Courps
description_all_C <- Corpus(VectorSource(description_all))
description_f_C <- Corpus(VectorSource(description_f))
description_nf_C <- Corpus(VectorSource(description_nf))

#Cleaning the data
description_all_C <- description_all_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
description_all_C <- tm_map(description_all_C, content_transformer(tolower))
description_all_C <- tm_map(description_all_C, removeWords, stopwords("english"))


description_f_C <- description_f_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
description_f_C <- tm_map(description_f_C, content_transformer(tolower))
description_f_C <- tm_map(description_f_C, removeWords, stopwords("english"))



description_nf_C <- description_nf_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
description_nf_C <- tm_map(description_nf_C, content_transformer(tolower))
description_nf_C <- tm_map(description_nf_C, removeWords, stopwords("english"))


#Creating Document Term Matrix, 
#and its data frame so that the first column will represent word 2nd will represent frequency
description_all_dtm <- TermDocumentMatrix(description_all_C) 
description_all_matrix <- as.matrix(description_all_dtm) 
description_all_words <- sort(rowSums(description_all_matrix),decreasing=TRUE) 
description_all_df <- data.frame(words = names(description_all_words),freq=description_all_words)


description_f_dtm <- TermDocumentMatrix(description_f_C) 
description_f_matrix <- as.matrix(description_f_dtm) 
description_f_words <- sort(rowSums(description_f_matrix),decreasing=TRUE) 
description_f_df <- data.frame(word = names(description_f_words),freq=description_f_words)


description_nf_dtm <- TermDocumentMatrix(description_nf_C) 
description_nf_matrix <- as.matrix(description_nf_dtm) 
description_nf_words <- sort(rowSums(description_nf_matrix),decreasing=TRUE) 
description_nf_df <- data.frame(word = names(description_nf_words),freq=description_nf_words)

#Generating the Word Cloud from DataFrames
set.seed(1234)
wordcloud(words = description_all_df$word, freq = description_all_df$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = description_f_df$word, freq = description_f_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = description_nf_df$word, freq = description_nf_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

##################################################################################
#Word cloud of requirements variable
#Data Selection Containing Only Text data
requirements_all <-fkj$requirements
requirements_all <- sample(fkj$requirements, 2000)
requirements_f <-Fraudulent$requirements
requirements_nf <- Non_Fraudulent$requirements
requirements_nf <- sample(Non_Fraudulent$requirements, 2000)

#Creating Data Courps
requirements_all_C <- Corpus(VectorSource(requirements_all))
requirements_f_C <- Corpus(VectorSource(requirements_f))
requirements_nf_C <- Corpus(VectorSource(requirements_nf))

#Cleaning the data
requirements_all_C <- requirements_all_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
requirements_all_C <- tm_map(requirements_all_C, content_transformer(tolower))
requirements_all_C <- tm_map(requirements_all_C, removeWords, stopwords("english"))


requirements_f_C <- requirements_f_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
requirements_f_C <- tm_map(requirements_f_C, content_transformer(tolower))
requirements_f_C <- tm_map(requirements_f_C, removeWords, stopwords("english"))



requirements_nf_C <- requirements_nf_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
requirements_nf_C <- tm_map(requirements_nf_C, content_transformer(tolower))
requirements_nf_C <- tm_map(requirements_nf_C, removeWords, stopwords("english"))


#Creating Document Term Matrix, 
#and its data frame so that the first column will represent word 2nd will represent frequency
requirements_all_dtm <- TermDocumentMatrix(requirements_all_C) 
requirements_all_matrix <- as.matrix(requirements_all_dtm) 
requirements_all_words <- sort(rowSums(requirements_all_matrix),decreasing=TRUE) 
requirements_all_df <- data.frame(words = names(requirements_all_words),freq=requirements_all_words)


requirements_f_dtm <- TermDocumentMatrix(requirements_f_C) 
requirements_f_matrix <- as.matrix(requirements_f_dtm) 
requirements_f_words <- sort(rowSums(requirements_f_matrix),decreasing=TRUE) 
requirements_f_df <- data.frame(word = names(requirements_f_words),freq=requirements_f_words)


requirements_nf_dtm <- TermDocumentMatrix(requirements_nf_C) 
requirements_nf_matrix <- as.matrix(requirements_nf_dtm) 
requirements_nf_words <- sort(rowSums(requirements_nf_matrix),decreasing=TRUE) 
requirements_nf_df <- data.frame(word = names(requirements_nf_words),freq=requirements_nf_words)

#Generating the Word Cloud from DataFrames
set.seed(1234)
wordcloud(words = requirements_all_df$word, freq = requirements_all_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = requirements_f_df$word, freq = requirements_f_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = requirements_nf_df$word, freq = requirements_nf_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
##################################################################################
#Word cloud of benefits variable
#Data Selection Containing Only Text data
benefits_all <-fkj$benefits
benefits_all <- sample(fkj$benefits, 5000)
benefits_f <-Fraudulent$benefits
benefits_nf <- Non_Fraudulent$benefits
benefits_nf <- sample(Non_Fraudulent$benefits, 5000)

#Creating Data Courps
benefits_all_C <- Corpus(VectorSource(benefits_all))
benefits_f_C <- Corpus(VectorSource(benefits_f))
benefits_nf_C <- Corpus(VectorSource(benefits_nf))

#Cleaning the data
benefits_all_C <- benefits_all_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
benefits_all_C <- tm_map(benefits_all_C, content_transformer(tolower))
benefits_all_C <- tm_map(benefits_all_C, removeWords, stopwords("english"))


benefits_f_C <- benefits_f_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
benefits_f_C <- tm_map(benefits_f_C, content_transformer(tolower))
benefits_f_C <- tm_map(benefits_f_C, removeWords, stopwords("english"))



benefits_nf_C <- benefits_nf_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
benefits_nf_C <- tm_map(benefits_nf_C, content_transformer(tolower))
benefits_nf_C <- tm_map(benefits_nf_C, removeWords, stopwords("english"))


#Creating Document Term Matrix, 
#and its data frame so that the first column will represent word 2nd will represent frequency
benefits_all_dtm <- TermDocumentMatrix(benefits_all_C) 
benefits_all_matrix <- as.matrix(benefits_all_dtm) 
benefits_all_words <- sort(rowSums(benefits_all_matrix),decreasing=TRUE) 
benefits_all_df <- data.frame(words = names(benefits_all_words),freq=benefits_all_words)


benefits_f_dtm <- TermDocumentMatrix(benefits_f_C) 
benefits_f_matrix <- as.matrix(benefits_f_dtm) 
benefits_f_words <- sort(rowSums(benefits_f_matrix),decreasing=TRUE) 
benefits_f_df <- data.frame(word = names(benefits_f_words),freq=benefits_f_words)


benefits_nf_dtm <- TermDocumentMatrix(benefits_nf_C) 
benefits_nf_matrix <- as.matrix(benefits_nf_dtm) 
benefits_nf_words <- sort(rowSums(benefits_nf_matrix),decreasing=TRUE) 
benefits_nf_df <- data.frame(word = names(benefits_nf_words),freq=benefits_nf_words)

#Generating the Word Cloud from DataFrames
set.seed(1234)
wordcloud(words = benefits_all_df$word, freq = benefits_all_df$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = benefits_f_df$word, freq = benefits_f_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = benefits_nf_df$word, freq = benefits_nf_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

##################################################################################
#Word cloud of employment_type variable
#Data Selection Containing Only Text data
employment_type_all <-fkj$employment_type
employment_type_f <-Fraudulent$employment_type
employment_type_nf <- Non_Fraudulent$employment_type

#Creating Data Courps
employment_type_all_C <- Corpus(VectorSource(employment_type_all))
employment_type_f_C <- Corpus(VectorSource(employment_type_f))
employment_type_nf_C <- Corpus(VectorSource(employment_type_nf))

#Cleaning the data
employment_type_all_C <- employment_type_all_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
employment_type_all_C <- tm_map(employment_type_all_C, content_transformer(tolower))
employment_type_all_C <- tm_map(employment_type_all_C, removeWords, stopwords("english"))


employment_type_f_C <- employment_type_f_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
employment_type_f_C <- tm_map(employment_type_f_C, content_transformer(tolower))
employment_type_f_C <- tm_map(employment_type_f_C, removeWords, stopwords("english"))



employment_type_nf_C <- employment_type_nf_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
employment_type_nf_C <- tm_map(employment_type_nf_C, content_transformer(tolower))
employment_type_nf_C <- tm_map(employment_type_nf_C, removeWords, stopwords("english"))


#Creating Document Term Matrix, 
#and its data frame so that the first column will represent word 2nd will represent frequency
employment_type_all_dtm <- TermDocumentMatrix(employment_type_all_C) 
employment_type_all_matrix <- as.matrix(employment_type_all_dtm) 
employment_type_all_words <- sort(rowSums(employment_type_all_matrix),decreasing=TRUE) 
employment_type_all_df <- data.frame(words = names(employment_type_all_words),freq=employment_type_all_words)


employment_type_f_dtm <- TermDocumentMatrix(employment_type_f_C) 
employment_type_f_matrix <- as.matrix(employment_type_f_dtm) 
employment_type_f_words <- sort(rowSums(employment_type_f_matrix),decreasing=TRUE) 
employment_type_f_df <- data.frame(word = names(employment_type_f_words),freq=employment_type_f_words)


employment_type_nf_dtm <- TermDocumentMatrix(employment_type_nf_C) 
employment_type_nf_matrix <- as.matrix(employment_type_nf_dtm) 
employment_type_nf_words <- sort(rowSums(employment_type_nf_matrix),decreasing=TRUE) 
employment_type_nf_df <- data.frame(word = names(employment_type_nf_words),freq=employment_type_nf_words)

#Generating the Word Cloud from DataFrames
set.seed(1234)
wordcloud(words = employment_type_all_df$word, freq = employment_type_all_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = employment_type_f_df$word, freq = employment_type_f_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = employment_type_nf_df$word, freq = employment_type_nf_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

##################################################################################
#Word cloud of required_experience variable
#Data Selection Containing Only Text data
required_experience_all <-fkj$required_experience
required_experience_f <-Fraudulent$required_experience
required_experience_nf <- Non_Fraudulent$required_experience

#Creating Data Courps
required_experience_all_C <- Corpus(VectorSource(required_experience_all))
required_experience_f_C <- Corpus(VectorSource(required_experience_f))
required_experience_nf_C <- Corpus(VectorSource(required_experience_nf))

#Cleaning the data
required_experience_all_C <- required_experience_all_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
required_experience_all_C <- tm_map(required_experience_all_C, content_transformer(tolower))
required_experience_all_C <- tm_map(required_experience_all_C, removeWords, stopwords("english"))


required_experience_f_C <- required_experience_f_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
required_experience_f_C <- tm_map(required_experience_f_C, content_transformer(tolower))
required_experience_f_C <- tm_map(required_experience_f_C, removeWords, stopwords("english"))



required_experience_nf_C <- required_experience_nf_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
required_experience_nf_C <- tm_map(required_experience_nf_C, content_transformer(tolower))
required_experience_nf_C <- tm_map(required_experience_nf_C, removeWords, stopwords("english"))


#Creating Document Term Matrix, 
#and its data frame so that the first column will represent word 2nd will represent frequency
required_experience_all_dtm <- TermDocumentMatrix(required_experience_all_C) 
required_experience_all_matrix <- as.matrix(required_experience_all_dtm) 
required_experience_all_words <- sort(rowSums(required_experience_all_matrix),decreasing=TRUE) 
required_experience_all_df <- data.frame(words = names(required_experience_all_words),freq=required_experience_all_words)


required_experience_f_dtm <- TermDocumentMatrix(required_experience_f_C) 
required_experience_f_matrix <- as.matrix(required_experience_f_dtm) 
required_experience_f_words <- sort(rowSums(required_experience_f_matrix),decreasing=TRUE) 
required_experience_f_df <- data.frame(word = names(required_experience_f_words),freq=required_experience_f_words)


required_experience_nf_dtm <- TermDocumentMatrix(required_experience_nf_C) 
required_experience_nf_matrix <- as.matrix(required_experience_nf_dtm) 
required_experience_nf_words <- sort(rowSums(required_experience_nf_matrix),decreasing=TRUE) 
required_experience_nf_df <- data.frame(word = names(required_experience_nf_words),freq=required_experience_nf_words)

#Generating the Word Cloud from DataFrames
set.seed(1234)
wordcloud(words = required_experience_all_df$word, freq = required_experience_all_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = required_experience_f_df$word, freq = required_experience_f_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = required_experience_nf_df$word, freq = required_experience_nf_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
##################################################################################
#Word cloud of required_education variable
#Data Selection Containing Only Text data
required_education_all <-fkj$required_education
required_education_f <-Fraudulent$required_education
required_education_nf <- Non_Fraudulent$required_education

#Creating Data Courps
required_education_all_C <- Corpus(VectorSource(required_education_all))
required_education_f_C <- Corpus(VectorSource(required_education_f))
required_education_nf_C <- Corpus(VectorSource(required_education_nf))

#Cleaning the data
required_education_all_C <- required_education_all_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
required_education_all_C <- tm_map(required_education_all_C, content_transformer(tolower))
required_education_all_C <- tm_map(required_education_all_C, removeWords, stopwords("english"))


required_education_f_C <- required_education_f_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
required_education_f_C <- tm_map(required_education_f_C, content_transformer(tolower))
required_education_f_C <- tm_map(required_education_f_C, removeWords, stopwords("english"))



required_education_nf_C <- required_education_nf_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
required_education_nf_C <- tm_map(required_education_nf_C, content_transformer(tolower))
required_education_nf_C <- tm_map(required_education_nf_C, removeWords, stopwords("english"))


#Creating Document Term Matrix, 
#and its data frame so that the first column will represent word 2nd will represent frequency
required_education_all_dtm <- TermDocumentMatrix(required_education_all_C) 
required_education_all_matrix <- as.matrix(required_education_all_dtm) 
required_education_all_words <- sort(rowSums(required_education_all_matrix),decreasing=TRUE) 
required_education_all_df <- data.frame(words = names(required_education_all_words),freq=required_education_all_words)


required_education_f_dtm <- TermDocumentMatrix(required_education_f_C) 
required_education_f_matrix <- as.matrix(required_education_f_dtm) 
required_education_f_words <- sort(rowSums(required_education_f_matrix),decreasing=TRUE) 
required_education_f_df <- data.frame(word = names(required_education_f_words),freq=required_education_f_words)


required_education_nf_dtm <- TermDocumentMatrix(required_education_nf_C) 
required_education_nf_matrix <- as.matrix(required_education_nf_dtm) 
required_education_nf_words <- sort(rowSums(required_education_nf_matrix),decreasing=TRUE) 
required_education_nf_df <- data.frame(word = names(required_education_nf_words),freq=required_education_nf_words)

#Generating the Word Cloud from DataFrames
set.seed(1234)
wordcloud(words = required_education_all_df$word, freq = required_education_all_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = required_education_f_df$word, freq = required_education_f_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = required_education_nf_df$word, freq = required_education_nf_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
##################################################################################
#Word cloud of industry variable
#Data Selection Containing Only Text data
industry_all <-fkj$industry
industry_f <-Fraudulent$industry
industry_nf <- Non_Fraudulent$industry

#Creating Data Courps
industry_all_C <- Corpus(VectorSource(industry_all))
industry_f_C <- Corpus(VectorSource(industry_f))
industry_nf_C <- Corpus(VectorSource(industry_nf))

#Cleaning the data
industry_all_C <- industry_all_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
industry_all_C <- tm_map(industry_all_C, content_transformer(tolower))
industry_all_C <- tm_map(industry_all_C, removeWords, stopwords("english"))


industry_f_C <- industry_f_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
industry_f_C <- tm_map(industry_f_C, content_transformer(tolower))
industry_f_C <- tm_map(industry_f_C, removeWords, stopwords("english"))



industry_nf_C <- industry_nf_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
industry_nf_C <- tm_map(industry_nf_C, content_transformer(tolower))
industry_nf_C <- tm_map(industry_nf_C, removeWords, stopwords("english"))


#Creating Document Term Matrix, 
#and its data frame so that the first column will represent word 2nd will represent frequency
industry_all_dtm <- TermDocumentMatrix(industry_all_C) 
industry_all_matrix <- as.matrix(industry_all_dtm) 
industry_all_words <- sort(rowSums(industry_all_matrix),decreasing=TRUE) 
industry_all_df <- data.frame(words = names(industry_all_words),freq=industry_all_words)


industry_f_dtm <- TermDocumentMatrix(industry_f_C) 
industry_f_matrix <- as.matrix(industry_f_dtm) 
industry_f_words <- sort(rowSums(industry_f_matrix),decreasing=TRUE) 
industry_f_df <- data.frame(word = names(industry_f_words),freq=industry_f_words)


industry_nf_dtm <- TermDocumentMatrix(industry_nf_C) 
industry_nf_matrix <- as.matrix(industry_nf_dtm) 
industry_nf_words <- sort(rowSums(industry_nf_matrix),decreasing=TRUE) 
industry_nf_df <- data.frame(word = names(industry_nf_words),freq=industry_nf_words)

#Generating the Word Cloud from DataFrames
set.seed(1234)
wordcloud(words = industry_all_df$word, freq = industry_all_df$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = industry_f_df$word, freq = industry_f_df$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = industry_nf_df$word, freq = industry_nf_df$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
##################################################################################
#Word cloud of function variable
#Data Selection Containing Only Text data
function_all <-fkj$function.
function_f <-Fraudulent$function.
function_nf <- Non_Fraudulent$function.

#Creating Data Courps
function_all_C <- Corpus(VectorSource(function_all))
function_f_C <- Corpus(VectorSource(function_f))
function_nf_C <- Corpus(VectorSource(function_nf))

#Cleaning the data
function_all_C <- function_all_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
function_all_C <- tm_map(function_all_C, content_transformer(tolower))
function_all_C <- tm_map(function_all_C, removeWords, stopwords("english"))


function_f_C <- function_f_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
function_f_C <- tm_map(function_f_C, content_transformer(tolower))
function_f_C <- tm_map(function_f_C, removeWords, stopwords("english"))



function_nf_C <- function_nf_C %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
function_nf_C <- tm_map(function_nf_C, content_transformer(tolower))
function_nf_C <- tm_map(function_nf_C, removeWords, stopwords("english"))


#Creating Document Term Matrix, 
#and its data frame so that the first column will represent word 2nd will represent frequency
function_all_dtm <- TermDocumentMatrix(function_all_C) 
function_all_matrix <- as.matrix(function_all_dtm) 
function_all_words <- sort(rowSums(function_all_matrix),decreasing=TRUE) 
function_all_df <- data.frame(words = names(function_all_words),freq=function_all_words)


function_f_dtm <- TermDocumentMatrix(function_f_C) 
function_f_matrix <- as.matrix(function_f_dtm) 
function_f_words <- sort(rowSums(function_f_matrix),decreasing=TRUE) 
function_f_df <- data.frame(word = names(function_f_words),freq=function_f_words)


function_nf_dtm <- TermDocumentMatrix(function_nf_C) 
function_nf_matrix <- as.matrix(function_nf_dtm) 
function_nf_words <- sort(rowSums(function_nf_matrix),decreasing=TRUE) 
function_nf_df <- data.frame(word = names(function_nf_words),freq=function_nf_words)

#Generating the Word Cloud from DataFrames
set.seed(1234)
wordcloud(words = function_all_df$word, freq = function_all_df$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = function_f_df$word, freq = function_f_df$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = function_nf_df$word, freq = function_nf_df$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

diagnose(fkj)
diagnose_category(fkj)


diagnose_outlier(fkj) %>% flextable()


plot_na_pareto(fkj)

#############################################
# Classification
# selection of 1000 random non-fraudulent data
Non_Fraudulent_1000 <- Non_Fraudulent[sample(1:nrow(Non_Fraudulent), 1000), ]
# diagnose(Non_Fraudulent_1000)
balancedData <-bind_rows(Fraudulent, Non_Fraudulent_1000)
# diagnose(balancedData)
#Dropping Job_Id column
balancedData = subset(balancedData, select = -c(job_id))

balancedData

set.seed(123)
split_data <- createDataPartition(y = balancedData$fraudulent, times = 1, p = 0.80, list= FALSE)
train_data <- balancedData[split_data, ]
test_data <- balancedData[-split_data, ]
# check the proportion of class variable
prop.table(summary(train_data$fraudulent))
prop.table(summary(test_data$fraudulent))
########################################
train_data$fraudulent = as.factor(train_data$fraudulent)
test_data$fraudulent = as.factor(test_data$fraudulent)
#Random forest Classifier
set.seed(1111)
# install.packages("C50")
library(C50)
#C5.0(train_predictors(x), train_target(y)) trains the C5.0 decision tree
MODEL <- C5.0(train_data[-16], train_data$fraudulent,
                     control = C5.0Control(minCases = 100))

trcontrol<- trainControl(method = "repeatedcv", number=2, repeats=1, search="random", verboseIter = TRUE)
grid <-data.frame(mtry = c(100))

set.seed(1122)
rf_model <- train(fraudulent ~ ., method = "rf", data = train_data, ntree = 200, trControl = trcontrol,tuneGrid = grid)

rf_model



predixtion <- predict(rf_model, test_data)
##################################################################################################################
# make a copy of data 
df<- fkj
#Data Cleaning
# replace empty factor levels with NA
find_empty_level<- which(levels(df$employment_type)=="")
levels(df$employment_type)[find_empty_level]<-"NA"
find_empty_level<- which(levels(df$location)=="")
levels(df$location)[find_empty_level]<-"NA"
find_empty_level<- which(levels(df$department)=="")
levels(df$department)[find_empty_level]<-"NA"
find_empty_level<- which(levels(df$salary_range)=="")
levels(df$salary_range)[find_empty_level]<-"NA"
find_empty_level<- which(levels(df$company_profile)=="")
levels(df$company_profile)[find_empty_level]<-"NA"
find_empty_level<- which(levels(df$salary_range)=="")
levels(df$salary_range)[find_empty_level]<-"NA"
find_empty_level<- which(levels(df$requirements)=="")
levels(df$requirements)[find_empty_level]<-"NA"
find_empty_level<- which(levels(df$employment_type)=="")
levels(df$employment_type)[find_empty_level]<-"NA"
find_empty_level<- which(levels(df$employment_type)=="")
levels(df$employment_type)[find_empty_level]<-"NA"
find_empty_level<- which(levels(df$required_experience)=="")
levels(df$required_experience)[find_empty_level]<-"NA"
find_empty_level<- which(levels(df$required_education)=="")
levels(df$required_education)[find_empty_level]<-"NA"
find_empty_level<- which(levels(df$industry)=="")
levels(df$industry)[find_empty_level]<-"NA"
find_empty_level<- which(levels(df$function.)=="")
levels(df$function.)[find_empty_level]<-"NA"
# coerce target var to factor
df$fraudulent<- factor(df$fraudulent)
# recode the target class var levels to something meaningful else there is an error when building models
levels(df$fraudulent)<- c("not_fake","fake")

names(df)
#Dropping Job_Id and Company Profile
df.1<- df[,c(10:18)]
str(df.1)
names(df.1)


# Run algorithms using 3-fold cross validation
set.seed(2020)
index <- createDataPartition(df.1$fraudulent, p = 0.8, list = FALSE, times = 1)
train_data <- df.1[index, ]
test_data  <- df.1[-index, ]
# create caret trainControl object to control the number of cross-validations performed
ctrl <- trainControl(method = "repeatedcv",
                     number = 3,
                     # repeated 3 times
                     repeats = 3, 
                     verboseIter = FALSE, 
                     classProbs=TRUE, 
                     summaryFunction=twoClassSummary
)
# Metric is AUPRC which is Area Under Precision Recall Curve (PRC). Its more robust then using ROC. Accuracy and Kappa are used for balanced classes, while PRC is used for imbalanced classes
set.seed(2020)
# turning "warnings" off
options(warn=-1)
metric <- "AUPRC"

# CART
set.seed(2020)
fit_cart<-caret::train(fraudulent ~ .,data = train_data,
                       method = "rpart",
                       preProcess = c("scale", "center"),
                       trControl = ctrl 
                       ,metric= "ROC"
)
# Logistic Regression
set.seed(2020)
fit_glm<-caret::train(fraudulent ~ .,data = train_data
                      , method = "glm", family = "binomial"
                      , preProcess = c("scale", "center")
                      , trControl = ctrl
                      , metric= "ROC"
)


# summarize accuracy of models
models <- resamples(list(cart=fit_cart, glm=fit_glm))
summary(models) # sensitivity of cart is maximum
# compare accuracy of models
bwplot(models)


# Make Predictions using the linear regression Model model
predictions <- predict(fit_glm, test_data)
confusionMatrix(predictions, test_data$fraudulent)

# Make Predictions using the best model
predictions <- predict(fit_cart, test_data)
confusionMatrix(predictions, test_data$fraudulent)


# Method 1: Under Sampling
set.seed(2020)
ctrl <- trainControl(method = "repeatedcv",
                     number = 3,
                     # repeated 3 times
                     repeats = 3, 
                     verboseIter = FALSE, 
                     classProbs=TRUE, 
                     summaryFunction=twoClassSummary,
                     sampling = "down"
)
fit_under<-caret::train(fraudulent ~ .,data = train_data,
                        method = "rpart",
                        preProcess = c("scale", "center"),
                        trControl = ctrl 
                        ,metric= "ROC"
)
# Method 2: Over Sampling
set.seed(2020)
ctrl <- trainControl(method = "repeatedcv",
                     number = 3,
                     # repeated 3 times
                     repeats = 3, 
                     verboseIter = FALSE, 
                     classProbs=TRUE, 
                     summaryFunction=twoClassSummary,
                     sampling = "up"
)

fit_over<-caret::train(fraudulent ~ .,data = train_data,
                       method = "rpart",
                       preProcess = c("scale", "center"),
                       trControl = ctrl 
                       ,metric= "ROC"
)


# summarize accuracy of models
models <- resamples(list(rpart_under=fit_under, rpart_over=fit_over))
summary(models) # highest sensistivity is for over sampling
# compare accuracy of models
bwplot(models)

# Make Predictions using the best model
predictions <- predict(fit_over, test_data)
# Using under-balancing as a method for balancing the data
confusionMatrix(predictions, test_data$fraudulent)
