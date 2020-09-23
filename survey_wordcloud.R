library(KoNLP)
library(wordcloud)
library(stringr)
library(XML)
library(rvest)
library(tm)
library(igraph)
library(wordcloud2)
library(arules)
library(combinat)
library(multilinguer)

.libPaths()
useSejongDic()
useNIADic() 

comments <- readLines("c:/users/R/survey/comments.txt", encoding = "UTF-8")
head(comments, 10)

comments <- paste0(comments, collapse = "")

nouns <- extractNoun(comments)
nouns1 <- unlist(nouns)
nouns2 <- Filter(function(x) {nchar(x) <= 20 & nchar(x) > 1}, nouns1)

nouns2 <- gsub(" ","", nouns2)
nouns2 <- gsub("<.+?>|\t", "", nouns2)
nouns2 <- gsub("\\d+","" ,nouns2)	
nouns2 <- gsub("\\.","" ,nouns2) 
nouns2 <- gsub("재택근무","" ,nouns2)
nouns2 <- gsub("생각","" ,nouns2)
nouns2 <- gsub("업무","" ,nouns2)
nouns2 <- gsub("근무","" ,nouns2)
nouns2 <- gsub("재택","" ,nouns2)
nouns2 <- gsub("하기","" ,nouns2)
nouns2 <- gsub("관련","" ,nouns2)
nouns2 <- gsub("들이","" ,nouns2)
nouns2 <- gsub("경우","" ,nouns2)
nouns2 <- gsub("가능","" ,nouns2)
nouns2 <- gsub("시행","" ,nouns2)
nouns2 <- gsub("발생","" ,nouns2)
nouns2 <- gsub("부분","" ,nouns2)
nouns2 <- gsub("진행","" ,nouns2)
nouns2 <- gsub("회사","" ,nouns2)
nouns2 <- gsub("시간","" ,nouns2)
nouns2 <- gsub("출근","" ,nouns2)

nouns3 <- Filter(function(x) {nchar(x) <= 20 & nchar(x) > 1}, nouns2)

wordcount <- table(nouns3)
head(sort(wordcount, decreasing = TRUE), 50)

windowsFonts(malgun=windowsFont("맑은고딕"))
wordcloud2(data=wordcount, minSize = 5, 
           minRotation = 0, maxRotation = 0, rotateRatio = 1, 
           fontFamily='맑은고딕',size=0.8)

#### not so beautiful 
palete <- brewer.pal(9, "Set3")
wordcloud2(names(wordcount), freq = wordcount, scale = c(5,1),
          rot.per = 0.25, min.freq = 1, random.order = F, 
          random.color = T, colors = palete)

#### don't understand
txt <- readLines("c:/users/R/survey/del_gsub.txt", encoding = "UTF-8")
cnt_txt <- length(txt)
for (i in 1:cnt_txt){
  nouns2 <- gsub((txt[i]), "", nouns2)
}
####

