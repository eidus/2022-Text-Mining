# setting
setwd("C:/Users/rladb/Documents/R_review")
install.packages("widyr")
install.packages('qdap')
# packages
library(dplyr)
library(stringr)
library(ggplot2)
library(tm) # text mining
library(qdapRegex)
library(readxl)
library(tidytext)
library(widyr)
library(qdap)
library(wordcloud2)
library(topicmodels)
library(scales)

# csv 파일 다운
m2<-read.csv('musk/rawdata.csv')


# 텍스트 전처리
mentions <- m2$Tweets
mentions <- gsub("@[A-z0-9]*","",mentions) # rm_tag가 듣질 않아서 따로 만듦. 사이트봐도 잘 안돼서 맘대로 함.
mentions <- rm_url(mentions)   # URL: Universal Resource Locator, HTTP: 
mentions <- bracketX(mentions) # 괄호 텍스트 삭제
mentions <- gsub("[0-9]","",mentions)
mentions <- replace_symbol(mentions) # 기호 -> 텍스트 dollar
mentions <- replace_abbreviation(mentions) # 축약어 -> 텍스트
mentions <- gsub("[^\x01-\x7F]", "",  mentions) # 이모티콘 제거
mentions <- tolower(mentions)
mentions <- removeWords(mentions, stopwords("en")) # 불용어 제거
mentions <- rm_non_words(mentions) # 문장부호 제거
mentions
# m2에 저장
m2$cleaned <- mentions
View(m2)
  
# 단어로 분류
tibble(text = mentions) %>% mutate(sent_order = 1:n())%>% unnest_tokens(output = word, input = text, token = "words") %>% anti_join(stop_words) -> men_pos

# 혹시 몰라 단어 더 없애기. amp가 가장 많이 나온다.그런데 amp를 검색하면 테슬라 모델이거나 어댑터로 나온다.
men_pos<-men_pos %>% filter((word != "real")&(word !="don")&(word !="ve")&(word != "haha" )&(word != "yeah" )&(word != "yup" )&(word != "amp" ))

## word cloud
# 단어로 분류한 것 count 후 내림차순 200개만 가져온다
count_word<-men_pos %>% select(sent_order,word) %>% count(word,sort = T)
count_word200 <- count_word %>% head(200)

set.seed(22)
wordcloud2(count_word200)

## 동시 출현 
men_pos %>% pairwise_count(word,sent_order, sort = T, upper = F) -> PW
PW[PW$n>10,]
head(PW,10)
PW %>% filter(item1 == 'speech')
PW %>% filter(item1 == 'free') 
PW %>% filter(item1 == 'twitter') 
PW %>% filter(item1 == 'that') 
 
## topic model
doc<- m2$cleaned

# 내가 만든 불용어 사전인데 최종적으로는 쓰지 않았음.
stopword <- c('nt','the','m','s','r','d','x','un','ve','k','m','ww','y','rb','gh','re','bf','d','i','yes','yeah','haha','c','v','amp','ok','just','percent','well','one','also')

# scales 패키지를 이용 : 품사까지 보려고고 
spacy_parse(doc,lamma = F, entity = F) -> doctable
doctable <- doctable[,c(1,4,6)]
doctable2 <- doctable %>% count(doc_id, token, sort = T)
#  %>% filter(!token %in% stopword)

# documentTermMatrix로 만들기
dtm <- doctable2 %>% cast_dtm(document = doc_id, term = token, value = n)
as.matrix(dtm[1:8,1:8])

#품사 종류 보기
table(doctable$pos)

# LDA 함수: 깁스 샘플링 적용 후 토픽 분류
ap_lda <- LDA(dtm, k = 4, method = "Gibbs",control = list(seed=20210721))
ap_topics <- ap_lda %>% 
  tidy(matrix = "beta")
ap_topics

ap_top10_terms <- ap_topics %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  arrange(topic, desc(beta))
ap_top10_terms

topic1 <- tidy(ap_lda, matrix= "beta")

# 10개만 추출
top1 <- topic1 %>% group_by(topic) %>% slice_max(beta, n = 10)

# 그래프
ggplot(top1)+geom_col( mapping = aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic)),show.legend = F)+facet_wrap(~topic, scales = "free")+coord_flip()
top1

count_word<-men_pos %>% select(sent_order,word) %>% count(word,sort = T)
count_word200 <- count_word %>% head(200)
head(count_word[count_word$word != "amp"&count_word$word !="don"&count_word$word !="ve",],200) -> count_word200

## word cloud
set.seed(22)
# 각 토픽별 word cloud
makewc <- function(x){topic1 %>% filter(topic == x) %>% select(term, beta) %>% arrange(desc(beta)) %>% head(100) %>% wordcloud2() }
makewc(1)
makewc(2)
makewc(3)
makewc(4)
