library(readxl)
library(dplyr)
#비건 데이터 불러오기

raw_vegan_data <- read_excel('vegan social data.xlsx') %>%
  mutate(name = row_number())
library(stringr)
library(textclean)

#기본적인 전처리
vegan_data <- raw_vegan_data %>%
  mutate(title = str_replace_all(title, "[^가-힣]", " "),
         title = str_squish(title))%>%
  
  #중복 댓글 제거
  distinct(title, .keep_all = T) %>%
  
  #짧은 문서 제거 - 3 단어 이상 추출출
  filter(str_count(title, boundary("word")) >= 3)

library(tidytext)
library(KoNLP)

comment <- vegan_data %>%
  unnest_tokens(input = title,
                output = word,
                token = extractNoun,
                drop = F) %>%
  filter(str_count(word) > 1) %>%
  
  #댓글 내 중복 단어 제거
  group_by(title) %>%
  distinct(word, .keep_all = T)%>%
  ungroup() %>%
  select(title, word)

#빈도가 높은 단어 제거하기

count_word <- comment %>%
  add_count(word) %>%
  filter(n <= 200) %>%
  select(-n)

count_word %>%
  count(word, sort = T) %>%
  print(n = 200)

#불용어 제거하기, 유의어 처리하기

count_word %>%
  count(word, sort = T) %>%
  print(n = 200)

stopword <- c("합니", "하세", "본문", "들이", "해서", "진짜", "안녕",
              "세계", "영상", "있습니")

count_word <- count_word %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word,
                       "아토" = "아토피",
                       "부탁드립니" = "부탁"))

#문서별 단어 빈도 구하기

count_word_doc <- count_word %>%
  count(title, word, sort =T)

dtm_comment <- count_word_doc %>%
  cast_dtm(document = title, term = word, value = n)

library(topicmodels)

lda_model <- LDA(dtm_comment,
                 k = 8,
                 method = "Gibbs",
                 control = list(seed =1234))

#------------------------------
#토픽별 주요 단어 살펴보기

term_topic <- tidy(lda_model, matrix = "beta")

term_topic %>%
  count(topic)

term_topic %>%
  filter(topic == 1) %>%
  summarise(sum_beta = sum(beta))

term_topic %>%
  filter(term == "국산")

term_topic %>%
  filter(topic == 6) %>%
  arrange(-beta)

terms(lda_model, 20) %>%
  data.frame()

#토픽별로 beta가 가장 높은 단어 추출하기
top_term_topic <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)

library(scales)
library(ggplot2)
windows()
ggplot(top_term_topic,
       aes(x = reorder_within(term, beta, topic),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(n.breaks = 4,
                      labels = number_format(accuracy = .01)) +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))

doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic

doc_topic %>%
  count(topic)

doc_topic %>%
  filter(document == 1)  %>%
  summarise(sum_gamma = sum(gamma))

doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n= 1)
doc_class

doc_class$document <- as.integer(doc_class$document)
vegan_data_topic <- raw_vegan_data %>%
  left_join(doc_class, by = c("title" = "document"))

vegan_data_topic %>%
  select(title, topic)

vegan_data_topic %>%
  count(topic)

vegan_data_topic <- vegan_data_topic %>%
  na.omit()

top_terms <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 6, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))
top_terms

count_topic <- vegan_data_topic %>%
  count(topic)

count_topic_word <- count_topic %>%
  left_join(top_terms, by = 'topic') %>%
  mutate(topic_name = paste("Topic", topic))
windows()
count_topic_word

ggplot(count_topic_word,
       aes(x = reorder(topic_name, n),
           y = n,
           fill = topic_name)) +
  geom_col(show.legend = F) +
  coord_flip() +
  
  geom_text(aes(label = n ) ,
            hjust = -0.2) +
  geom_text(aes(label = term),
            hjust = 1.03,
            col = "white",
            fontface = "bold",
            family = "nanumgothic") +
  
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 320)) +
  labs(x = NULL)

comment_topic <- vegan_data_topic %>%
  mutate(title = str_squish(replace_html(title))) %>%
  arrange(-gamma)

comment_topic %>%
  select(gamma, title)

comment_topic %>%
  filter(topic == 2 & str_detect(title, "식사")) %>%
  head(50) %>%
  pull(title)

name_topic <- tibble(topic = 1:8,
                     name = c("1. 비건인들의 힐링",
                              "2. 비건인들의 외식 러빙헛",
                              "3. 비건인들의 힐링(2)",
                              "4. 비건인들의 과일",
                              "5. 비건인들의 다이어트",
                              "6. 비건인들의 정보공유",
                              "7. 비건인들의 단백질 섭취",
                              "8. 비건인들의 반찬"))

top_term_topic_name <- top_term_topic %>%
  left_join(name_topic, name_topic, by = "topic")
top_term_topic_name

#막대 그래프 만들기
windows()
ggplot(top_term_topic_name,
       aes(x = reorder_within(term, beta, name),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ name, scales = "free", ncol = 2) +
  coord_flip() +
  scale_x_reordered() +
  
  labs(title = "한울벗 채식나라 비건인 라이프 스타일 분석",
       subtitle = " 토픽별 주요 단어 TOP 10",
       x = NULL, y = NULL) +
  
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        title = element_text(size=12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

