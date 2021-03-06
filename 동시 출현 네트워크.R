#비건 데이터 불러오기
library(readxl)
vegan_social_data <- read_excel("vegan social data.xlsx")

# 전처리

library(dplyr)
library(stringr)
library(textclean)

vegan_data <- vegan_social_data %>%
  select(title) %>%
  mutate(title = str_replace_all(title, "[^가-힣]", " "),
         title = str_squish(title),
         name = row_number())

library(tidytext)
library(KoNLP)

vegan_pos <- vegan_data %>%
  unnest_tokens(input = title,
                output = word,
                token = SimplePos22,
                drop = F)
vegan_pos %>%
  select(word, title)

#품사별로 행 분리
library(tidyr)
vegan_pos <- vegan_pos %>%
  separate_rows(word, sep = "[+]")
vegan_pos %>%
  select(word, title)

#명사 추출하기

noun <- vegan_pos %>%
  filter(str_detect(word, "/n")) %>%
  mutate(word = str_remove(word, "/.*$"))

noun %>%
  select(word, title)

#어떤 명사가 많이 사용되었는지 확인
noun %>%
  count(word, sort =T)

#동사, 형용사 추출하기
pvpa <- vegan_pos %>%
  filter(str_detect(word, "/pv|/pa")) %>%
  mutate(word = str_replace(word, "/.*$", "다"))

pvpa %>%
  select(word, title)

#count를 이용하여 댓글에 어떤 동사와 형용사가 많이 사용되었는지 확인
pvpa %>%
  count(word, sort = T)

comment <- bind_rows(noun, pvpa) %>%
  filter(str_count(word) >= 2) %>%
  arrange(name)

comment %>%
  select(word, title)

library(widyr)
pair <- comment %>%
  pairwise_count(item =word,
                 feature = name,
                 sort = T)

#채식과 자주 함께 사용된 단어를 확인
pair %>% filter(item1 == "채식")

#건강과 자주 함께 사용된 단어를 확인
pair %>% filter(item1 == "건강")


#-----------------------------------------------
#동시 출현 네트워크
library("tidygraph")

graph_comment <- pair %>%
  filter(n >=30) %>%
  as_tbl_graph()

graph_comment

library(ggraph)
windows()
ggraph(graph_comment) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))

#그래프 다듬기

library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()
windows()
set.seed(1234)
ggraph(graph_comment, layout = "fr") +
  
  geom_edge_link(color = "gray50",
                 alpha = 0.5) +
  geom_node_point(color = "lightcoral",
                  sie = 5) +
  
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5,
                 family = "nanumgothic") +
  theme_graph()

#네트워크 그래프 함수 만들기

word_network <- function(x) {
  ggraph(x, layout = "fr") +
    geom_edge_link(color = "gray50",
                   alpha =0.5) +
    geom_node_point(color = "lightcoral",
                    size = 5) +
    geom_node_text(aes(label = name),
                   repel = T,
                   size = 5,
                   family = "nanumgothic") +
    windows() +
    theme_graph()
}

set.seed(1234)
word_network(graph_comment)

#유의어 처리하기

comment <- comment %>%
  mutate(word = ifelse(str_detect(word, "채식"), "채식", word),
         word = ifelse(str_detect(word, "건강"), "건강", word))

#단어 동시 출현 빈도 구하기
pair <- comment %>%
  pairwise_count(item = word,
                 feature = name,
                 sort = T)

# 네트워크 그래프 데이터 만들기
graph_comment <- pair %>%
  filter(n >=40) %>%
  as_tbl_graph()

#네트워크 그래프 만들기
set.seed(1234)
word_network(graph_comment)

#_---------------------------

set.seed(1234)
graph_comment <- pair %>%
  filter(n >= 40) %>%
  as_tbl_graph(directed =F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

graph_comment

set.seed(1234)
ggraph(graph_comment, layout = "fr") + #레이아웃
  
  geom_edge_link(color = "gray50",
                 alpha = 0.5) +
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  scale_size(range =c(5, 15)) +
  
  geom_node_text(aes(label = name),
                 repel = T,
                 size =4,
                 family = "nanumgothic") +
  windows() +
  theme_graph()