#ºñ°Ç µ¥ÀÌÅÍ ºÒ·¯¿À±â
library(readxl)
vegan_social_data <- read_excel("vegan social data.xlsx")

# ÀüÃ³¸®

library(dplyr)
library(stringr)
library(textclean)

vegan_data <- vegan_social_data %>%
  select(title) %>%
  mutate(title = str_replace_all(title, "[^°¡-ÆR]", " "),
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

#Ç°»çº°·Î Çà ºÐ¸®
library(tidyr)
vegan_pos <- vegan_pos %>%
  separate_rows(word, sep = "[+]")
vegan_pos %>%
  select(word, title)

#¸í»ç ÃßÃâÇÏ±â

noun <- vegan_pos %>%
  filter(str_detect(word, "/n")) %>%
  mutate(word = str_remove(word, "/.*$"))

noun %>%
  select(word, title)

#¾î¶² ¸í»ç°¡ ¸¹ÀÌ »ç¿ëµÇ¾ú´ÂÁö È®ÀÎ
noun %>%
  count(word, sort =T)

#µ¿»ç, Çü¿ë»ç ÃßÃâÇÏ±â
pvpa <- vegan_pos %>%
  filter(str_detect(word, "/pv|/pa")) %>%
  mutate(word = str_replace(word, "/.*$", "´Ù"))

pvpa %>%
  select(word, title)

#count¸¦ ÀÌ¿ëÇÏ¿© ´ñ±Û¿¡ ¾î¶² µ¿»ç¿Í Çü¿ë»ç°¡ ¸¹ÀÌ »ç¿ëµÇ¾ú´ÂÁö È®ÀÎ
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

#Ã¤½Ä°ú ÀÚÁÖ ÇÔ²² »ç¿ëµÈ ´Ü¾î¸¦ È®ÀÎ
pair %>% filter(item1 == "Ã¤½Ä")

#°Ç°­°ú ÀÚÁÖ ÇÔ²² »ç¿ëµÈ ´Ü¾î¸¦ È®ÀÎ
pair %>% filter(item1 == "°Ç°­")


#-----------------------------------------------
#µ¿½Ã ÃâÇö ³×Æ®¿öÅ©
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

#±×·¡ÇÁ ´Ùµë±â

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

#³×Æ®¿öÅ© ±×·¡ÇÁ ÇÔ¼ö ¸¸µé±â

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

#À¯ÀÇ¾î Ã³¸®ÇÏ±â

comment <- comment %>%
  mutate(word = ifelse(str_detect(word, "Ã¤½Ä"), "Ã¤½Ä", word),
         word = ifelse(str_detect(word, "°Ç°­"), "°Ç°­", word))

#´Ü¾î µ¿½Ã ÃâÇö ºóµµ ±¸ÇÏ±â
pair <- comment %>%
  pairwise_count(item = word,
                 feature = name,
                 sort = T)

# ³×Æ®¿öÅ© ±×·¡ÇÁ µ¥ÀÌÅÍ ¸¸µé±â
graph_comment <- pair %>%
  filter(n >=40) %>%
  as_tbl_graph()

#³×Æ®¿öÅ© ±×·¡ÇÁ ¸¸µé±â
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
ggraph(graph_comment, layout = "fr") + #·¹ÀÌ¾Æ¿ô
  
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