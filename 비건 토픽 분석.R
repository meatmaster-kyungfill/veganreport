library(readxl)
library(dplyr)
#∫Ò∞« µ•¿Ã≈Õ ∫“∑Øø¿±‚

raw_vegan_data <- read_excel('vegan social data.xlsx') %>%
  mutate(name = row_number())
library(stringr)
library(textclean)

#±‚∫ª¿˚¿Œ ¿¸√≥∏Æ
vegan_data <- raw_vegan_data %>%
  mutate(title = str_replace_all(title, "[^∞°-∆R]", " "),
         title = str_squish(title))%>%
  
  #¡ﬂ∫π ¥Ò±€ ¡¶∞≈
  distinct(title, .keep_all = T) %>%
  
  #¬™¿∫ πÆº≠ ¡¶∞≈ - 3 ¥‹æÓ ¿ÃªÛ √ﬂ√‚√‚
  filter(str_count(title, boundary("word")) >= 3)

library(tidytext)
library(KoNLP)

comment <- vegan_data %>%
  unnest_tokens(input = title,
                output = word,
                token = extractNoun,
                drop = F) %>%
  filter(str_count(word) > 1) %>%
  
  #¥Ò±€ ≥ª ¡ﬂ∫π ¥‹æÓ ¡¶∞≈
  group_by(title) %>%
  distinct(word, .keep_all = T)%>%
  ungroup() %>%
  select(title, word)

#∫Ûµµ∞° ≥Ù¿∫ ¥‹æÓ ¡¶∞≈«œ±‚

count_word <- comment %>%
  add_count(word) %>%
  filter(n <= 200) %>%
  select(-n)

count_word %>%
  count(word, sort = T) %>%
  print(n = 200)

#∫“øÎæÓ ¡¶∞≈«œ±‚, ¿Ø¿«æÓ √≥∏Æ«œ±‚

count_word %>%
  count(word, sort = T) %>%
  print(n = 200)

stopword <- c("«’¥œ", "«œºº", "∫ªπÆ", "µÈ¿Ã", "«ÿº≠", "¡¯¬•", "æ»≥Á",
              "ºº∞Ë", "øµªÛ", "¿÷Ω¿¥œ")

count_word <- count_word %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word,
                       "æ∆≈‰" = "æ∆≈‰««",
                       "∫Œ≈πµÂ∏≥¥œ" = "∫Œ≈π"))

#πÆº≠∫∞ ¥‹æÓ ∫Ûµµ ±∏«œ±‚

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
#≈‰«»∫∞ ¡÷ø‰ ¥‹æÓ ªÏ∆Ï∫∏±‚

term_topic <- tidy(lda_model, matrix = "beta")

term_topic %>%
  count(topic)

term_topic %>%
  filter(topic == 1) %>%
  summarise(sum_beta = sum(beta))

term_topic %>%
  filter(term == "±πªÍ")

term_topic %>%
  filter(topic == 6) %>%
  arrange(-beta)

terms(lda_model, 20) %>%
  data.frame()

#≈‰«»∫∞∑Œ beta∞° ∞°¿Â ≥Ù¿∫ ¥‹æÓ √ﬂ√‚«œ±‚
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
  filter(topic == 2 & str_detect(title, "ΩƒªÁ")) %>%
  head(50) %>%
  pull(title)

name_topic <- tibble(topic = 1:8,
                     name = c("1. ∫Ò∞«¿ŒµÈ¿« »˙∏µ",
                              "2. ∫Ò∞«¿ŒµÈ¿« ø‹Ωƒ ∑Ø∫˘«Í",
                              "3. ∫Ò∞«¿ŒµÈ¿« »˙∏µ(2)",
                              "4. ∫Ò∞«¿ŒµÈ¿« ∞˙¿œ",
                              "5. ∫Ò∞«¿ŒµÈ¿« ¥Ÿ¿ÃæÓ∆Æ",
                              "6. ∫Ò∞«¿ŒµÈ¿« ¡§∫∏∞¯¿Ø",
                              "7. ∫Ò∞«¿ŒµÈ¿« ¥‹πÈ¡˙ º∑√Î",
                              "8. ∫Ò∞«¿ŒµÈ¿« π›¬˘"))

top_term_topic_name <- top_term_topic %>%
  left_join(name_topic, name_topic, by = "topic")
top_term_topic_name

#∏∑¥Î ±◊∑°«¡ ∏∏µÈ±‚
windows()
ggplot(top_term_topic_name,
       aes(x = reorder_within(term, beta, name),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ name, scales = "free", ncol = 2) +
  coord_flip() +
  scale_x_reordered() +
  
  labs(title = "«—øÔπ˛ √§Ωƒ≥™∂Û ∫Ò∞«¿Œ ∂Û¿Ã«¡ Ω∫≈∏¿œ ∫–ºÆ",
       subtitle = " ≈‰«»∫∞ ¡÷ø‰ ¥‹æÓ TOP 10",
       x = NULL, y = NULL) +
  
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        title = element_text(size=12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

