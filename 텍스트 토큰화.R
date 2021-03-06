#비건 데이터 불러오기
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
