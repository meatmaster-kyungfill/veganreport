#∫Ò∞« µ•¿Ã≈Õ ∫“∑Øø¿±‚
vegan_social_data <- read_excel("vegan social data.xlsx")

# ¿¸√≥∏Æ

library(dplyr)
library(stringr)
library(textclean)

vegan_data <- vegan_social_data %>%
  select(title) %>%
  mutate(title = str_replace_all(title, "[^∞°-∆R]", " "),
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

#«∞ªÁ∫∞∑Œ «‡ ∫–∏Æ
library(tidyr)
vegan_pos <- vegan_pos %>%
  separate_rows(word, sep = "[+]")
vegan_pos %>%
  select(word, title)

#∏ÌªÁ √ﬂ√‚«œ±‚

noun <- vegan_pos %>%
  filter(str_detect(word, "/n")) %>%
  mutate(word = str_remove(word, "/.*$"))

noun %>%
  select(word, title)

#æÓ∂≤ ∏ÌªÁ∞° ∏π¿Ã ªÁøÎµ«æ˙¥¬¡ˆ »Æ¿Œ
noun %>%
  count(word, sort =T)

#µøªÁ, «¸øÎªÁ √ﬂ√‚«œ±‚
pvpa <- vegan_pos %>%
  filter(str_detect(word, "/pv|/pa")) %>%
  mutate(word = str_replace(word, "/.*$", "¥Ÿ"))

pvpa %>%
  select(word, title)

#count∏¶ ¿ÃøÎ«œø© ¥Ò±€ø° æÓ∂≤ µøªÁøÕ «¸øÎªÁ∞° ∏π¿Ã ªÁøÎµ«æ˙¥¬¡ˆ »Æ¿Œ
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

#√§Ωƒ∞˙ ¿⁄¡÷ «‘≤≤ ªÁøÎµ» ¥‹æÓ∏¶ »Æ¿Œ
pair %>% filter(item1 == "√§Ωƒ")

#∞«∞≠∞˙ ¿⁄¡÷ «‘≤≤ ªÁøÎµ» ¥‹æÓ∏¶ »Æ¿Œ
pair %>% filter(item1 == "∞«∞≠")
