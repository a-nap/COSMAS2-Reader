library(tidyr)
library(stringr)
library(dplyr)
library(readr)
library(purrr)
library(wordcloud2)

# Reading in the raw COSMAS file
# FIXME  add encoding options "latin1" and "UTF-8"
# raw.file <- read_file("test.txt", locale(encoding="latin1"))
raw.file <- read_file("w_sentence_korpusansicht.TXT", locale(encoding="latin1"))

# Metadata ----------------------------------------------------------------
# Save COSMAS version 
C2API_Version <- raw.file %>%
  str_extract(regex("(?<=C2API-Version )(.*)(?= -)")) %>%
  str_subset(regex(".*"))

# Split file into sections
sections <- raw.file %>%
  str_split("\\_{80}")

# Save export date
Export_Date <- sections[[1]][2] %>%
                str_extract(regex("(?<=\\n\\nDatum).+(?=\\nArchiv)")) %>%
                str_extract(regex("(?<=:\\s)(.*)$"))
# Save the search phrase
phrase <- sections[[1]][2] %>%
            str_extract(regex("(?<=\\nSuchanfrage).+(?=\\nSuchoptionen)")) %>%
            str_extract(regex("(?<=:\\s)(.*)$"))

# Sentences and their information -----------------------------------------
corpora <- sections[[1]][3]
corporaID <- corpora %>% 
  str_split("\\n") %>%
  unlist() %>%
  head(-7) %>%
  str_extract(regex('(^\\w+)\\s(?=.*$)')) %>%
  str_trim() %>%
  str_subset(regex(".*")) %>%
  unique() %>%
  str_c(collapse = "|")

all_sentences <- sections[[1]][4]
all_sentences %>%
  str_split(regex("\\nKorpus-Ansicht\\,\\s+[:digit:]*\\s+Einträge")) %>%
  unlist()

  
text_parts <- all_sentences %>%
  str_match_all(regex(paste("(.*?)<B>(.+?)</>(.*?)\\(((?:",corporaID,")/.*?)\\)\\s*\\n", sep=""), 
                      dotall = TRUE))

# Source information
data <- data.frame(Sources = text_parts[[1]][,5])

# Tokens
data$Token <- text_parts[[1]][,3] %>%
  str_trim()
unique_tokens <- unique(data$Token)
print(unique_tokens, row.names=FALSE)

# Context sentence BEFORE token sentence
data$Precontext <- text_parts[[1]][,2] %>%
  str_extract_all(boundary("sentence")) %>%
  map(function(x) {nth(x,-2)} ) %>%
  str_trim() %>%
  unlist()

# Sentence part BEFORE token
data$Prehit <- text_parts[[1]][,2] %>%
  str_extract_all(boundary("sentence")) %>%
  map(last) %>%
  str_trim() %>%
  unlist()

# Sentence part AFTER token
data$Posthit <- text_parts[[1]][,4] %>%
  str_extract_all(boundary("sentence")) %>%
  map(first) %>%
  str_trim() %>%
  unlist()

# Extract context sentence AFTER token sentence
data$Postcontext <- text_parts[[1]][,4] %>%
  str_extract_all(boundary("sentence")) %>%
  map(function(x) {nth(x,2)} ) %>%
  str_trim() %>%
  unlist()

# Creating data frame for export ------------------------------------------
data <- 
  data %>%
  unite(Prehit, Token, Posthit, col="Sentence", sep = " ", remove=F) %>%
  mutate(C2API_Version = C2API_Version, Export_Date = Export_Date) %>%
  select(C2API_Version, Export_Date, Token, Precontext, Sentence, Postcontext) %>%
  replace_na(list(Precontext = "", Postcontext = ""))

unique_words <- data %>%
  select(Sentence) %>%
  str_to_sentence("de") %>%
  str_extract_all(boundary("word")) %>%
  unlist() %>%
  str_subset(regex("[^und, der, die, das, Der, Die, Das, mond][:alpha:]")) %>%
  data.frame()
colnames(unique_words) <- "word"
unique_words <- unique_words %>%
  count(word, sort=TRUE)

wordcloud2(data=unique_words, size=1.6, color='random-dark', shape = "circle")
dev.off()

