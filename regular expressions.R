library(tidyr)
library(stringr)
library(dplyr)
library(readr)
library(purrr)

# Reading in the raw COSMAS file
# FIXME  add encoding options "latin1" and "UTF-8"
raw.file <- read_file("test6.TXT", locale(encoding="latin1"))

# Metadata ----------------------------------------------------------------
# Save COSMAS version 
C2API_Version <- raw.file %>%
  str_extract(regex("(?<=C2API-Version )(.*)(?= -)")) %>%
  str_subset(regex(".*"))

# Split file into sections
sections <- raw.file %>%
  str_split("\\_{80}")

# Save export date
export_date <- sections[[1]][2] %>%
                str_extract(regex("(?<=\\n\\nDatum).+(?=\\nArchiv)")) %>%
                str_extract(regex("(?<=:\\s)(.*)$"))
# Save the search phrase
phrase <- sections[[1]][2] %>%
            str_extract(regex("(?<=\\nSuchanfrage).+(?=\\nSuchoptionen)")) %>%
            str_extract(regex("(?<=:\\s)(.*)$"))

# Sentences and their information -----------------------------------------
all_sentences <- sections[[1]][4]
text_parts <- all_sentences %>%
  str_match_all(regex("(.*?)<B>(.+?)</>(.*?)\\(((?:A09|A97)/.*?)\\)\\s*\\n", 
                      dotall = TRUE))

# Source information
data <- data.frame(Sources = text_parts[[1]][,5])

# Tokens
data$Token <- text_parts[[1]][,3]

# Context sentence BEFORE token sentence
data$Precontext <- text_parts[[1]][,2] %>%
  str_extract_all(boundary("sentence")) %>%
  map(function(x) {nth(x,-2)} ) %>%
  unlist()

# Sentence part BEFORE token
data$Prehit <- text_parts[[1]][,2] %>%
  str_extract_all(boundary("sentence")) %>%
  map(last) %>%
  unlist()

# Sentence part AFTER token
data$Posthit <- text_parts[[1]][,4] %>%
  str_extract_all(boundary("sentence")) %>%
  map(first) %>%
  unlist()

# Extract context sentence AFTER token sentence
data$Postcontext <- text_parts[[1]][,4] %>%
  str_extract_all(boundary("sentence")) %>%
  map(function(x) {nth(x,2)} ) %>%
  unlist()

# Creating data frame for export ------------------------------------------
# Adding metadata (COSMAS API version and export date)
data$C2API_Version <- C2API_Version
data$Export_Date <- export_date

data <- data %>%
  unite(Prehit, Token, Posthit, col="Sentence", sep = " ", remove=F) %>%
  select(C2API_Version, Export_Date, Token, Precontext, Sentence, Postcontext)