
# Loading Packages --------------------------------------------------------


if(!require("quanteda")) {install.packages("quanteda"); library("quanteda")}
if(!require("seededlda")) {install.packages("seededlda"); library("seededlda")}
if(!require("lubridate")) {install.packages("lubridate"); library("lubridate")}
if(!require("readtext")) {install.packages("readtext"); library("readtext")}
if(!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
if(!require("scales")) {install.packages("scales"); library("scales")}
if(!require("RColorBrewer")) {install.packages("RColorBrewer"); library("RColorBrewer")}
if(!require("pdftools")) {install.packages("pdftools"); library("pdftools")}
if(!require("stringr")) {install.packages("stringr"); library("stringr")}
if(!require("purrr")) {install.packages("purrr"); library("purrr")}
if(!require("stringi")) {install.packages("stringi"); library("stringi")}
if(!require("fuzzyjoin")) {install.packages("fuzzyjoin"); library("fuzzyjoin")}
if(!require("glue")) {install.packages("glue"); library("glue")}
if(!require("textcat")) {install.packages("textcat"); library("textcat")}


# KORPUS LADEN ------------------------------------------------------------

corpus_all <- readRDS("DATA/CORPUS_ALL.rds")
head(summary(corpus_all))

corpus_all[[3]]

ndoc(corpus_all)        

summary(corpus_all$year)

summary(as.factor(corpus_all$typ))

AP_all <- corpus_subset(corpus_all, typ == "BookReview" | typ == "Article" | typ == "Preface" | typ == "Group")

ndoc(AP_all)

# Tokenize the corpus
toks <- tokens(AP_all)

# Count the total number of tokens
total_tokens <- sum(ntoken(toks))

# Print the result
print(total_tokens)

AP_all$institution <- "SAPK"

summary(AP_all)

### ALLE CORPORA SAVEN
saveRDS(AP_all, file ="/Users/luki/Dropbox/Mein Mac (Lukass-MacBook-Pro.local)/Desktop/R_DataAnalysis/ALL_CORPORA/AP_ALL.rds")

           