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

# Korpus laden ------------------------------------------------------------

working_corp <- readRDS("DATA/Corpus_Article_ALL.rds")
korpus.stats <- summary(working_corp)
ndoc(working_corp)




# Tokenisierung -----------------------------------------------------------


### Anzahl Artikel pro Jahr => geplottete Darstellung
overview <- as.data.frame(table(working_corp$year))
overview

plot(overview)
summary(working_corp$year)

### Tokenisierung 

tokens <- tokens(working_corp,
                 what = "word",
                 remove_punct = TRUE,
                 remove_symbols = TRUE,
                 remove_numbers = TRUE,
                 remove_url = TRUE,
                 remove_separators = TRUE,
                 split_hyphens = TRUE,
                 include_docvars = TRUE)

print(tokens[2], max_ndoc = 1, max_ntoken = -1)



# Entfernen von Stoppwörtern

# Stoppwörter

tokens <- tokens(tokens, include_docvars = TRUE) %>% 
  tokens_remove(pattern = stopwords("de", source = "marimo")) %>% 
  tokens_remove(c(stopwords("french"), "?", "??")) %>%
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex")

head(summary(tokens))

print(tokens[2], max_ndoc = 1, max_ntoken = -1)

tokens <- tokens_tolower(tokens)
print(tokens[2], max_ndoc = 1, max_ntoken = -1)

# Händische Stopplisten: Anhand des jeweiligen Kopfs der Magazine
### EVENTUEL ERGÄNZEN MIT: SKOS, ETC. ETC. 

tokens <-  tokens_remove(tokens, c( 
  "ZESO", 
  "ZOF",
  "ZOEF", 
  "monatsschrift",  
  "armenpflege",
  "jugendfürsorge",
  "offizielles",
  "organ", 
  "schweizerischen", 
  "armenpfleger-konferenz", 
  "beilage", 
  "zentralblatt", 
  "staats", 
  "gemeinde-verwaltung", 
  "redaktion",  
  "pfarrer", 
  "wild",
  "zürich",
  "verlag",
  "exp", 
  "akt", 
  "institut", 
  "orell", 
  "füssli", 
  "armenpfleger",
  "erscheint", 
  "monatlich", 
  "jährlicher", 
  "abonnementspreis", 
  "direkte abonnenten", 
  "postabonnenten", 
  "insertionspreis", 
  "pro", 
  "nonpareille-zeile",
  "rp.",
  "nachdruck", 
  "unserer", 
  "orginalartikel", 
  "quellenangabe",
  "gestattet", 
  "armenpfleger", 
  "schwerpunkt", 
  "zeso", 
  "zeitschrift", 
  "jahrgang", 
  "nr.", 
  "öffentliche", 
  "fürsorge",
  "enthaltend", 
  "entscheide", 
  "gebiete",
  "fürsorge-",
  "sozialversicherungswesens",
  "konferenz",
  "sköf", 
  "nnd", 
  "red"
))

print(tokens[2], max_ndoc = 1, max_ntoken = -1)

#Stemming
tokens <- tokens_wordstem(tokens,
                          language = "german")

print(tokens[2], max_ndoc = 1, max_ntoken = -1)


# DFM erstellen -----------------------------------------------------------


meine.dfm <- dfm(tokens)
nfeat(meine.dfm)
ndoc(meine.dfm)

topfeatures(meine.dfm, n=100)
meine.dfm
meine.dfm[ntoken(meine.dfm) == 0,]


#t1 <- textstat_frequency(meine.dfm)
#view(t1)

#ggplot(t1[1:30, ], aes(x = reorder(feature, frequency), y = frequency)) +
# geom_point() +
#  coord_flip() +
#  labs(x = NULL, y = "Frequency")

head(summary(meine.dfm$year))

dfm <- dfm_trim(meine.dfm, 
                    max_docfreq = 0.70,
                    min_docfreq = 0.005,
                    docfreq_type = 'prop')

nfeat(meine.dfm) - nfeat(dfm)

#Länge der Texte habe ich schon beim Aufbereiten kontrolliert

stm <- convert(dfm,
               to = "stm",
               docvars = docvars(working_corp))


# DFM und STM speichern ---------------------------------------------------

getwd()

write_rds(x = dfm,
          file = "DATA/dfm.rds")

write_rds(x = stm, 
          file = "DATA/stm.rds") 

