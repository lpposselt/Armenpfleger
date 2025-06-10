
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


# Daten aus drei Quellen laden --------------------------------------------

AP <- readRDS("DATA/1_Rohdaten_Armenpfleger.rds")
ZOEF <- readRDS("DATA/1_Rohdaten_ZOEF.rds")
ZESO <- readRDS("DATA/1_Rohdaten_ZESO.rds")


# Korpora daraus erstellen ------------------------------------------------

## AP

corp1 <- AP %>% 
    corpus(docid_field="document", text_field = "text_of_interest") 

head(summary(corp1))
typeof(corp1$year)

#Jahr zur numerischen Variable machen
docvars(corp1, "year") <- as.numeric(corp1$year) 
typeof(corp1$year)
summary(corp1$year)


## ZOEF 

corp2 <- ZOEF %>% 
    corpus(docid_field="document", text_field = "text_of_interest")

head(summary(corp2))
typeof(corp2$year)
docvars(corp2, "year") <- as.numeric(corp2$year) 
typeof(corp2$year)
summary(corp2$year)


## ZESO

corp3 <- ZESO %>% 
  corpus(docid_field="document", text_field = "text_of_interest")

head(summary(corp3))
typeof(corp3$year)
docvars(corp3, "year") <- as.numeric(corp3$year) 
typeof(corp3$year)
summary(corp3$year)



# Aufbereitung Finaler Korpus ---------------------------------------------

#Korpora zusammenführen
corpus_all <- corpus(corp1 + corp2 + corp3)
ndoc(corpus_all)

summary(corpus_all$year)


#Zu kurze Texte rausschmeissen: 

corpus_all2 <- corpus_trim(
  corpus_all,
  what = "documents",
  min_ntoken = 100
)

head(summary(corpus_all2))

#Anzahl rausgegickter Dokumente
ndoc(corpus_all) - ndoc(corpus_all2)
#124 Dokumente rausgekickt, weil zu kurz

#Sprache für jeden einzelnen Text feststellen: 
docvars(corpus_all2, field = "sprache") <- textcat(corpus_all2)

head(docvars(corpus_all2))

docvars(corpus_all2, "sprache") <- as.factor(corpus_all2$sprache) 
summary(corpus_all2$sprache)

#Nichtdeutsche Texte rausschmeissen
c_all <- corpus_subset(corpus_all2, sprache=="german")

ndoc(corpus_all2) - ndoc(c_all) 
#24 Dokumente wurden wegen der Sprache rausgeschmissen. 



# Korpus speichern --------------------------------------------------------

saveRDS(c_all, file="DATA/Corpus_ALL.rds")

### Korpus ausschliesslich mit Artikel (Nach Texttyp gefiltert)

head(summary(c_all))
corpus_artikel <- corpus_subset(c_all, typ=="Article")
ndoc(corpus_artikel)

head(summary(corpus_artikel))

### Anzahl der Texte, die keine Artikel sind: 
ndoc(c_all) - ndoc(corpus_artikel)

### Korpus mit Artikel abspeichern
saveRDS(corpus_artikel, file ="DATA/Corpus_Article_ALL.rds")
