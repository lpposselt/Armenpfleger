
library(quanteda)
library(readtext)
library(paletteer)
library(tidyverse)
library("RColorBrewer")
library(viridis)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)

if(!require("seededlda")) {install.packages("seededlda"); library("seededlda")}
if(!require("lubridate")) {install.packages("lubridate"); library("lubridate")}
if(!require("readtext")) {install.packages("readtext"); library("readtext")}
if(!require("scales")) {install.packages("scales"); library("scales")}
if(!require("pdftools")) {install.packages("pdftools"); library("pdftools")}
if(!require("stringr")) {install.packages("stringr"); library("stringr")}
if(!require("purrr")) {install.packages("purrr"); library("purrr")}
if(!require("stringi")) {install.packages("stringi"); library("stringi")}
if(!require("quanteda.textstats")) {install.packages("quanteda.textstats"); library("quanteda.textstats")}
if(!require("quanteda.textplots")) {install.packages("quanteda.textplots"); library("quanteda.textplots")}
if(!require("scales")) {install.packages("scales"); library("scales")}
if(!require("ggdendro")) {install.packages("ggdendro"); library("ggdendro")}
if(!require("topicmodels")) {install.packages("topicmodels"); library("topicmodels")}
if(!require("ldatuning")) {install.packages("ldatuning"); library("ldatuning")}
if(!require("stm")) {install.packages("stm"); library("stm")}
if(!require("wordcloud")) {install.packages("wordcloud"); library("wordcloud")}
if(!require("devtools")) {install.packages("devtools"); library("devtools")}
if(!require("tidytext")) {install.packages("tidytext"); library("tidytext")}
if(!require("broom")) {install.packages("broom"); library("broom")}
if(!require("readr")) {install.packages("readr"); library("readr")}




### STANDARDEINSTELLUNGEN:

# Set the default theme for all plots
theme_set(
  theme_minimal(base_size = 24) + # Adjust base font size for all text elements
    theme(
      axis.title = element_text(size = 28),  # Axis titles
      axis.text = element_text(size = 28),   # Axis labels
      plot.title = element_text(size = 28, face = "bold", hjust = 0.5), # Title
      legend.title = element_text(size = 26), # Legend title
      legend.text = element_text(size = 26),
      legend.position = "bottom",
      panel.background = element_rect(fill = "white", color = NA), # Ensures no grey
      panel.grid.major = element_line(color = "grey90"), # Subtle grid lines
      panel.grid.minor = element_blank() # Removes minor grid lines
    )
)

# Set default figure dimensions and resolution
figure_width <- 12
figure_height <- 10
dpi <- 300

# Optional: Create a function to save plots with consistent dimensions
save_plot <- function(plot, filename) {
  ggsave(
    filename,
    plot = plot,
    width = figure_width,
    height = figure_height,
    dpi = dpi
  )
}



modell.stm <- readRDS("20250215stm_12_KorpusALL.rda")

# MODELL BEGUTACHTEN -----------------------------------------------

as.data.frame(t(labelTopics(modell.stm, n = 5)$prob))

par(mar=c(0.5, 0.5, 0.5, 0.5))
cloud(modell.stm, topic = 8, scale = c(2.25,.5))

plot(modell.stm, type = "summary", text.cex = 0.5, main = "Topic shares on the corpus as a whole", xlab = "estimated share of topics")

plot(modell.stm, type = "hist", topics = sample(1:n.topics, size = 9), main = "histogram of the topic shares within the documents")


uebersicht <- as.data.frame(t(labelTopics(modell.stm, n = 20)$prob))
uebersicht

write_delim(uebersicht, path = "20250409FINALstm12_topics.csv", delim = ";") # Datei ist Excel-kompatibel

#TOPIC 2: ALCOHOLISM
#TOPIC 4: SOCIAL WORK
#TOPIC 12: SOCIAL INSURANCES / AHV

##### KORPUS LADEN

data <- read.csv("/Users/luki/Desktop/AP_ALL/code/df_cleaned.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

data$text_cl[1]





corp_all <- corpus(data$text, docnames = data$doc_id)
docvars(corp_all) <- data[, c("year", "institution", "author")]
summary(corp_all)

corp_all <- corpus_subset(corp_all, year > 1918)
summary(corp_all)
class(corp_all)

# Tokenisierung -----------------------------------------------------------

toks_ger <- tokens(corp_all, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(pattern = stopwords("de", source = "marimo")) %>% 
  tokens_remove(c(stopwords("french"), "?", "??")) %>%
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex")

print(toks_ger[2], max_ndoc = 1, max_ntoken = -1)

toks_ger <- tokens_tolower(toks_ger)
print(toks_ger[2], max_ndoc = 1, max_ntoken = -1)

# Händische Stopplisten: Anhand des jeweiligen Kopfs der Magazine

toks_2 <-  tokens_remove(toks_ger, c( 
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
  "sköf"
))

### DFM erstellen und Top-Features checken

m.dfm <- dfm(toks_2)

nfeat(m.dfm)
ndoc(m.dfm)

topfeatures(m.dfm, n = 100)

m2.dfm <- dfm_remove(m.dfm, c("art", "gallen", "skos", "basel", "abs", "öffentlichen", "sozialhilfe", "man"))
nfeat(m.dfm) - nfeat(m2.dfm)

topfeatures(m2.dfm)

ndoc(m2.dfm)
summary(m2.dfm$year)

nfeat(m2.dfm)

soz_dfm <- dfm_trim(m2.dfm, 
                    max_docfreq = 0.70,
                    min_docfreq = 0.001,
                    docfreq_type = 'prop')

nfeat(m2.dfm)
nfeat(soz_dfm)


ndoc(soz_dfm)
soz2_dfm <- dfm_subset(soz_dfm, ntoken(soz_dfm) > 0)
ndoc(soz2_dfm)


# Modell im ZEITVERLAUF  -----------------------------------------------


n.topics <- 12
dfm2stm <- convert(soz2_dfm, to = "stm")

modell.stm.labels <- labelTopics(modell.stm, 1:n.topics)
dfm2stm$meta$datum <- as.numeric(dfm2stm$meta$year)
modell.stm.effekt <- estimateEffect(1:n.topics ~ s(year), modell.stm, meta = dfm2stm$meta)

summary(modell.stm.effekt)


# GRAFIKEN

# Extract effect estimates
effect_summary <- summary(modell.stm.effekt)
effect_summary

effect_data_2 <- plot.estimateEffect(
  modell.stm.effekt, covariate = "year", topics = 2, model = modell.stm,
  method = "continuous", print = FALSE
)

effect_data_4 <- plot.estimateEffect(
  modell.stm.effekt, covariate = "year", topics = 4, model = modell.stm,
  method = "continuous", print = FALSE
)

effect_data_12 <- plot.estimateEffect(
  modell.stm.effekt, covariate = "year", topics = 12, model = modell.stm,
  method = "continuous", print = FALSE
)

# Check if it returns NULL
str(effect_data_2)
str(effect_data_4)


### EXTRACT THE INFORMATION: 

# Extract values from effect_data_2
effect_df_2 <- data.frame(
  year = effect_data_2$x,  # Years
  est = unlist(effect_data_2$means),  # Estimated topic proportions
  ci.lower = effect_data_2$ci[[1]][1,],  # Lower CI
  ci.upper = effect_data_2$ci[[1]][2,]   # Upper CI
)

effect_df_2$topic <- "Alcoholism"

# Extract values from effect_data_4
effect_df_4 <- data.frame(
  year = effect_data_4$x,  # Years
  est = unlist(effect_data_4$means),  # Estimated topic proportions
  ci.lower = effect_data_4$ci[[1]][1,],  # Lower CI
  ci.upper = effect_data_4$ci[[1]][2,]   # Upper CI
)

# Add topic label
effect_df_4$topic <- "Social Work"

# Repeat for Topic 12
effect_df_12 <- data.frame(
  year = effect_data_12$x,  
  est = unlist(effect_data_12$means),
  ci.lower = effect_data_12$ci[[1]][1,],
  ci.upper = effect_data_12$ci[[1]][2,]
)

effect_df_12$topic <- "Social Insurances"
effect_df_12
# Combine all topics into one dataframe
effect_df <- bind_rows(effect_df_2, effect_df_4, effect_df_12)

# Check the structure
str(effect_df)
head(effect_df)

effect_df$year <- floor(effect_df$year)  # Rounds down to the year

table(effect_df$year, effect_df$topic)  # Check if each topic has its own row per year
effect_df <- effect_df %>%
  distinct(year, topic, .keep_all = TRUE)  # Keep only unique year-topic pairs


ggplot(effect_df, aes(x = year, y = est, color = topic, group = topic)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ci.lower, ymax = ci.upper, fill = topic), alpha = 0.2) +
  scale_x_continuous(breaks = seq(min(effect_df$year), max(effect_df$year), by = 5)) +
  theme_minimal() +
  labs(
    title = "Selected Topics Over Time",
    x = "Year",
    y = "Topic Proportion",
    color = "Topic",
    fill = "Topic"
  ) +
  theme(legend.position = "right")


#### New colors: 

ggplot(effect_df, aes(x = year, y = est, color = topic, group = topic)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ci.lower, ymax = ci.upper, fill = topic), alpha = 0.2) +
  scale_x_continuous(
    breaks = seq(min(effect_df$year), max(effect_df$year), by = 5),  # Set breaks every 5 years
    labels = as.integer  # Ensure no decimal places
  ) +
  scale_color_viridis_d(option = "plasma") +  
  scale_fill_viridis_d(option = "plasma", alpha = 0.5) +  
  theme_minimal() +
  labs(
    title = "Selected Topics Over Time",
    x = "Year",
    y = "Topic Proportion",
    color = "Topic",
    fill = "Topic"
  ) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)  # Rotate x-axis labels
  )

###################
#### FINAL
###################

q <- ggplot(effect_df, aes(x = year, y = est, color = topic, group = topic)) +
  geom_line(size = 1.2) +
  scale_x_continuous(
    breaks = seq(min(effect_df$year), max(effect_df$year), by = 5),  # Set breaks every 5 years
    labels = as.integer  # Ensure no decimal places
  ) +
  labs(
    title = "Selected Topics Over Time",
    x = "Year",
    y = "Topic Proportion",
    color = "Topic"
  ) +
  scale_color_viridis_d(option = "viridis") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

q

save_plot(q, "20250409topics.pdf")
