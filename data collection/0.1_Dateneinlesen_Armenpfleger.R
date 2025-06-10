
########################### Skript Dateneinlesen ########################### 


### PROBLEM: TEILWEISE ZWEI SPALTEN IM TEXT -> Zeilenmässiges einlesen geht nicht wie gewünscht. 


### IDEALFALL: Datensatz mit allen Texten und Metavariablen. 
## Metainformationen nicht als Listen speichern, sondern direkt als Variable im Datensatz (Tibble).




# Pakete laden ------------------------------------------------------------

library(tidyverse)
library(pdftools)
library(fuzzyjoin)
library(glue)


# PDF-Dateien ------------------------------------------------------------

files_location <- list.files("/Volumes/PhD/PhD_Webscraping/Armenpfleger_Komplett",
                             full.names = TRUE)
files_name <- list.files("/Volumes/PhD/PhD_Webscraping/Armenpfleger_Komplett")

# Variablen um Titel zu speichern
output <- tibble(
  document = character(),
  title = character(),
  text_of_interest = character(),
  year = character(),
  author = character(),
  typ = character(),
  zeitschrift = character(),
  heftnummer = character(),
  band = character(),
  url = character()
)


# Loop --------------------------------------------------------------------

for(i in seq_along(files_location)) {
  
  #seq_along sagt, dass es alles durchgehen soll. Für jedes Element in files_location => mache alles gleich. Mehrere Arten, wie man es schreiben kann. (alternativ: 1:files_location)
  
  ### 1. einlesen und aufbereiten
  text <- pdf_text(files_location[i])
  
  
  ### 1.1 Unterschiedliche Metadaten 
  # Textobjekt für Metadaten generieren (1 (!) Sting)
  text_metadata <- str_c(text,
                         collapse = " ")
  # einfach die ersten 4 Zahlen werden extrahiert (\\d{4} = Kurzschreibweise)
  year <-  str_trim(str_extract(files_name[i], "\\d{4}")) 
  author <- str_trim(str_extract(text_metadata, "(?<=Autor\\(en\\):)(.*?)(?=\\\n)"))
  typ <- str_trim(str_extract(text_metadata, "(?<=Objekttyp: )(.*?)(?=\\\n)"))
  zeitschrift <- str_trim(str_extract(text_metadata,"(?<=Zeitschrift: )(.*?)(?=:)"))
  heftnummer <- str_trim(str_extract(text_metadata, "(?<=Heft )(.*?)(?=\\\n)"))
  band <- str_trim(str_extract(text_metadata, "(?<=Band \\(Jahr\\):)(.+?)(?=\\()"))
  url <- str_trim(str_extract(text_metadata, "(?<=Persistenter Link:)(.*?)(?=\\\n)"))
  
  
  text <- pdf_text(files_location[i]) %>% 
    str_split("\n") %>% 
    unlist()  
  
  
  ### 2. Titel extrahieren
  title_start <- 1
  title_end <- text %>% 
    str_which(".+",
              negate = TRUE)
  
  title_end <- title_end[1] - 1
  
  title <- text[title_start:title_end]
  
  if(length(title) > 1) {
    title <- title %>% 
      str_squish() %>% 
      str_c(collapse = " ")
  } else {
    title <- title %>% 
      str_squish()
  }
  
  
  ### 3. Textbeginn 
  
  ## 3.1 wenn kein Titel auf dem Deckblatt definiert ist
  # => Schlüsselwörter können erweitert werden
  if(str_detect(title, "(O|o)bjekttyp:") | 
     str_detect(title, "(A|a)utor(en):")) {
    
    title <- "KEIN TITEL GEFUNDEN"
    
    # Text extrahieren
    # => erste Seite (Deckblatt) wird entfernt.
    end_first_page <- text %>% 
      str_which("http://www.e-periodica.ch")
    end_first_page <- end_first_page[1] + 1
    
    text_of_interest <- text[end_first_page:length(text)]
    
    ## 3.2 wenn der Titel auf dem Deckblatt definiert ist
  } else {
    
    # Text extrahieren
    # der eigentliche text beginnt mit dem entsprechenden Titel
    # die erste Seite (Deckblatt) wird jeweils entfernt.
    end_first_page <- text %>% 
      str_which("http://www.e-periodica.ch")
    end_first_page <- end_first_page[1] + 1
    
    text_of_interest <- text[end_first_page:length(text)]
    
    # Titelzeile/Textstart finden (fuzzy join)
    text_start <- as_tibble(text_of_interest) %>% 
      mutate(value = str_squish(value)) %>% 
      rowid_to_column() %>% 
      stringdist_left_join(as_tibble(title), 
                           by = "value", 
                           ignore_case = T, 
                           method = "jw", 
                           distance_col = "dist") %>% 
      arrange(dist) %>% 
      slice(1) 
    
    # Falls die Distanz zu gross ist, wird davon ausgegangen,
    # dass der Titel nicht im Text zu finden ist. Text beginnt 
    # nach dem Deckblatt.
    # => Distanzwert kann angepasst werden
    if(text_start$dist >= 0.15) {
      text_start <- 1
    } else {
      text_start <- text_start %>% 
        pull(rowid)
    }
    
    text_of_interest <- text_of_interest[text_start:length(text_of_interest)]
    
  }
  
  
  
  
  
  ### 4. Textende 
  
  ## 4.1. Einfacher Ansatz (Schlüsselwörter finden)
  # Schlüsselwörter können erweitert werden
  
  if(any(str_detect(text_of_interest, "(I|i)nserate[:]?"))) {
    text_end <- text_of_interest %>% 
      str_which("(I|i)nserate[:]?") - 1
  } else {
    text_end <- length(text_of_interest)
  }
  
  text_of_interest <- text_of_interest[1:text_end]
  text_of_interest <- text_of_interest %>% 
    str_squish() %>% 
    str_c(collapse = " ") %>% 
    str_squish()
  
  
  ## 4.2. Komplexer Ansatz (in dieser Form leider nicht brauchbar...)
  
  # Leider sind die PDFs zu uneinheitlich. Ich weiss nicht wie ich das Text-
  # ende genau definieren soll.. Ich lass dir den Code mal drin. Vielleicht 
  # fällt dir ja noch eine Möglichkeit ein:)
  
  # # Textende finden
  # empty_lines <- integer()
  # 
  # for(j in seq_along(text_of_interest)){
  #   
  #   if(text_of_interest[j] == "") {
  #     empty_lines <- append(empty_lines, j)
  #   } else{
  #     empty_lines <- empty_lines
  #   }
  # }
  # 
  # # Annahme: höchste Anzahl aufeinanderfolgender leerer Zeilen = Textende
  # empty_lines <- split(empty_lines, cumsum(c(1, diff(empty_lines) != 1)))
  # max_empty_lines <- 0
  # 
  # for(k in 1:length(empty_lines)) {
  #   length_empty_line <- length(empty_lines[[k]])
  #   if(length_empty_line > max_empty_lines) {
  #     max_empty_lines <- length_empty_line
  #   } else{
  #     length_empty_line <- max_empty_lines
  #   }
  # }
  # 
  # # wenn es sich um <= 2 leere Zeilen handelt
  # # wird angenommen, dass das Textende mit dem 
  # # Dokumentenende zusammenfällt.
  # if(max_empty_lines <= 2) {
  #   text_end <- length(text_of_interest)
  # } else{
  #   text_end <- empty_lines[[max_empty_lines]][1]
  # }
  # 
  # text_of_interest <- text_of_interest[1:text_end]
  # text_of_interest <- text_of_interest %>% 
  #   str_squish() %>% 
  #   str_c(collapse = " ") %>% 
  #   str_squish()
  
  
  ### 5. abspeichern
  temp <- tibble(
    document = files_name[i],
    title = title,
    text_of_interest = text_of_interest, 
    year = year,
    author = author, 
    typ = typ, 
    zeitschrift = zeitschrift, 
    heftnummer = heftnummer, 
    band = band,
    url = url
  )
  
  output <- output %>% 
    bind_rows(temp)
  
  
  ### 6. aufräumen
  suppressWarnings(
    rm(text_metadaten, year, author, 
       typ, zeitschrift, heftnummer, band, url,
       text, title, title_start, title_end,
       end_first_page, text_of_interest,
       text_start, text_end, empty_lines,
       max_empty_lines, length_empty_line,
       temp) 
  )
  gc()
  
  message(glue("PDF-Dokument: {files_name[i]} ({i}/{length(files_name)})"))
  if(i == length(files_name)) {
    message("Fertig!")
  }
  
}


# Überprüfen ob Einlesen geklappt hat

view(output)

as.character(output$text_of_interest)[1000]



# Als Datensatz - R speichern
getwd()
saveRDS(output, file="/Users/luki/Dropbox/Mein Mac (Lukass-MacBook-Pro.local)/Desktop/R_DataAnalysis/R_Expertendiskurse/DATA/1_Rohdaten_Armenpfleger.rds")


