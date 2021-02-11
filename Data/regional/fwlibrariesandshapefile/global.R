#### SCRIG App Global File ####-------------------------------------####
#### Neccessary Libraries ####
library(anytime)      ##For Formatting dates correctly
library(cowplot)      ##For  getting tables beside benchmarking charts
library(DT)            ##For Datatables
library(flextable)    ## For getting tables in reports
library(ggplot2)       ##For graphs
library(grid)         ## For arranging tables beside benchmarking charts
library(gridExtra)    ## FOr arranging tables beside benchmarking charts
library(gtools)       ## FOr Smartbind
library(knitr)        ## For Generating reports
library(later)        ## For shceduling a garbage clean evey x seconds
library(leaflet)      ## For interactive maps
library(plotly)       ## For interactive charts
library(plyr)         ## For formatting
library(RColorBrewer) ## For colours for charts
library(openxlsx)     ## For reading in .xlsx files firectly (no reliance on Java)
library(rgdal)         ##For readOGR (mapping)
library(rintrojs)     ## For help guide 
library(rlist)        
library(rmarkdown)     ##For RMarkdown (in general)
library(scales)
library(shiny)         ##For App
library(shinyalert)   ## For error alerts
library(shinycssloaders)  ## For loading spinners
library(shinyjs)       ##For more functions (javascript)
library(shinythemes)   ##For decoration
library(shinyWidgets)
library(stringi)        ## For string manipulation
library(tidyverse)      ## Loads a lot of packages all at once (dplyr,tidyr,readr,ggplot2 amongst others)
library(tmap)          ##For static maps
library(shinyBS)
options(stringsAsFactors = FALSE,scipen = 999)

#### Loading in Measures Data (for Later Graphs) ####
trend_limits <- read.xlsx("./Data/TrendLimits2.xlsx", sheet = 1, colNames = TRUE,na.strings = "NA") %>% 
  mutate(Measure = gsub("\\s+"," ",Measure)) %>% 
  mutate(Measure = gsub(".","",Measure,fixed = TRUE))

#####################################################################################################################
#### Static Dataset Set up ####
#### Loading in Data ####
#### Splitting Data into Ps, protected characteristics and other breakdowns ####
# Productivity #
productivity_data <- read.xlsx("./Data/Data From ODP.xlsx", sheet = "Productivity") %>% 
  filter(trimws(Data.Level) == "Main") %>% 
  group_by(Indicator) %>% 
  mutate(Indicator2 = paste(Indicator, "-", max(Year, na.rm = TRUE))) %>% 
  ungroup() %>% 
  select(-Indicator) %>% 
  mutate(Figure = as.numeric(Figure)) %>% 
  dplyr::rename("Indicator" = Indicator2) %>% 
  as.data.frame()

# Participation #
participation_data <- read.xlsx("./Data/Data From ODP.xlsx", sheet = "Participation") %>% 
  filter(trimws(Data.Level) == "Main") %>% 
  group_by(Indicator) %>% 
  mutate(Indicator2 = paste(Indicator, "-", max(Year, na.rm = TRUE))) %>% 
  ungroup() %>% 
  select(-Indicator) %>% 
  mutate(Figure = as.numeric(Figure)) %>% 
  dplyr::rename("Indicator" = Indicator2) %>% 
  as.data.frame() 

dependencyRatioYear <- read.xlsx("./Data/Data From ODP.xlsx", sheet = "Population") %>% 
  filter(trimws(Data.Level) == "Main") %>% 
  filter(Indicator == "Dependency Ratio") %>% 
  filter(Year <= Sys.Date() %>% format("%Y")) %>% 
  pull(Year) %>% 
  unique() %>% 
  as.numeric() %>% 
  max()

# Population #
population_data <- read.xlsx("./Data/Data From ODP.xlsx", sheet = "Population") %>% 
  filter(trimws(Data.Level) == "Main") %>% 
  group_by(Indicator) %>% 
  mutate(Indicator2 = case_when(
    Indicator == "Working Age Population Forecasts" ~ paste(Indicator, "-", min(Year, na.rm = TRUE)),
    Indicator == "Dependency Ratio" ~ paste(Indicator, "-",dependencyRatioYear) ,
    TRUE ~ paste(Indicator, "-", max(Year, na.rm = TRUE))
    )) %>% 
  ungroup() %>% 
  select(-Indicator, -Scotland.Average, -Circle) %>% 
  mutate(Figure = as.numeric(Figure)) %>% 
  dplyr::rename("Indicator" = Indicator2) %>% 
  as.data.frame()

# People #
people_data <- read.xlsx("./Data/Data From ODP.xlsx", sheet = "People") %>% 
  filter(trimws(Data.Level) == "Main") %>% 
  group_by(Indicator) %>% 
  mutate(Indicator2 = paste(Indicator, "-", max(Year, na.rm = TRUE))) %>% 
  ungroup() %>% 
  select(-Indicator) %>% 
  mutate(Figure = as.numeric(Figure)) %>% 
  dplyr::rename("Indicator" = Indicator2) %>% 
  as.data.frame()

# Place #
place_data <- read.xlsx("./Data/Data From ODP.xlsx", sheet = "Place") %>% 
  filter(trimws(Data.Level) == "Main") %>% 
  group_by(Indicator) %>% 
  mutate(Indicator2 = paste(Indicator, "-", max(Year, na.rm = TRUE))) %>% 
  ungroup() %>% 
  select(-Indicator, -Scotland.Average, -Circle) %>% 
  mutate(Figure = as.numeric(Figure)) %>% 
  dplyr::rename("Indicator" = Indicator2) %>% 
  as.data.frame()

#Protected Characteristics
protectChar_data <- read.xlsx("./Data/Data From ODP.xlsx", sheet = "Protected_Characteristics") %>% 
  filter(trimws(Data.Level) == "Sub EQ") %>% 
  mutate("Category" = case_when(
    grepl("White", Measure, ignore.case = TRUE) ~ "Race", 
    grepl("Ethnic Minority", Measure, ignore.case = TRUE) ~ "Race", 
    grepl("disabled", Measure, ignore.case = TRUE) ~ "Disability", 
    grepl("female", Measure, ignore.case = TRUE) ~ "Gender",
    grepl("Male", Measure, fixed = TRUE) ~ "Gender",
    grepl("Aged", Measure, fixed = TRUE) ~ "Age"
  )) %>% 
  group_by(Indicator) %>% 
  mutate(Indicator2 = case_when(
    Indicator == "Working Age Population Forecasts" ~ paste(Indicator, "-", min(Year, na.rm = TRUE)),
    Indicator == "Dependency Ratio" ~ paste(Indicator, "-",dependencyRatioYear) ,
    TRUE ~ paste(Indicator, "-", max(Year, na.rm = TRUE))
  )) %>% 
  ungroup() %>% 
  select(-Indicator) %>% 
  mutate(Figure = as.numeric(Figure)) %>% 
  dplyr::rename("Indicator" = Indicator2) %>% 
  as.data.frame()

#Other Breakdown
otherBreak_data <- read.xlsx("./Data/Data From ODP.xlsx", sheet = "Other_Breakdowns") %>% 
  filter(trimws(Data.Level) == "Sub Other") %>% 
  group_by(Indicator) %>% 
  mutate(Indicator2 = paste(Indicator, "-", max(Year, na.rm = TRUE))) %>% 
  ungroup() %>% 
  select(-Indicator) %>% 
  dplyr::rename("Indicator" = Indicator2) %>% 
  mutate("Category" = Indicator) %>% 
  mutate(Figure = as.numeric(Figure)) %>% 
  as.data.frame()

#### Creating "Negative" measure list ####
neg_measures <- read.xlsx("./Data/Negative Measures.xlsx") 
neg_measures <- c(colnames(neg_measures)[1], neg_measures %>% unlist()) %>% sort()

#### Useful Functions ######
## Wrapping text like in Excel ##
wrap_text <- function(text,width){
  paste(strwrap(text,width),collapse = "\n")
}

## Making a "Proper" case function for R (Upper case first character lower case rest, e.g. TEST/test -> Test) ##
proper_case <- function(text) {
  if (length(text) > 0) {
    ## Splitting the Text up in case there are spaces 
    text_pieces <- strsplit(text," ", fixed = TRUE)[[1]]
    
    for (i in seq_len(length(text_pieces))) {
      if (grepl("EU",text_pieces[i],fixed = TRUE) == FALSE) {
        if (!(toupper(text_pieces[i]) %in% c("A","THE", "IS", "FOR"))) {
          text_pieces[i] <- paste0(toupper(enc2utf8(substr(text_pieces[i],1,1))),
                                   tolower(enc2utf8(substring(text_pieces[i],2))))
        } else {
          text_pieces[i] <- paste0(tolower(enc2utf8(text_pieces[i])))
        }
      }
    }
    text <- paste(text_pieces, collapse = " ")
    return(text)
  }
}

## Formatting Figures in a table according to the rules set out ##
formatting_figures <- function(data1,col_start,col_end=ncol(data1)){
    data1 <- data1 %>% as.data.frame()
    
    for (i in col_start:col_end) {
      data1[,i] <- as.numeric(data1[,i])
    }

      if (nrow(data1) > 0) {
        data1 <- data1 %>% 
          mutate_at(c(col_start:col_end),  ~case_when(
            is.na(.) ~ "-",
            toupper(Data.Type) %in% c("PERCENTAGE","PERCENTAGE (%)") ~ percent(., accuracy = 0.1, scale = 1, big.mark = ","),
            toupper(Data.Type) %in% c("RATIO", "RATE") ~ comma(., accuracy = 0.01, scale = 1),
            ((grepl("£",Data.Type,fixed = TRUE) == TRUE) &
              (grepl("weekly",Measure,fixed = TRUE) == FALSE) &
              (grepl("hourly",Measure,fixed = TRUE) == FALSE)) ~ dollar(round_any(., 5), accurary = 0, scale = 1, prefix = "£"),
            ((grepl("£",Data.Type,fixed = TRUE) == TRUE) &
               (grepl("weekly",Measure,fixed = TRUE) == TRUE) &
               (grepl("hourly",Measure,fixed = TRUE) == FALSE)) ~ dollar(., accurary = 0.1, scale = 1, prefix = "£"),
            ((grepl("£",Data.Type,fixed = TRUE) == TRUE) &
               (grepl("weekly",Measure,fixed = TRUE) == FALSE) &
               (grepl("hourly",Measure,fixed = TRUE) == TRUE)) ~ dollar(., accurary = 0.01, scale = 1, prefix = "£"),
            ((grepl("Number",Data.Type,fixed = TRUE) == TRUE) & (grepl("£",Data.Type,fixed = TRUE) == FALSE)) ~ comma(., accuracy = 1, scale = 1),
            TRUE ~ comma(., accuracy = 0, scale = 1)
          ))
      }
    
    return(data1)
}

## Ordering table according to specific order ##
order_table <- function(data){
  table <- data %>% 
    as.data.frame() %>% 
    add_column(Order = NA)
  
  if ("P" %in% colnames(table)) {
    ##Ordering Table in order of Ps, then alphabetically by measure
    table <- table %>% 
      mutate(Order = case_when(
        toupper(P) == "PRODUCTIVITY" ~ 1,
        toupper(P) == "PARTICIPATION" ~ 2,
        toupper(P) == "POPULATION" ~ 3,
        toupper(P) == "PEOPLE" ~ 4,
        toupper(P) == "PLACE" ~ 5,
        TRUE ~ 6
      ))
    
    if ("Breakdown" %in% colnames(table)) {
      table <- table %>%  
        arrange(Order,Breakdown,Measure) %>% 
        select(-Order)
    } else if ("Protected Characteristic" %in% colnames(table)) {
      table <- table %>% 
        arrange(Order,`Protected Characteristic`,Measure)  %>% 
        select(-Order)
      
    } else if ("updateFreq" %in% colnames(table)) {
      table <- table %>% 
        arrange(Order,Indicator) %>% 
        select(-Order)
      
    } else if (ncol(table) == 4) {
      table <- table %>% 
        arrange(Order,Indicator) %>% 
        select(-Order)
      
    } else { 
      table <- table %>% 
        arrange(Order,Measure) %>% 
        select(-Order)
    }
  }
  return(table)
}

## Formatting Figures for just one string element (case by case)  (not for a table)##
formatting_string <- function(string,type,measure){
  ## Getting the Figure ##
  figure <- string %>% as.numeric()
  
  ## Getting the Type of data ##
  type <- type %>% enc2utf8() %>% toupper()
  type <- gsub(enc2utf8("<A3>"),"£",type,ignore.case = TRUE)
  
  ## Getting the measure name ##
  measure <- measure %>% enc2utf8() %>% tolower()

  if ((!is.na(type)) & (!is.na(measure))) {
    figure <- case_when(
      is.na(figure) ~ "-",
      type %in% c("PERCENTAGE","PERCENTAGE (%)") ~ percent(figure, accuracy = 0.1, scale = 1, big.mark = ","),
      type %in% c("RATIO", "RATE") ~ comma(figure, accuracy = 0.01, scale = 1),
      ((grepl("£",type,fixed = TRUE) == TRUE) &
         (grepl("weekly",measure,fixed = TRUE) == FALSE) &
         (grepl("hourly",measure,fixed = TRUE) == FALSE)) ~ dollar(round_any(figure, 5), accurary = 0, scale = 1, prefix = "£"),
      ((grepl("£",type,fixed = TRUE) == TRUE) &
         (grepl("weekly",measure,fixed = TRUE) == TRUE) &
         (grepl("hourly",measure,fixed = TRUE) == FALSE)) ~ dollar(figure, accurary = 0.1, scale = 1, prefix = "£"),
      ((grepl("£",type,fixed = TRUE) == TRUE) &
         (grepl("weekly",measure,fixed = TRUE) == FALSE) &
         (grepl("hourly",measure,fixed = TRUE) == TRUE)) ~ dollar(figure, accurary = 0.01, scale = 1, prefix = "£"),
      ((grepl("NUMBER",type,fixed = TRUE) == TRUE) & (grepl("£",type,fixed = TRUE) == FALSE)) ~ comma(figure, accuracy = 1, scale = 1),
      TRUE ~ comma(figure, accuracy = 0, scale = 1)
      )
    return(figure)
  }

}

#### Normalising Figures Function (for benchmarking) ####
normalise_fig <- function(data_input,negative=FALSE){
  ##Getting the Data together for calculation
  data1 <- data_input %>% 
    as.data.frame() %>% 
    add_column(normal_value = NA)
  
  min_val <-  data1 %>% 
    pull(Figure) %>% 
    as.character() %>% 
    as.numeric() %>% 
    min(na.rm = TRUE)
  
  max_val <- data1 %>% 
    pull(Figure) %>% 
    as.character() %>% 
    as.numeric() %>% 
    max(na.rm = TRUE)
  
  if ((!is.na(min_val)) & (!is.na(max_val))) {
    for (i in seq_len(nrow(data1))) {
      normal_value <- NA
      old_value <- NA
      
      if (negative == FALSE) { ## Scenario if the measure is positive (e.g. Earnings)
        old_value <- as.numeric(data1[i,"Figure"])
        
        if (!is.na(old_value)) {
          normal_value <- (old_value - min_val)/(max_val - min_val)
        }
        
      } else {## Scenario if the measure is negative (e.g. fuel poverty)
        old_value <- as.numeric(data1[i,"Figure"])
        
        if (!is.na(old_value)) {
          normal_value <- 1 - ((old_value - min_val)/(max_val - min_val))
        }
      }
      
      if (!is.na(normal_value)) { # Replaces value if there is a figure caluclated 
        data1$normal_value[i] <- normal_value
      }
    }
    
  } 
  return(data1)
}

#### Map Data ####
## Map data for the 5Ps
#Local Authority Codes for Maps
shape <- readOGR(dsn = "./Data/Files for Maps",layer = "Scotland1")
proj4string(shape) <- CRS("+init=epsg:4326") ## Initialising Projection as Long Lat (for downloadable reports)

temp1 <- shape@data %>% 
  # Productivity #
  add_column(productivity_actual = 0) %>% 
  add_column(productivity_total = 0) %>% 
  # Participation #
  add_column(participation_actual = 0) %>% 
  add_column(participation_total = 0) %>% 
  # Population #
  add_column(population_actual = 0) %>% 
  add_column(population_total = 0) %>% 
  # People #
  add_column(people_actual = 0) %>% 
  add_column(people_total = 0) %>% 
  # Place #
  add_column(place_actual = 0) %>% 
  add_column(place_total = 0)  %>% 
  # Correcting Na h-Eileanan an Iar -> Na h-Eileanan Siar # 
  mutate(NAME = ifelse(NAME == "Na h-Eileanan an Iar", "Na h-Eileanan Siar", NAME))

for (i in seq_len(nrow(temp1))) {
  area <- temp1[i,"NAME"] %>% trimws()
  
  temp2 <- bind_rows(
    productivity_data,
    participation_data,
    population_data,
    people_data,
    place_data
  ) %>% 
    filter(toupper(Geographical.Breakdown) == toupper(area))
  
  ## Counting How many indicators are actually available for a given area ##
  temp1[i,"productivity_actual"] <- temp2 %>% 
    filter(toupper(P) == "PRODUCTIVITY") %>%
     
    filter(!is.na(Figure)) %>% 
    pull(Indicator) %>% 
    unique() %>% 
    length()
  
  temp1[i,"participation_actual"] <-  temp2 %>% 
    filter(toupper(P) == "PARTICIPATION") %>% 
     
    filter(!is.na(Figure)) %>% 
    pull(Indicator) %>% 
    unique() %>% 
    length()
  
  temp1[i,"population_actual"] <-  temp2 %>% 
    filter(toupper(P) == "POPULATION") %>% 
     
    filter(!is.na(Figure)) %>% 
    pull(Indicator) %>% 
    unique() %>% 
    length()
  
  temp1[i,"people_actual"] <-  temp2 %>% 
    filter(toupper(P) == "PEOPLE") %>% 
     
    filter(!is.na(Figure)) %>% 
    pull(Indicator) %>% 
    unique() %>% 
    length()
  
  temp1[i,"place_actual"] <-  temp2 %>% 
    filter(toupper(P) == "PLACE") %>% 
     
    filter(!is.na(Figure)) %>% 
    pull(Indicator) %>% 
    unique() %>% 
    length()
  
  ## Counting How many indciators are available in total ##
  temp1[i,"productivity_total"] <-  productivity_data %>% 
    pull(Indicator) %>% 
    unique() %>% 
    length()
  
  temp1[i,"participation_total"] <-  participation_data %>%  
    pull(Indicator) %>% 
    unique() %>% 
    length()
  
  temp1[i,"population_total"] <- population_data %>%  
    pull(Indicator) %>% 
    unique() %>% 
    length()
  
  temp1[i,"people_total"] <-  people_data %>% 
    pull(Indicator) %>% 
    unique() %>% 
    length()
  
  temp1[i,"place_total"] <-  place_data %>% 
    pull(Indicator) %>% 
    unique() %>% 
    length()
  
}
shape@data <- temp1

codes <- read_csv("Data/Files for Maps/LA_codes for maps.csv",locale = locale(encoding = "UTF-8")) %>% as.data.frame()

#### Creating List of Indicators which are also in Protected Characteristics ####
protectChar_list <- protectChar_data %>%
  pull(Indicator) %>% 
  unique() %>% 
  sort()

#### Creating list of indicators which are also in Other Breakdowns ####
otherBreak_list <- otherBreak_data %>%  
  pull(Indicator) %>% 
  unique() %>% 
  sort()

#### Working in the metadata ####
meta_data <- read.xlsx("./Data/Data From ODP.xlsx", sheet = "meta_data", colNames = TRUE,na.strings = c("#","x","")) %>% 
  mutate(DateModified = anydate(DateModified) %>% format("%d %B %Y")) %>% 
  distinct()




rm(temp1, temp2, area, dependencyRatioYear, i)