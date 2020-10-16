#### Enterprise and Skills Dashboard Global File ####
#### Neccessary Libraries ####
library(scales)             ##For easier formatting of figures
library(shinydashboard)     ##For Dashboard template
library(shiny)              ##For App
library(plyr)               ##For rounding
library(tidyverse)          ##For installing multiple useful libraries at once (particularly tidyr, ggplot2, dplyr amongst others)
library(shinythemes)        ##For themes
library(shinyjs)            ##For more functions (javascript)
library(DT)                 ##For Datatables
library(stringr)            ##For string case changes
library(shinyjs)            ##For javascript elements in app
library(ggrepel)            ##For text repel for graphs
library(openxlsx)           ##For reading excel files (not reliant on Java)
library(shinycssloaders)    ##For the Spinners
library(plotly)             ##For interactive graphs
library(shinyEffects)       ##For added effects, like zooming in on buttons on the home page
library(V8)
library(scales)
library(shinyWidgets)
library(ggiraph)

options(stringsAsFactors = FALSE,  scipen = 999)
####---------------------------------------------------------------------####
#### Reading in Measures Information (for graph labelling) ####
label_info <- read.xlsx("./Data/measureLimits.xlsx",  sheet = 1,  colNames = TRUE) %>% 
  mutate_if(is.character, trimws)

colnames(label_info) <- gsub(".", " ", colnames(label_info), fixed = TRUE)

#### Reading in Performance Changes ####
performanceChange <- read.xlsx("./Data/Indicator Performance Changes.xlsx",na.strings = "NA")
performanceChange[is.na(performanceChange$Comparator.Rank),"Comparator.Rank"]  <- ""

#### Reading in all data ####-----------------------------------------------####
#### Reading in NPF data ####
npf_data <-  read.xlsx("./Data/AU_NPF_261119- Mental Wellbeing, Pay Gap, Exports (SCRIG) and Living Wage (SCRIG) added.xlsx", na.strings = c("NA", "x", "n/a", "*")) %>% 
  select(Year, Figure, Breakdown, Dissaggregation, Indicator, Outcome, Measure, Source, Yearlab) %>% 
  rename("Disaggregation" = Dissaggregation) %>% 
  filter(Indicator %in% performanceChange$Indicator) %>% 
  mutate(Year = case_when(
    ((nchar(Year) > 4) & (grepl("-", Year, fixed = TRUE) == FALSE)) ~ as.Date(as.numeric(Year), origin = "1899-12-30") %>% format(format = "%d %m %Y") %>% as.character(),
    TRUE ~ Year %>% as.character() %>% trimws()
  )) %>%
  mutate(Yearlab = str_sub(Year, -4)) %>% 
  mutate(Source = Source %>% as.character() %>% trimws())  %>% 
  mutate(Figure = case_when(
    Indicator == "Greenhouse gas emissions" ~ Figure *100, 
    TRUE ~ Figure
  ))

npf_data$Disaggregation <- gsub("Local authority", "Local Authority", npf_data$Disaggregation, fixed = TRUE)
npf_data$Disaggregation <- gsub("Industry Sector", "Sector", npf_data$Disaggregation, fixed = TRUE)

##Adding Data type column to npf data for easier formatting
short_list <- label_info %>% 
  select(Indicator, dataType) %>% 
  distinct()

npf_data <- left_join(npf_data, short_list) %>% 
  mutate(Breakdown == case_when(
    Disaggregation == "Scotland" ~ "Scotland",
    TRUE ~ Breakdown
  ))

npf_data$Indicator <- gsub("International Exporting", "International exporting", npf_data$Indicator, fixed = TRUE)
npf_data$Indicator <- gsub("Employees on the Living Wage", "Employees on the Living wage", npf_data$Indicator, fixed = TRUE)

#### Reading in International Data ####
## GERD - for "Spend on Research and Development" (Economy:Productivity)##
GERD_data <- read.xlsx("./Data/GERD OECD data 2016.xlsx",  sheet = 1,  colNames = TRUE,  na.strings  = c("NA", "x"))

## TEA Rates for "Entrepreneurial Activity" (Economy:Productivity) ##
TEA_data <- read.xlsx("./Data/TEA OECD countries 2018 and 2017.xlsx",  sheet = 1,  colNames = TRUE,  na.strings = c("NA", "x"))

## Productivity Levels data 2017 for "Productivity"##
productivity_data <- read.xlsx("./Data/Economy indicator - Productivity - Feb 19 - Data - revised.xlsx",  sheet = 1,  colNames = TRUE, na.strings = c("NA", "x")) 
productivity_data$Country <- gsub("*", "", productivity_data$Country, fixed = TRUE)

##Equalities list to check within data making
equalities_list <- c("Age", "Disability", "Ethnicity", "Gender", "Religion", "Scottish Index of Multiple Deprivation", "Urban Rural classification")

#### Preview Graphs ####------------------------------------------------####
#### Making the Smaller Graphs that appear in the tables ####
indicator_list <- npf_data %>% pull(Indicator) %>% as.character() %>% unique() %>% sort()

for (i in seq_along(indicator_list)) {
  ##Narrowing Down data
  data_graph <- npf_data %>% 
    mutate(Figure = as.numeric(Figure)) %>% 
    filter(toupper(Indicator) == toupper(indicator_list[i])) %>% 
    #filter(toupper(Breakdown) == "TOTAL") %>% 
    filter(toupper(Disaggregation) == "SCOTLAND") %>% 
    select(-Disaggregation)
  
  image_file <- indicator_list[i]
  image_file <- gsub("'", "", image_file, fixed = TRUE)
  
  ## Getting Upper and Lower Limits 
  low_lim <- label_info %>% 
    filter(toupper(Indicator) == toupper(indicator_list[i])) %>% 
    pull(`Lower Lim`) %>% 
    unique() %>% 
    as.numeric()
  
  high_lim <- label_info %>% 
    filter(toupper(Indicator) == toupper(indicator_list[i])) %>% 
    pull(`Higher Lim`) %>% 
    unique() %>% 
    as.numeric()
  
  if ( (length(low_lim) > 0) & (length(high_lim) > 0)) {
    if ( (is.na(low_lim)) | (is.na(high_lim))) {
      low_lim <- 0
      high_lim <- 100
    }
  }
  
  if (nrow(data_graph) > 0) {
    ## Creating Plot   
    ggplot(data = data_graph, aes(x = Year, y = Figure, group = Breakdown)) +
      geom_line(size = 2) +
      geom_point() +
      scale_y_continuous(limits = c(low_lim, high_lim)) + 
      labs(x = "", y = "") +
      theme(axis.text.x = element_blank(),
            axis.line = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_rect(fill = "white", colour = "white")
      )
    ggsave(paste("./www/", image_file, ".png", sep = ""),dpi = 10) 
  }
  rm(low_lim, high_lim, data_graph, image_file)
}

#### Functions ####-------------------------------------------------------####
#### Useful Functions ######
## Making a "Proper" case function for R (Upper case first character,  lower case rest)
proper_case <- function(text) {
  text <- paste0( (substr(text, 1, 1) %>% enc2utf8() %>% toupper()),  (substring(text, 2) %>% enc2utf8() %>% tolower()))
  return(text)
}

## Setting Formatting Rules for any tables/graphs ##
formattingFigures <- function(dataset, colnames2Format) {
  temp1 <- label_info %>% 
    select(Indicator, dataType) %>% 
    distinct()
  
  dataset1 <- left_join(dataset, temp1)
  
  dataset1 <- dataset1 %>% 
    mutate_at(colnames2Format, function(x) {
      case_when(
        is.na(x) ~ "-", 
        dataset1$dataType == "£ MILLION" ~ dollar(x, accuracy = 1, scale = 1, prefix = "£", suffix = " million"),
        dataset1$dataType == "INDEX"  ~ number(x, accuracy = 0.1, scale = 1, big.mark = ","),
        dataset1$dataType == "MILLION TONNES" ~ number(x, accuracy = 0.1, scale = 1, big.mark = ",", suffix = " million tonnes"),
        dataset1$dataType == "PERCENTAGE  SMALL(%)" ~ percent(x, accuracy = 0.01, scale = 1),
        dataset1$dataType == "PERCENTAGE (%)" ~ percent(x, accuracy = 0.1, scale = 1),
        dataset1$dataType == "PERCENTAGE POINTS (%)" ~ percent(abs(x), accuracy = 0.1, scale = 1, suffix = "pp", prefix = case_when(
          x < 0 ~ "-",
          x == 0 ~ "<=>",
          x > 0 ~ "+"
        )),
        dataset1$dataType == "PERCENTAGE POINTS SMALL(%)" ~ percent(x, accuracy = 0.01, scale = 1, suffix = "pp"),
        dataset1$dataType == "PERCENTAGE WHOLE (%)" ~ percent(x, accuracy = 1, scale = 1),
        dataset1$dataType == "RANK" ~ number(x, accuracy = 1, scale = 1, big.mark = ",", suffix = case_when(
          str_sub(x, -2) == 11 ~ "th",
          str_sub(x, -2) == 12 ~ "th",
          str_sub(x, -2) == 13 ~ "th",
          str_sub(x, -1) == 1 ~ "st",
          str_sub(x, -1) == 2 ~ "nd",
          str_sub(x, -1) == 3 ~ "rd",
          TRUE ~ "th"
        )),
        dataset1$dataType == "SCORE" ~ number(x, accuracy = 0.1, scale = 1, big.mark = ","),
        dataset1$dataType == "VALUE" ~ number(x, accuracy = 1, scale = 1, big.mark = ",")
      )
    }) %>% 
    select(colnames(dataset))
  
  return(dataset1)
}

## Get Changes formatted to see whether a change has occured ##
formattingChanges <- function(dataset) {
  dataset1 <- left_join(dataset, performanceChange)

  dataset1 <- dataset1 %>% 
    mutate(`Longer Term Trend` = case_when(
      `Long.Term.Performance` == "Improved" ~  "<img src = 'Good.png' height = '50'/>", 
      `Long.Term.Performance` == "No Change" ~  "<img src = 'No Change.png' height = '50'/>", 
      `Long.Term.Performance` == "Worsened" ~ "<img src = 'Bad.png' height = '50'/>",
      TRUE ~ ""
    )) %>% 
    mutate(DifferenceShort = case_when(
      `Short.Term.Performance` == "Improved" ~ 1, 
      `Short.Term.Performance` == "No Change" ~ 0,
      `Short.Term.Performance` == "Worsened" ~ -1
    )) %>%
    mutate(`OECD/UK Comparison` = case_when(
      ### OECD Comparators ###
      ((Wider.Performance == "Better") & (Comparator.Type == "OECD")) ~ paste0(
        "<div class = 'container'>",
        "<img src = 'Good.png' height = '50'/>", 
        "<div class = 'centered'>", `Comparator.Rank` %>% trimws(), "</div>",
        "</div>"
      ),
      ((Wider.Performance == "No difference") & (Comparator.Type == "OECD")) ~ paste0(
        "<div class = 'container'>",
        "<img src = 'No Change.png' height = '50'/>", 
        "<div class = 'centered'>", `Comparator.Rank` %>% trimws(), "</div>",
        "</div>"
      ), 
      ((Wider.Performance == "Worse") & (Comparator.Type == "OECD")) ~ paste0(
        "<div class = 'container'>",
        "<img src = 'Bad.png' height = '50'/>", 
        "<div class = 'centered'>", `Comparator.Rank` %>% trimws(), "</div>",
        "</div>"
      ),
      
      ### UK Comparators ###
      ((Wider.Performance == "Better") & (Comparator.Type == "UK")) ~paste0(
        "<div class = 'container'>",
        "<img src = 'Good UK.png' height = '50'/>", 
        "<div class = 'centered'>", `Comparator.Rank` %>% trimws(), "</div>",
        "</div>"
      ),
      ((Wider.Performance == "No difference") & (Comparator.Type == "UK")) ~ paste0(
        "<div class = 'container'>",
        "<img src = 'No Change UK.png' height = '50'/>", 
        "<div class = 'centered'>", `Comparator.Rank` %>% trimws(), "</div>",
        "</div>"
      ),
      ((Wider.Performance == "Worse") & (Comparator.Type == "UK")) ~ paste0(
        "<div class = 'container'>",
        "<img src = 'Bad UK.png' height = '50'/>", 
        "<div class = 'centered'>", `Comparator.Rank` %>% trimws(), "</div>",
        "</div>"
      ),
      TRUE ~ ""
    )) %>% 
    select(colnames(dataset), `Longer Term Trend`,DifferenceShort,`OECD/UK Comparison`)
  return(dataset1)
}
#print(unique(sort(performanceChange$Wider.Performance)))
rm(i)

#### Defining Indicators ####-------------------------------------------------####
#### Defining Indicators for each of the PEWS ####
productivity_indicators <- c(
  "Productivity",
  "Economic growth",
  "International exporting",
  "Spend on Research and Development",
  "High growth businesses",
  "The number of businesses",
  "Scotland's Reputation",
  "Skills shortage vacancies",
  "Young people's participation",
  "Economic participation",
  "Educational attainment 7",
  "Access to superfast broadband",
  "Entrepreneurial activity",
  "Work place learning",
  "Innovative businesses",
  "Skills underutilisation",
  "Skill profile of the population" 
)  %>% 
  sort()

equality_indicators <- c(
  "Income inequality",
  "Pay gap",
  "Gender balance in organisations"
) %>% sort()

wellbeing_indicators <- c(
  "Social capital",
  "Employees on the Living wage",
  "Employee voice",
  "Mental wellbeing"
) %>% sort()

sustainability_indicators <- c(
  "Carbon footprint",
  "Natural capital",
  "Greenhouse gas emissions",
  "Energy from renewable sources"
) %>% sort()

#### Sourcing Module Pages ####
source("./Modules/Enterprise and Skills Second Page Module.R", encoding = "UTF-8")
source("./Modules/Enterprise and Skills Third Page Module.R", encoding = "UTF-8")



#### KAMIL ##############


# constants ####
St <- 2000     # Starting year
Yr <- 2018     # Current year

current_year <- 2018
comparison_countries <- c('Scotland', 'Scotland (international and RUK)', 'Scotland (international)', 'Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'New Zealand', 'Iceland', 'UK', 'OECD - Total', 'OECD members')
comparison_countries_eu <- c('Scotland', 'Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Iceland', 'UK', 'EU')

# libraries ####
library(shiny)                # For the app iteslf.
library(shinythemes)          # For the "cerulean" theme.
library(shinyhelper)          # Used for help modal boxes.
library(shinyEffects)         # Used for home page effects.
library(shinyanimate)         # Used for home page effects.
library(shinycssloaders)      # For loaders.
library(magrittr)             # Dependency of other packages. Lets use the pipe operator (%>%).
library(data.table)           # Needed for function na.omit().
library(DT)                   # For the interactive tables.
library(dygraphs)             # For the interactive graphs.
library(leaflet)              # For interactive maps.
library(RColorBrewer)         # Used to create a colour palette for the map.
library(rgdal)                # Needed to load shapfile for the map.
library(plyr)                 # Needed for function revalue(), for editing country names.
library(rmarkdown)            # Used for reports, especially important is function render().
library(knitr)                # Takes Rmd. file and turns it into .md document which includes the R code and it’s output.
library(networkD3)            # Used to create the sankey diagram.
library(treemap)              # Used to create the static treemap.
library(dplyr)                # Used for data manipulation.
library(ggplot2)              # Used for plots in country and sector profiles.
library(plotly)               # Used for the streamgraph.
library(ggsci)                # Used for colour palettes.
# library(ggflags)              # Expands ggplot with new geom for adding flags.
library(countrycode)          # Enables conversion from Common country names to ISO codes.
library(knitr)                # Used for sector definitions table
library(kableExtra)           # Used for styling the sector definitions table
library(rsconnect)            # Used for shinyapp.io uploads
library(tidyr)                # Used for data manipulation
library(openxlsx)             # Used to read excel spreadsheets
library(plotly)               # Used for plots

# Dependencies ####
library(DBI)
library(XML)
library(anytime)
library(backports)
library(broom)
library(cellranger)
library(classInt)
library(cowplot)
library(dbplyr)
library(dichromat)
library(flextable)
library(forcats)
library(fs)
library(gdtools)
library(gtools)
library(haven)
library(leafsync)
library(lubridate)
library(lwgeom)
library(modelr)
library(prettyunits)
library(progress)
library(readxl)
library(reprex)
library(rgeos)
library(rintrojs)
library(rlist)
library(sf)
library(shinyBS)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sunburstR)
library(viridis)

# LOADING DATA ####
# Home page data and manipulation ####
# circular_barplot_home_ggplot
sunburst_home_e <- read.csv("./www/home/sunburst_home_e.csv", stringsAsFactors = FALSE)
sunburst_home_e$individual <- as.factor(sunburst_home_e$individual)
sunburst_home_e$group <- as.factor(sunburst_home_e$group)
data <- sunburst_home_e
# Transform data in a tidy format (long format)
data <- data %>% gather(key = "observation", value="value", -c(1,2))

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>%
  group_by(group) %>%
  summarize(start=min(id), end=max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
# grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
# grid_data$start <- grid_data$start - 1
# grid_data <- grid_data[-1,]

# LOADING DATA - INTERNATIONAL ####
growth_int <- read.csv("./www/international/growth_int.csv", check.names=FALSE)
productivity_int <- read.csv("./www/international/productivity_int.csv", check.names=FALSE)
exporting_int <- read.csv("./www/international/exporting_int.csv", check.names=FALSE)
rd_int <- read.csv("./www/international/rd_int.csv", check.names=FALSE)
entrepreneurialism_int <- read.csv("./www/international/entrepreneurialism_int.csv", check.names=FALSE)
entrepreneurialism2_int <- read.csv("./www/international/entrepreneurialism2_int.csv", check.names=FALSE)
depratio_int <- read.csv("./www/international/depratio_int.csv", check.names=FALSE)
migration_int <- read.csv("./www/international/migration_int.csv", check.names=FALSE)
eactivity_int <- read.csv("./www/international/eactivity_int.csv", check.names=FALSE)
employment_int <- read.csv("./www/international/employment_int.csv", check.names=FALSE)
unemployment_int <- read.csv("./www/international/unemployment_int.csv", check.names=FALSE)
genderpaygap_int <- read.csv("./www/international/genderpaygap_int.csv", check.names=FALSE)
genderpaygap2_int <- read.csv("./www/international/genderpaygap2_int.csv", check.names=FALSE)
skillsunderprimary_int <- read.csv("./www/international/skillsunderprimary_int.csv", check.names=FALSE)
skillstertiary_int <- read.csv("./www/international/skillstertiary_int.csv", check.names=FALSE)
yunemployment_int <- read.csv("./www/international/yunemployment_int.csv", check.names=FALSE)
evoice_int <- read.csv("./www/international/evoice_int.csv", check.names=FALSE)
evoice2_int <- read.csv("./www/international/evoice2_int.csv", check.names=FALSE)
lifeexpall_int <- read.csv("./www/international/lifeexpall_int.csv", check.names=FALSE)
lifeexpf_int <- read.csv("./www/international/lifeexpf_int.csv", check.names=FALSE)
broadband_int <- read.csv("./www/international/broadband_int.csv", check.names=FALSE)
emissions_int <- read.csv("./www/international/emissions_int.csv", check.names=FALSE)
ggemissions_int <- read.csv("./www/international/ggemissions_int.csv", check.names=FALSE)
ggemissions2_int <- read.csv("./www/international/ggemissions2_int.csv", check.names=FALSE)


# LOADING DATA - SCOTLAND ####
growth_sco <- read.csv("./www/scotland/growth_sco.csv", check.names=FALSE)
productivity_sco <- read.csv("./www/scotland/productivity_sco.csv", check.names=FALSE)
exporting_sco <- read.csv("./www/scotland/exporting_sco.csv", check.names=FALSE)
exporting_destination_sco <- read.csv("./www/scotland/exporting_destination_sco.csv", check.names=FALSE)
exporting_sector_sco <- read.csv("./www/scotland/exporting_sector_sco.csv", check.names=FALSE)
rd_sco <- read.csv("./www/scotland/rd_sco.csv", check.names=FALSE)
entrepreneurialism_sco <- read.csv("./www/scotland/entrepreneurialism_sco.csv", check.names=FALSE)
reputation_sco <- read.csv("./www/scotland/reputation_sco.csv", check.names=FALSE)
nbusiness_sector_sco <- read.csv("./www/scotland/nbusiness_sector_sco.csv", check.names=FALSE)
nbusiness_region_sco <- read.csv("./www/scotland/nbusiness_region_sco.csv", check.names=FALSE)
nbusiness_employees_sco <- read.csv("./www/scotland/nbusiness_employees_sco.csv", check.names=FALSE)
hbusiness_percent_sco <- read.csv("./www/scotland/hbusiness_percent_sco.csv", check.names=FALSE)
ibusiness_sco <- read.csv("./www/scotland/ibusiness_sco.csv", check.names=FALSE)
rd_overview_sco <- read.csv("./www/scotland/rd_overview_sco.csv", check.names=FALSE)
depratio_sco <- read.csv("./www/scotland/depratio_sco.csv", check.names=FALSE)
migration_in_overseas_age_sco <- read.csv("./www/scotland/migration_in_overseas_age_sco.csv", check.names=FALSE)
migration_in_overseas_sex_sco <- read.csv("./www/scotland/migration_in_overseas_sex_sco.csv", check.names=FALSE)
migration_in_source_sco <- read.csv("./www/scotland/migration_in_source_sco.csv", check.names=FALSE)
migration_type_overseas_sco <- read.csv("./www/scotland/migration_type_overseas_sco.csv", check.names=FALSE)

eactivity_sco <- read.csv("./www/scotland/eactivity_sco.csv", check.names=FALSE)
livingwage_sco <- read.csv("./www/scotland/livingwage_sco.csv", check.names=FALSE)
genderpaygap_sco <- read.csv("./www/scotland/genderpaygap_sco.csv", check.names=FALSE)
evoice_sco <- read.csv("./www/scotland/evoice_sco.csv", check.names=FALSE)
wplearning_sco <- read.csv("./www/scotland/wplearning_sco.csv", check.names=FALSE)
youngpplpart_sco <- read.csv("./www/scotland/youngpplpart_sco.csv", check.names=FALSE)
skillshortage_sco <- read.csv("./www/scotland/skillshortage_sco.csv", check.names=FALSE)

rphousingc_sco <- read.csv("./www/scotland/rphousingc_sco.csv", check.names=FALSE)
hlifeexp_male_sco <- read.csv("./www/scotland/hlifeexp_male_sco.csv", check.names=FALSE)
hlifeexp_female_sco <- read.csv("./www/scotland/hlifeexp_female_sco.csv", check.names=FALSE)
mwellbeing_sco <- read.csv("./www/scotland/mwellbeing_sco.csv", check.names=FALSE)


greenandbluespace_sco <- read.csv("./www/scotland/greenandbluespace_sco.csv", check.names=FALSE)


cfootprint_sco <- read.csv("./www/scotland/cfootprint_sco.csv", check.names=FALSE)
naturalf_sco <- read.csv("./www/scotland/naturalf_sco.csv", check.names=FALSE)
gasemissions_sco <- read.csv("./www/scotland/gasemissions_sco.csv", check.names=FALSE)


# LOADING DATA - REGIONAL ####
nbusiness_reg <- read.csv("./www/regional/nbusiness_reg.csv", check.names=FALSE)
growth_reg <- read.csv("./www/regional/growth_reg.csv", check.names=FALSE)
exporting_reg <- read.csv("./www/regional/exporting_reg.csv", check.names=FALSE)
rd_reg <- read.csv("./www/regional/rd_reg.csv", check.names=FALSE)
depratio_reg <- read.csv("./www/regional/depratio_reg.csv", check.names=FALSE)
migration_reg <- read.csv("./www/regional/migration_reg.csv", check.names=FALSE)
working_age_reg <- read.csv("./www/regional/working_age_reg.csv", check.names=FALSE)
eactivity_reg <- read.csv("./www/regional/eactivity_reg.csv", check.names=FALSE)
lwage_reg <- read.csv("./www/regional/lwage_reg.csv", check.names=FALSE)
gpaygap_reg <- read.csv("./www/regional/gpaygap_reg.csv", check.names=FALSE)
hlifeexp_male_reg <- read.csv("./www/regional/hlifeexp_male_reg.csv", check.names=FALSE)
hlifeexp_female_reg <- read.csv("./www/regional/hlifeexp_female_reg.csv", check.names=FALSE)
wplearning_reg <- read.csv("./www/regional/wplearning_reg.csv", check.names=FALSE)
blueorgreen_reg <- read.csv("./www/regional/blueorgreen_reg.csv", check.names=FALSE)
pubservsat_reg <- read.csv("./www/regional/pubservsat_reg.csv", check.names=FALSE)
broadband_reg <- read.csv("./www/regional/broadband_reg.csv", check.names=FALSE)
# LOADING DATA - EQUALITIES DASHBOARD ####
gpaygap_eq <- read.csv("./www/equalities/gpaygap_eq.csv", check.names=FALSE)
dempgap_eq <- read.csv("./www/equalities/dempgap_eq.csv", check.names=FALSE)
youthunemp_eq <- read.csv("./www/equalities/youthunemp_eq.csv", check.names=FALSE)
ethnicmgap_eq <- read.csv("./www/equalities/ethnicmgap_eq.csv", check.names=FALSE)
socioecon_eq <- read.csv("./www/equalities/socioecon_eq.csv", check.names=FALSE)
religion_eq <- read.csv("./www/equalities/religion_eq.csv", check.names=FALSE)
orientation_eq <- read.csv("./www/equalities/orientation_eq.csv", check.names=FALSE)


# HOME PAGE ############################################################################################################################################


# PRODUCTIVITY #########################################################################################################################################
# growth_int
growth_int_long <- growth_int %>% 
  gather("Year", "Value", 2:ncol(growth_int))
growth_int_wide <- growth_int_long %>% 
  spread("Country", "Value", 2:3)
growth_int_wide$Year <- as.integer(growth_int_wide$Year)
positions_selected_countries_growth_int <- which(names(growth_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'UK', 'Scotland'))
start_year_growth_int <- min(growth_int_long$Year)
end_year_growth_int <- max(growth_int_long$Year)

# productivity_int
productivity_int_long <- productivity_int %>% 
  gather("Year", "Value", 2:ncol(productivity_int))
productivity_int_wide <- productivity_int_long %>% 
  spread("Country", "Value", 2:3)
productivity_int_wide$Year <- as.integer(productivity_int_wide$Year)
positions_selected_countries_productivity_int <- which(names(productivity_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK'))
start_year_productivity_int <- min(productivity_int_long$Year)
end_year_productivity_int <- max(productivity_int_long$Year)

# exporting_int
exporting_int_long <- exporting_int %>% 
  gather("Year", "Value", 2:ncol(exporting_int))
exporting_int_wide <- exporting_int_long %>% 
  spread("Country", "Value", 2:3)
exporting_int_wide$Year <- as.integer(exporting_int_wide$Year)
positions_selected_countries_exporting_int <- which(names(exporting_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK'))
start_year_exporting_int <- min(exporting_int_long$Year)
end_year_exporting_int <- max(exporting_int_long$Year)

# rd_int
rd_int_long <- rd_int %>% 
  gather("Year", "Value", 2:ncol(rd_int))
rd_int_wide <- rd_int_long %>% 
  spread("Country", "Value", 2:3)
rd_int_wide$Year <- as.integer(rd_int_wide$Year)
positions_selected_countries_rd_int <- which(names(rd_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK'))
start_year_rd_int <- min(rd_int_long$Year)
end_year_rd_int <- max(rd_int_long$Year)

# entrepreneurialism_int
entrepreneurialism_int_long <- entrepreneurialism_int %>% 
  gather("Year", "Value", 2:ncol(entrepreneurialism_int))
entrepreneurialism_int_wide <- entrepreneurialism_int_long %>% 
  spread("Country", "Value", 2:3)
entrepreneurialism_int_wide$Year <- as.integer(entrepreneurialism_int_wide$Year)
positions_selected_countries_entrepreneurialism_int <- which(names(entrepreneurialism_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'United Kingdom'))
start_year_entrepreneurialism_int <- min(entrepreneurialism_int_long$Year)
end_year_entrepreneurialism_int <- max(entrepreneurialism_int_long$Year)

# growth_sco
growth_sco_long <- growth_sco
growth_sco_wide <- growth_sco_long %>% 
  spread("Sector", "Value", 2:3)
growth_sco_wide$Year <- as.integer(growth_sco_wide$Year)
positions_selected_sectors_growth_sco <- which(names(growth_sco_wide) %in% c('Total'))
start_year_growth_sco <- min(growth_sco_long$Year)
end_year_growth_sco <- max(growth_sco_long$Year)

# productivity_sco
start_year_productivity_sco <- min(productivity_sco$Year)
end_year_productivity_sco <- max(productivity_sco$Year)

# exporting_destination_sco
exporting_destination_sco <- as.data.frame.matrix(xtabs(Value ~ Year + Destination, exporting_destination_sco))
exporting_destination_sco <- cbind(Year = rownames(exporting_destination_sco), exporting_destination_sco)
exporting_destination_sco$Year <- as.numeric(levels(exporting_destination_sco$Year))[exporting_destination_sco$Year] # New way to convert year factor into numeric year
positions_selected_exporting_destination_sco <- which(names(exporting_destination_sco) %in% c('EU', 'International', 'Non-EU', 'Rest of UK'))
start_year_exporting_destination_sco <- min(exporting_destination_sco$Year)
end_year_exporting_destination_sco <- max(exporting_destination_sco$Year)

# exporting_sector_sco
exporting_sector_sco <- as.data.frame.matrix(xtabs(Value ~ Year + Sector, exporting_sector_sco))
exporting_sector_sco <- cbind(Year = rownames(exporting_sector_sco), exporting_sector_sco)
exporting_sector_sco$Year <- as.numeric(levels(exporting_sector_sco$Year))[exporting_sector_sco$Year]
positions_selected_exporting_sector_sco <- which(names(exporting_sector_sco) %in% c('Total Manufacturing', 'Total Services', 'Agriculture, Forestry and Fishing'))
start_year_exporting_sector_sco <- min(exporting_sector_sco$Year)
end_year_exporting_sector_sco <- max(exporting_sector_sco$Year)

# rd_sco
rd_sco <- as.data.frame.matrix(xtabs(Value ~ Year + Performer, rd_sco))
rd_sco <- cbind(Year = rownames(rd_sco), rd_sco)
rd_sco$Year <- as.numeric(levels(rd_sco$Year))[rd_sco$Year]
positions_selected_performers_rd_sco <- which(names(rd_sco) %in% c('Business Enterprise', 'Government incl. Research Councils', 'Gross Expenditure on Research and Development', 'Higher Education', 'Private Non-Profit'))
start_year_rd_sco <- min(rd_sco$Year)
end_year_rd_sco <- max(rd_sco$Year)

# entrepreneurialism_sco
start_year_entrepreneurialism_sco <- min(entrepreneurialism_sco$Year)
end_year_entrepreneurialism_sco <- max(entrepreneurialism_sco$Year)
entrepreneurialism_sco$Year <- as.integer(entrepreneurialism_sco$Year)

# reputation_sco
start_year_reputation_sco <- min(reputation_sco$Year)
end_year_reputation_sco <- max(reputation_sco$Year)

# nbusiness_sector_sco
nbusiness_sector_sco <- as.data.frame.matrix(xtabs(Value ~ Year + Sector, nbusiness_sector_sco))
nbusiness_sector_sco <- cbind(Year = rownames(nbusiness_sector_sco), nbusiness_sector_sco)
nbusiness_sector_sco$Year <- as.numeric(levels(nbusiness_sector_sco$Year))[nbusiness_sector_sco$Year]
positions_selected_sectors_nbusiness_sector_sco <- which(names(nbusiness_sector_sco) %in% c('All'))
start_year_nbusiness_sector_sco <- min(nbusiness_sector_sco$Year)
end_year_nbusiness_sector_sco <- max(nbusiness_sector_sco$Year)

# nbusiness_region_sco
nbusiness_region_sco <- as.data.frame.matrix(xtabs(Value ~ Year + Region, nbusiness_region_sco))
nbusiness_region_sco <- cbind(Year = rownames(nbusiness_region_sco), nbusiness_region_sco)
nbusiness_region_sco$Year <- as.numeric(levels(nbusiness_region_sco$Year))[nbusiness_region_sco$Year]
positions_selected_regions_nbusiness_region_sco <- which(names(nbusiness_region_sco) %in% c('All foreign-owned'))
start_year_nbusiness_region_sco <- min(nbusiness_region_sco$Year)
end_year_nbusiness_region_sco <- max(nbusiness_region_sco$Year)

# nbusiness_employees_sco
nbusiness_employees_sco <- as.data.frame.matrix(xtabs(Value ~ Year + Employees, nbusiness_employees_sco))
nbusiness_employees_sco <- cbind(Year = rownames(nbusiness_employees_sco), nbusiness_employees_sco)
nbusiness_employees_sco$Year <- as.numeric(levels(nbusiness_employees_sco$Year))[nbusiness_employees_sco$Year]
positions_selected_employees_nbusiness_employees_sco <- which(names(nbusiness_employees_sco) %in% c('All employees'))
start_year_nbusiness_employees_sco <- min(nbusiness_employees_sco$Year)
end_year_nbusiness_employees_sco <- max(nbusiness_employees_sco$Year)

# hbusiness_percent_sco
start_year_hbusiness_percent_sco <- min(hbusiness_percent_sco$Year)
end_year_hbusiness_percent_sco <- max(hbusiness_percent_sco$Year)

# ibusiness_sco
ibusiness_sco <- as.data.frame.matrix(xtabs(Value ~ Year + Activity, ibusiness_sco))
ibusiness_sco <- cbind(Year = rownames(ibusiness_sco), ibusiness_sco)
ibusiness_sco$Year <- as.numeric(levels(ibusiness_sco$Year))[ibusiness_sco$Year]
positions_selected_activities_ibusiness_sco <- which(names(ibusiness_sco) %in% c('Innovation active'))
start_year_ibusiness_sco <- min(ibusiness_sco$Year)
end_year_ibusiness_sco <- max(ibusiness_sco$Year)

# map data
mapex <- readOGR(dsn="./www/regional/fwlibrariesandshapefile", layer="Scotland1")

# growth_reg
start_year_growth_reg <- min(growth_reg$Year)
end_year_growth_reg <- max(growth_reg$Year)

# exporting_reg
start_year_exporting_reg <- min(exporting_reg$Year)
end_year_exporting_reg <- max(exporting_reg$Year)

# rd_reg
start_year_rd_reg <- min(rd_reg$Year)
end_year_rd_reg <- max(rd_reg$Year)

# nbusiness_reg
start_year_nbusiness_reg <- min(nbusiness_reg$Year)
end_year_nbusiness_reg <- max(nbusiness_reg$Year)

# growth_overview_int
growth_overview_int <- subset(growth_int_long, growth_int_long$Country %in% comparison_countries)
growth_overview_int <- subset(growth_overview_int, growth_overview_int$Year == max(growth_overview_int$Year))
growth_overview_int <- growth_overview_int[,c(1,3)]
growth_overview_int <- growth_overview_int[order(growth_overview_int$Value, decreasing = FALSE),]
growth_overview_int$Country <- factor(growth_overview_int$Country, levels = growth_overview_int$Country)

# text for growth_overview_int
text_scotland_thisyear_growth_int <- subset(growth_int_long, growth_int_long$Country == "Scotland")
text_scotland_thisyear_growth_int <- subset(text_scotland_thisyear_growth_int, text_scotland_thisyear_growth_int$Year == max(growth_int_long$Year))
text_scotland_thisyear_growth_int <- text_scotland_thisyear_growth_int$Value
text_oecd_thisyear_growth_int <- subset(growth_int_long, growth_int_long$Country == "OECD - Total")
text_oecd_thisyear_growth_int <- subset(text_oecd_thisyear_growth_int, text_oecd_thisyear_growth_int$Year == max(growth_int_long$Year))
text_oecd_thisyear_growth_int <- text_oecd_thisyear_growth_int$Value
text_oecd_last5_growth_int <- subset(growth_int, growth_int$Country == "OECD - Total")
start_text_oecd_last5 <- as.numeric(max(growth_int_long$Year))
end_text_oecd_last5 <- start_text_oecd_last5-5
text_oecd_last5_growth_int <- text_oecd_last5_growth_int[,seq(end_text_oecd_last5:start_text_oecd_last5)]
text_oecd_last5_growth_int <- sum(text_oecd_last5_growth_int[1,-1])
text_oecd_last5_growth_int <- round(text_oecd_last5_growth_int/5, digits = 2)
text_scotland_last5_growth_int <- subset(growth_int, growth_int$Country == "Scotland")
start_text_scotland_last5 <- as.numeric(max(growth_int_long$Year))
end_text_scotland_last5 <- start_text_scotland_last5-5
text_scotland_last5_growth_int <- text_scotland_last5_growth_int[,seq(end_text_scotland_last5:start_text_scotland_last5)]
text_scotland_last5_growth_int <- sum(text_scotland_last5_growth_int[1,-1])
text_scotland_last5_growth_int <- round(text_scotland_last5_growth_int/5, digits = 2)

# exporting_overview_int
exporting_overview_int <- subset(exporting_int_long, exporting_int_long$Country %in% comparison_countries)
exporting_overview_int <- subset(exporting_overview_int, exporting_overview_int$Year == max(exporting_overview_int$Year))
exporting_overview_int <- exporting_overview_int[,c(1,3)]
exporting_overview_int <- exporting_overview_int[order(exporting_overview_int$Value, decreasing = FALSE),]
exporting_overview_int$Country <- factor(exporting_overview_int$Country, levels = exporting_overview_int$Country)

# text for exporting_overview_int
text_scotland_thisyear_exporting_int <- subset(exporting_int_long, exporting_int_long$Country == "Scotland (international and RUK)")
text_scotland_thisyear_exporting_int <- subset(text_scotland_thisyear_exporting_int, text_scotland_thisyear_exporting_int$Year == max(exporting_int_long$Year))
text_scotland_thisyear_exporting_int <- text_scotland_thisyear_exporting_int$Value
text_oecd_thisyear_exporting_int <- subset(exporting_int_long, exporting_int_long$Country == "OECD members")
text_oecd_thisyear_exporting_int <- subset(text_oecd_thisyear_exporting_int, text_oecd_thisyear_exporting_int$Year == max(exporting_int_long$Year))
text_oecd_thisyear_exporting_int <- text_oecd_thisyear_exporting_int$Value
text_oecd_last5_exporting_int <- subset(exporting_int, exporting_int$Country == "OECD members")
start_text_oecd_last5 <- as.numeric(max(exporting_int_long$Year))
end_text_oecd_last5 <- start_text_oecd_last5-5
text_oecd_last5_exporting_int <- text_oecd_last5_exporting_int[,seq(end_text_oecd_last5:start_text_oecd_last5)]
text_oecd_last5_exporting_int <- sum(text_oecd_last5_exporting_int[1,-1])
text_oecd_last5_exporting_int <- round(text_oecd_last5_exporting_int/5, digits = 2)
text_scotland_last5_exporting_int <- subset(exporting_int, exporting_int$Country == "Scotland (international and RUK)")
start_text_scotland_last5 <- as.numeric(max(exporting_int_long$Year))
end_text_scotland_last5 <- start_text_scotland_last5-5
text_scotland_last5_exporting_int <- text_scotland_last5_exporting_int[,seq(end_text_scotland_last5:start_text_scotland_last5)]
text_scotland_last5_exporting_int <- sum(text_scotland_last5_exporting_int[1,-1])
text_scotland_last5_exporting_int <- round(text_scotland_last5_exporting_int/5, digits = 2)

# productivity_overview_int
productivity_overview_int <- subset(productivity_int_long, productivity_int_long$Country %in% comparison_countries)
productivity_overview_int <- subset(productivity_overview_int, productivity_overview_int$Year == max(productivity_overview_int$Year))
productivity_overview_int <- productivity_overview_int[,c(1,3)]
productivity_overview_int <- productivity_overview_int[order(productivity_overview_int$Value, decreasing = FALSE),]
productivity_overview_int$Country <- factor(productivity_overview_int$Country, levels = productivity_overview_int$Country)

# text for productivity_overview_int
text_scotland_thisyear_productivity_int <- subset(productivity_int_long, productivity_int_long$Country == "Scotland")
text_scotland_thisyear_productivity_int <- subset(text_scotland_thisyear_productivity_int, text_scotland_thisyear_productivity_int$Year == max(productivity_int_long$Year))
text_scotland_thisyear_productivity_int <- text_scotland_thisyear_productivity_int$Value
text_oecd_thisyear_productivity_int <- subset(productivity_int_long, productivity_int_long$Country == "OECD - Total")
text_oecd_thisyear_productivity_int <- subset(text_oecd_thisyear_productivity_int, text_oecd_thisyear_productivity_int$Year == max(productivity_int_long$Year))
text_oecd_thisyear_productivity_int <- text_oecd_thisyear_productivity_int$Value
text_oecd_last5_productivity_int <- subset(productivity_int, productivity_int$Country == "OECD - Total")
start_text_oecd_last5 <- as.numeric(max(productivity_int_long$Year))
end_text_oecd_last5 <- start_text_oecd_last5-5
text_oecd_last5_productivity_int <- text_oecd_last5_productivity_int[,seq(end_text_oecd_last5:start_text_oecd_last5)]
text_oecd_last5_productivity_int <- sum(text_oecd_last5_productivity_int[1,-1])
text_oecd_last5_productivity_int <- round(text_oecd_last5_productivity_int/5, digits = 2)
text_scotland_last5_productivity_int <- subset(productivity_int, productivity_int$Country == "Scotland")
start_text_scotland_last5 <- as.numeric(max(productivity_int_long$Year))
end_text_scotland_last5 <- start_text_scotland_last5-5
text_scotland_last5_productivity_int <- text_scotland_last5_productivity_int[,seq(end_text_scotland_last5:start_text_scotland_last5)]
text_scotland_last5_productivity_int <- sum(text_scotland_last5_productivity_int[1,-1])
text_scotland_last5_productivity_int <- round(text_scotland_last5_productivity_int/5, digits = 2)

# reputation_overview_sco
reputation_overview_sco <- reputation_sco

# rd_overview_int (don't copy, omitting empty rows)
rd_overview_int <- subset(rd_int_long, rd_int_long$Country %in% comparison_countries)
rd_overview_int <- subset(rd_overview_int, rd_overview_int$Year == max(rd_overview_int$Year))
rd_overview_int <- rd_overview_int[,c(1,3)]
rd_overview_int <- rd_overview_int[order(rd_overview_int$Value, decreasing = FALSE),]
rd_overview_int$Country <- factor(rd_overview_int$Country, levels = rd_overview_int$Country)
rd_overview_int <- subset(rd_overview_int, rd_overview_int$Value != is.na(rd_overview_int$Value))

# rd_overview_sco (Pie chart)
rd_overview_sco <- subset(rd_overview_sco, rd_overview_sco$Year == 2017)
rd_overview_sco <- rd_overview_sco[,c(1,3)]
rd_overview_sco <- rd_overview_sco[order(rd_overview_sco$Value, decreasing = FALSE),]
scotland_gross_expenditure_on_rd <- subset(rd_overview_sco, rd_overview_sco$Performer == "Gross Expenditure on Research and Development")
scotland_gross_expenditure_on_rd <- scotland_gross_expenditure_on_rd[,c(2)]
rd_overview_sco$Value <- round(rd_overview_sco$Value/scotland_gross_expenditure_on_rd, digits=2)
rd_overview_sco <- subset(rd_overview_sco, rd_overview_sco$Performer != "Gross Expenditure on Research and Development")
rd_overview_sco$Performer <- factor(rd_overview_sco$Performer, levels = rd_overview_sco$Performer)

# text for rd_overview_int
text_scotland_thisyear_rd_int <- subset(rd_int_long, rd_int_long$Country == "Scotland")
text_scotland_thisyear_rd_int <- subset(text_scotland_thisyear_rd_int, text_scotland_thisyear_rd_int$Year == max(rd_int_long$Year))
text_scotland_thisyear_rd_int <- text_scotland_thisyear_rd_int$Value
text_oecd_thisyear_rd_int <- subset(rd_int_long, rd_int_long$Country == "OECD - Total")
text_oecd_thisyear_rd_int <- subset(text_oecd_thisyear_rd_int, text_oecd_thisyear_rd_int$Year == max(rd_int_long$Year))
text_oecd_thisyear_rd_int <- text_oecd_thisyear_rd_int$Value
text_oecd_last5_rd_int <- subset(rd_int, rd_int$Country == "OECD - Total")
start_text_oecd_last5 <- as.numeric(max(rd_int_long$Year))
end_text_oecd_last5 <- start_text_oecd_last5-5
text_oecd_last5_rd_int <- text_oecd_last5_rd_int[,seq(end_text_oecd_last5:start_text_oecd_last5)]
text_oecd_last5_rd_int <- sum(text_oecd_last5_rd_int[1,-1])
text_oecd_last5_rd_int <- round(text_oecd_last5_rd_int/5, digits = 2)
text_scotland_last5_rd_int <- subset(rd_int, rd_int$Country == "Scotland")
start_text_scotland_last5 <- as.numeric(max(rd_int_long$Year))
end_text_scotland_last5 <- start_text_scotland_last5-5
text_scotland_last5_rd_int <- text_scotland_last5_rd_int[,seq(end_text_scotland_last5:start_text_scotland_last5)]
text_scotland_last5_rd_int <- sum(text_scotland_last5_rd_int[1,-1])
text_scotland_last5_rd_int <- round(text_scotland_last5_rd_int/5, digits = 2)

# Business overview text figures
text_entrepreneurialism_sco <- subset(entrepreneurialism_sco, entrepreneurialism_sco$Year == max(entrepreneurialism_sco$Year))
text_year_entrepreneurialism_sco <- text_entrepreneurialism_sco$Year
text_value_entrepreneurialism_sco <- text_entrepreneurialism_sco$Value
text_nbusiness_sector_sco <- subset(nbusiness_sector_sco, nbusiness_sector_sco$Year == max(nbusiness_sector_sco$Year))
text_year_nbusiness_sector_sco <- text_nbusiness_sector_sco$Year
text_all_nbusiness_sector_sco <- text_nbusiness_sector_sco[,c('All')]
text_nbusiness_region_sco <- subset(nbusiness_region_sco, nbusiness_region_sco$Year == max(nbusiness_sector_sco$Year))
text_foreign_nbusiness_region_sco <- text_nbusiness_region_sco[, c('All foreign-owned')]
text_hbusiness_percent_sco <- subset(hbusiness_percent_sco, hbusiness_percent_sco$Year == max(nbusiness_sector_sco$Year))
text_hbusiness_percent_sco <- text_hbusiness_percent_sco$Value

# OECD bar - growth
rank_growth <- subset(growth_int_long, growth_int_long$Country != "European Union (28 countries)")
rank_growth <- subset(rank_growth, rank_growth$Country != "OECD - Total")
rank_growth <- subset(rank_growth, rank_growth$Country != "Euro area (19 countries)")
rank_growth <- subset(rank_growth, rank_growth$Year == max(rank_growth$Year))
rank_growth <- rank_growth[,c(1,3)]
rank_growth <- rank_growth[order(rank_growth$Value, decreasing = TRUE),]
rank_growth$Rank <- order(rank_growth$Value, decreasing = TRUE)
rank_growth_quantiles <- rank_growth$Rank
rank_growth$Quantile <- ecdf(rank_growth_quantiles)(rank_growth_quantiles)
rank_growth <- subset(rank_growth, rank_growth$Country == "Scotland")
rank_growth_quantile <- rank_growth$Quantile
rank_growth <- rank_growth$Rank

# OECD bar - productivity
rank_productivity <- subset(productivity_int_long, productivity_int_long$Country != "European Union (28 countries)")
rank_productivity <- subset(rank_productivity, rank_productivity$Country != "OECD - Total")
rank_productivity <- subset(rank_productivity, rank_productivity$Country != "Euro area (19 countries)")
rank_productivity <- subset(rank_productivity, rank_productivity$Year == max(rank_productivity$Year))
rank_productivity <- rank_productivity[,c(1,3)]
rank_productivity <- rank_productivity[order(rank_productivity$Value, decreasing = TRUE),]
rank_productivity$Rank <- order(rank_productivity$Value, decreasing = TRUE)
rank_productivity_quantiles <- rank_productivity$Rank
rank_productivity$Quantile <- ecdf(rank_productivity_quantiles)(rank_productivity_quantiles)
rank_productivity <- subset(rank_productivity, rank_productivity$Country == "Scotland")
rank_productivity_quantile <- rank_productivity$Quantile
rank_productivity <- rank_productivity$Rank

# OECD bar - exporting
rank_exporting <- subset(exporting_int_long, exporting_int_long$Country != "OECD members")
rank_exporting <- subset(rank_exporting, rank_exporting$Country != "Scotland (international)")
rank_exporting <- subset(rank_exporting, rank_exporting$Country != "European Union")
rank_exporting <- subset(rank_exporting, rank_exporting$Year == max(rank_exporting$Year))
rank_exporting <- rank_exporting[,c(1,3)]
rank_exporting <- rank_exporting[order(rank_exporting$Value, decreasing = TRUE),]
rank_exporting$Rank <- order(rank_exporting$Value, decreasing = TRUE)
rank_exporting_quantiles <- rank_exporting$Rank
rank_exporting$Quantile <- ecdf(rank_exporting_quantiles)(rank_exporting_quantiles)
rank_exporting <- subset(rank_exporting, rank_exporting$Country == "Scotland (international and RUK)")
rank_exporting_quantile <- rank_exporting$Quantile
rank_exporting <- rank_exporting$Rank

# OECD bar - rd
rank_rd <- subset(rd_int_long, rd_int_long$Country != "European Union (28 countries)")
rank_rd <- subset(rank_rd, rank_rd$Country != "OECD - Total")
rank_rd <- subset(rank_rd, rank_rd$Country != "Euro area (19 countries)")
rank_rd <- subset(rank_rd, rank_rd$Year == max(rank_rd$Year))
rank_rd <- rank_rd[,c(1,3)]
rank_rd <- rank_rd[order(rank_rd$Value, decreasing = TRUE),]
rank_rd$Rank <- order(rank_rd$Value, decreasing = TRUE)
rank_rd_quantiles <- rank_rd$Rank
rank_rd$Quantile <- ecdf(rank_rd_quantiles)(rank_rd_quantiles)
rank_rd <- subset(rank_rd, rank_rd$Country == "Scotland")
rank_rd_quantile <- rank_rd$Quantile
rank_rd <- rank_rd$Rank

# OECD bar - entrepreneurialism (unusual, don't copy)
rank_entrepreneurialism <- subset(entrepreneurialism_int_long, entrepreneurialism_int_long$Country != "European Union (28 countries)")
rank_entrepreneurialism <- subset(rank_entrepreneurialism, rank_entrepreneurialism$Country != "OECD - Total")
rank_entrepreneurialism <- subset(rank_entrepreneurialism, rank_entrepreneurialism$Country != "Euro area (19 countries)")
rank_entrepreneurialism <- subset(rank_entrepreneurialism, rank_entrepreneurialism$Year == 2018)
rank_entrepreneurialism <- rank_entrepreneurialism[,c(1,3)]
rank_entrepreneurialism <- rank_entrepreneurialism[order(rank_entrepreneurialism$Value, decreasing = TRUE),]
rank_entrepreneurialism$Rank <- order(rank_entrepreneurialism$Value, decreasing = TRUE)
rank_entrepreneurialism_quantiles <- rank_entrepreneurialism$Rank
rank_entrepreneurialism$Quantile <- ecdf(rank_entrepreneurialism_quantiles)(rank_entrepreneurialism_quantiles)
rank_entrepreneurialism <- subset(rank_entrepreneurialism, rank_entrepreneurialism$Country == "Scotland")
rank_entrepreneurialism_quantile <- rank_entrepreneurialism$Quantile
rank_entrepreneurialism <- rank_entrepreneurialism$Rank

# POPULATION #########################################################################################################################################
# depratio_int
depratio_int_long <- depratio_int %>% 
  gather("Year", "Value", 2:ncol(depratio_int))
depratio_int_wide <- depratio_int_long %>% 
  spread("Country", "Value", 2:3)
depratio_int_wide$Year <- as.integer(depratio_int_wide$Year)
positions_selected_countries_depratio_int <- which(names(depratio_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland'))
start_year_depratio_int <- min(depratio_int_long$Year)
end_year_depratio_int <- max(depratio_int_long$Year)

# migration_int
migration_int_long <- migration_int %>% 
  gather("Year", "Value", 2:ncol(migration_int))
migration_int_wide <- migration_int_long %>% 
  spread("Country", "Value", 2:3)
migration_int_wide$Year <- as.integer(migration_int_wide$Year)
positions_selected_countries_migration_int <- which(names(migration_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland'))
start_year_migration_int <- min(migration_int_long$Year)
end_year_migration_int <- max(migration_int_long$Year)

# depratio_sco (no checkbox input)
start_year_depratio_sco <- min(depratio_sco$Year)
end_year_depratio_sco <- max(depratio_sco$Year)

# OECD bar - depratio
rank_depratio <- subset(depratio_int_long, depratio_int_long$Country != "European Union")
rank_depratio <- subset(rank_depratio, rank_depratio$Country != "OECD members")
rank_depratio <- subset(rank_depratio, rank_depratio$Year == max(rank_depratio$Year))
rank_depratio <- rank_depratio[,c(1,3)]
rank_depratio <- rank_depratio[order(rank_depratio$Value, decreasing = TRUE),]
rank_depratio$Rank <- order(rank_depratio$Value, decreasing = TRUE)
rank_depratio_quantiles <- rank_depratio$Rank
rank_depratio$Quantile <- ecdf(rank_depratio_quantiles)(rank_depratio_quantiles)
rank_depratio <- subset(rank_depratio, rank_depratio$Country == "Scotland")
rank_depratio_quantile <- rank_depratio$Quantile
rank_depratio <- rank_depratio$Rank

# depratio_overview_int
depratio_overview_int <- subset(depratio_int_long, depratio_int_long$Country %in% comparison_countries)
depratio_overview_int <- subset(depratio_overview_int, depratio_overview_int$Year == max(depratio_overview_int$Year))
depratio_overview_int <- depratio_overview_int[,c(1,3)]
depratio_overview_int <- depratio_overview_int[order(depratio_overview_int$Value, decreasing = FALSE),]
depratio_overview_int$Country <- factor(depratio_overview_int$Country, levels = depratio_overview_int$Country)

# migration_overview_int
migration_overview_int <- subset(migration_int_long, migration_int_long$Country %in% comparison_countries)
migration_overview_int <- subset(migration_overview_int, migration_overview_int$Year == max(migration_overview_int$Year))
migration_overview_int <- migration_overview_int[,c(1,3)]
migration_overview_int <- migration_overview_int[order(migration_overview_int$Value, decreasing = FALSE),]
migration_overview_int$Country <- factor(migration_overview_int$Country, levels = migration_overview_int$Country)

# text for depratio_overview_int
text_scotland_thisyear_depratio_int <- subset(depratio_int_long, depratio_int_long$Country == "Scotland")
text_scotland_thisyear_depratio_int <- subset(text_scotland_thisyear_depratio_int, text_scotland_thisyear_depratio_int$Year == max(depratio_int_long$Year))
text_scotland_thisyear_depratio_int <- text_scotland_thisyear_depratio_int$Value
text_oecd_thisyear_depratio_int <- subset(depratio_int_long, depratio_int_long$Country == "OECD members")
text_oecd_thisyear_depratio_int <- subset(text_oecd_thisyear_depratio_int, text_oecd_thisyear_depratio_int$Year == max(depratio_int_long$Year))
text_oecd_thisyear_depratio_int <- text_oecd_thisyear_depratio_int$Value
text_oecd_last5_depratio_int <- subset(depratio_int, depratio_int$Country == "OECD members")
start_text_oecd_last5 <- as.numeric(max(depratio_int_long$Year))
end_text_oecd_last5 <- start_text_oecd_last5-5
text_oecd_last5_depratio_int <- text_oecd_last5_depratio_int[,seq(end_text_oecd_last5:start_text_oecd_last5)]
text_oecd_last5_depratio_int <- sum(text_oecd_last5_depratio_int[1,-1])
text_oecd_last5_depratio_int <- round(text_oecd_last5_depratio_int/5, digits = 2)
text_scotland_last5_depratio_int <- subset(depratio_int, depratio_int$Country == "Scotland")
start_text_scotland_last5 <- as.numeric(max(depratio_int_long$Year))
end_text_scotland_last5 <- start_text_scotland_last5-5
text_scotland_last5_depratio_int <- text_scotland_last5_depratio_int[,seq(end_text_scotland_last5:start_text_scotland_last5)]
text_scotland_last5_depratio_int <- sum(text_scotland_last5_depratio_int[1,-1])
text_scotland_last5_depratio_int <- round(text_scotland_last5_depratio_int/5, digits = 2)

# migration_type_overseas_sco
migration_type_overseas_sco_long <- migration_type_overseas_sco
migration_type_overseas_sco_wide <- migration_type_overseas_sco_long %>%
  spread("Type", "Value", 2:3)
migration_type_overseas_sco_wide$Year <- as.integer(migration_type_overseas_sco_wide$Year)
positions_selected_types_migration_type_overseas_sco <- which(names(migration_type_overseas_sco_wide) %in% c('In', 'Net', 'Out'))
start_year_migration_type_overseas_sco <- min(migration_type_overseas_sco_long$Year)
end_year_migration_type_overseas_sco <- max(migration_type_overseas_sco_long$Year)

# migration_in_source_sco
migration_in_source_sco_long <- migration_in_source_sco
migration_in_source_sco_wide <- migration_in_source_sco_long %>%
  spread("Source", "Value", 2:3)
migration_in_source_sco_wide$Year <- as.integer(migration_in_source_sco_wide$Year)
positions_selected_sources_migration_in_source_sco <- which(names(migration_in_source_sco_wide) %in% c('To-from Overseas', 'To-from Rest of UK'))
start_year_migration_in_source_sco <- min(migration_in_source_sco_long$Year)
end_year_migration_in_source_sco <- max(migration_in_source_sco_long$Year)

# migration_in_overseas_age_sco
migration_in_overseas_age_sco_long <- migration_in_overseas_age_sco
migration_in_overseas_age_sco_wide <- migration_in_overseas_age_sco_long %>%
  spread("Age", "Value", 2:3)
migration_in_overseas_age_sco_wide$Year <- as.integer(migration_in_overseas_age_sco_wide$Year)
positions_selected_ages_migration_in_overseas_age_sco <- which(names(migration_in_overseas_age_sco_wide) %in% c('All'))
start_year_migration_in_overseas_age_sco <- min(migration_in_overseas_age_sco_long$Year)
end_year_migration_in_overseas_age_sco <- max(migration_in_overseas_age_sco_long$Year)

# migration_in_overseas_sex_sco
migration_in_overseas_sex_sco_long <- migration_in_overseas_sex_sco
migration_in_overseas_sex_sco_wide <- migration_in_overseas_sex_sco_long %>%
  spread("Sex", "Value", 2:3)
migration_in_overseas_sex_sco_wide$Year <- as.integer(migration_in_overseas_sex_sco_wide$Year)
positions_selected_sexes_migration_in_overseas_sex_sco <- which(names(migration_in_overseas_sex_sco_wide) %in% c('All', 'Male', 'Female'))
start_year_migration_in_overseas_sex_sco <- min(migration_in_overseas_sex_sco_long$Year)
end_year_migration_in_overseas_sex_sco <- max(migration_in_overseas_sex_sco_long$Year)

# depratio_reg
start_year_depratio_reg <- min(depratio_reg$Year)
end_year_depratio_reg <- max(depratio_reg$Year)

# migration_reg
start_year_migration_reg <- min(migration_reg$Year)
end_year_migration_reg <- max(migration_reg$Year)

# working_age_reg
start_year_working_age_reg <- min(working_age_reg$Year)
end_year_working_age_reg <- max(working_age_reg$Year)

# PARTICIPATION #########################################################################################################################################
# eactivity_int
eactivity_int_long <- eactivity_int %>% 
  gather("Year", "Value", 2:ncol(eactivity_int))
eactivity_int_wide <- eactivity_int_long %>% 
  spread("Country", "Value", 2:3)
eactivity_int_wide$Year <- as.integer(eactivity_int_wide$Year)
positions_selected_countries_eactivity_int <- which(names(eactivity_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland'))
start_year_eactivity_int <- min(eactivity_int_long$Year)
end_year_eactivity_int <- max(eactivity_int_long$Year)

# employment_int
employment_int_long <- employment_int %>% 
  gather("Year", "Value", 2:ncol(employment_int))
employment_int_wide <- employment_int_long %>% 
  spread("Country", "Value", 2:3)
employment_int_wide$Year <- as.integer(employment_int_wide$Year)
positions_selected_countries_employment_int <- which(names(employment_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland'))
start_year_employment_int <- min(employment_int_long$Year)
end_year_employment_int <- max(employment_int_long$Year)

# unemployment_int
unemployment_int_long <- unemployment_int %>% 
  gather("Year", "Value", 2:ncol(unemployment_int))
unemployment_int_wide <- unemployment_int_long %>% 
  spread("Country", "Value", 2:3)
unemployment_int_wide$Year <- as.integer(unemployment_int_wide$Year)
positions_selected_countries_unemployment_int <- which(names(unemployment_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland'))
start_year_unemployment_int <- min(unemployment_int_long$Year)
end_year_unemployment_int <- max(unemployment_int_long$Year)

# genderpaygap_int
genderpaygap_int_long <- genderpaygap_int %>% 
  gather("Year", "Value", 2:ncol(genderpaygap_int))
genderpaygap_int_wide <- genderpaygap_int_long %>% 
  spread("Country", "Value", 2:3)
genderpaygap_int_wide$Year <- as.integer(genderpaygap_int_wide$Year)
positions_selected_countries_genderpaygap_int <- which(names(genderpaygap_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland'))
start_year_genderpaygap_int <- min(genderpaygap_int_long$Year)
end_year_genderpaygap_int <- max(genderpaygap_int_long$Year)

# skillsunderprimary_int
skillsunderprimary_int_long <- skillsunderprimary_int %>% 
  gather("Year", "Value", 2:ncol(skillsunderprimary_int))
skillsunderprimary_int_wide <- skillsunderprimary_int_long %>% 
  spread("Country", "Value", 2:3)
skillsunderprimary_int_wide$Year <- as.integer(skillsunderprimary_int_wide$Year)
positions_selected_countries_skillsunderprimary_int <- which(names(skillsunderprimary_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland'))
start_year_skillsunderprimary_int <- min(skillsunderprimary_int_long$Year)
end_year_skillsunderprimary_int <- max(skillsunderprimary_int_long$Year)

# skillstertiary_int
skillstertiary_int_long <- skillstertiary_int %>% 
  gather("Year", "Value", 2:ncol(skillstertiary_int))
skillstertiary_int_wide <- skillstertiary_int_long %>% 
  spread("Country", "Value", 2:3)
skillstertiary_int_wide$Year <- as.integer(skillstertiary_int_wide$Year)
positions_selected_countries_skillstertiary_int <- which(names(skillstertiary_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland'))
start_year_skillstertiary_int <- min(skillstertiary_int_long$Year)
end_year_skillstertiary_int <- max(skillstertiary_int_long$Year)

# yunemployment_int
yunemployment_int_long <- yunemployment_int %>% 
  gather("Year", "Value", 2:ncol(yunemployment_int))
yunemployment_int_wide <- yunemployment_int_long %>% 
  spread("Country", "Value", 2:3)
yunemployment_int_wide$Year <- as.integer(yunemployment_int_wide$Year)
positions_selected_countries_yunemployment_int <- which(names(yunemployment_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland', 'EU'))
start_year_yunemployment_int <- min(yunemployment_int_long$Year)
end_year_yunemployment_int <- max(yunemployment_int_long$Year)

# evoice_int
evoice_int_long <- evoice_int %>% 
  gather("Year", "Value", 2:ncol(evoice_int))
evoice_int_wide <- evoice_int_long %>% 
  spread("Country", "Value", 2:3)
evoice_int_wide$Year <- as.integer(evoice_int_wide$Year)
positions_selected_countries_evoice_int <- which(names(evoice_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland'))
start_year_evoice_int <- min(evoice_int_long$Year)
end_year_evoice_int <- max(evoice_int_long$Year)

# rank_eactivity
rank_eactivity <- subset(eactivity_int_long, eactivity_int_long$Country != "European Union 28")
rank_eactivity <- subset(rank_eactivity, rank_eactivity$Country != "OECD countries")
rank_eactivity <- subset(rank_eactivity, rank_eactivity$Country != "Euro area (19 countries)")
rank_eactivity <- subset(rank_eactivity, rank_eactivity$Year == max(rank_eactivity$Year))
rank_eactivity <- rank_eactivity[,c(1,3)]
rank_eactivity <- rank_eactivity[order(rank_eactivity$Value, decreasing = TRUE),]
rank_eactivity$Rank <- order(rank_eactivity$Value, decreasing = TRUE)
rank_eactivity_quantiles <- rank_eactivity$Rank
rank_eactivity$Quantile <- ecdf(rank_eactivity_quantiles)(rank_eactivity_quantiles)
rank_eactivity <- subset(rank_eactivity, rank_eactivity$Country == "Scotland")
rank_eactivity_quantile <- rank_eactivity$Quantile
rank_eactivity <- rank_eactivity$Rank

# rank_employment
rank_employment <- subset(employment_int_long, employment_int_long$Country != "European Union 28")
rank_employment <- subset(rank_employment, rank_employment$Country != "OECD countries")
rank_employment <- subset(rank_employment, rank_employment$Country != "Euro area (19 countries)")
rank_employment <- subset(rank_employment, rank_employment$Year == max(rank_employment$Year))
rank_employment <- rank_employment[,c(1,3)]
rank_employment <- rank_employment[order(rank_employment$Value, decreasing = TRUE),]
rank_employment$Rank <- order(rank_employment$Value, decreasing = TRUE)
rank_employment_quantiles <- rank_employment$Rank
rank_employment$Quantile <- ecdf(rank_employment_quantiles)(rank_employment_quantiles)
rank_employment <- subset(rank_employment, rank_employment$Country == "Scotland")
rank_employment_quantile <- rank_employment$Quantile
rank_employment <- rank_employment$Rank

# rank_unemployment
rank_unemployment <- subset(unemployment_int_long, unemployment_int_long$Country != "European Union 28")
rank_unemployment <- subset(rank_unemployment, rank_unemployment$Country != "OECD countries")
rank_unemployment <- subset(rank_unemployment, rank_unemployment$Country != "Euro area (19 countries)")
rank_unemployment <- subset(rank_unemployment, rank_unemployment$Year == max(rank_unemployment$Year))
rank_unemployment <- rank_unemployment[,c(1,3)]
rank_unemployment <- rank_unemployment[order(rank_unemployment$Value, decreasing = TRUE),]
rank_unemployment$Rank <- order(rank_unemployment$Value, decreasing = TRUE)
rank_unemployment_quantiles <- rank_unemployment$Rank
rank_unemployment$Quantile <- ecdf(rank_unemployment_quantiles)(rank_unemployment_quantiles)
rank_unemployment <- subset(rank_unemployment, rank_unemployment$Country == "Scotland")
rank_unemployment_quantile <- rank_unemployment$Quantile
rank_unemployment <- rank_unemployment$Rank

# eactivity_overview_int
eactivity_overview_int <- subset(eactivity_int_long, eactivity_int_long$Country %in% comparison_countries)
eactivity_overview_int <- subset(eactivity_overview_int, eactivity_overview_int$Year == max(eactivity_overview_int$Year))
eactivity_overview_int <- eactivity_overview_int[,c(1,3)]
eactivity_overview_int <- eactivity_overview_int[order(eactivity_overview_int$Value, decreasing = FALSE),]
eactivity_overview_int$Country <- factor(eactivity_overview_int$Country, levels = eactivity_overview_int$Country)

# employment_overview_int
employment_overview_int <- subset(employment_int_long, employment_int_long$Country %in% comparison_countries)
employment_overview_int <- subset(employment_overview_int, employment_overview_int$Year == max(employment_overview_int$Year))
employment_overview_int <- employment_overview_int[,c(1,3)]
employment_overview_int <- employment_overview_int[order(employment_overview_int$Value, decreasing = FALSE),]
employment_overview_int$Country <- factor(employment_overview_int$Country, levels = employment_overview_int$Country)

# unemployment_overview_int
unemployment_overview_int <- subset(unemployment_int_long, unemployment_int_long$Country %in% comparison_countries)
unemployment_overview_int <- subset(unemployment_overview_int, unemployment_overview_int$Year == max(unemployment_overview_int$Year))
unemployment_overview_int <- unemployment_overview_int[,c(1,3)]
unemployment_overview_int <- unemployment_overview_int[order(unemployment_overview_int$Value, decreasing = FALSE),]
unemployment_overview_int$Country <- factor(unemployment_overview_int$Country, levels = unemployment_overview_int$Country)

# genderpaygap_overview_int
genderpaygap_overview_int <- subset(genderpaygap_int_long, genderpaygap_int_long$Country %in% comparison_countries)
genderpaygap_overview_int <- subset(genderpaygap_overview_int, genderpaygap_overview_int$Year == 2017)
genderpaygap_overview_int <- genderpaygap_overview_int[,c(1,3)]
genderpaygap_overview_int <- genderpaygap_overview_int[order(genderpaygap_overview_int$Value, decreasing = FALSE),]
genderpaygap_overview_int$Country <- factor(genderpaygap_overview_int$Country, levels = genderpaygap_overview_int$Country)

# skillsunderprimary_overview_int
skillsunderprimary_overview_int <- subset(skillsunderprimary_int_long, skillsunderprimary_int_long$Country %in% comparison_countries_eu)
skillsunderprimary_overview_int <- subset(skillsunderprimary_overview_int, skillsunderprimary_overview_int$Year == max(skillsunderprimary_overview_int$Year))
skillsunderprimary_overview_int <- skillsunderprimary_overview_int[,c(1,3)]
skillsunderprimary_overview_int <- skillsunderprimary_overview_int[order(skillsunderprimary_overview_int$Value, decreasing = FALSE),]
skillsunderprimary_overview_int$Country <- factor(skillsunderprimary_overview_int$Country, levels = skillsunderprimary_overview_int$Country)

# text for skillsunderprimary_overview_int
text_scotland_thisyear_skillsunderprimary_int <- subset(skillsunderprimary_int_long, skillsunderprimary_int_long$Country == "Scotland")
text_scotland_thisyear_skillsunderprimary_int <- subset(text_scotland_thisyear_skillsunderprimary_int, text_scotland_thisyear_skillsunderprimary_int$Year == max(skillsunderprimary_int_long$Year))
text_scotland_thisyear_skillsunderprimary_int <- text_scotland_thisyear_skillsunderprimary_int$Value
text_eu_thisyear_skillsunderprimary_int <- subset(skillsunderprimary_int_long, skillsunderprimary_int_long$Country == "EU")
text_eu_thisyear_skillsunderprimary_int <- subset(text_eu_thisyear_skillsunderprimary_int, text_eu_thisyear_skillsunderprimary_int$Year == max(skillsunderprimary_int_long$Year))
text_eu_thisyear_skillsunderprimary_int <- text_eu_thisyear_skillsunderprimary_int$Value

# yunemployment_overview_int
yunemployment_overview_int <- subset(yunemployment_int_long, yunemployment_int_long$Country %in% comparison_countries_eu)
yunemployment_overview_int <- subset(yunemployment_overview_int, yunemployment_overview_int$Year == max(yunemployment_overview_int$Year))
yunemployment_overview_int <- yunemployment_overview_int[,c(1,3)]
yunemployment_overview_int <- yunemployment_overview_int[order(yunemployment_overview_int$Value, decreasing = FALSE),]
yunemployment_overview_int$Country <- factor(yunemployment_overview_int$Country, levels = yunemployment_overview_int$Country)

# text for yunemployment_overview_int
text_scotland_thisyear_yunemployment_int <- subset(yunemployment_int_long, yunemployment_int_long$Country == "Scotland")
text_scotland_thisyear_yunemployment_int <- subset(text_scotland_thisyear_yunemployment_int, text_scotland_thisyear_yunemployment_int$Year == max(yunemployment_int_long$Year))
text_scotland_thisyear_yunemployment_int <- text_scotland_thisyear_yunemployment_int$Value
text_eu_thisyear_yunemployment_int <- subset(yunemployment_int_long, yunemployment_int_long$Country == "EU")
text_eu_thisyear_yunemployment_int <- subset(text_eu_thisyear_yunemployment_int, text_eu_thisyear_yunemployment_int$Year == max(yunemployment_int_long$Year))
text_eu_thisyear_yunemployment_int <- text_eu_thisyear_yunemployment_int$Value

# eactivity_sco
start_year_eactivity_sco <- min(eactivity_sco$Year)
end_year_eactivity_sco <- max(eactivity_sco$Year)

# livingwage_sco
start_year_livingwage_sco <- min(livingwage_sco$Year)
end_year_livingwage_sco <- max(livingwage_sco$Year)

# genderpaygap_sco
start_year_genderpaygap_sco <- min(genderpaygap_sco$Year)
end_year_genderpaygap_sco <- max(genderpaygap_sco$Year)

# evoice_sco
start_year_evoice_sco <- min(evoice_sco$Year)
end_year_evoice_sco <- max(evoice_sco$Year)

# wplearning_sco
start_year_wplearning_sco <- min(wplearning_sco$Year)
end_year_wplearning_sco <- max(wplearning_sco$Year)

# youngpplpart_sco
start_year_youngpplpart_sco <- min(youngpplpart_sco$Year)
end_year_youngpplpart_sco <- max(youngpplpart_sco$Year)

# skillshortage_sco
start_year_skillshortage_sco <- min(skillshortage_sco$Year)
end_year_skillshortage_sco <- max(skillshortage_sco$Year)

# eactivity_reg
start_year_eactivity_reg <- min(eactivity_reg$Year)
end_year_eactivity_reg <- max(eactivity_reg$Year)

# lwage_reg
start_year_lwage_reg <- min(lwage_reg$Year)
end_year_lwage_reg <- max(lwage_reg$Year)

# gpaygap_reg
start_year_gpaygap_reg <- min(gpaygap_reg$Year)
end_year_gpaygap_reg <- max(gpaygap_reg$Year)

# wplearning_reg
start_year_wplearning_reg <- min(wplearning_reg$Year)
end_year_wplearning_reg <- max(wplearning_reg$Year)

# text for eactivity_overview_int
text_scotland_thisyear_eactivity_int <- subset(eactivity_int_long, eactivity_int_long$Country == "Scotland")
text_scotland_thisyear_eactivity_int <- subset(text_scotland_thisyear_eactivity_int, text_scotland_thisyear_eactivity_int$Year == max(eactivity_int_long$Year))
text_scotland_thisyear_eactivity_int <- text_scotland_thisyear_eactivity_int$Value
text_oecd_thisyear_eactivity_int <- subset(eactivity_int_long, eactivity_int_long$Country == "OECD - Total")
text_oecd_thisyear_eactivity_int <- subset(text_oecd_thisyear_eactivity_int, text_oecd_thisyear_eactivity_int$Year == max(eactivity_int_long$Year))
text_oecd_thisyear_eactivity_int <- text_oecd_thisyear_eactivity_int$Value
text_oecd_last5_eactivity_int <- subset(eactivity_int, eactivity_int$Country == "OECD - Total")
start_text_oecd_last5 <- as.numeric(max(eactivity_int_long$Year))
end_text_oecd_last5 <- start_text_oecd_last5-5
text_oecd_last5_eactivity_int <- text_oecd_last5_eactivity_int[,seq(end_text_oecd_last5:start_text_oecd_last5)]
text_oecd_last5_eactivity_int <- sum(text_oecd_last5_eactivity_int[1,-1])
text_oecd_last5_eactivity_int <- round(text_oecd_last5_eactivity_int/5, digits = 2)
text_scotland_last5_eactivity_int <- subset(eactivity_int, eactivity_int$Country == "Scotland")
start_text_scotland_last5 <- as.numeric(max(eactivity_int_long$Year))
end_text_scotland_last5 <- start_text_scotland_last5-5
text_scotland_last5_eactivity_int <- text_scotland_last5_eactivity_int[,seq(end_text_scotland_last5:start_text_scotland_last5)]
text_scotland_last5_eactivity_int <- sum(text_scotland_last5_eactivity_int[1,-1])
text_scotland_last5_eactivity_int <- round(text_scotland_last5_eactivity_int/5, digits = 2)

# text for employment_overview_int
text_scotland_thisyear_employment_int <- subset(employment_int_long, employment_int_long$Country == "Scotland")
text_scotland_thisyear_employment_int <- subset(text_scotland_thisyear_employment_int, text_scotland_thisyear_employment_int$Year == max(employment_int_long$Year))
text_scotland_thisyear_employment_int <- text_scotland_thisyear_employment_int$Value
text_oecd_thisyear_employment_int <- subset(employment_int_long, employment_int_long$Country == "OECD - Total")
text_oecd_thisyear_employment_int <- subset(text_oecd_thisyear_employment_int, text_oecd_thisyear_employment_int$Year == max(employment_int_long$Year))
text_oecd_thisyear_employment_int <- text_oecd_thisyear_employment_int$Value
text_oecd_last5_employment_int <- subset(employment_int, employment_int$Country == "OECD - Total")
start_text_oecd_last5 <- as.numeric(max(employment_int_long$Year))
end_text_oecd_last5 <- start_text_oecd_last5-5
text_oecd_last5_employment_int <- text_oecd_last5_employment_int[,seq(end_text_oecd_last5:start_text_oecd_last5)]
text_oecd_last5_employment_int <- sum(text_oecd_last5_employment_int[1,-1])
text_oecd_last5_employment_int <- round(text_oecd_last5_employment_int/5, digits = 2)
text_scotland_last5_employment_int <- subset(employment_int, employment_int$Country == "Scotland")
start_text_scotland_last5 <- as.numeric(max(employment_int_long$Year))
end_text_scotland_last5 <- start_text_scotland_last5-5
text_scotland_last5_employment_int <- text_scotland_last5_employment_int[,seq(end_text_scotland_last5:start_text_scotland_last5)]
text_scotland_last5_employment_int <- sum(text_scotland_last5_employment_int[1,-1])
text_scotland_last5_employment_int <- round(text_scotland_last5_employment_int/5, digits = 2)

# text for unemployment_overview_int
text_scotland_thisyear_unemployment_int <- subset(unemployment_int_long, unemployment_int_long$Country == "Scotland")
text_scotland_thisyear_unemployment_int <- subset(text_scotland_thisyear_unemployment_int, text_scotland_thisyear_unemployment_int$Year == max(unemployment_int_long$Year))
text_scotland_thisyear_unemployment_int <- text_scotland_thisyear_unemployment_int$Value
text_oecd_thisyear_unemployment_int <- subset(unemployment_int_long, unemployment_int_long$Country == "OECD - Total")
text_oecd_thisyear_unemployment_int <- subset(text_oecd_thisyear_unemployment_int, text_oecd_thisyear_unemployment_int$Year == max(unemployment_int_long$Year))
text_oecd_thisyear_unemployment_int <- text_oecd_thisyear_unemployment_int$Value
text_oecd_last5_unemployment_int <- subset(unemployment_int, unemployment_int$Country == "OECD - Total")
start_text_oecd_last5 <- as.numeric(max(unemployment_int_long$Year))
end_text_oecd_last5 <- start_text_oecd_last5-5
text_oecd_last5_unemployment_int <- text_oecd_last5_unemployment_int[,seq(end_text_oecd_last5:start_text_oecd_last5)]
text_oecd_last5_unemployment_int <- sum(text_oecd_last5_unemployment_int[1,-1])
text_oecd_last5_unemployment_int <- round(text_oecd_last5_unemployment_int/5, digits = 2)
text_scotland_last5_unemployment_int <- subset(unemployment_int, unemployment_int$Country == "Scotland")
start_text_scotland_last5 <- as.numeric(max(unemployment_int_long$Year))
end_text_scotland_last5 <- start_text_scotland_last5-5
text_scotland_last5_unemployment_int <- text_scotland_last5_unemployment_int[,seq(end_text_scotland_last5:start_text_scotland_last5)]
text_scotland_last5_unemployment_int <- sum(text_scotland_last5_unemployment_int[1,-1])
text_scotland_last5_unemployment_int <- round(text_scotland_last5_unemployment_int/5, digits = 2)

# yunemployment_int
yunemployment_int_long <- yunemployment_int %>% 
  gather("Year", "Value", 2:ncol(yunemployment_int))
yunemployment_int_wide <- yunemployment_int_long %>% 
  spread("Country", "Value", 2:3)
yunemployment_int_wide$Year <- as.integer(yunemployment_int_wide$Year)
positions_selected_countries_yunemployment_int <- which(names(yunemployment_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland'))
start_year_yunemployment_int <- min(yunemployment_int_long$Year)
end_year_yunemployment_int <- max(yunemployment_int_long$Year)

# PEOPLE ################################################################################################################################################
# lifeexpall_int
lifeexpall_int_long <- lifeexpall_int %>% 
  gather("Year", "Value", 2:ncol(lifeexpall_int))
lifeexpall_int_wide <- lifeexpall_int_long %>% 
  spread("Country", "Value", 2:3)
lifeexpall_int_wide$Year <- as.integer(lifeexpall_int_wide$Year)
positions_selected_countries_lifeexpall_int <- which(names(lifeexpall_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland'))
start_year_lifeexpall_int <- min(lifeexpall_int_long$Year)
end_year_lifeexpall_int <- max(lifeexpall_int_long$Year)

# lifeexpf_int


# lifeexpm_int


# hlifeexp_male_reg
start_year_hlifeexp_male_reg <- min(hlifeexp_male_reg$Year)
end_year_hlifeexp_male_reg <- max(hlifeexp_male_reg$Year)

# hlifeexp_female_reg
start_year_hlifeexp_female_reg <- min(hlifeexp_female_reg$Year)
end_year_hlifeexp_female_reg <- max(hlifeexp_female_reg$Year)

# rphousingc_sco
start_year_rphousingc_sco <- min(rphousingc_sco$Year)
end_year_rphousingc_sco <- max(rphousingc_sco$Year)

# mwellbeing_sco
start_year_mwellbeing_sco <- min(mwellbeing_sco$Year)
end_year_mwellbeing_sco <- max(mwellbeing_sco$Year)


# rank_lifeexpall
rank_lifeexpall <- subset(lifeexpall_int_long, lifeexpall_int_long$Country != "European Union 28")
rank_lifeexpall <- subset(rank_lifeexpall, rank_lifeexpall$Country != "OECD countries")
rank_lifeexpall <- subset(rank_lifeexpall, rank_lifeexpall$Country != "Euro area (19 countries)")
rank_lifeexpall <- subset(rank_lifeexpall, rank_lifeexpall$Year == max(rank_lifeexpall$Year))
rank_lifeexpall <- rank_lifeexpall[,c(1,3)]
rank_lifeexpall <- rank_lifeexpall[order(rank_lifeexpall$Value, decreasing = TRUE),]
rank_lifeexpall$Rank <- order(rank_lifeexpall$Value, decreasing = TRUE)
rank_lifeexpall_quantiles <- rank_lifeexpall$Rank
rank_lifeexpall$Quantile <- ecdf(rank_lifeexpall_quantiles)(rank_lifeexpall_quantiles)
rank_lifeexpall <- subset(rank_lifeexpall, rank_lifeexpall$Country == "Scotland")
rank_lifeexpall_quantile <- rank_lifeexpall$Quantile
rank_lifeexpall <- rank_lifeexpall$Rank

# lifeexpall_overview_int
lifeexpall_overview_int <- subset(lifeexpall_int_long, lifeexpall_int_long$Country %in% comparison_countries)
lifeexpall_overview_int <- subset(lifeexpall_overview_int, lifeexpall_overview_int$Year == max(lifeexpall_overview_int$Year))
lifeexpall_overview_int <- lifeexpall_overview_int[,c(1,3)]
lifeexpall_overview_int <- lifeexpall_overview_int[order(lifeexpall_overview_int$Value, decreasing = FALSE),]
lifeexpall_overview_int$Country <- factor(lifeexpall_overview_int$Country, levels = lifeexpall_overview_int$Country)

# text for lifeexpall_overview_int
text_scotland_thisyear_lifeexpall_int <- subset(lifeexpall_int_long, lifeexpall_int_long$Country == "Scotland")
text_scotland_thisyear_lifeexpall_int <- subset(text_scotland_thisyear_lifeexpall_int, text_scotland_thisyear_lifeexpall_int$Year == max(lifeexpall_int_long$Year))
text_scotland_thisyear_lifeexpall_int <- text_scotland_thisyear_lifeexpall_int$Value
text_oecd_thisyear_lifeexpall_int <- subset(lifeexpall_int_long, lifeexpall_int_long$Country == "OECD - Total")
text_oecd_thisyear_lifeexpall_int <- subset(text_oecd_thisyear_lifeexpall_int, text_oecd_thisyear_lifeexpall_int$Year == max(lifeexpall_int_long$Year))
text_oecd_thisyear_lifeexpall_int <- text_oecd_thisyear_lifeexpall_int$Value
text_oecd_last5_lifeexpall_int <- subset(lifeexpall_int, lifeexpall_int$Country == "OECD - Total")
start_text_oecd_last5 <- as.numeric(max(lifeexpall_int_long$Year))
end_text_oecd_last5 <- start_text_oecd_last5-5
text_oecd_last5_lifeexpall_int <- text_oecd_last5_lifeexpall_int[,seq(end_text_oecd_last5:start_text_oecd_last5)]
text_oecd_last5_lifeexpall_int <- sum(text_oecd_last5_lifeexpall_int[1,-1])
text_oecd_last5_lifeexpall_int <- round(text_oecd_last5_lifeexpall_int/5, digits = 2)
text_scotland_last5_lifeexpall_int <- subset(lifeexpall_int, lifeexpall_int$Country == "Scotland")
start_text_scotland_last5 <- as.numeric(max(lifeexpall_int_long$Year))
end_text_scotland_last5 <- start_text_scotland_last5-5
text_scotland_last5_lifeexpall_int <- text_scotland_last5_lifeexpall_int[,seq(end_text_scotland_last5:start_text_scotland_last5)]
text_scotland_last5_lifeexpall_int <- sum(text_scotland_last5_lifeexpall_int[1,-1])
text_scotland_last5_lifeexpall_int <- round(text_scotland_last5_lifeexpall_int/5, digits = 2)





# PLACE #################################################################################################################################################
# broadband_int
broadband_int_long <- broadband_int %>% 
  gather("Year", "Value", 2:ncol(broadband_int))
broadband_int_wide <- broadband_int_long %>% 
  spread("Country", "Value", 2:3)
broadband_int_wide$Year <- as.integer(broadband_int_wide$Year)
positions_selected_countries_broadband_int <- which(names(broadband_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland'))
start_year_broadband_int <- min(broadband_int_long$Year)
end_year_broadband_int <- max(broadband_int_long$Year)

# blueorgreen_reg
start_year_blueorgreen_reg <- min(blueorgreen_reg$Year)
end_year_blueorgreen_reg <- max(blueorgreen_reg$Year)

# pubservsat_reg 
start_year_pubservsat_reg  <- min(pubservsat_reg $Year)
end_year_pubservsat_reg  <- max(pubservsat_reg $Year)

# broadband_reg
start_year_broadband_reg <- min(broadband_reg$Year)
end_year_broadband_reg <- max(broadband_reg$Year)

# greenandbluespace_sco
start_year_greenandbluespace_sco <- min(greenandbluespace_sco$Year)
end_year_greenandbluespace_sco <- max(greenandbluespace_sco$Year)



# broadband_int
broadband_int_long <- broadband_int %>% 
  gather("Year", "Value", 2:ncol(broadband_int))
broadband_int_wide <- broadband_int_long %>% 
  spread("Country", "Value", 2:3)
broadband_int_wide$Year <- as.integer(broadband_int_wide$Year)
positions_selected_countries_broadband_int <- which(names(broadband_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'EU', 'Scotland'))
start_year_broadband_int <- min(broadband_int_long$Year)
end_year_broadband_int <- max(broadband_int_long$Year)

# broadband_overview_int
broadband_overview_int <- subset(broadband_int_long, broadband_int_long$Country %in% comparison_countries_eu)
broadband_overview_int <- subset(broadband_overview_int, broadband_overview_int$Year == max(broadband_overview_int$Year))
broadband_overview_int <- broadband_overview_int[,c(1,3)]
broadband_overview_int <- broadband_overview_int[order(broadband_overview_int$Value, decreasing = FALSE),]
broadband_overview_int$Country <- factor(broadband_overview_int$Country, levels = broadband_overview_int$Country)

# EU bar - broadband
rank_broadband <- subset(broadband_int_long, broadband_int_long$Country != "EU")
rank_broadband <- subset(broadband_int_long, broadband_int_long$Country != "Scotland")
rank_broadband <- subset(broadband_int_long, broadband_int_long$Country != "Switzerland")
rank_broadband <- subset(broadband_int_long, broadband_int_long$Country != "Norway")
rank_broadband <- subset(broadband_int_long, broadband_int_long$Country != "Iceland")
rank_broadband <- subset(rank_broadband, rank_broadband$Year == max(rank_broadband$Year))
rank_broadband <- rank_broadband[,c(1,3)]
rank_broadband <- rank_broadband[order(rank_broadband$Value, decreasing = TRUE),]
rank_broadband$Rank <- order(rank_broadband$Value, decreasing = TRUE)
rank_broadband_quantiles <- rank_broadband$Rank
rank_broadband$Quantile <- ecdf(rank_broadband_quantiles)(rank_broadband_quantiles)
rank_broadband <- subset(rank_broadband, rank_broadband$Country == "Scotland")
rank_broadband_quantile <- rank_broadband$Quantile
rank_broadband <- rank_broadband$Rank

# text for broadband_overview_int
text_scotland_thisyear_broadband_int <- subset(broadband_int_long, broadband_int_long$Country == "Scotland")
text_scotland_thisyear_broadband_int <- subset(text_scotland_thisyear_broadband_int, text_scotland_thisyear_broadband_int$Year == max(broadband_int_long$Year))
text_scotland_thisyear_broadband_int <- text_scotland_thisyear_broadband_int$Value
text_eu_thisyear_broadband_int <- subset(broadband_int_long, broadband_int_long$Country == "EU")
text_eu_thisyear_broadband_int <- subset(text_eu_thisyear_broadband_int, text_eu_thisyear_broadband_int$Year == max(broadband_int_long$Year))
text_eu_thisyear_broadband_int <- text_eu_thisyear_broadband_int$Value







# SUSTAINABILITY ########################################################################################################################################
# ggemissions_int
ggemissions_int_long <- ggemissions_int %>% 
  gather("Year", "Value", 2:ncol(ggemissions_int))
ggemissions_int_wide <- ggemissions_int_long %>% 
  spread("Country", "Value", 2:3)
ggemissions_int_wide$Year <- as.integer(ggemissions_int_wide$Year)
positions_selected_countries_ggemissions_int <- which(names(ggemissions_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'UK', 'Scotland'))
start_year_ggemissions_int <- min(ggemissions_int_long$Year)
end_year_ggemissions_int <- max(ggemissions_int_long$Year)

# cfootprint_sco
start_year_cfootprint_sco <- min(cfootprint_sco$Year)
end_year_cfootprint_sco <- max(cfootprint_sco$Year)

# naturalf_sco
start_year_naturalf_sco <- min(naturalf_sco$Year)
end_year_naturalf_sco <- max(naturalf_sco$Year)

# gasemissions_sco
start_year_gasemissions_sco <- min(gasemissions_sco$Year)
end_year_gasemissions_sco <- max(gasemissions_sco$Year)

# ggemissions2_int
ggemissions2_int_long <- ggemissions2_int %>% 
  gather("Year", "Value", 2:ncol(ggemissions2_int))
ggemissions2_int_wide <- ggemissions2_int_long %>% 
  spread("Country", "Value", 2:3)
ggemissions2_int_wide$Year <- as.integer(ggemissions2_int_wide$Year)
positions_selected_countries_ggemissions2_int <- which(names(ggemissions2_int_wide) %in% c('Denmark', 'Finland', 'Iceland', 'Sweden', 'Norway', 'Switzerland', 'New Zealand', 'Netherlands', 'Belgium', 'Ireland', 'EU', 'Scotland'))
start_year_ggemissions2_int <- min(ggemissions2_int_long$Year)
end_year_ggemissions2_int <- max(ggemissions2_int_long$Year)

# ggemissions2_overview_int
ggemissions2_overview_int <- subset(ggemissions2_int_long, ggemissions2_int_long$Country %in% comparison_countries_eu)
ggemissions2_overview_int <- subset(ggemissions2_overview_int, ggemissions2_overview_int$Year == max(ggemissions2_overview_int$Year))
ggemissions2_overview_int <- ggemissions2_overview_int[,c(1,3)]
ggemissions2_overview_int <- ggemissions2_overview_int[order(ggemissions2_overview_int$Value, decreasing = FALSE),]
ggemissions2_overview_int$Country <- factor(ggemissions2_overview_int$Country, levels = ggemissions2_overview_int$Country)

# EU bar - ggemissions2
rank_ggemissions2 <- subset(ggemissions2_int_long, ggemissions2_int_long$Country != "EU")
rank_ggemissions2 <- subset(rank_ggemissions2, rank_ggemissions2$Year == max(rank_ggemissions2$Year))
rank_ggemissions2 <- rank_ggemissions2[,c(1,3)]
rank_ggemissions2 <- rank_ggemissions2[order(rank_ggemissions2$Value, decreasing = TRUE),]
rank_ggemissions2$Rank <- order(rank_ggemissions2$Value, decreasing = TRUE)
rank_ggemissions2_quantiles <- rank_ggemissions2$Rank
rank_ggemissions2$Quantile <- ecdf(rank_ggemissions2_quantiles)(rank_ggemissions2_quantiles)
rank_ggemissions2 <- subset(rank_ggemissions2, rank_ggemissions2$Country == "Scotland")
rank_ggemissions2_quantile <- rank_ggemissions2$Quantile
rank_ggemissions2 <- rank_ggemissions2$Rank

# text for ggemissions2_overview_int
text_scotland_thisyear_ggemissions2_int <- subset(ggemissions2_int_long, ggemissions2_int_long$Country == "Scotland")
text_scotland_thisyear_ggemissions2_int <- subset(text_scotland_thisyear_ggemissions2_int, text_scotland_thisyear_ggemissions2_int$Year == max(ggemissions2_int_long$Year))
text_scotland_thisyear_ggemissions2_int <- text_scotland_thisyear_ggemissions2_int$Value
text_eu_thisyear_ggemissions2_int <- subset(ggemissions2_int_long, ggemissions2_int_long$Country == "EU")
text_eu_thisyear_ggemissions2_int <- subset(text_eu_thisyear_ggemissions2_int, text_eu_thisyear_ggemissions2_int$Year == max(ggemissions2_int_long$Year))
text_eu_thisyear_ggemissions2_int <- text_eu_thisyear_ggemissions2_int$Value

# EQUALITIES DASHBOARD ####
# text figures for Gender
gender_year <- max(gpaygap_eq$Year)
gender_last_figure <- subset(gpaygap_eq, gpaygap_eq$Year == gender_year)
gender_last_figure <- gender_last_figure$Value
gender_year_start <- gender_year-5
gender_5years_figure <- subset(gpaygap_eq, gpaygap_eq$Year > gender_year_start)
gender_5years_figure <- gender_5years_figure$Value
gender_5years_figure <- sum(gender_5years_figure)
gender_5years_figure <- round(gender_5years_figure/5, digits = 2)

# text figures for Disability
disability_year <- max(dempgap_eq$Year)
disability_last_figure <- subset(dempgap_eq, dempgap_eq$Year == disability_year)
disability_last_figure <- disability_last_figure$Value
disability_year_start <- disability_year-5
disability_5years_figure <- subset(dempgap_eq, dempgap_eq$Year > disability_year_start)
disability_5years_figure <- disability_5years_figure$Value
disability_5years_figure <- sum(disability_5years_figure)
disability_5years_figure <- round(disability_5years_figure/5, digits = 2)

# text figures for Age
age_year <- max(youthunemp_eq$Year)
age_last_figure <- subset(youthunemp_eq, youthunemp_eq$Year == age_year)
age_last_figure <- age_last_figure$Value
age_year_start <- age_year-5
age_5years_figure <- subset(youthunemp_eq, youthunemp_eq$Year > age_year_start)
age_5years_figure <- age_5years_figure$Value
age_5years_figure <- sum(age_5years_figure)
age_5years_figure <- round(age_5years_figure/5, digits = 2)

# text figures for Ethnicity
ethnicity_year <- max(ethnicmgap_eq$Year)
ethnicity_last_figure <- subset(ethnicmgap_eq, ethnicmgap_eq$Year == ethnicity_year)
ethnicity_last_figure <- ethnicity_last_figure$Value
ethnicity_year_start <- ethnicity_year-5
ethnicity_5years_figure <- subset(ethnicmgap_eq, ethnicmgap_eq$Year > ethnicity_year_start)
ethnicity_5years_figure <- ethnicity_5years_figure$Value
ethnicity_5years_figure <- sum(ethnicity_5years_figure)
ethnicity_5years_figure <- round(ethnicity_5years_figure/5, digits = 2)

growth_plain_sco <- read.csv("./www/scotland/growth_plain_sco.csv", check.names=FALSE)
exporting_plain_sco <- read.csv("./www/scotland/exporting_plain_sco.csv", check.names=FALSE)
rd_plain_sco <- read.csv("./www/scotland/rd_plain_sco.csv", check.names=FALSE)









