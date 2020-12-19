# Enterprise and Skills Dashboard
The Economy Board requested that OCEA lead on developing a performance framework for the Economy Board, taking into account the framework developed for the Enterprise and Skills Strategic Board.

# Enetrprise and Skills Dashboard

## 📦 Packages
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

## 📖 Updating with new data
Link to the Word document describing the process of updating the dashboard with new data [here](https://github.com/DataScienceScotland/sg-enterprise-and-skills-dashboard/blob/master/Updating%20ESD.docx).

## 🔗 Links
* [Enetrprise and Skills Dashboard (This shiny app)](https://scotland.shinyapps.io/sg-enterprise-and-skills-dashboard/)
* [Open Data Platform)](https://statistics.gov.scot/data/search)

## 📧 Contact
[![](https://img.shields.io/twitter/url?label=/SzymkowskiDev&style=social&url=https%3A%2F%2Ftwitter.com%2FSzymkowskiDev)](https://twitter.com/SzymkowskiDev) [![](https://img.shields.io/twitter/url?label=/kamil-szymkowski/&logo=linkedin&logoColor=%230077B5&style=social&url=https%3A%2F%2Fwww.linkedin.com%2Fin%2Fkamil-szymkowski%2F)](https://www.linkedin.com/in/kamil-szymkowski/) [![](https://img.shields.io/twitter/url?label=@szymkowskidev&logo=medium&logoColor=%23292929&style=social&url=https%3A%2F%2Fmedium.com%2F%40szymkowskidev)](https://medium.com/@szymkowskidev) [![](https://img.shields.io/twitter/url?label=/SzymkowskiDev&logo=github&logoColor=%23292929&style=social&url=https%3A%2F%2Fgithub.com%2FSzymkowskiDev)](https://github.com/SzymkowskiDev)
