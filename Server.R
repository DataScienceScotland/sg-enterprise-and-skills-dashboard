#### Enterprise and Skills Dashboard Server Page ####

### Beginning of the Server ####
shinyServer(function(input, output, session) {
  #### Admin Settings (Using javascript, stopping the app when the window is closed, and R options) ####
  useShinyjs()
  session$onSessionEnded(stopApp) ##Stops the app when the browser window is closed
  options(stringsAsFactors = FALSE, scipen = 999)
  
  #######################################################################
  #### Listening to icons to return to the home page ####
  observeEvent(input$homeButton, ignoreInit = TRUE, {
    updateNavbarPage(session, inputId = "mainMenu1", selected = "Home")
  })
  observeEvent(input$titleDash, ignoreInit = TRUE, {
    updateNavbarPage(session, inputId = "mainMenu1", selected = "Home")
  })
  #######################################################################
  #### Listening to Links on Home Page to get to relevant pages ####
  observeEvent(input$homeProductivityLink, ignoreInit = TRUE,{
    updateNavbarPage(session, inputId = "mainMenu1", selected = "productivitySummary")
  })
  
  observeEvent(input$homeEqualityLink, ignoreInit = TRUE,{
    updateNavbarPage(session, inputId = "mainMenu1", selected = "equalitySummary")
  })
  
  observeEvent(input$homeWellnessLink, ignoreInit = TRUE,{
    updateNavbarPage(session, inputId = "mainMenu1", selected = "wellbeingSummary")
  })
  
  observeEvent(input$homeSustainabilityLink, ignoreInit = TRUE,{
    updateNavbarPage(session, inputId = "mainMenu1", selected = "sustainabilitySummary")
  })
  #######################################################################
  #### Second Page Processing ####-----------------------------------####
  ### Productivity ###
  productivitySecondPage <- npf_data %>% 
    filter(Indicator %in% productivity_indicators) %>% 
    distinct()
  
  callModule(ES_secondPage_Server, "Productivity", dataset = productivitySecondPage)
  
  ### Equality ###
  equalitySecondPage <- npf_data %>% 
    filter(Indicator %in% equality_indicators) %>% 
    distinct()
  
  callModule(ES_secondPage_Server, "Equality", dataset = equalitySecondPage)
  
  ### Wellbeing ###
  wellbeingSecondPage <- npf_data %>% 
    filter(Indicator %in% wellbeing_indicators) %>% 
    distinct()
  
  callModule(ES_secondPage_Server, "Wellbeing", dataset = wellbeingSecondPage)
  
  ### Sustainability ###
  sustainabilitySecondPage <- npf_data %>% 
    filter(Indicator %in% sustainability_indicators) %>% 
    distinct()
  
  callModule(ES_secondPage_Server, "Sustainability", dataset = sustainabilitySecondPage)
  
  #### Responding to any clicking on tables clicked in the 2nd page ####
  observeEvent(input$doubleClick, ignoreInit = TRUE,{
    ## Getting Indicator Name ##
    indicatorname <- input$doubleClick[3]
    indicatorname <- gsub("'","",indicatorname,fixed = TRUE)
    indicatorname <- gsub(" ","_",indicatorname,fixed = TRUE)
    indicatorname <- paste(indicatorname,"page",sep = "_")
    
    ## Jumping to Relevant Page ##
    updateNavbarPage(session, "mainMenu1", selected = indicatorname)
  })
  #######################################################################
  #### Third Page Processing ####
  #### Productivity ####
  ## Access to Superfast Broadband ##
  observe({
    if (input$mainMenu1 == "Access_to_superfast_broadband_page") {
      callModule(ES_thirdPage_Server,id =  "accessBroadband", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "ACCESS TO SUPERFAST BROADBAND"),
                 ID_name = "accessBroadband", 
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Economic Growth ##
  observe({
    if (input$mainMenu1 == "Economic_growth_page") {
      callModule(ES_thirdPage_Server,id =  "econGrowth", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "ECONOMIC GROWTH"),
                 ID_name = "econGrowth",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Economic Participation ##
  observe({
    if (input$mainMenu1 == "Economic_participation_page") {
      callModule(ES_thirdPage_Server,id =  "econParticipate", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "ECONOMIC PARTICIPATION"),
                 ID_name = "econParticipate",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Educational Attainment 7 ##
  observe({
    if (input$mainMenu1 == "Educational_attainment_7_page") {
      callModule(ES_thirdPage_Server,id =  "eduAttain7", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "EDUCATIONAL ATTAINMENT 7"),
                 ID_name = "eduAttain7",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Entrepreneurial Activity - International Comparator Available ##
  observe({
    if (input$mainMenu1 == "Entrepreneurial_activity_page") {
      callModule(ES_thirdPage_Server,id =  "entAct", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "ENTREPRENEURIAL ACTIVITY"),
                 ID_name = "entAct", 
                 internationalData = TEA_data,
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## High Growth Businesses ##
  observe({
    if (input$mainMenu1 == "High_growth_businesses_page") {
      callModule(ES_thirdPage_Server,id =  "highGrowth", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "HIGH GROWTH BUSINESSES"),
                 ID_name = "highGrowth",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Innovative Businesses ##
  observe({
    if (input$mainMenu1 == "Innovative_businesses_page") {
      callModule(ES_thirdPage_Server,id =  "innovateBus", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "INNOVATIVE BUSINESSES"),
                 ID_name = "innovateBus",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## International Exporting ##
  observe({
    if (input$mainMenu1 == "International_exporting_page") {
      callModule(ES_thirdPage_Server,id =  "intExp", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "INTERNATIONAL EXPORTING"),
                 ID_name = "intExp",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Productivity - International Comparator Available ##
  observe({
    if (input$mainMenu1 == "Productivity_page") {
      callModule(ES_thirdPage_Server,id =  "productivity", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "PRODUCTIVITY"),
                 ID_name = "productivity", 
                 internationalData = productivity_data,
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Scotland's Reputation ##
  observe({
    if (input$mainMenu1 == "Scotlands_Reputation_page") {
      callModule(ES_thirdPage_Server,id =  "scotRep", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "SCOTLAND'S REPUTATION"),
                 ID_name = "scotRep",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Skill Profile of the Population ##
  observe({
    if (input$mainMenu1 == "Skill_profile_of_the_population_page") {
      callModule(ES_thirdPage_Server,id =  "skillProfile", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "SKILL PROFILE OF THE POPULATION"),
                 ID_name = "skillProfile",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Skills Shortage Vacancies ##
  observe({
    if (input$mainMenu1 == "Skills_shortage_vacancies_page") {
      callModule(ES_thirdPage_Server,id =  "skillShort", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "SKILLS SHORTAGE VACANCIES"),
                 ID_name = "skillShort",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Skills Underutilisation ##
  observe({
    if (input$mainMenu1 == "Skills_underutilisation_page") {
      callModule(ES_thirdPage_Server,id =  "skillUnder", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "SKILLS UNDERUTILISATION"),
                 ID_name = "skillUnder",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Spend on Research and Development - International Comprator Available ##
  observe({
    if (input$mainMenu1 == "Spend_on_Research_and_Development_page") {
      callModule(ES_thirdPage_Server,id =  "spendRD", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "SPEND ON RESEARCH AND DEVELOPMENT"),
                 ID_name = "spendRD", 
                 internationalData = GERD_data,
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## The Number of Businesses ##
  observe({
    if (input$mainMenu1 == "The_number_of_businesses_page") {
      callModule(ES_thirdPage_Server, id =  "numberBusiness", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "THE NUMBER OF BUSINESSES"),
                 ID_name = "numberBusiness",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Work Place Learning ##
  observe({
    if (input$mainMenu1 == "Work_place_learning_page") {
      callModule(ES_thirdPage_Server, id =  "workLearn", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "WORK PLACE LEARNING"),
                 ID_name = "workLearn",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Young People's Participation ##
  observe({
    if (input$mainMenu1 == "Young_peoples_participation_page") {
      callModule(ES_thirdPage_Server, id =  "youngParticipate", 
                 dataset = productivitySecondPage %>% 
                   filter(toupper(Indicator) == "YOUNG PEOPLE'S PARTICIPATION"),
                 ID_name = "youngParticipate",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  #### Equality ####
  ## Gender Balance in Organisations ##
  observe({
    if (input$mainMenu1 == "Gender_balance_in_organisations_page") {
      callModule(ES_thirdPage_Server, id =  "genderBalance", 
                 dataset = equalitySecondPage %>% 
                   filter(toupper(Indicator) == "GENDER BALANCE IN ORGANISATIONS"),
                 ID_name = "genderBalance",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Income Inequality ##
  observe({
    if (input$mainMenu1 == "Income_inequality_page") {
      callModule(ES_thirdPage_Server,id =  "incomeInequal", 
                 dataset = equalitySecondPage %>% 
                   filter(toupper(Indicator) == "INCOME INEQUALITY"),
                 ID_name = "incomeInequal",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Pay Gap ##
  observe({
    if (input$mainMenu1 == "Pay_gap_page") {
      callModule(ES_thirdPage_Server,id =  "payGap", 
                 dataset = equalitySecondPage %>% 
                   filter(toupper(Indicator) == "PAY GAP"),
                 ID_name = "payGap",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  #### Wellbeing ####
  ## Employee Voice ##
  observe({
    if (input$mainMenu1 == "Employee_voice_page") {
      callModule(ES_thirdPage_Server,id =  "employeeVoice", 
                 dataset = wellbeingSecondPage %>% 
                   filter(toupper(Indicator) == "EMPLOYEE VOICE"),
                 ID_name = "employeeVoice",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Employees on the Living Wage ##
  observe({
    if (input$mainMenu1 == "Employees_on_the_Living_wage_page") {
      callModule(ES_thirdPage_Server,id =  "empLivingWage", 
                 dataset = wellbeingSecondPage %>% 
                   filter(toupper(Indicator) == "EMPLOYEES ON THE LIVING WAGE"),
                 ID_name = "empLivingWage",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Mental Wellbeing ##
  observe({
    if (input$mainMenu1 == "Mental_wellbeing_page") {
      callModule(ES_thirdPage_Server,id =  "mentalWellbeing", 
                 dataset = wellbeingSecondPage %>% 
                   filter(toupper(Indicator) == "MENTAL WELLBEING"),
                 ID_name = "mentalWellbeing",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Social Capital ##
  observe({
    if (input$mainMenu1 == "Social_capital_page") {
      callModule(ES_thirdPage_Server,id =  "socialCapital", 
                 dataset = wellbeingSecondPage %>% 
                   filter(toupper(Indicator) == "SOCIAL CAPITAL"),
                 ID_name = "socialCapital",
                 CurrentPage = reactive({input$mainMenu1})
      )
    } 
  })
  
  #### Sustainability ####
  ## Carbon Footprint ##
  observe({
    if (input$mainMenu1 == "Carbon_footprint_page") {
      callModule(ES_thirdPage_Server,id =  "carbonFoot", 
                 dataset = sustainabilitySecondPage %>% 
                   filter(toupper(Indicator) == "CARBON FOOTPRINT"),
                 ID_name = "carbonFoot",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  ## Energy From Renewable Sources ##
  observe({
    if (input$mainMenu1 == "Energy_from_renewable_sources_page") {
      callModule(ES_thirdPage_Server,id = "energyRenew", 
                 dataset = sustainabilitySecondPage %>% 
                   filter(toupper(Indicator) == "ENERGY FROM RENEWABLE SOURCES"),
                 ID_name = "energyRenew",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Greenhouse Gas Emissions ##
  observe({
    if (input$mainMenu1 == "Greenhouse_gas_emissions_page") {
      callModule(ES_thirdPage_Server,id =  "greenGas", 
                 dataset = sustainabilitySecondPage %>% 
                   filter(toupper(Indicator) == "GREENHOUSE GAS EMISSIONS"),
                 ID_name = "greenGas",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  ## Natural Capital ##
  observe({
    if (input$mainMenu1 == "Natural_capital_page") {
      callModule(ES_thirdPage_Server,id =  "natCap", 
                 dataset = sustainabilitySecondPage %>% 
                   filter(toupper(Indicator) == "NATURAL CAPITAL"),
                 ID_name = "natCap",
                 CurrentPage = reactive({input$mainMenu1})
      )
    }
  })
  
  
  ## KAMIL #####
  
  # PRODUCTIVITY ###################################################################################################################################
  # PRODUCTIVITY OVERVIEW
  # Horizontal bar - growth
  output$growth_bar <- renderUI({
    if (rank_growth_quantile >= 0.75) {
      column(width = 2,
             tags$b("Growth rate: "),
             tags$p(as.character(rank_growth), style="font-size: 48px"),
             style="background-color: red;"
      )
    } else if (rank_growth_quantile <= 0.25) {
      column(width = 2,
             tags$b("Growth rate: "),
             tags$p(as.character(rank_growth), style="font-size: 48px"),
             style="background-color: green;"
      )
    } else {
      column(width = 2,
             tags$b("Growth rate: "),
             tags$p(as.character(rank_growth), style="font-size: 48px"),
             style="background-color: orange;"
      )
    }
  })
  # Horizontal bar - productivity
  output$productivity_bar <- renderUI({
    if (rank_productivity_quantile >= 0.75) {
      column(width = 2,
             tags$b("Productivity: "),
             tags$p(as.character(rank_productivity), style="font-size: 48px"),
             style="background-color: red;"
      )
    } else if (rank_productivity_quantile <= 0.25) {
      column(width = 2,
             tags$b("Productivity: "),
             tags$p(as.character(rank_productivity), style="font-size: 48px"),
             style="background-color: green;"
      )
    } else {
      column(width = 2,
             tags$b("Productivity: "),
             tags$p(as.character(rank_productivity), style="font-size: 48px"),
             style="background-color: orange;"
      )
    }
  })
  # Horizontal bar - exporting
  output$exporting_bar <- renderUI({
    if (rank_exporting_quantile >= 0.75) {
      column(width = 2,
             tags$b("Exporting: "),
             tags$p(as.character(rank_exporting), style="font-size: 48px"),
             style="background-color: red;"
      )
    } else if (rank_exporting_quantile <= 0.25) {
      column(width = 2,
             tags$b("Exporting: "),
             tags$p(as.character(rank_exporting), style="font-size: 48px"),
             style="background-color: green;"
      )
    } else {
      column(width = 2,
             tags$b("Exporting: "),
             tags$p(as.character(rank_exporting), style="font-size: 48px"),
             style="background-color: orange;"
      )
    }
  })
  # Horizontal bar - rd
  output$rd_bar <- renderUI({
    if (rank_rd_quantile >= 0.75) {
      column(width = 2,
             tags$b("R&D: "),
             tags$p(as.character(rank_rd), style="font-size: 48px"),
             style="background-color: red;"
      )
    } else if (rank_rd_quantile <= 0.25) {
      column(width = 2,
             tags$b("R&D: "),
             tags$p(as.character(rank_rd), style="font-size: 48px"),
             style="background-color: green;"
      )
    } else {
      column(width = 2,
             tags$b("R&D: "),
             tags$p(as.character(rank_rd), style="font-size: 48px"),
             style="background-color: orange;"
      )
    }
  })
  # Horizontal bar - entrepreneurialism
  output$entrepreneurialism_bar <- renderUI({
    if (rank_entrepreneurialism_quantile >= 0.75) {
      column(width = 2,
             tags$b("Entrepreneurialism: "),
             tags$p(as.character(rank_entrepreneurialism), style="font-size: 48px"),
             style="background-color: red;"
      )
    } else if (rank_entrepreneurialism_quantile <= 0.25) {
      column(width = 2,
             tags$b("Entrepreneurialism: "),
             tags$p(as.character(rank_entrepreneurialism), style="font-size: 48px"),
             style="background-color: green;"
      )
    } else {
      column(width = 2,
             tags$b("Entrepreneurialism: "),
             tags$p(as.character(rank_entrepreneurialism), style="font-size: 48px"),
             style="background-color: orange;"
      )
    }
  })
  
  
  # ggplot barplot for growth_overview
  output$growth_overview_int_barplot <- renderPlot({
    growth_overview_int2 <- growth_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(growth_overview_int$Country == "Scotland","blue","grey"))
    ggplot(growth_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(growth_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("GDP growth rate") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  # ggplot barplot for exporting_overview
  output$exporting_overview_int_barplot <- renderPlot({
    exporting_overview_int2 <- exporting_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(exporting_overview_int$Country == "Scotland (international and RUK)","blue","grey"))
    ggplot(exporting_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(exporting_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Exports as % of GDP") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  # ggplot barplot for productivity_overview
  output$productivity_overview_int_barplot <- renderPlot({
    productivity_overview_int2 <- productivity_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(productivity_overview_int$Country == "Scotland","blue","grey"))
    ggplot(productivity_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(productivity_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="% of USA") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Productivity (USA=100)") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  # ggplot lineplot for reputation_overview_sco
  output$reputation_overview_sco_lineplot <- renderPlot({
    ggplot(reputation_overview_sco) +
      geom_line(stat="identity", aes(x=reputation_overview_sco$Value, y=reputation_overview_sco$Year), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      coord_flip(clip = "off", expand = TRUE) +
      labs(x = "NBI Index", y = "") +
      theme_minimal() +
      ggtitle("Anholt GfK-Roper Nation Brands Index (NBI)") +
      scale_color_jama() +
      geom_point(aes(x=reputation_overview_sco$Value, y=reputation_overview_sco$Year), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=reputation_overview_sco$Value, y=reputation_overview_sco$Year, label=reputation_overview_sco$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # ggplot barplot for rd_overview_int
  output$rd_overview_int_barplot <- renderPlot({
    rd_overview_int2 <- rd_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(rd_overview_int$Country == "Scotland","blue","grey"))
    ggplot(rd_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(rd_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("% of GDP spent on R&D") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  # Pie chart
  # # ggplot pieplot for rd_overview_sco
  # rd_overview_sco2 <- rd_overview_sco %>%
  #   mutate(Performer = (Performer),
  #          Value = as.numeric(Value),
  #          fill_type = ifelse(rd_overview_sco$Performer == "Government incl. Research Councils","blue","grey")) 
  # output$rd_overview_sco_pieplot <- renderPlot({
  #   ggplot(rd_overview_sco2, aes(x="", y=Value, fill=Performer)) +
  #     geom_bar(stat="identity", width=1, show.legend = TRUE, color="white") +
  #     coord_polar("y", start=0) +
  #     theme_void() +
  #     ggtitle("Breakdown of Scotland's R&D Expenditure") +
  #     guides(fill=guide_legend(title="Performer",reverse = TRUE)) +
  #     geom_text(aes(label = paste(round(Value / sum(Value) * 100, 1), "%", sep=""), x=1.75),
  #               position = position_stack(vjust = 0.5))
  # })
  
  # PRODUCTIVITY DETAILS
  # Dygraph for growth_int
  output$growth_int_graph <- renderDygraph({
    dygraph(
      growth_int_wide[,c(1, as.numeric(input$growth_int_input))]
    ) %>%
      dyGroup(names(growth_int_wide)[c(1, as.numeric(input$growth_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "GDP Growth Rate") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_growth_int", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for productivity_int
  output$productivity_int_graph <- renderDygraph({
    dygraph(
      productivity_int_wide[,c(1, as.numeric(input$productivity_int_input))]
    ) %>%
      dyGroup(names(productivity_int_wide)[c(1, as.numeric(input$productivity_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "% of USA") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_productivity", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for exporting_int
  output$exporting_int_graph <- renderDygraph({
    dygraph(
      exporting_int_wide[,c(1, as.numeric(input$exporting_int_input))]
    ) %>%
      dyGroup(names(exporting_int_wide)[c(1, as.numeric(input$exporting_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "% of USA") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_exporting", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for rd_int
  output$rd_int_graph <- renderDygraph({
    dygraph(
      rd_int_wide[,c(1, as.numeric(input$rd_int_input))]
    ) %>%
      dyGroup(names(rd_int_wide)[c(1, as.numeric(input$rd_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "% of GDP") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_rd", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for entrepreneurialism_int
  output$entrepreneurialism_int_graph <- renderDygraph({
    dygraph(
      entrepreneurialism_int_wide[,c(1, as.numeric(input$entrepreneurialism_int_input))]
    ) %>%
      dyGroup(names(entrepreneurialism_int_wide)[c(1, as.numeric(input$entrepreneurialism_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "% of GDP") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_entrepreneurialism", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # ggplot barplot for entrepreneurialism2_overview_int
  output$entrepreneurialism2_overview_int_barplot <- renderPlot({
    entrepreneurialism2_overview_int <- entrepreneurialism2_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = "grey")
    ggplot(entrepreneurialism2_overview_int, aes(x=Country, y=Value)) +
      geom_col(fill=factor(entrepreneurialism2_overview_int$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Percent of 18-64 population entrepreneurs") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  
  # Dygraph for growth_sco
  output$growth_sco_graph <- renderDygraph({
    dygraph(
      growth_sco_wide[,c(1, as.numeric(input$growth_sco_input))]
    ) %>%
      dyGroup(names(growth_sco_wide)[c(1, as.numeric(input$growth_sco_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "GDP Growth Rate") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_growth_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for productivity_sco
  output$productivity_sco_graph <- renderDygraph({
    dygraph(
      productivity_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Output per hour") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_productivity_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for exporting_destination_sco
  output$exporting_destination_sco_graph <- renderDygraph({
    dygraph(
      exporting_destination_sco[,c(1, as.numeric(input$exporting_destination_sco_input))]
    ) %>%
      dyGroup(names(exporting_destination_sco)[c(1, as.numeric(input$exporting_destination_sco_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "£ million") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_exporting_destination_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for exporting_sector_sco
  output$exporting_sector_sco_graph <- renderDygraph({
    dygraph(
      exporting_sector_sco[,c(1, as.numeric(input$exporting_sector_sco_input))]
    ) %>%
      dyGroup(names(exporting_sector_sco)[c(1, as.numeric(input$exporting_sector_sco_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "£ million") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_exporting_sector_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for rd_sco
  output$rd_sco_graph <- renderDygraph({
    dygraph(
      rd_sco[,c(1, as.numeric(input$rd_sco_input))]
    ) %>%
      dyGroup(names(rd_sco)[c(1, as.numeric(input$rd_sco_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "GDP Growth Rate") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_rd_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for entrepreneurialism_sco
  output$entrepreneurialism_sco_graph <- renderDygraph({
    dygraph(
      entrepreneurialism_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "TEA rate") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_entrepreneurialism_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for reputation_sco
  output$reputation_sco_graph <- renderDygraph({
    dygraph(
      reputation_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "NBI") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_reputation_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for nbusiness_sector_sco
  output$nbusiness_sector_sco_graph <- renderDygraph({
    dygraph(
      nbusiness_sector_sco[,c(1, as.numeric(input$nbusiness_sector_sco_input))]
    ) %>%
      dyGroup(names(nbusiness_sector_sco)[c(1, as.numeric(input$nbusiness_sector_sco_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Number of businesses") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_nbusiness_sector_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for nbusiness_region_sco
  output$nbusiness_region_sco_graph <- renderDygraph({
    dygraph(
      nbusiness_region_sco[,c(1, as.numeric(input$nbusiness_region_sco_input))]
    ) %>%
      dyGroup(names(nbusiness_region_sco)[c(1, as.numeric(input$nbusiness_region_sco_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Number of businesses") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_nbusiness_region_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for nbusiness_employees_sco
  output$nbusiness_employees_sco_graph <- renderDygraph({
    dygraph(
      nbusiness_employees_sco[,c(1, as.numeric(input$nbusiness_employees_sco_input))]
    ) %>%
      dyGroup(names(nbusiness_employees_sco)[c(1, as.numeric(input$nbusiness_employees_sco_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Number of businesses") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_nbusiness_employees_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for hbusiness_percent_sco
  output$hbusiness_percent_sco_graph <- renderDygraph({
    dygraph(
      hbusiness_percent_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Percent of all stocks") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_hbusiness_percent_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Dygraph for ibusiness_sco
  output$ibusiness_sco_graph <- renderDygraph({
    dygraph(
      ibusiness_sco[,c(1, as.numeric(input$ibusiness_sco_input))]
    ) %>%
      dyGroup(names(ibusiness_sco)[c(1, as.numeric(input$ibusiness_sco_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Percent") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_ibusiness_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  # Leaflet map for growth_reg
  output$growth_reg_map_caption <- renderText({
    paste("Map 1. GVA per Head across Scottish council areas in ", as.character(input$growth_reg_input), " (£ per Head)", sep="")
  })
  growth_reg_map_data <- reactive({
    growth_reg_one <- growth_reg[ which(growth_reg$Year == input$growth_reg_input), ]
    growth_reg_one <- growth_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, growth_reg_one, by = intersect(names(mapex@data), names(growth_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_growth_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Growth, bins = 9)
    list_return <- list(mapex = mapex, choropleth_growth_reg = choropleth_growth_reg)
    return(list_return)
  })
  output$growth_reg_map <- renderLeaflet({
    mapex <- growth_reg_map_data()$mapex
    choropleth_growth_reg <- growth_reg_map_data()$choropleth_growth_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_growth_reg(mapex$Growth), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " £", as.character(mapex$Growth), " per Head", sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_growth_reg, values = mapex$Growth, title = paste("GVA per Head", " (£)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  # Leaflet map for exporting_reg
  output$exporting_reg_map_caption <- renderText({
    paste("Map 2. % of total Scottish exports across Scottish council areas in ", as.character(input$exporting_reg_input), sep="")
  })
  exporting_reg_map_data <- reactive({
    exporting_reg_one <- exporting_reg[ which(exporting_reg$Year == input$exporting_reg_input), ]
    exporting_reg_one <- exporting_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, exporting_reg_one, by = intersect(names(mapex@data), names(exporting_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_exporting_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Exporting, bins = 9)
    list_return <- list(mapex = mapex, choropleth_exporting_reg = choropleth_exporting_reg)
    return(list_return)
  })
  output$exporting_reg_map <- renderLeaflet({
    mapex <- exporting_reg_map_data()$mapex
    choropleth_exporting_reg <- exporting_reg_map_data()$choropleth_exporting_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex@data$NAME, fillColor = ~choropleth_exporting_reg(mapex@data$Exporting), fillOpacity=1, popup = ~paste(as.character(mapex@data$NAME), " ", as.character(mapex@data$Exporting), " %", sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_exporting_reg, values = mapex@data$Exporting, title = paste("% of total Scottish exports ", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  
  # Leaflet map for rd_reg
  output$rd_reg_map_caption <- renderText({
    paste("Map 3. BERD as share of GDP across Scottish council areas in ", as.character(input$rd_reg_input), sep="")
  })
  rd_reg_map_data <- reactive({
    rd_reg_one <- rd_reg[ which(rd_reg$Year == input$rd_reg_input), ]
    rd_reg_one <- rd_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, rd_reg_one, by = intersect(names(mapex@data), names(rd_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_rd_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$RD, bins = 9)
    list_return <- list(mapex = mapex, choropleth_rd_reg = choropleth_rd_reg)
    return(list_return)
  })
  output$rd_reg_map <- renderLeaflet({
    mapex <- rd_reg_map_data()$mapex
    choropleth_rd_reg <- rd_reg_map_data()$choropleth_rd_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex@data$NAME, fillColor = ~choropleth_rd_reg(mapex@data$RD), fillOpacity=1, popup = ~paste(as.character(mapex@data$NAME), " ", as.character(mapex@data$RD), " %", sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_rd_reg, values = mapex@data$RD, title = paste("Total BERD as share of GDP ", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  
  # Leaflet map for nbusiness_reg
  output$nbusiness_reg_map_caption <- renderText({
    paste("Map 4. The Number of Businesses across Scottish council areas in ", as.character(input$nbusiness_reg_input), sep="")
  })
  nbusiness_reg_map_data <- reactive({
    nbusiness_reg_one <- nbusiness_reg[ which(nbusiness_reg$Year == input$nbusiness_reg_input), ]
    nbusiness_reg_one <- nbusiness_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, nbusiness_reg_one, by = intersect(names(mapex@data), names(nbusiness_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_nbusiness_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Businesses, bins = 9)
    list_return <- list(mapex = mapex, choropleth_nbusiness_reg = choropleth_nbusiness_reg)
    return(list_return)
  })    
  output$nbusiness_reg_map <- renderLeaflet({
    mapex <- nbusiness_reg_map_data()$mapex
    choropleth_nbusiness_reg <- nbusiness_reg_map_data()$choropleth_nbusiness_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex@data$NAME, fillColor = ~choropleth_nbusiness_reg(mapex@data$Businesses), fillOpacity=1, popup = ~paste(as.character(mapex@data$NAME), " ", as.character(mapex@data$Businesses), " businesses", sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_nbusiness_reg, values = mapex@data$Businesses, title = paste("Number of businesses ", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  
  # POPULATION #####################################################################################################################################
  # POPULATION OVERVIEW
  # Horizontal bar - departio
  output$depratio_bar <- renderUI({
    if (rank_depratio_quantile >= 0.75) {
      column(width = 2,
             tags$b("Dependency ratio: "),
             tags$p(as.character(rank_depratio), style="font-size: 48px"),
             style="background-color: red;"
      )
    } else if (rank_depratio_quantile <= 0.25) {
      column(width = 2,
             tags$b("Dependency ratio: "),
             tags$p(as.character(rank_depratio), style="font-size: 48px"),
             style="background-color: green;"
      )
    } else {
      column(width = 2,
             tags$b("Dependency ratio: "),
             tags$p(as.character(rank_depratio), style="font-size: 48px"),
             style="background-color: orange;"
      )
    }
  })
  
  # ggplot barplot for depratio_overview
  output$depratio_overview_int_barplot <- renderPlot({
    depratio_overview_int2 <- depratio_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(depratio_overview_int$Country == "Scotland","blue","grey"))
    ggplot(depratio_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(depratio_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Age dependency ratio (% of working-age population)") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  # POPULATION DETAILS
  # Dygraph for depratio_int
  output$depratio_int_graph <- renderDygraph({
    dygraph(
      depratio_int_wide[,c(1, as.numeric(input$depratio_int_input))]
    ) %>%
      dyGroup(names(depratio_int_wide)[c(1, as.numeric(input$depratio_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "% of working-age population") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_depratio_int", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for migration_int
  output$migration_int_graph <- renderDygraph({
    dygraph(
      migration_int_wide[,c(1, as.numeric(input$migration_int_input))]
    ) %>%
      dyGroup(names(migration_int_wide)[c(1, as.numeric(input$migration_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Crude rate of net migration plus statistical adjustment") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_migration_int", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for depratio_sco
  output$depratio_sco_graph <- renderDygraph({
    dygraph(
      depratio_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Ratio = (ppl aged 0 to 15 and 65+)/(ppl aged 16 to 64)") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_depratio_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for migration_type_overseas_sco
  output$migration_type_overseas_sco_graph <- renderDygraph({
    dygraph(
      migration_type_overseas_sco_wide[,c(1, as.numeric(input$migration_type_overseas_sco_input))]
    ) %>%
      dyGroup(names(migration_type_overseas_sco_wide)[c(1, as.numeric(input$migration_type_overseas_sco_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "People") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_migration_type_overseas_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for migration_in_source_sco
  output$migration_in_source_sco_graph <- renderDygraph({
    dygraph(
      migration_in_source_sco_wide[,c(1, as.numeric(input$migration_in_source_sco_input))]
    ) %>%
      dyGroup(names(migration_in_source_sco_wide)[c(1, as.numeric(input$migration_in_source_sco_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "People") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_migration_in_source_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for migration_in_overseas_age_sco
  output$migration_in_overseas_age_sco_graph <- renderDygraph({
    dygraph(
      migration_in_overseas_age_sco_wide[,c(1, as.numeric(input$migration_in_overseas_age_sco_input))]
    ) %>%
      dyGroup(names(migration_in_overseas_age_sco_wide)[c(1, as.numeric(input$migration_in_overseas_age_sco_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "People") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_migration_in_overseas_age_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for migration_in_overseas_sex_sco
  output$migration_in_overseas_sex_sco_graph <- renderDygraph({
    dygraph(
      migration_in_overseas_sex_sco_wide[,c(1, as.numeric(input$migration_in_overseas_sex_sco_input))]
    ) %>%
      dyGroup(names(migration_in_overseas_sex_sco_wide)[c(1, as.numeric(input$migration_in_overseas_sex_sco_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "People") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_migration_in_overseas_sex_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Leaflet map for depratio_reg
  output$depratio_reg_map_caption <- renderText({
    paste("Map 1. Dependency ratio from population estimates and projections across Scottish council areas in ", as.character(input$depratio_reg_input), sep="")
  })
  depratio_reg_map_data <- reactive({
    depratio_reg_one <- depratio_reg[ which(depratio_reg$Year == input$depratio_reg_input), ]
    depratio_reg_one <- depratio_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, depratio_reg_one, by = intersect(names(mapex@data), names(depratio_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_depratio_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$depratio, bins = 9)
    list_return <- list(mapex = mapex, choropleth_depratio_reg = choropleth_depratio_reg)
    return(list_return)
  })
  output$depratio_reg_map <- renderLeaflet({
    mapex <- depratio_reg_map_data()$mapex
    choropleth_depratio_reg <- depratio_reg_map_data()$choropleth_depratio_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_depratio_reg(mapex$depratio), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$depratio), sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_depratio_reg, values = mapex$depratio, title = paste("Dependency ratio", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  
  # Leaflet map for migration_reg
  output$migration_reg_map_caption <- renderText({
    paste("Map 2. Total net migration across Scottish council areas in ", as.character(input$migration_reg_input), sep="")
  })
  migration_reg_map_data <- reactive({
    migration_reg_one <- migration_reg[ which(migration_reg$Year == input$migration_reg_input), ]
    migration_reg_one <- migration_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, migration_reg_one, by = intersect(names(mapex@data), names(migration_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_migration_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$migration, bins = 9)
    list_return <- list(mapex = mapex, choropleth_migration_reg = choropleth_migration_reg)
    return(list_return)
  })
  output$migration_reg_map <- renderLeaflet({
    mapex <- migration_reg_map_data()$mapex
    choropleth_migration_reg <- migration_reg_map_data()$choropleth_migration_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_migration_reg(mapex$migration), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$migration), sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_migration_reg, values = mapex$migration, title = paste("Total net migration", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  
  # Leaflet map for working_age_reg
  output$working_age_reg_map_caption <- renderText({
    paste("Map 3. Total working age population forecast across Scottish council areas in ", as.character(input$working_age_reg_input), sep="")
  })
  working_age_reg_map_data <- reactive({
    working_age_reg_one <- working_age_reg[ which(working_age_reg$Year == input$working_age_reg_input), ]
    working_age_reg_one <- working_age_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, working_age_reg_one, by = intersect(names(mapex@data), names(working_age_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_working_age_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$working_age, bins = 9)
    list_return <- list(mapex = mapex, choropleth_working_age_reg = choropleth_working_age_reg)
    return(list_return)
  })
  output$working_age_reg_map <- renderLeaflet({
    mapex <- working_age_reg_map_data()$mapex
    choropleth_working_age_reg <- working_age_reg_map_data()$choropleth_working_age_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_working_age_reg(mapex$working_age), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$working_age), sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_working_age_reg, values = mapex$working_age, title = paste("Total working age population", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  
  # PARTICIPATION ##################################################################################################################################
  # Horizontal bar - eactivity_bar
  output$eactivity_bar <- renderUI({
    if (rank_depratio_quantile >= 0.75) {
      column(width = 2,
             tags$b("Economic activity: "),
             tags$p(as.character(rank_eactivity), style="font-size: 48px"),
             style="background-color: red;"
      )
    } else if (rank_depratio_quantile <= 0.25) {
      column(width = 2,
             tags$b("Economic activity: "),
             tags$p(as.character(rank_eactivity), style="font-size: 48px"),
             style="background-color: green;"
      )
    } else {
      column(width = 2,
             tags$b("Economic activity: "),
             tags$p(as.character(rank_eactivity), style="font-size: 48px"),
             style="background-color: orange;"
      )
    }
  })
  # Horizontal bar - employment_bar
  output$employment_bar <- renderUI({
    if (rank_depratio_quantile >= 0.75) {
      column(width = 2,
             tags$b("Employment rate: "),
             tags$p(as.character(rank_employment), style="font-size: 48px"),
             style="background-color: red;"
      )
    } else if (rank_depratio_quantile <= 0.25) {
      column(width = 2,
             tags$b("Employment rate: "),
             tags$p(as.character(rank_employment), style="font-size: 48px"),
             style="background-color: green;"
      )
    } else {
      column(width = 2,
             tags$b("Employment rate: "),
             tags$p(as.character(rank_employment), style="font-size: 48px"),
             style="background-color: orange;"
      )
    }
  })
  # Horizontal bar - unemployment_bar
  output$unemployment_bar <- renderUI({
    if (rank_depratio_quantile >= 0.75) {
      column(width = 2,
             tags$b("Unemployment rate: "),
             tags$p(as.character(rank_unemployment), style="font-size: 48px"),
             style="background-color: red;"
      )
    } else if (rank_depratio_quantile <= 0.25) {
      column(width = 2,
             tags$b("Unemployment rate: "),
             tags$p(as.character(rank_unemployment), style="font-size: 48px"),
             style="background-color: green;"
      )
    } else {
      column(width = 2,
             tags$b("Unemployment rate: "),
             tags$p(as.character(rank_unemployment), style="font-size: 48px"),
             style="background-color: orange;"
      )
    }
  })
  # ggplot barplot for eactivity_overview
  output$eactivity_overview_int_barplot <- renderPlot({
    eactivity_overview_int2 <- eactivity_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(eactivity_overview_int$Country == "Scotland","blue","grey"))
    ggplot(eactivity_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(eactivity_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Economic activity rate") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  # ggplot barplot for employment_overview
  output$employment_overview_int_barplot <- renderPlot({
    employment_overview_int2 <- employment_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(employment_overview_int$Country == "Scotland","blue","grey"))
    ggplot(employment_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(employment_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Employment rate") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  # ggplot barplot for unemployment_overview
  output$unemployment_overview_int_barplot <- renderPlot({
    unemployment_overview_int2 <- unemployment_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(unemployment_overview_int$Country == "Scotland","blue","grey"))
    ggplot(unemployment_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(unemployment_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Unemployment rate") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  # ggplot barplot for genderpaygap_overview
  output$genderpaygap_overview_int_barplot <- renderPlot({
    genderpaygap_overview_int2 <- genderpaygap_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(genderpaygap_overview_int$Country == "Scotland","blue","grey"))
    genderpaygap_overview_int2 <- subset(genderpaygap_overview_int2, genderpaygap_overview_int2$Value > 0)
    ggplot(genderpaygap_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(genderpaygap_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Gender Pay Gap") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  # ggplot barplot for skillsunderprimary_overview
  output$skillsunderprimary_overview_int_barplot <- renderPlot({
    skillsunderprimary_overview_int2 <- skillsunderprimary_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(skillsunderprimary_overview_int$Country == "Scotland","blue","grey"))
    ggplot(skillsunderprimary_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(skillsunderprimary_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Attaintment level of qualifications less than primary, primary and lower secondary education") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  # ggplot barplot for yunemployment_overview
  output$yunemployment_overview_int_barplot <- renderPlot({
    yunemployment_overview_int2 <- yunemployment_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(yunemployment_overview_int$Country == "Scotland","blue","grey"))
    ggplot(yunemployment_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(yunemployment_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Youth unemployment rate") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  # Dygraph for eactivity_int
  output$eactivity_int_graph <- renderDygraph({
    dygraph(
      eactivity_int_wide[,c(1, as.numeric(input$eactivity_int_input))]
    ) %>%
      dyGroup(names(eactivity_int_wide)[c(1, as.numeric(input$eactivity_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Economic Activity rate") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_eactivity_int", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for employment_int
  output$employment_int_graph <- renderDygraph({
    dygraph(
      employment_int_wide[,c(1, as.numeric(input$employment_int_input))]
    ) %>%
      dyGroup(names(employment_int_wide)[c(1, as.numeric(input$employment_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Employment rate") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_employment_int", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for unemployment_int
  output$unemployment_int_graph <- renderDygraph({
    dygraph(
      unemployment_int_wide[,c(1, as.numeric(input$unemployment_int_input))]
    ) %>%
      dyGroup(names(unemployment_int_wide)[c(1, as.numeric(input$unemployment_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Unemployment rate") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_unemployment_int", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # # Dygraph for genderpaygap_int
  # output$genderpaygap_int_graph <- renderDygraph({
  #   dygraph(
  #     genderpaygap_int_wide[,c(1, as.numeric(input$genderpaygap_int_input))]
  #   ) %>%
  #     dyGroup(names(genderpaygap_int_wide)[c(1, as.numeric(input$genderpaygap_int_input))], strokeWidth = 2) %>%
  #     dyRangeSelector() %>%
  #     dyAxis("x", label = "Year", rangePad = 5) %>%
  #     dyAxis("y", label = "genderpaygap rate") %>%
  #     dyHighlight(
  #       highlightCircleSize = 3,
  #       highlightSeriesBackgroundAlpha = 0.2,
  #       hideOnMouseOut = FALSE,
  #       highlightSeriesOpts = list(strokeWidth = 6)
  #     ) %>%
  #     dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
  #     dyLegend(labelsDiv = "legendDivID_genderpaygap_int", labelsSeparateLines = TRUE) %>%
  #     dyUnzoom() %>%
  #     dyCrosshair(direction = "vertical")
  # })
  
  # ggplot barplot for genderpaygap2_overview_int
  output$genderpaygap2_overview_int_barplot <- renderPlot({
    genderpaygap2_overview_int <- genderpaygap2_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = "grey")
    ggplot(genderpaygap2_overview_int, aes(x=Country, y=Value)) +
      geom_col(fill=factor(genderpaygap2_overview_int$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  
  # Dygraph for skillsunderprimary_int
  output$skillsunderprimary_int_graph <- renderDygraph({
    dygraph(
      skillsunderprimary_int_wide[,c(1, as.numeric(input$skillsunderprimary_int_input))]
    ) %>%
      dyGroup(names(skillsunderprimary_int_wide)[c(1, as.numeric(input$skillsunderprimary_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Percantage") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_skillsunderprimary_int", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for skillstertiary_int
  output$skillstertiary_int_graph <- renderDygraph({
    dygraph(
      skillstertiary_int_wide[,c(1, as.numeric(input$skillstertiary_int_input))]
    ) %>%
      dyGroup(names(skillstertiary_int_wide)[c(1, as.numeric(input$skillstertiary_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Percantage") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_skillstertiary_int", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for yunemployment_int
  output$yunemployment_int_graph <- renderDygraph({
    dygraph(
      yunemployment_int_wide[,c(1, as.numeric(input$yunemployment_int_input))]
    ) %>%
      dyGroup(names(yunemployment_int_wide)[c(1, as.numeric(input$yunemployment_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Youth unemployment rate") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_yunemployment_int", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # # Dygraph for evoice_int
  # output$evoice_int_graph <- renderDygraph({
  #   dygraph(
  #     evoice_int_wide[,c(1, as.numeric(input$evoice_int_input))]
  #   ) %>%
  #     dyGroup(names(evoice_int_wide)[c(1, as.numeric(input$evoice_int_input))], strokeWidth = 2) %>%
  #     dyRangeSelector() %>%
  #     dyAxis("x", label = "Year", rangePad = 5) %>%
  #     dyAxis("y", label = "Percantage") %>%
  #     dyHighlight(
  #       highlightCircleSize = 3,
  #       highlightSeriesBackgroundAlpha = 0.2,
  #       hideOnMouseOut = FALSE,
  #       highlightSeriesOpts = list(strokeWidth = 6)
  #     ) %>%
  #     dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
  #     dyLegend(labelsDiv = "legendDivID_evoice_int", labelsSeparateLines = TRUE) %>%
  #     dyUnzoom() %>%
  #     dyCrosshair(direction = "vertical")
  # })
  
  # ggplot barplot for evoice2_overview_int
  output$evoice2_overview_int_barplot <- renderPlot({
    evoice2_overview_int <- evoice2_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = "grey")
    ggplot(evoice2_overview_int, aes(x=Country, y=Value)) +
      geom_col(fill=factor(evoice2_overview_int$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  # Dygraph for eactivity_sco
  output$eactivity_sco_graph <- renderDygraph({
    dygraph(
      eactivity_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "%") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_eactivity_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for livingwage_sco
  output$livingwage_sco_graph <- renderDygraph({
    dygraph(
      livingwage_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "%") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_livingwage_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for genderpaygap_sco
  output$genderpaygap_sco_graph <- renderDygraph({
    dygraph(
      genderpaygap_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "percentage of male earnings") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_genderpaygap_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for evoice_sco
  output$evoice_sco_graph <- renderDygraph({
    dygraph(
      evoice_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "%") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_evoice_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for wplearning_sco
  output$wplearning_sco_graph <- renderDygraph({
    dygraph(
      wplearning_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "%") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_wplearning_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for youngpplpart_sco
  output$youngpplpart_sco_graph <- renderDygraph({
    dygraph(
      youngpplpart_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "%") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_youngpplpart_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for skillshortage_sco
  output$skillshortage_sco_graph <- renderDygraph({
    dygraph(
      skillshortage_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "%") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_skillshortage_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  
  
  
  
  
  # Leaflet map for eactivity_reg
  output$eactivity_reg_map_caption <- renderText({
    paste("Map 1. Total economic activity rate (16-64) ", as.character(input$eactivity_reg_input), sep="")
  })
  eactivity_reg_map_data <- reactive({
    eactivity_reg_one <- eactivity_reg[ which(eactivity_reg$Year == input$eactivity_reg_input), ]
    eactivity_reg_one <- eactivity_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, eactivity_reg_one, by = intersect(names(mapex@data), names(eactivity_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_eactivity_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$eactivity, bins = 9)
    list_return <- list(mapex = mapex, choropleth_eactivity_reg = choropleth_eactivity_reg)
    return(list_return)
  })
  output$eactivity_reg_map <- renderLeaflet({
    mapex <- eactivity_reg_map_data()$mapex
    choropleth_eactivity_reg <- eactivity_reg_map_data()$choropleth_eactivity_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_eactivity_reg(mapex$eactivity), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$eactivity), " %", sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_eactivity_reg, values = mapex$eactivity, title = paste("Economic Activity", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  # Leaflet map for lwage_reg
  output$lwage_reg_map_caption <- renderText({
    paste("Map 2. Percentage of Employees Earning Below the Living Wage ", as.character(input$lwage_reg_input), sep="")
  })
  lwage_reg_map_data <- reactive({
    lwage_reg_one <- lwage_reg[ which(lwage_reg$Year == input$lwage_reg_input), ]
    lwage_reg_one <- lwage_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, lwage_reg_one, by = intersect(names(mapex@data), names(lwage_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_lwage_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$lwage, bins = 9)
    list_return <- list(mapex = mapex, choropleth_lwage_reg = choropleth_lwage_reg)
    return(list_return)
  })
  output$lwage_reg_map <- renderLeaflet({
    mapex <- lwage_reg_map_data()$mapex
    choropleth_lwage_reg <- lwage_reg_map_data()$choropleth_lwage_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_lwage_reg(mapex$lwage), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$lwage), " %", sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_lwage_reg, values = mapex$lwage, title = paste("Economic Activity", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  # Leaflet map for gpaygap_reg
  output$gpaygap_reg_map_caption <- renderText({
    paste("Map 3. Median hourly gender pay gap - full time employees ", as.character(input$gpaygap_reg_input), sep="")
  })
  gpaygap_reg_map_data <- reactive({
    gpaygap_reg_one <- gpaygap_reg[ which(gpaygap_reg$Year == input$gpaygap_reg_input), ]
    gpaygap_reg_one <- gpaygap_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, gpaygap_reg_one, by = intersect(names(mapex@data), names(gpaygap_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_gpaygap_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$gpaygap, bins = 9)
    list_return <- list(mapex = mapex, choropleth_gpaygap_reg = choropleth_gpaygap_reg)
    return(list_return)
  })
  output$gpaygap_reg_map <- renderLeaflet({
    mapex <- gpaygap_reg_map_data()$mapex
    choropleth_gpaygap_reg <- gpaygap_reg_map_data()$choropleth_gpaygap_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_gpaygap_reg(mapex$gpaygap), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$gpaygap), " %", sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_gpaygap_reg, values = mapex$gpaygap, title = paste("Gender Pay Gap", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  # Leaflet map for wplearning_reg
  output$wplearning_reg_map_caption <- renderText({
    paste("Map 4. Percentage of All Employees Who received Job Related Training in the last 13 weeks (aged 16-64) ", as.character(input$wplearning_reg_input), sep="")
  })
  wplearning_reg_map_data <- reactive({
    wplearning_reg_one <- wplearning_reg[ which(wplearning_reg$Year == input$wplearning_reg_input), ]
    wplearning_reg_one <- wplearning_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, wplearning_reg_one, by = intersect(names(mapex@data), names(wplearning_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_wplearning_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$wplearning, bins = 9)
    list_return <- list(mapex = mapex, choropleth_wplearning_reg = choropleth_wplearning_reg)
    return(list_return)
  })
  output$wplearning_reg_map <- renderLeaflet({
    mapex <- wplearning_reg_map_data()$mapex
    choropleth_wplearning_reg <- wplearning_reg_map_data()$choropleth_wplearning_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_wplearning_reg(mapex$wplearning), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$wplearning), " %", sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_wplearning_reg, values = mapex$wplearning, title = paste("% who Received Job Related Training", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  
  # PEOPLE #########################################################################################################################################
  
  # Horizontal bar - lifeexpall_bar
  output$lifeexpall_bar <- renderUI({
    if (rank_depratio_quantile >= 0.75) {
      column(width = 2,
             tags$b("Life expectancy: "),
             tags$p(as.character(rank_lifeexpall), style="font-size: 48px"),
             style="background-color: red;"
      )
    } else if (rank_depratio_quantile <= 0.25) {
      column(width = 2,
             tags$b("Life expectancy: "),
             tags$p(as.character(rank_lifeexpall), style="font-size: 48px"),
             style="background-color: green;"
      )
    } else {
      column(width = 2,
             tags$b("Life expectancy: "),
             tags$p(as.character(rank_lifeexpall), style="font-size: 48px"),
             style="background-color: orange;"
      )
    }
  })
  # ggplot barplot for lifeexpall_overview
  output$lifeexpall_overview_int_barplot <- renderPlot({
    lifeexpall_overview_int2 <- lifeexpall_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(lifeexpall_overview_int$Country == "Scotland","blue","grey"))
    ggplot(lifeexpall_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(lifeexpall_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="Years") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Life expectancy") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  
  # Dygraph for lifeexpall_int
  output$lifeexpall_int_graph <- renderDygraph({
    dygraph(
      lifeexpall_int_wide[,c(1, as.numeric(input$lifeexpall_int_input))]
    ) %>%
      dyGroup(names(lifeexpall_int_wide)[c(1, as.numeric(input$lifeexpall_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Percantage") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_lifeexpall_int", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for rphousingc_sco
  output$rphousingc_sco_graph <- renderDygraph({
    dygraph(
      rphousingc_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Percentage Of People") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_rphousingc_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # ggplot barplot for hlifeexp_male_overview_int
  output$hlifeexp_male_overview_sco_barplot <- renderPlot({
    hlifeexp_male_overview_sco <- hlifeexp_male_sco %>%
      mutate(Age = (Age),
             Value = as.numeric(Value),
             fill_type = "grey")
    ggplot(hlifeexp_male_overview_sco, aes(x=Age, y=Value)) +
      geom_col(fill=factor(hlifeexp_male_overview_sco$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("") +
      geom_text(aes(x=Age, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  # ggplot barplot for hlifeexp_female_overview_sco
  output$hlifeexp_female_overview_sco_barplot <- renderPlot({
    hlifeexp_female_overview_sco <- hlifeexp_female_sco %>%
      mutate(Age = (Age),
             Value = as.numeric(Value),
             fill_type = "grey")
    ggplot(hlifeexp_female_overview_sco, aes(x=Age, y=Value)) +
      geom_col(fill=factor(hlifeexp_female_overview_sco$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("") +
      geom_text(aes(x=Age, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  # Dygraph for mwellbeing_sco
  output$mwellbeing_sco_graph <- renderDygraph({
    dygraph(
      mwellbeing_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "SWEMWBS Score") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_mwellbeing_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Leaflet map for hlifeexp_male_reg
  output$hlifeexp_male_reg_map_caption <- renderText({
    paste("Map 4. Healthy life expectancy (male) ", as.character(input$hlifeexp_male_reg_input), sep="")
  })
  hlifeexp_male_reg_map_data <- reactive({
    hlifeexp_male_reg_one <- hlifeexp_male_reg[ which(hlifeexp_male_reg$Year == input$hlifeexp_male_reg_input), ]
    hlifeexp_male_reg_one <- hlifeexp_male_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, hlifeexp_male_reg_one, by = intersect(names(mapex@data), names(hlifeexp_male_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_hlifeexp_male_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$hlifeexp_male, bins = 9)
    list_return <- list(mapex = mapex, choropleth_hlifeexp_male_reg = choropleth_hlifeexp_male_reg)
    return(list_return)
  })
  output$hlifeexp_male_reg_map <- renderLeaflet({
    mapex <- hlifeexp_male_reg_map_data()$mapex
    choropleth_hlifeexp_male_reg <- hlifeexp_male_reg_map_data()$choropleth_hlifeexp_male_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_hlifeexp_male_reg(mapex$hlifeexp_male), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$hlifeexp_male), " year", sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_hlifeexp_male_reg, values = mapex$hlifeexp_male, title = paste("Healthy life expectancy (male)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  # Leaflet map for hlifeexp_female_reg
  output$hlifeexp_female_reg_map_caption <- renderText({
    paste("Map 5. Healthy life expectancy (female) ", as.character(input$hlifeexp_female_reg_input), sep="")
  })
  hlifeexp_female_reg_map_data <- reactive({
    hlifeexp_female_reg_one <- hlifeexp_female_reg[ which(hlifeexp_female_reg$Year == input$hlifeexp_female_reg_input), ]
    hlifeexp_female_reg_one <- hlifeexp_female_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, hlifeexp_female_reg_one, by = intersect(names(mapex@data), names(hlifeexp_female_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_hlifeexp_female_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$hlifeexp_female, bins = 9)
    list_return <- list(mapex = mapex, choropleth_hlifeexp_female_reg = choropleth_hlifeexp_female_reg)
    return(list_return)
  })
  output$hlifeexp_female_reg_map <- renderLeaflet({
    mapex <- hlifeexp_female_reg_map_data()$mapex
    choropleth_hlifeexp_female_reg <- hlifeexp_female_reg_map_data()$choropleth_hlifeexp_female_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_hlifeexp_female_reg(mapex$hlifeexp_female), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$hlifeexp_female), " year", sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_hlifeexp_female_reg, values = mapex$hlifeexp_female, title = paste("Healthy life expectancy (female)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  
  # PLACE ##########################################################################################################################################
  # Horizontal bar - broadband
  output$broadband_bar <- renderUI({
    if (rank_broadband_quantile >= 0.75) {
      column(width = 2,
             tags$b("Broadband: "),
             tags$p(as.character(rank_broadband), style="font-size: 48px"),
             style="background-color: red;"
      )
    } else if (rank_broadband_quantile <= 0.25) {
      column(width = 2,
             tags$b("Broadband: "),
             tags$p(as.character(rank_broadband), style="font-size: 48px"),
             style="background-color: green;"
      )
    } else {
      column(width = 2,
             tags$b("Broadband: "),
             tags$p(as.character(rank_broadband), style="font-size: 48px"),
             style="background-color: orange;"
      )
    }
  })
  
  # ggplot barplot for broadband_overview
  output$broadband_overview_int_barplot <- renderPlot({
    broadband_overview_int2 <- broadband_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(broadband_overview_int$Country == "Scotland","blue","grey"))
    ggplot(broadband_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(broadband_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="% of total households") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Broadband (% of total households)") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  # Dygraph for broadband_int
  output$broadband_int_graph <- renderDygraph({
    dygraph(
      broadband_int_wide[,c(1, as.numeric(input$broadband_int_input))]
    ) %>%
      dyGroup(names(broadband_int_wide)[c(1, as.numeric(input$broadband_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Percantage") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_broadband_int", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for greenandbluespace_sco
  output$greenandbluespace_sco_graph <- renderDygraph({
    dygraph(
      greenandbluespace_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "%") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_greenandbluespace_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Leaflet map for blueorgreen_reg
  output$blueorgreen_reg_map_caption <- renderText({
    paste("Map 1. Proportion of adults who live within 5 min of their local green/blue space ", as.character(input$blueorgreen_reg_input), sep="")
  })
  blueorgreen_reg_map_data <- reactive({
    blueorgreen_reg_one <- blueorgreen_reg[ which(blueorgreen_reg$Year == input$blueorgreen_reg_input), ]
    blueorgreen_reg_one <- blueorgreen_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, blueorgreen_reg_one, by = intersect(names(mapex@data), names(blueorgreen_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_blueorgreen_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$blueorgreen, bins = 9)
    list_return <- list(mapex = mapex, choropleth_blueorgreen_reg = choropleth_blueorgreen_reg)
    return(list_return)
  })
  output$blueorgreen_reg_map <- renderLeaflet({
    mapex <- blueorgreen_reg_map_data()$mapex
    choropleth_blueorgreen_reg <- blueorgreen_reg_map_data()$choropleth_blueorgreen_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_blueorgreen_reg(mapex$blueorgreen), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$blueorgreen), " %", sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_blueorgreen_reg, values = mapex$blueorgreen, title = paste("% of ppl living close to green/blue space", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  # Leaflet map for pubservsat_reg
  output$pubservsat_reg_map_caption <- renderText({
    paste("Map 2. Percentage of the population very or fairly satisfied with the quality of public services delivered (local health services,local schools and public transport) ", as.character(input$pubservsat_reg_input), sep="")
  })
  pubservsat_reg_map_data <- reactive({
    pubservsat_reg_one <- pubservsat_reg[ which(pubservsat_reg$Year == input$pubservsat_reg_input), ]
    pubservsat_reg_one <- pubservsat_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, pubservsat_reg_one, by = intersect(names(mapex@data), names(pubservsat_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_pubservsat_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$pubservsat, bins = 9)
    list_return <- list(mapex = mapex, choropleth_pubservsat_reg = choropleth_pubservsat_reg)
    return(list_return)
  })
  output$pubservsat_reg_map <- renderLeaflet({
    mapex <- pubservsat_reg_map_data()$mapex
    choropleth_pubservsat_reg <- pubservsat_reg_map_data()$choropleth_pubservsat_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_pubservsat_reg(mapex$pubservsat), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$pubservsat), " %", sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_pubservsat_reg, values = mapex$pubservsat, title = paste("% satisfied", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  # Leaflet map for broadband_reg
  output$broadband_reg_map_caption <- renderText({
    paste("Map 3. Proportion of residential and non-residential addresses where superfast broadband is not available (as a Percentage) ", as.character(input$broadband_reg_input), sep="")
  })
  broadband_reg_map_data <- reactive({
    broadband_reg_one <- broadband_reg[ which(broadband_reg$Year == input$broadband_reg_input), ]
    broadband_reg_one <- broadband_reg_one[ ,c(1,3)]
    merged <- merge(mapex@data, broadband_reg_one, by = intersect(names(mapex@data), names(broadband_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
    mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
    choropleth_broadband_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$broadband, bins = 9)
    list_return <- list(mapex = mapex, choropleth_broadband_reg = choropleth_broadband_reg)
    return(list_return)
  })
  output$broadband_reg_map <- renderLeaflet({
    mapex <- broadband_reg_map_data()$mapex
    choropleth_broadband_reg <- broadband_reg_map_data()$choropleth_broadband_reg
    leaflet(mapex) %>%
      setView(zoom = 6, lat = 57, lng= -4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_broadband_reg(mapex$broadband), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$broadband), " %", sep = ""),
                  highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
      ) %>%
      addLegend("bottomright", pal = choropleth_broadband_reg, values = mapex$broadband, title = paste("% who don't have access", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
  })
  
  # SUSTAINABILITY #################################################################################################################################
  # Horizontal bar - ggemissions2
  output$ggemissions2_bar <- renderUI({
    if (rank_ggemissions2_quantile >= 0.75) {
      column(width = 2,
             tags$b("Greenhouse Gas Emissions: "),
             tags$p(as.character(rank_ggemissions2), style="font-size: 48px"),
             style="background-color: red;"
      )
    } else if (rank_ggemissions2_quantile <= 0.25) {
      column(width = 2,
             tags$b("Greenhouse Gas Emissions: "),
             tags$p(as.character(rank_ggemissions2), style="font-size: 48px"),
             style="background-color: green;"
      )
    } else {
      column(width = 2,
             tags$b("Greenhouse Gas Emissions: "),
             tags$p(as.character(rank_ggemissions2), style="font-size: 48px"),
             style="background-color: orange;"
      )
    }
  })
  
  # ggplot barplot for ggemissions2_overview
  output$ggemissions2_overview_int_barplot <- renderPlot({
    ggemissions2_overview_int2 <- ggemissions2_overview_int %>%
      mutate(Country = (Country),
             Value = as.numeric(Value),
             fill_type = ifelse(ggemissions2_overview_int$Country == "Scotland","blue","grey"))
    ggplot(ggemissions2_overview_int2, aes(x=Country, y=Value)) +
      geom_col(fill=factor(ggemissions2_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="Tonnes CO2e per capita") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Greenhouse Gas Emissions (Tonnes CO2e per capita)") +
      geom_text(aes(x=Country, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  # Dygraph for ggemissions_int
  output$ggemissions_int_graph <- renderDygraph({
    dygraph(
      ggemissions_int_wide[,c(1, as.numeric(input$ggemissions_int_input))]
    ) %>%
      dyGroup(names(ggemissions_int_wide)[c(1, as.numeric(input$ggemissions_int_input))], strokeWidth = 2) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Kilograms per capita, Thousands") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_ggemissions_int", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for cfootprint_sco
  output$cfootprint_sco_graph <- renderDygraph({
    dygraph(
      cfootprint_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Mt Co2Equiv") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_cfootprint_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for naturalf_sco
  output$naturalf_sco_graph <- renderDygraph({
    dygraph(
      naturalf_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "%") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_naturalf_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # Dygraph for gasemissions_sco
  output$gasemissions_sco_graph <- renderDygraph({
    dygraph(
      gasemissions_sco
    ) %>%
      dyRangeSelector() %>%
      dyAxis("x", label = "Year", rangePad = 5) %>%
      dyAxis("y", label = "Mt Co2Equiv") %>%
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE,
        highlightSeriesOpts = list(strokeWidth = 6)
      ) %>%
      dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      dyLegend(labelsDiv = "legendDivID_gasemissions_sco", labelsSeparateLines = TRUE) %>%
      dyUnzoom() %>%
      dyCrosshair(direction = "vertical")
  })
  
  # EQUALITIES DASHBOARD ###########################################################################################################################
  # gpaygap_eq_overview_int_lineplot
  output$gpaygap_eq_overview_int_lineplot <- renderPlot({
    ggplot(gpaygap_eq) +
      geom_line(stat="identity", aes(x=gpaygap_eq$Year, y=gpaygap_eq$Value), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      labs(x = "Year", y = "%") +
      theme_minimal() +
      ggtitle("Gender Pay Gap - Full-time gender pay gap (%)") +
      scale_color_jama() +
      geom_point(aes(x=gpaygap_eq$Year, y=gpaygap_eq$Value), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=gpaygap_eq$Year, y=gpaygap_eq$Value, label=gpaygap_eq$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # dempgap_eq_overview_int_lineplot
  output$dempgap_eq_overview_int_lineplot <- renderPlot({
    ggplot(dempgap_eq) +
      geom_line(stat="identity", aes(x=dempgap_eq$Year, y=dempgap_eq$Value), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      labs(x = "Year", y = "%") +
      theme_minimal() +
      ggtitle("Disabled Employment Gap") +
      scale_color_jama() +
      geom_point(aes(x=dempgap_eq$Year, y=dempgap_eq$Value), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=dempgap_eq$Year, y=dempgap_eq$Value, label=dempgap_eq$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # youthunemp_eq_overview_int_lineplot
  output$youthunemp_eq_overview_int_lineplot <- renderPlot({
    ggplot(youthunemp_eq) +
      geom_line(stat="identity", aes(x=youthunemp_eq$Year, y=youthunemp_eq$Value), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      labs(x = "Year", y = "%") +
      theme_minimal() +
      ggtitle("Youth Unemployment Rate (16 - 24)") +
      scale_color_jama() +
      geom_point(aes(x=youthunemp_eq$Year, y=youthunemp_eq$Value), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=youthunemp_eq$Year, y=youthunemp_eq$Value, label=youthunemp_eq$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # ethnicmgap_eq_overview_int_lineplot
  output$ethnicmgap_eq_overview_int_lineplot <- renderPlot({
    ggplot(ethnicmgap_eq) +
      geom_line(stat="identity", aes(x=ethnicmgap_eq$Year, y=ethnicmgap_eq$Value), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      labs(x = "Year", y = "%") +
      theme_minimal() +
      ggtitle("Ethnic Minority Employment Gap") +
      scale_color_jama() +
      geom_point(aes(x=ethnicmgap_eq$Year, y=ethnicmgap_eq$Value), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=ethnicmgap_eq$Year, y=ethnicmgap_eq$Value, label=ethnicmgap_eq$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # socioecon_eq_overview_int_barplot
  output$socioecon_eq_overview_int_barplot <- renderPlot({
    socioecon_eq2 <- socioecon_eq %>%
      mutate(SIMD = (SIMD),
             Value = as.numeric(Value),
             fill_type = "grey")
    ggplot(socioecon_eq2, aes(x=SIMD, y=Value)) +
      geom_col(fill=factor(socioecon_eq2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Employment gap between most and least deprived SIMD quintiles") +
      geom_text(aes(x=SIMD, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  # religion_eq_overview_int_barplot
  output$religion_eq_overview_int_barplot <- renderPlot({
    religion_eq2 <- religion_eq %>%
      mutate(Religion = (Religion),
             Value = as.numeric(Value),
             fill_type = "grey")
    ggplot(religion_eq2, aes(x=Religion, y=Value)) +
      geom_col(fill=factor(religion_eq2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Adults in relative poverty AHC (below 60% of UK median income)") +
      geom_text(aes(x=Religion, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
  # orientation_eq_overview_int_barplot
  output$orientation_eq_overview_int_barplot <- renderPlot({
    orientation_eq2 <- orientation_eq %>%
      mutate(Orientation = (Orientation),
             Value = as.numeric(Value),
             fill_type = "grey")
    ggplot(orientation_eq2, aes(x=Orientation, y=Value)) +
      geom_col(fill=factor(orientation_eq2$fill_type), show.legend = FALSE, width=0.9) +
      coord_flip(clip="off", expand=TRUE) +
      labs(x="", y="%") +
      theme_minimal() +
      theme(
        axis.text.x=element_blank()
      ) +
      ggtitle("Experiences of discrimination") +
      geom_text(aes(x=Orientation, y=Value, label=Value),
                vjust=0.3, size=4, hjust=1.1, col="white")
  })
  
#### OVERVIEWS ###########
  
  # # ggplot lineplot for growth_plain_sco_lineplot
  # output$growth_plain_sco_lineplot <- renderPlot({
  #   ggplot(growth_plain_sco) +
  #     geom_line(stat="identity", aes(x=growth_plain_sco$Year, y=growth_plain_sco$Value), color ="#00BFC4", show.legend = FALSE, size=1.5) +
  #     coord_flip(clip = "off", expand = TRUE) +
  #     labs(x = "NBI Index", y = "") +
  #     theme_minimal() +
  #     ggtitle("Anholt GfK-Roper Nation Brands Index (NBI)") +
  #     scale_color_jama() +
  #     geom_point(aes(x=growth_plain_sco$Value, y=growth_plain_sco$Year), color = "#F8766D", show.legend = FALSE, size=4) +
  #     geom_text(aes(x=growth_plain_sco$Value, y=growth_plain_sco$Year, label=growth_plain_sco$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  # })
  
  
  # ggplot lineplot for growth_plain_sco
  output$growth_plain_sco_lineplot <- renderPlot({
    ggplot(growth_plain_sco) +
      geom_path(stat="identity", aes(x=growth_plain_sco$Value, y=growth_plain_sco$Year), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      coord_flip(clip = "off", expand = TRUE) +
      labs(x = "Growth rate", y = "") +
      theme_minimal() +
      ggtitle("GDP Growth rate") +
      scale_color_jama() +
      geom_point(aes(x=growth_plain_sco$Value, y=growth_plain_sco$Year), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=growth_plain_sco$Value, y=growth_plain_sco$Year, label=growth_plain_sco$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # ggplot lineplot for productivity_sco
  output$productivity_sco_lineplot <- renderPlot({
    ggplot(productivity_sco) +
      geom_path(stat="identity", aes(x=productivity_sco$Value, y=productivity_sco$Year), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      coord_flip(clip = "off", expand = TRUE) +
      labs(x = "Productivity", y = "") +
      theme_minimal() +
      ggtitle("Productivity (USA=100)") +
      scale_color_jama() +
      geom_point(aes(x=productivity_sco$Value, y=productivity_sco$Year), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=productivity_sco$Value, y=productivity_sco$Year, label=productivity_sco$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # ggplot lineplot for exporting_plain_sco
  output$exporting_plain_sco_lineplot <- renderPlot({
    ggplot(exporting_plain_sco) +
      geom_path(stat="identity", aes(x=exporting_plain_sco$Value, y=exporting_plain_sco$Year), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      coord_flip(clip = "off", expand = TRUE) +
      labs(x = "Exporting (million pounds)", y = "") +
      theme_minimal() +
      ggtitle("Scotland's Total Exports") +
      scale_color_jama() +
      geom_point(aes(x=exporting_plain_sco$Value, y=exporting_plain_sco$Year), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=exporting_plain_sco$Value, y=exporting_plain_sco$Year, label=exporting_plain_sco$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # ggplot lineplot for rd_plain_sco
  output$rd_plain_sco_lineplot <- renderPlot({
    ggplot(rd_plain_sco) +
      geom_path(stat="identity", aes(x=rd_plain_sco$Value, y=rd_plain_sco$Year), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      coord_flip(clip = "off", expand = TRUE) +
      labs(x = "Percent GDP", y = "") +
      theme_minimal() +
      ggtitle("Scotland's Expenditure on R&D") +
      scale_color_jama() +
      geom_point(aes(x=rd_plain_sco$Value, y=rd_plain_sco$Year), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=rd_plain_sco$Value, y=rd_plain_sco$Year, label=rd_plain_sco$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # ggplot lineplot for eactivity_sco
  output$eactivity_sco_lineplot <- renderPlot({
    ggplot(eactivity_sco) +
      geom_path(stat="identity", aes(x=eactivity_sco$Value, y=eactivity_sco$Year), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      coord_flip(clip = "off", expand = TRUE) +
      labs(x = "TEA", y = "") +
      theme_minimal() +
      ggtitle("Total Early-stage Entrepreneurial Activity") +
      scale_color_jama() +
      geom_point(aes(x=eactivity_sco$Value, y=eactivity_sco$Year), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=eactivity_sco$Value, y=eactivity_sco$Year, label=eactivity_sco$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # ggplot lineplot for evoice_sco
  output$evoice_sco_lineplot <- renderPlot({
    ggplot(evoice_sco) +
      geom_path(stat="identity", aes(x=evoice_sco$Value, y=evoice_sco$Year), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      coord_flip(clip = "off", expand = TRUE) +
      labs(x = "Union membership", y = "") +
      theme_minimal() +
      ggtitle("Employee Voice") +
      scale_color_jama() +
      geom_point(aes(x=evoice_sco$Value, y=evoice_sco$Year), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=evoice_sco$Value, y=evoice_sco$Year, label=evoice_sco$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # ggplot lineplot for livingwage_sco
  output$livingwage_sco_lineplot <- renderPlot({
    ggplot(livingwage_sco) +
      geom_path(stat="identity", aes(x=livingwage_sco$Value, y=livingwage_sco$Year), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      coord_flip(clip = "off", expand = TRUE) +
      labs(x = "Percent", y = "") +
      theme_minimal() +
      ggtitle("Percentage of employees earning less than the living wage") +
      scale_color_jama() +
      geom_point(aes(x=livingwage_sco$Value, y=livingwage_sco$Year), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=livingwage_sco$Value, y=livingwage_sco$Year, label=livingwage_sco$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # ggplot lineplot for mwellbeing_sco
  output$mwellbeing_sco_lineplot <- renderPlot({
    ggplot(mwellbeing_sco) +
      geom_path(stat="identity", aes(x=mwellbeing_sco$Value, y=mwellbeing_sco$Year), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      coord_flip(clip = "off", expand = TRUE) +
      labs(x = "SWEMWBS Score", y = "") +
      theme_minimal() +
      ggtitle("Mental Wellbeing Survey") +
      scale_color_jama() +
      geom_point(aes(x=mwellbeing_sco$Value, y=mwellbeing_sco$Year), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=mwellbeing_sco$Value, y=mwellbeing_sco$Year, label=mwellbeing_sco$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # ggplot lineplot for cfootprint_sco
  output$cfootprint_sco_lineplot <- renderPlot({
    ggplot(cfootprint_sco) +
      geom_path(stat="identity", aes(x=cfootprint_sco$Value, y=cfootprint_sco$Year), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      coord_flip(clip = "off", expand = TRUE) +
      labs(x = "Mt Co2Equiv", y = "") +
      theme_minimal() +
      ggtitle("Scotland's Carbon Footrpint") +
      scale_color_jama() +
      geom_point(aes(x=cfootprint_sco$Value, y=cfootprint_sco$Year), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=cfootprint_sco$Value, y=cfootprint_sco$Year, label=cfootprint_sco$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # ggplot lineplot for naturalf_sco
  output$naturalf_sco_lineplot <- renderPlot({
    ggplot(naturalf_sco) +
      geom_path(stat="identity", aes(x=naturalf_sco$Value, y=naturalf_sco$Year), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      coord_flip(clip = "off", expand = TRUE) +
      labs(x = "Percent", y = "") +
      theme_minimal() +
      ggtitle("Natural features, on protected nature sites, in favourable condition") +
      scale_color_jama() +
      geom_point(aes(x=naturalf_sco$Value, y=naturalf_sco$Year), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=naturalf_sco$Value, y=naturalf_sco$Year, label=naturalf_sco$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  # ggplot lineplot for gasemissions_sco
  output$gasemissions_sco_lineplot <- renderPlot({
    ggplot(gasemissions_sco) +
      geom_path(stat="identity", aes(x=gasemissions_sco$Value, y=gasemissions_sco$Year), color ="#00BFC4", show.legend = FALSE, size=1.5) +
      coord_flip(clip = "off", expand = TRUE) +
      labs(x = "Mt Co2Equiv", y = "") +
      theme_minimal() +
      ggtitle("Scotland's greenhouse gas emissions") +
      scale_color_jama() +
      geom_point(aes(x=gasemissions_sco$Value, y=gasemissions_sco$Year), color = "#F8766D", show.legend = FALSE, size=4) +
      geom_text(aes(x=gasemissions_sco$Value, y=gasemissions_sco$Year, label=gasemissions_sco$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
  })
  
  
  
  
  
}
) ## End of App