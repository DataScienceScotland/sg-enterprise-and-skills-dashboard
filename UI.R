#### Enterprise and Skills Dashboard UI Page ####


############################# ---------------------------------------------------------------------------
#### Setting Loading Spinner animations ####
options(
  spinner.color = "#F1A864", spinner.size = 1, spinner.color.background = "#FFFFFF", spinner.type = 4, proxy.height = "10px",
  stringsAsFactors = FALSE
)

#########################################################################################################
#### Header Panel ####--------------------------------------------------------------------------####
header <- wellPanel(
  div(
    actionButton(
      inputId = "homeButton", label = NULL,
      style = "width: 300px;
                            height:60px;
                            background: url('Logo.png');
            
                            background-position: center;
                            background-repeat: no-repeat;
               text-shadow: none !important;
               box-shadow: none !important;
               "
    ),
    style = "background-color: #DE3979; margin-left: -2em; margin-right: -2em;margin-bottom: -1em; margin-top: 1em;"
  )
)
#                background-size: cover;
###########################################################################################################
#### Main Page Options ####--------------------------------------------------------------------####
body <- navbarPage(
  #### Page Admin Section ####
  title = "",
  theme = "color: #FFFFFF;",
  id = "mainMenu1",
  windowTitle = "Enterprise and Skills Strategic Board Dashboard",
  #### Home Page ####
  tabPanel("Home",
    icon = icon("home"),
    fluidRow(
      setZoom(id = "SetEffects"), setShadow(id = "SetEffects"),
      column(
        6,
        div(
          align = "center", id = "SetEffects",
          div(img(src = "industry-solid.png", height = "128")),
          actionLink(inputId = "homeProductivityLink", label = "Productivity", style = "color: #DE3979;font-size: 50pt;
                            font-family: Arial;font-weight: bold; text-align: center")
        )
      ),
      column(
        6,
        div(
          align = "center", id = "SetEffects",
          div(img(src = "people-carry-solid.png", height = "128")),
          actionLink(inputId = "homeEqualityLink", label = "Equality", style = "color: #DE3979;font-size: 50pt;
                            font-family: Arial;font-weight: bold;")
        )
      )
    ),
    p(" "),
    fluidRow(
      column(
        6,
        div(
          align = "center", id = "SetEffects",
          div(img(src = "spa-solid.png", height = "128")),
          actionLink(inputId = "homeWellnessLink", label = "Wellbeing", style = "color: #DE3979;font-size: 50pt;
                            font-family: Arial;font-weight: bold;"),
        )
      ),
      column(
        6,
        div(
          align = "center", id = "SetEffects",
          div(img(src = "tree-solid.png", height = "128")),
          actionLink(inputId = "homeSustainabilityLink", label = "Sustainability", style = "color: #DE3979;font-size: 50pt;
                            font-family: Arial;font-weight: bold;"),
        )
      )
    )
  ),
  #### Background ####
  tabPanel("Background",
    icon = icon("info"),
    fluidRow(
      div(
        h2("The Enterprise and Skills Strategic Board"),

        p("The Enterprise and Skills Strategic Board was created in November 2017 in response to the", tags$span(a("Enterprise and Skills Review ", href = "https://www.gov.scot/publications/enterprise-skills-review-report-phase-2/"))),

        p("Its objective is to align and co-ordinate the activities of Scotland's enterprise and skills agencies: Scottish Enterprise, Highlands and Islands Enterprise, Skills Development Scotland, the Scottish Funding Council and the South of Scotland Enterprise Agency."),

        p("The Strategic Board seeks to maximise the impact of the collective investment that Scotland makes in enterprise and skills development, and to create the conditions that are conducive to delivering inclusive and sustainable growth."),

        p("The Strategic Board's ", tags$span(a("Strategic Plan ", href = "https://www.gov.scot/publications/working-collaboratively-better-scotland/")), "was published in October 2018, setting out its long term objectives and plans."),

        h2("Dashboard Purpose"),

        p("The Enterprise and Skills dashboard provides a visual overview of Scotland's performance in terms of Productivity, Equality, Wellbeing and Sustainability; the key areas of interest for the Enterprise and Skills Strategic Board."),

        p("The nature of these domains is such that marked improvements will take years, perhaps even decades.  For this reason the dashboard presents a set of indicators at National Performance Framework (NPF) level to help monitor progress towards these objectives. These indicators reflect key components of Productivity, Equality, Wellbeing and Sustainability."),

        h2("Performance Framework"),

        p("The Enterprise and Skills interactive dashboard is just one element of the Strategic Board's performance framework, the others are illustrated below.  Yellow boxes indicate outputs produced by the Analytical Unit."),

        img(src = "npf.png", width = "50%"),

        p("The ", tags$span(a("Annual Analysis ", href = "https://www.gov.scot/binaries/content/documents/govscot/publications/minutes/2020/03/enterprise-and-skills-strategic-board-january-2020/documents/essb-annual-analysis-2019/essb-annual-analysis-2019/govscot%3Adocument/ESSB%2B-%2BAnnual%2BAnalysis%2B2019%2B-%2BFinal%2B-%2B240220.pdf")), "and Quarterly Reports are available on the analytical page of the Strategic Board's webpage, and the Evaluation reports will be added when complete."),

        h2("Data"),

        p("The dashboard provides a visual overview of the data and can be explored by the domains of interest, or by equalities characteristics, region or sector.  It shows key trends and comparisons, both within Scotland and with other countries."),

        p("Data will be provided for the most recent period available, and disaggregated and international data will be provided where this is available. Data for Scotland is available on Scotland's official statistics", tags$span(a("Open Data Platfrom ", href = "https://statistics.gov.scot/home")), ".  For more detailed information, the", tags$span(a("National Performance Framework ", href = "https://nationalperformance.gov.scot/")), " contains the latest data and policy context for each indicator, as well as detailed information on data sources."),
        style = "margin: auto; width: 80%; font-size: 20px;"
      )
    )
  ),
  #### Productivity Section ####
  navbarMenu("Productivity",
    icon = icon("industry"),
    tabPanel("Summary Table",
      value = "productivitySummary",
      h2("Overview"),
      tabsetPanel(
        tabPanel(
          "Scotland",
          fluidRow(
            width = 12,
            column(
              width = 4,
              h2("Growth"),
              plotOutput("growth_plain_sco_lineplot", height = "200px")
            ),
            column(
              width = 4,
              h2("Productivity"),
              plotOutput("productivity_sco_lineplot", height = "200px")
            ),
            column(
              width = 4,
              h2("Reputation"),
              plotOutput("reputation_overview_sco_lineplot", height = "200px")
            )
          ),
          fluidRow(
            width = 12,
            column(
              width = 4,
              h2("Exporting"),
              plotOutput("exporting_plain_sco_lineplot", height = "200px")
            ),
            column(
              width = 4,
              h2("R&D"),
              plotOutput("rd_plain_sco_lineplot", height = "200px")
            ),
            column(
              width = 4,
              h2("Entrepreneurialism"),
              plotOutput("eactivity_sco_lineplot", height = "200px")
            )
          )
        ),
        tabPanel(
          "International",
          fluidRow(
            width = 12,
            column(
              width = 4,
              h2("Growth"),
              plotOutput("growth_overview_int_barplot", height = "200px"),
              p("Scotland's growth rate was ", text_scotland_thisyear_growth_int, "% in 2018, compared to ", text_oecd_thisyear_growth_int, "% in the OECD. Over the past 5 years, the average GDP growth rate was ", text_scotland_last5_growth_int, "% in Scotland, compared to ", text_oecd_last5_growth_int, "% in the OECD overall.")
            ),
            column(
              width = 4,
              h2("Productivity"),
              plotOutput("productivity_overview_int_barplot", height = "200px"),
              p("Scotland's productivity was ", text_scotland_thisyear_productivity_int, "% in 2018, compared to ", text_oecd_thisyear_productivity_int, "% in the OECD. Over the past 5 years, the average productivity was ", text_scotland_last5_productivity_int, "% in Scotland, compared to ", text_oecd_last5_productivity_int, "% in the OECD overall.")
            ),
            column(
              width = 4,
              h2("Exporting"),
              plotOutput("exporting_overview_int_barplot", height = "200px"),
              p("Scotland's exporting as % of GDP was ", text_scotland_thisyear_exporting_int, "% in 2018, compared to ", text_oecd_thisyear_exporting_int, "% in the OECD. Over the past 5 years, the average exporting as % of GDP was ", text_scotland_last5_exporting_int, "% in Scotland, compared to ", text_oecd_last5_exporting_int, "% in the OECD overall."),
              p("*(International & RUK)")
            )
          ),
          fluidRow(
            width = 12,
            column(
              width = 4,
              h2("R&D"),
              plotOutput("rd_overview_int_barplot", height = "200px"),
              p("Scotland's expenditure on R&D as % of GDP was ", text_scotland_thisyear_rd_int, "% in 2017, compared to ", text_oecd_thisyear_rd_int, "% in the OECD. Over the past 5 years, the average expenditure on R&D as % of GDP was ", text_scotland_last5_rd_int, "% in Scotland, compared to ", text_oecd_last5_rd_int, "% in the OECD overall.")
              # Pie chart
              # plotOutput("rd_overview_sco_pieplot", height = "170px")
            ),
            column(width = 4, ),
            column(width = 4, )
          )
        )
      ),


      # TU KONIEC ############

      ES_secondPage_UI("Productivity")
    ),
    tabPanel("Access to Superfast Broadband",
      value = "Access_to_superfast_broadband_page",
      ES_thirdPage_UI("accessBroadband", "Access to superfast broadband")
    ),
    tabPanel("Economic Growth",
      value = "Economic_growth_page",
      ES_thirdPage_UI("econGrowth", "Economic growth")
    ),
    tabPanel("Economic Participation",
      value = "Economic_participation_page",
      ES_thirdPage_UI("econParticipate", "Economic participation")
    ),
    tabPanel("Educational Attainment",
      value = "Educational_attainment_7_page",
      ES_thirdPage_UI("eduAttain7", "Educational attainment")
    ),
    tabPanel("Entrepreneurial Activity",
      value = "Entrepreneurial_activity_page",
      ES_thirdPage_UI("entAct", "Entrepreneurial activity")
    ),
    tabPanel("High Growth Businesses",
      value = "High_growth_businesses_page",
      ES_thirdPage_UI("highGrowth", "High growth businesses")
    ),
    tabPanel("Innovative Businesses",
      value = "Innovative_businesses_page",
      ES_thirdPage_UI("innovateBus", "Innovative businesses")
    ),
    tabPanel("International Exporting",
      value = "International_exporting_page",
      ES_thirdPage_UI("intExp", "International Exporting")
    ),
    tabPanel("Productivity",
      value = "Productivity_page",
      ES_thirdPage_UI("productivity", "Productivity")
    ),
    tabPanel("Scotland's Reputation",
      value = "Scotlands_Reputation_page",
      ES_thirdPage_UI("scotRep", "Scotland's Reputation")
    ),
    tabPanel("Skill Profile of the Population",
      value = "Skill_profile_of_the_population_page",
      ES_thirdPage_UI("skillProfile", "Skill Profile of the Population")
    ),
    tabPanel("Skills Shortage Vacancies",
      value = "Skills_shortage_vacancies_page",
      ES_thirdPage_UI("skillShort", "Skills shortage vacancies")
    ),
    tabPanel("Skills Underutilisation",
      value = "Skills_underutilisation_page",
      ES_thirdPage_UI("skillUnder", "Skills Underutilisation")
    ),
    tabPanel("Spend on Research and Development",
      value = "Spend_on_Research_and_Development_page",
      ES_thirdPage_UI("spendRD", "Spend on Research and Development")
    ),
    tabPanel("The Number of Businesses",
      value = "The_number_of_businesses_page",
      ES_thirdPage_UI("numberBusiness", "The number of businesses")
    ),
    tabPanel("Work Place Learning",
      value = "Work_place_learning_page",
      ES_thirdPage_UI("workLearn", "Work place learning")
    ),
    tabPanel("Young People's Participation",
      value = "Young_peoples_participation_page",
      ES_thirdPage_UI("youngParticipate", "Young people's participation")
    )
  ),
  #### Equality Section ####
  navbarMenu("Equality",
    icon = icon("people-carry"),
    tabPanel("Summary Table",
      value = "equalitySummary",
      h2("Overview"),
      tabsetPanel(
        tabPanel(
          "Scotland",
          fluidRow(
            width = 12,
            column(
              width = 4,
              h2("Gender"),
              plotOutput("gpaygap_eq_overview_int_lineplot", height = "200px"),
              p("Source: Annual Survey of Hours and Earnings"),
              p("Scotland's gender pay gap was ", gender_last_figure, "% in ", gender_year, ". In the past 5 years, the average gender pay gap was ", gender_5years_figure, "% .")
            ),
            column(
              width = 4,
              h2("Disability"),
              plotOutput("dempgap_eq_overview_int_lineplot", height = "200px"),
              p("Source: Annual Population Survey"),
              p("Scotland's disabled employment pay gap was ", disability_last_figure, "% in ", disability_year, ". In the past 5 years, the average disabled employment pay gap was ", disability_5years_figure, "% .")
            ),
            column(
              width = 4,
              h2("Age"),
              plotOutput("youthunemp_eq_overview_int_lineplot", height = "200px"),
              p("Source: Annual Population Survey"),
              p("Scotland's youth unemployment rate was ", age_last_figure, "% in ", age_year, ". In the past 5 years, the average youth unemployment rate was ", age_5years_figure, "% .")
            )
          ),
          fluidRow(
            width = 12,
            column(
              width = 4,
              h2("Ethnicity"),
              plotOutput("ethnicmgap_eq_overview_int_lineplot", height = "200px"),
              p("Source: Annual Population Survey"),
              p("Scotland's ethnic minority employment gap was ", ethnicity_last_figure, "% in ", ethnicity_year, ". In the past 5 years, the average ethnic minority employment gap was ", ethnicity_5years_figure, "% .")
            ),
            column(
              width = 4,
              h2("Socio-economic status"),
              plotOutput("socioecon_eq_overview_int_barplot", height = "200px"),
              p("Source: Regional employment patterns in Scotland"),
              p("In 2018, employment gap for the most deprived (Q1) was 62.5%, compared to 79.2% for the least deprived (Q5).")
            ),
            column(width = 4, )
          )
        )
      ),
      ES_secondPage_UI("Equality")
    ),
    tabPanel("Gender Balance in Organisations",
      value = "Gender_balance_in_organisations_page",
      ES_thirdPage_UI("genderBalance", "Gender balance in organisations")
    ),
    tabPanel("Income Inequality",
      value = "Income_inequality_page",
      ES_thirdPage_UI("incomeInequal", "Income inequality")
    ),
    tabPanel("Pay Gap",
      value = "Pay_gap_page",
      ES_thirdPage_UI("payGap", "Pay gap")
    )
  ),
  #### Wellbeing Section ####
  navbarMenu("Wellbeing",
    icon = icon("spa"),
    tabPanel("Summary Table",
      value = "wellbeingSummary",
      h2("Overview"),
      tabsetPanel(
        tabPanel(
          "Scotland",
          fluidRow(
            width = 12,
            column(
              width = 4,
              h2("Employee Voice"),
              plotOutput("evoice_sco_lineplot", height = "200px")
            ),
            column(
              width = 4,
              h2("Living Wage"),
              plotOutput("livingwage_sco_lineplot", height = "200px")
            ),
            column(
              width = 4,
              h2("Mental Wellbeing"),
              plotOutput("mwellbeing_sco_lineplot", height = "200px")
            )
          )
        )
      ),
      ES_secondPage_UI("Wellbeing")
    ),
    tabPanel("Employee Voice",
      value = "Employee_voice_page",
      ES_thirdPage_UI("employeeVoice", "Employee voice")
    ),
    tabPanel("Employees on the Living Wage",
      value = "Employees_on_the_Living_wage_page",
      ES_thirdPage_UI("empLivingWage", "Employees on the Living wage")
    ),
    tabPanel("Mental Wellbeing",
      value = "Mental_wellbeing_page",
      ES_thirdPage_UI("mentalWellbeing", "Mental wellbeing")
    ),
    tabPanel("Social Capital",
      value = "Social_capital_page",
      ES_thirdPage_UI("socialCapital", "Social capital")
    )
  ),
  #### Sustainability Section ####
  navbarMenu("Sustainability",
    icon = icon("tree"),
    tabPanel("Summary Table",
      value = "sustainabilitySummary",
      h2("Overview"),
      tabsetPanel(
        tabPanel(
          "Scotland",
          fluidRow(
            width = 12,
            column(
              width = 4,
              h2("Carbon Footprint"),
              plotOutput("cfootprint_sco_lineplot", height = "200px")
            ),
            column(
              width = 4,
              h2("Natural Capital"),
              plotOutput("naturalf_sco_lineplot", height = "200px")
            ),
            column(
              width = 4,
              h2("Greenhouse Gas E."),
              plotOutput("gasemissions_sco_lineplot", height = "200px")
            )
          )
        )
      ),
      ES_secondPage_UI("Sustainability")
    ),
    tabPanel("Carbon Footprint",
      value = "Carbon_footprint_page",
      ES_thirdPage_UI("carbonFoot", "Carbon footprint")
    ),
    tabPanel("Energy from Renewable Sources",
      value = "Energy_from_renewable_sources_page",
      ES_thirdPage_UI("energyRenew", "Energy from renewable sources")
    ),
    tabPanel("Greenhouse Gas Emissions",
      value = "Greenhouse_gas_emissions_page",
      ES_thirdPage_UI("greenGas", "Greenhouse gas emissions")
    ),
    tabPanel("Natural Capital",
      value = "Natural_capital_page",
      ES_thirdPage_UI("natCap", "Natural capital")
    )
  )
)


#############################################################################################################
#### Combining all UI Elements Together ####
ui <- fluidPage(
  theme = shinytheme("paper"), ## Sets the base blue theme
  #### Including Custom Stylings which are kept in separate file ####
  includeCSS("./www/Strategic Board Dashboard Style.css"),

  #### Allowing certain javascript elements (alerts, elements and intro) ####
  useShinyjs(), ## Enabling javascript elements

  #### Beginning of app ####
  ## Header ##
  div(
    header,
    style = "background-color: #DE3979; padding: 0px; margin: -2em; color: #DE3979;"
  ),
  ## Body ##
  div(
    body,
    style = "margin: -1em"
  ),

  # FOOTER ##########################################################################################################################################
  fluidRow(
    br(),
    wellPanel(
      fluidRow(
        # FOOTER - ABOUT
        # column(width = 3,
        #        icon("info", lib = "font-awesome"),
        #        strong("ABOUT"),
        #        p("The Economy Board requested that OCEA lead on developing a performance framework for the Economy Board, taking into account the framework developed for the Enterprise and Skills Strategic Board."),
        # ),
        # FOOTER - COPYRIGHT NOTICE
        column(
          width = 4,
          icon("copyright", lib = "font-awesome"),
          strong("COPYRIGHT NOTICE"),
          p(
            "You may use or re-use this information (not including logos) free of charge in any format or medium, under the terms of the ",
            a("Open Government Licence", href = "http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/"), "."
          )
        ),
        # FOOTER - CONTACT DETAILS
        column(
          width = 4,
          icon("at", lib = "font-awesome"),
          strong("CONTACT DETAILS"),
          p("We welcome your feedback:"),
          p("EnterpriseandskillsPMO@gov.scot", style = "line-height: 0%;")
        ),
        # FOOTER - EXTERNAL LINKS
        column(
          width = 4,
          icon("external-link", lib = "font-awesome"),
          strong("EXTERNAL LINKS"),
          p(a("Open Data Platfrom", href = "https://statistics.gov.scot/data/search")),
          p(a("National Performance Framework", href = "https://nationalperformance.gov.scot/")),
          p(a("SCRIG ", href = "https://www.inclusivegrowth.scot/")),
          p(a("SCRIG dashboard ", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"))
        )
      ),
      fluidRow(
        p("Reload the page should you experience any issues."),
        style = "text-align: center; outline: 0px;"
      )
    )
  )
)
