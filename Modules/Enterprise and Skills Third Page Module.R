#### Enterprise and Skills 3rd Page Module ####

#### UI Component ####
ES_thirdPage_UI <- function(id, variableName) {
  #### Defining Namespace ####
  ns <- NS(id)

  #### Beginning of Page ####
  page <- NULL

  ## Getting List of Diaggregations ##
  disag <- label_info %>%
    filter(Indicator == variableName) %>%
    pull(Disaggregation) %>%
    unique()
  temp1 <- disag[disag != "Scotland"] %>% sort()

  if (length(temp1) > 0) {
    ## Subbing Out Regions ##
    if (length(temp1[temp1 %in% c("Local Authority", "Local Authority area", "Region", "Region of Ownership")]) > 0) {
      temp1 <- temp1[!(temp1 %in% c("Local Authority", "Local Authority area", "Region", "Region of Ownership"))]
      temp1 <- c(temp1, "Regions")
    }

    ## Subbing out equalities ##
    if (length(temp1[temp1 %in% equalities_list]) > 0) {
      temp1 <- temp1[!(temp1 %in% equalities_list)]
      temp1 <- c(temp1, "Equality Characteristics")
    }

    ## Subbing Out Employee Sizebands ##
    if (length(temp1[temp1 %in% c("Employee Sizeband", "Establishment size")]) > 0) {
      temp1 <- temp1[!(temp1 %in% c("Employee Sizeband", "Establishment size"))]
      temp1 <- c(temp1, "Employee Sizeband")
    }

    disag <- c("Scotland", temp1)
  } else {
    disag <- "Scotland"
  }

  if (length(disag) > 1) {
    disag <- c(disag, "All Available Breakdowns")
  }

  if (!(toupper(variableName) %in% c("ENTREPRENEURIAL ACTIVITY", "PRODUCTIVITY", "SPEND ON RESEARCH AND DEVELOPMENT"))) {
    page <- tagList(
      fluidRow(
        sidebarLayout(
          sidebarPanel(
            fluidRow(
              column(12,
                align = "left",
                p("The options below allow you to change what is displayed on the right."),
                awesomeRadio(
                  inputId = ns("graphOrTable"),
                  label = "I want to see: ",
                  selected = "All",
                  choices = c("All", "Only Graphs", "Only Tables"),
                  status = "warning"
                ),

                p(paste("The disaggregation choices below reflect the options that are available for the indicator, which is why options may be different from another page.")),

                ## Disaggregation Filter ##
                awesomeRadio(
                  inputId = ns("sidebarRadio"),
                  label = "I want to compare: ",
                  selected = "Scotland",
                  choices = disag %>% unique(),
                  status = "warning"
                ),

                ## Drop Down Menus ##
                ## Drop Down Menus ##
                if (length(disag) > 1) {
                  p("The options below reflect the options available to the indicator, which means the options may be different than those seen on a different page.")
                },

                ## Regions ##
                uiOutput(ns("regionalDropDown")),

                ## Equality Characteristics ##
                uiOutput(ns("equalityDropDown")),

                ## Employee Sizeband ##
                uiOutput(ns("employeeDropDown")),

                ## Energy Type ##
                uiOutput(ns("energyDropDown")),

                ## Habitat ##
                uiOutput(ns("habitatDropDown")),

                ## Sector ##
                uiOutput(ns("sectorDropDown")),

                ## Category ##
                uiOutput(ns("categoryDropDown")),
              )
            )
          ),
          mainPanel(
            fluidRow(
              column(
                12,
                h3(variableName),
                uiOutput(ns("graphExplain")),
                ggiraphOutput(ns("mainPagePlot"), width = "100%") %>% withSpinner()
              )
            )
          )
        ),
        hr(),
        DTOutput(ns("mainPageTable"), width = "100%") %>% withSpinner()
      )
    )
  } else {
    page <- tabsetPanel(
      tabPanel(
        title = div("Trend Data", class = "thirdPageHeader"),

        sidebarLayout(
          sidebarPanel(fluidRow(
            p("The options below allow you to change what is displayed on the right."),

            column(12,
              align = "left",
              awesomeRadio(
                inputId = ns("graphOrTable"),
                label = "I want to see: ",
                selected = "All",
                choices = c("All", "Only Graphs", "Only Tables"),
                status = "warning"
              ),
              p(paste("The disaggregation choices below reflect the options that are available for the indicator, which is why options may be different from another page.")),

              ## Disaggregation Filter ##
              awesomeRadio(
                inputId = ns("sidebarRadio"),
                label = "I want to compare: ",
                selected = "Scotland",
                choices = disag %>% unique(),
                status = "warning"
              ),

              ## Drop Down Menus ##
              if (length(disag) > 1) {
                p("The options below reflect the options available to the indicator, which means the options may be different than those seen on a different page.")
              },

              ## Regions ##
              uiOutput(ns("regionalDropDown")),

              ## Equality Characteristics ##
              uiOutput(ns("equalityDropDown")),

              ## Employee Sizeband ##
              uiOutput(ns("employeeDropDown")),

              ## Energy Type ##
              uiOutput(ns("energyDropDown")),

              ## Habitat ##
              uiOutput(ns("habitatDropDown")),

              ## Sector ##
              uiOutput(ns("sectorDropDown")),

              ## Category ##
              uiOutput(ns("categoryDropDown"))
            )
          )),
          mainPanel(
            fluidRow(
              column(
                12,
                h3(variableName),
                uiOutput(ns("graphExplain")),
                ggiraphOutput(ns("mainPagePlot"), width = "100%") %>% withSpinner()
              )
            )
          )
        ),
        hr(),
        DTOutput(ns("mainPageTable")) %>% withSpinner()
      ),
      tabPanel(
        title = "International Data",
        column(
          12,
          p(
            paste(
              variableName,
              "has an international comparator available which means Scotland can be ranked against other OECD countries.",
              "Below is a graph to illustrate how Scotland's performance compares to these countries."
            )
          ),
          p(paste(
            "You can hover over the chart to get more information about these countries performance.",
            "The tool bar for the graph appears when hovered over the chart",
            "You can also zoom in on the graph by clicking the magnifying glass in the tool bar on the bottom left and then either double clicking or using the scroll bar on a mouse when hovered over the graph.",
            "Any changes can be reset by clicking on the button in the tool bar on the bottom left.",
            "You can also save the images as a PNG file by clicking the download button."
          )),

          ggiraphOutput(ns("internationalGraph"), width = "100%") %>% withSpinner(),
          p(textOutput(ns("intCaveat")))
        )
      )
    )
  }

  return(page)
}

#### Server Component ####
ES_thirdPage_Server <- function(input, output, session, dataset, ID_name, internationalData = NA,
                                CurrentPage = NA) {
  #### Setting the Namespace (needs to match the one for the page) ####
  ns <- NS(namespace = ID_name)

  page_name <- paste(
    dataset %>% pull(Indicator) %>% unique(),
    "page"
  )
  page_name <- gsub(" ", "_", page_name, fixed = TRUE)
  page_name <- gsub("'", "", page_name, fixed = TRUE)

  #### Getting What Should be in the radio buttons ####
  disagg1 <- dataset %>%
    filter(Disaggregation != "Scotland") %>%
    pull(Disaggregation) %>%
    unique()

  ## Subbing Out Regions ##
  if (length(disagg1[disagg1 %in% c("Local Authority", "Local Authority area", "Region", "Region of Ownership")]) > 0) {
    disagg1 <- disagg1[!(disagg1 %in% c("Local Authority", "Local Authority area", "Region", "Region of Ownership"))]
    disagg1 <- c(disagg1, "Regions")
  }

  ## Subbing out equalities ##
  if (length(disagg1[disagg1 %in% equalities_list]) > 0) {
    disagg1 <- disagg1[!(disagg1 %in% equalities_list)]
    disagg1 <- c(disagg1, "Equality Characteristics")
  }

  ## Subbing Out Employee Sizebands ##
  if (length(disagg1[disagg1 %in% c("Employee Sizeband", "Establishment size")]) > 0) {
    disagg1 <- disagg1[!(disagg1 %in% c("Employee Sizeband", "Establishment size"))]
    disagg1 <- c(disagg1, "Employee Sizeband")
  }

  disagg1 <- disagg1 %>% unique()


  output$disaggAvailable <- reactive({
    sidebarOptions <- disagg1
    return(sidebarOptions %>% unique())
  })

  #### Getting the Various DropDown Options ####
  ### Regional Dropdown options ###
  output$regionalDropDown <- renderUI({
    dataRegion <- dataset %>%
      filter(Disaggregation %in% c("Local Authority", "Local Authority area", "Region", "Region of Ownership")) %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()

    region_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(region_list))
    names(finalList) <- region_list

    for (i in seq_along(region_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == region_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }

    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]

      picker <- pickerInput(
        inputId = ns("regionalDropDown"),
        label = "Select regions from the drop down list below: ",
        choices = finalList,
        selected = selected,
        options = list(
          `live-search` = TRUE,
          `size` = 5,
          `max-options` = 6
        ),
        choicesOpt = list(
          `optgroup-header` = rep_len("font-size: 150%", length(region_list))
        ),
        multiple = TRUE
      )

      entireDropdown <- fluidRow(
        column(
          12,
          picker
        ),
        column(
          6,
          actionBttn(inputId = ns("regionAddAll"), label = "Add All", style = "simple", color = "warning")
        ),
        column(
          6,
          actionBttn(inputId = ns("regionRemoveAll"), label = "Remove All", style = "simple", color = "warning")
        )
      )

      if (((length(input$sidebarRadio[input$sidebarRadio %in% c("Regions", "All Available Breakdowns")]) > 0) & (length(disagg1[disagg1 %in% c("Regions")]) > 0))) {
        return(entireDropdown)
      }
    }
  })

  ### Equality Characteristics options ###
  output$equalityDropDown <- renderUI({
    dataRegion <- dataset %>%
      filter(Disaggregation %in% equalities_list) %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()


    equal_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(equal_list))
    names(finalList) <- equal_list

    for (i in seq_along(equal_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == equal_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }
    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]

      picker <- pickerInput(
        inputId = ns("equalDropDown"),
        label = "Select equality characteristics from the drop down list below: ",
        choices = finalList,
        selected = selected,
        options = list(
          `live-search` = TRUE,
          `size` = 5,
          `max-options` = 6
        ),
        choicesOpt = list(
          `optgroup-header` = rep_len("font-size: 150%", length(equal_list))
        ),
        multiple = TRUE
      )

      entireDropdown <- fluidRow(
        column(
          12,
          picker
        ),
        column(
          6,
          actionBttn(inputId = ns("equalAddAll"), label = "Add All", style = "simple", color = "warning")
        ),
        column(
          6,
          actionBttn(inputId = ns("equalRemoveAll"), label = "Remove All", style = "simple", color = "warning")
        )
      )

      if (((length(input$sidebarRadio[input$sidebarRadio %in% c("Equality Characteristics", "All Available Breakdowns")]) > 0) & (length(disagg1[disagg1 %in% c("Equality Characteristics")]) > 0))) {
        return(entireDropdown)
      }
    }
  })

  ### Employee Sizeband options ###
  output$employeeDropDown <- renderUI({
    dataRegion <- dataset %>%
      filter(Disaggregation %in% c("Employee Sizeband", "Establishment size")) %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()


    equal_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(equal_list))
    names(finalList) <- equal_list

    for (i in seq_along(equal_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == equal_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }

    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]

      picker <- pickerInput(
        inputId = ns("employeeDropDown"),
        label = "Select employee sizebands from the drop down list below: ",
        choices = finalList,
        selected = selected,
        options = list(
          `live-search` = TRUE,
          `size` = 5,
          `max-options` = 6
        ),
        choicesOpt = list(
          `optgroup-header` = rep_len("font-size: 150%", length(equal_list))
        ),
        multiple = TRUE
      )

      entireDropdown <- fluidRow(
        column(
          12,
          picker
        ),
        column(
          6,
          actionBttn(inputId = ns("employeenAddAll"), label = "Add All", style = "simple", color = "warning")
        ),
        column(
          6,
          actionBttn(inputId = ns("employeeRemoveAll"), label = "Remove All", style = "simple", color = "warning")
        )
      )

      if (length(empShow) > 0) {
        if (((length(input$sidebarRadio[input$sidebarRadio %in% c("Employee Sizeband", "All Available Breakdowns")]) > 0) & (length(disagg1[disagg1 %in% c("Employee Sizeband")]) > 0))) {
          return(entireDropdown)
        }
      }
    }
  })

  ### Energy Type options ###
  output$energyDropDown <- renderUI({
    dataRegion <- dataset %>%
      filter(Disaggregation %in% "Energy type") %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()


    equal_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(equal_list))
    names(finalList) <- equal_list

    for (i in seq_along(equal_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == equal_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }
    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]

      picker <- pickerInput(
        inputId = ns("energyDropDown"),
        label = "Select energy type from the drop down list below: ",
        choices = finalList,
        selected = selected,
        options = list(
          `live-search` = TRUE,
          `size` = 5,
          `max-options` = 6
        ),
        choicesOpt = list(
          `optgroup-header` = rep_len("font-size: 150%", length(equal_list))
        ),
        multiple = TRUE
      )

      entireDropdown <- fluidRow(
        column(
          12,
          picker
        ),
        column(
          6,
          actionBttn(inputId = ns("energyAddAll"), label = "Add All", style = "simple", color = "warning")
        ),
        column(
          6,
          actionBttn(inputId = ns("energyRemoveAll"), label = "Remove All", style = "simple", color = "warning")
        )
      )

      if (((length(input$sidebarRadio[input$sidebarRadio %in% c("Energy type", "All Available Breakdowns")]) > 0) & (length(disagg1[disagg1 %in% c("Energy type")]) > 0))) {
        return(entireDropdown)
      }
    }
  })

  ### Habitat options ###
  output$habitatDropDown <- renderUI({
    dataRegion <- dataset %>%
      filter(Disaggregation %in% "Habitat") %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()


    equal_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(equal_list))
    names(finalList) <- equal_list

    for (i in seq_along(equal_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == equal_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }

    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]

      picker <- pickerInput(
        inputId = ns("habitatDropDown"),
        label = "Select habitat type from the drop down list below: ",
        choices = finalList,
        selected = selected,
        options = list(
          `live-search` = TRUE,
          `size` = 5,
          `max-options` = 6
        ),
        choicesOpt = list(
          `optgroup-header` = rep_len("font-size: 150%", length(equal_list))
        ),
        multiple = TRUE
      )

      entireDropdown <- fluidRow(
        column(
          12,
          picker
        ),
        column(
          6,
          actionBttn(inputId = ns("habitatAddAll"), label = "Add All", style = "simple", color = "warning")
        ),
        column(
          6,
          actionBttn(inputId = ns("habitatRemoveAll"), label = "Remove All", style = "simple", color = "warning")
        )
      )

      if (((length(input$sidebarRadio[input$sidebarRadio %in% c("Habitat", "All Available Breakdowns")]) > 0) & (length(disagg1[disagg1 %in% c("Habitat")]) > 0))) {
        return(entireDropdown)
      }
    }
  })

  ### Sector options ###
  output$sectorDropDown <- renderUI({
    dataRegion <- dataset %>%
      filter(Disaggregation %in% "Sector") %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()


    equal_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(equal_list))
    names(finalList) <- equal_list

    for (i in seq_along(equal_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == equal_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }

    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]

      picker <- pickerInput(
        inputId = ns("sectorDropDown"),
        label = "Select sectors from the drop down list below: ",
        choices = finalList,
        selected = selected,
        options = list(
          `live-search` = TRUE,
          `size` = 5,
          `max-options` = 6
        ),
        choicesOpt = list(
          `optgroup-header` = rep_len("font-size: 150%", length(equal_list))
        ),
        multiple = TRUE
      )

      entireDropdown <- fluidRow(
        column(
          12,
          picker
        ),
        column(
          6,
          actionBttn(inputId = ns("sectorAddAll"), label = "Add All", style = "simple", color = "warning")
        ),
        column(
          6,
          actionBttn(inputId = ns("sectorRemoveAll"), label = "Remove All", style = "simple", color = "warning")
        )
      )

      if (((length(input$sidebarRadio[input$sidebarRadio %in% c("Sector", "All Available Breakdowns")]) > 0) & (length(disagg1[disagg1 %in% c("Sector")]) > 0))) {
        return(entireDropdown)
      }
    }
  })

  ### Category options ###
  output$categoryDropDown <- renderUI({
    dataRegion <- dataset %>%
      filter(Disaggregation %in% "Category") %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()


    equal_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(equal_list))
    names(finalList) <- equal_list

    for (i in seq_along(equal_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == equal_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }

    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]

      picker <- pickerInput(
        inputId = ns("categoryDropDown"),
        label = "Select categories from the drop down list below: ",
        choices = finalList,
        selected = selected,
        options = list(
          `live-search` = TRUE,
          `size` = 5,
          `max-options` = 6
        ),
        choicesOpt = list(
          `optgroup-header` = rep_len("font-size: 150%", length(equal_list))
        ),
        multiple = TRUE
      )

      entireDropdown <- fluidRow(
        column(
          12,
          picker
        ),
        column(
          6,
          actionBttn(inputId = ns("categoryAddAll"), label = "Add All", style = "simple", color = "warning")
        ),
        column(
          6,
          actionBttn(inputId = ns("categoryRemoveAll"), label = "Remove All", style = "simple", color = "warning")
        )
      )

      if (((length(input$sidebarRadio[input$sidebarRadio %in% c("Category", "All Available Breakdowns")]) > 0) & (length(disagg1[disagg1 %in% c("Category")]) > 0))) {
        return(entireDropdown)
      }
    }
  })
  #### Adding Reactions to the "Add All" Buttons ####
  ## Regional ##
  observeEvent(input$regionAddAll, ignoreInit = TRUE, {
    dataRegion <- dataset %>%
      filter(Disaggregation %in% c("Local Authority", "Local Authority area", "Region", "Region of Ownership")) %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()


    equal_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(equal_list))
    names(finalList) <- equal_list

    for (i in seq_along(equal_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == equal_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }

    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]

      updatePickerInput(
        session = session,
        "regionalDropDown",
        selected = selected
      )
    }
  })

  ## Equality Characteristics ##
  observeEvent(input$equalAddAll, ignoreInit = TRUE, {
    dataRegion <- dataset %>%
      filter(Disaggregation %in% equalities_list) %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()


    equal_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(equal_list))
    names(finalList) <- equal_list

    for (i in seq_along(equal_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == equal_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }

    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]


      updatePickerInput(
        session = session,
        "equalDropDown",
        selected = selected
      )
    }
  })

  ## Employee Sizeband ##
  observeEvent(input$employeenAddAll, ignoreInit = TRUE, {
    dataRegion <- dataset %>%
      filter(Disaggregation %in% c("Employee Sizeband", "Establishment size")) %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()


    equal_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(equal_list))
    names(finalList) <- equal_list

    for (i in seq_along(equal_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == equal_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }

    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]


      updatePickerInput(
        session = session,
        "employeeDropDown",
        selected = selected
      )
    }
  })


  ## Energy Type  ##
  observeEvent(input$energyAddAll, ignoreInit = TRUE, {
    dataRegion <- dataset %>%
      filter(Disaggregation %in% c("Energy type")) %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()


    equal_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(equal_list))
    names(finalList) <- equal_list

    for (i in seq_along(equal_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == equal_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }

    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]


      updatePickerInput(
        session = session,
        "energyDropDown",
        selected = selected
      )
    }
  })

  ## Habitat ##
  observeEvent(input$habitatAddAll, ignoreInit = TRUE, {
    dataRegion <- dataset %>%
      filter(Disaggregation %in% c("Habitat")) %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()


    equal_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(equal_list))
    names(finalList) <- equal_list

    for (i in seq_along(equal_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == equal_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }

    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]


      updatePickerInput(
        session = session,
        "habitatDropDown",
        selected = selected
      )
    }
  })

  ## Sector ##
  observeEvent(input$sectorAddAll, ignoreInit = TRUE, {
    dataRegion <- dataset %>%
      filter(Disaggregation %in% c("Sector")) %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()


    equal_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(equal_list))
    names(finalList) <- equal_list

    for (i in seq_along(equal_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == equal_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }

    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]


      updatePickerInput(
        session = session,
        "sectorDropDown",
        selected = selected
      )
    }
  })

  ## Category ##
  observeEvent(input$categoryAddAll, ignoreInit = TRUE, {
    dataRegion <- dataset %>%
      filter(Disaggregation %in% c("Category")) %>%
      select(Disaggregation, Breakdown) %>%
      distinct() %>%
      as.data.frame()


    equal_list <- dataRegion %>%
      pull(Disaggregation) %>%
      unique() %>%
      sort()
    finalList <- vector(mode = "list", length = length(equal_list))
    names(finalList) <- equal_list

    for (i in seq_along(equal_list)) {
      options <- dataRegion %>%
        filter(Disaggregation == equal_list[i]) %>%
        pull(Breakdown) %>%
        sort()

      finalList[[i]] <- options
    }

    if (length(finalList) > 0) {
      selected <- finalList[[1]]
      selected <- selected[1:6]


      updatePickerInput(
        session = session,
        "categoryDropDown",
        selected = selected
      )
    }
  })

  #### Adding Reactions to the "Remove All" Buttons ####
  ## Regional ##
  observeEvent(input$regionRemoveAll, ignoreInit = TRUE, {
    updatePickerInput(
      session = session,
      "regionalDropDown",
      selected = ""
    )
  })

  ## Equality Characteristics ##
  observeEvent(input$equalRemoveAll, ignoreInit = TRUE, {
    updatePickerInput(
      session = session,
      "equalDropDown",
      selected = ""
    )
  })

  ## Employee Sizeband ##
  observeEvent(input$employeeRemoveAll, ignoreInit = TRUE, {
    updatePickerInput(
      session = session,
      "employeeDropDown",
      selected = ""
    )
  })


  ## Energy Type  ##
  observeEvent(input$energyRemoveAll, ignoreInit = TRUE, {
    updatePickerInput(
      session = session,
      "energyDropDown",
      selected = ""
    )
  })

  ## Habitat ##
  observeEvent(input$habitatRemoveAll, ignoreInit = TRUE, {
    updatePickerInput(
      session = session,
      "habitatDropDown",
      selected = ""
    )
  })

  ## Sector ##
  observeEvent(input$sectorRemoveAll, ignoreInit = TRUE, {
    updatePickerInput(
      session = session,
      "sectorDropDown",
      selected = ""
    )
  })

  ## Category ##
  observeEvent(input$categoryRemoveAll, ignoreInit = TRUE, {
    updatePickerInput(
      session = session,
      "categoryDropDown",
      selected = ""
    )
  })
  #### Consolodating all the responses into one breakdown list ####
  breakdownFilter <- reactive({
    gather1 <- NA

    if (length(input$sidebarRadio) > 0) {
      if ((input$sidebarRadio %in% c("Scotland", "All Available Breakdowns"))) {
        gather1 <- c(gather1, "Scotland")
      }
      if ((input$sidebarRadio %in% c("Regions", "All Available Breakdowns"))) {
        gather1 <- c(gather1, input$regionalDropDown)
      }
      if ((input$sidebarRadio %in% c("Equality Characteristics", "All Available Breakdowns"))) {
        gather1 <- c(gather1, input$equalDropDown)
      }
      if ((input$sidebarRadio %in% c("Employee Sizeband", "All Available Breakdowns"))) {
        gather1 <- c(gather1, input$employeeDropDown)
      }
      if ((input$sidebarRadio %in% c("Energy type", "All Available Breakdowns"))) {
        gather1 <- c(gather1, input$energyDropDown)
      }
      if ((input$sidebarRadio %in% c("Habitat", "All Available Breakdowns"))) {
        gather1 <- c(gather1, input$habitatDropDown)
      }
      if ((input$sidebarRadio %in% c("Sector", "All Available Breakdowns"))) {
        gather1 <- c(gather1, input$sectorDropDown)
      }
      if ((input$sidebarRadio %in% c("Category", "All Available Breakdowns"))) {
        gather1 <- c(gather1, input$categoryDropDown)
      }
    }

    gather1 <- gather1 %>%
      na.omit() %>%
      as.character()

    if (length(gather1) > 0) {
      return(gather1)
    }
  })
  #### Collecting data for the graph ####
  dataGraph <- reactive({
    if (page_name == CurrentPage()) {
      if (!is.null(breakdownFilter())) {
        data_want <- dataset %>%
          filter(Breakdown %in% breakdownFilter()) %>%
          distinct() %>%
          as.data.frame()

        if ("Scotland" %in% breakdownFilter()) {
          data1 <- dataset %>%
            filter(Disaggregation == "Scotland") %>%
            distinct() %>%
            as.data.frame() %>%
            mutate(Breakdown = "Scotland")

          data_want <- bind_rows(data_want, data1) %>% distinct()
        }

        data_want <- data_want %>%
          distinct() %>%
          mutate(FormattedFigures = case_when(
            is.na(Figure) ~ "-",
            dataType == "£ MILLION" ~ dollar(Figure, accuracy = 1, scale = 1, prefix = "£", suffix = " million"),
            dataType == "INDEX" ~ number(Figure, accuracy = 0.1, scale = 1, big.mark = ","),
            dataType == "MILLION TONNES" ~ number(Figure, accuracy = 0.1, scale = 1, big.mark = ",", suffix = " million tonnes"),
            dataType == "PERCENTAGE  SMALL(%)" ~ percent(Figure, accuracy = 0.01, scale = 1),
            dataType == "PERCENTAGE (%)" ~ percent(Figure, accuracy = 0.1, scale = 1),
            dataType == "PERCENTAGE POINTS (%)" ~ percent(abs(Figure), accuracy = 0.1, scale = 1, suffix = "pp", prefix = case_when(
              Figure < 0 ~ "-",
              Figure == 0 ~ "<=>",
              Figure > 0 ~ "+"
            )),
            dataType == "PERCENTAGE POINTS SMALL(%)" ~ percent(Figure, accuracy = 0.01, scale = 1, suffix = "pp"),
            dataType == "PERCENTAGE WHOLE (%)" ~ percent(Figure, accuracy = 1, scale = 1),
            dataType == "RANK" ~ number(Figure, accuracy = 1, scale = 1, big.mark = ",", suffix = case_when(
              str_sub(Figure, -2) == 11 ~ "th",
              str_sub(Figure, -2) == 12 ~ "th",
              str_sub(Figure, -2) == 13 ~ "th",
              str_sub(Figure, -1) == 1 ~ "st",
              str_sub(Figure, -1) == 2 ~ "nd",
              str_sub(Figure, -1) == 3 ~ "rd",
              TRUE ~ "th"
            )),
            dataType == "SCORE" ~ number(Figure, accuracy = 0.1, scale = 1, big.mark = ","),
            dataType == "VALUE" ~ number(Figure, accuracy = 1, scale = 1, big.mark = ",")
          )) %>%
          mutate(HoverText = paste(
            "Year:", Year, "\n",
            "Breakdown:", Breakdown, "\n",
            "Figure:", FormattedFigures
          )) %>%
          mutate(Yearlab = str_sub(Year, -4)) %>%
          arrange(Yearlab, Year) %>%
          select(-Yearlab) %>%
          mutate(Breakdown = str_wrap(Breakdown, 20))

        return(data_want %>% as.data.frame())
      }
    }
  })

  #### Creating the Graph ####
  output$mainPagePlot <- renderGirafe({
    if (!is.null(dataGraph())) {
      data1 <- dataGraph() %>%
        filter(!is.na(Figure))

      if (nrow(data1) > 0) {
        year_test <- data1 %>%
          pull(Year) %>%
          unique() %>%
          length()

        y_label <- label_info %>%
          filter(Indicator == data1 %>%
            pull(Indicator) %>%
            unique()) %>%
          pull(Y_Label) %>%
          unique()

        showLegend <- case_when(
          length(unique(data1$Breakdown)) == 1 ~ FALSE,
          TRUE ~ TRUE
        )

        angle_text <- case_when(
          year_test > 10 ~ 90,
          TRUE ~ 0
        )

        hjust_text <- case_when(
          year_test > 10 ~ 1,
          TRUE ~ 0.5
        )

        y_lower <- case_when(
          data1 %>% pull(Figure) %>% min(na.rm = TRUE) < 0 ~ label_info %>%
            filter(Indicator == data1 %>%
              pull(Indicator) %>%
              unique()) %>%
            pull(`Lower Lim`) %>%
            unique() %>%
            as.numeric(),
          TRUE ~ 0
        )

        y_higher <- data1 %>%
          pull(Figure) %>%
          min(na.rm = TRUE) < 0 ~ label_info %>%
          filter(Indicator == data1 %>%
            pull(Indicator) %>%
            unique()) %>%
          pull(`Higher Lim`) %>%
          unique() %>%
          as.numeric()

        graph1 <- NULL
        if (year_test > 1) {
          if (data1 %>% pull(Indicator) %>% unique() == "Economic participation") {
            data1$Year <- factor(data1$Year, levels = data1$Year)
          }

          graph1 <- ggplot(data = data1, aes(x = Year, y = Figure, group = Breakdown, colour = Breakdown)) +
            geom_line(size = 2) +
            geom_point_interactive(aes(tooltip = HoverText), size = 1.5) +
            ggtitle(data1 %>% pull(Measure) %>% unique() %>% str_wrap(width = 75)) +
            labs(caption = paste("Source:", data1 %>% pull(Source) %>% unique())) +
            scale_x_discrete(name = "", expand = expand_scale(add = c(0.6, 1))) +
            scale_y_continuous(name = y_label, limits = c(y_lower, NA)) +
            facet_grid(cols = vars(Disaggregation), scales = "free") +
            theme(
              text = element_text(size = 14, color = "black", face = "bold"),
              axis.line = element_line(colour = "grey50"),
              axis.ticks.length = unit(0.25, "cm"),
              panel.grid.major.y = element_line(colour = "slategrey", linetype = "dotted"),
              panel.grid.major.x = element_line(colour = "gray98", linetype = "solid"),
              axis.text.x = element_text(angle = angle_text, hjust = hjust_text),
              panel.background = element_rect(fill = "gray98"),
              plot.title = element_text(hjust = 0.5),
              panel.spacing = unit(2, "lines"),
              strip.background = element_blank(),
              strip.text = element_text(family = "Ariel", size = 14),
              legend.position = case_when(
                showLegend == FALSE ~ "none",
                showLegend == TRUE ~ "right"
              )
            )
        } else {
          graph1 <- ggplot(data = data1, aes(x = Breakdown, y = Figure)) +
            geom_col_interactive(aes(tooltip = HoverText), fill = "deepskyblue") +
            ggtitle(data1 %>% pull(Measure) %>% unique()) +
            labs(caption = paste("Source:", data1 %>% pull(Source) %>% unique())) +
            scale_x_discrete(name = "") +
            scale_y_continuous(name = y_label, limits = c(y_lower, NA)) +
            facet_grid(cols = vars(Disaggregation), scales = "free") +
            theme(
              text = element_text(size = 14, color = "black", face = "bold"),
              axis.line = element_line(colour = "grey50"),
              axis.ticks.length = unit(0.25, "cm"),
              panel.grid.major.y = element_line(colour = "slategrey", linetype = "dotted"),
              panel.grid.major.x = element_line(colour = "gray98", linetype = "solid"),
              panel.background = element_rect(fill = "gray98"),
              plot.title = element_text(hjust = 0.5),
              panel.spacing = unit(2, "lines"),
              strip.background = element_blank(),
              strip.text = element_text(family = "Ariel", size = 14),
              legend.position = case_when(
                showLegend == FALSE ~ "none",
                showLegend == TRUE ~ "right"
              )
            )
        }

        if (!is.null(graph1)) {
          graph1 <- girafe(
            ggobj = graph1,
            width_svg = 12,
            height_svg = 6
          )
          graph1 <- girafe_options(
            graph1,
            opts_tooltip(css = "background-color: white; color: black; border-style: solid; border-color: black; border-width: 1px;"),
            opts_zoom(min = 0.7, max = 2),
            opts_toolbar(position = "bottomleft")
          )

          showGraph <- case_when(
            ((input$graphOrTable %in% c("All", "Only Graphs")) & (page_name == CurrentPage())) ~ TRUE,
            TRUE ~ FALSE
          )

          if (showGraph == TRUE) {
            return(graph1)
          }
        }
      }
    }
  })

  output$graphExplain <- renderUI({
    showGraph <- case_when(
      ((input$graphOrTable %in% c("All", "Only Graphs")) & (page_name == CurrentPage())) ~ TRUE,
      TRUE ~ FALSE
    )

    if (showGraph == TRUE) {
      return({
        p("The below graph will show a time series if there are multiple years worth of data available or a bar chart if there is not.")
        p(paste(
          "You can hover over the chart to get more information.",
          "The tool bar for the graph appears when hovered over the chart",
          "You can also zoom in on the graph by clicking the magnifying glass in the tool bar on the bottom left and then either double clicking or using the scroll bar on a mouse when hovered over the graph.",
          "Any changes can be reset by clicking on the button in the tool bar on the bottom left.",
          "You can also save the images as a PNG file by clicking the download button."
        ))
      })
    }
  })
  #### Gathering the Data for the table ####
  dataForTable <- reactive({
    if (!is.null(dataGraph())) {
      dataBase <- req(dataGraph()) %>%
        select(Source, Measure, Disaggregation, Breakdown, Year, FormattedFigures) %>%
        distinct() %>%
        pivot_wider(names_from = Year, values_from = FormattedFigures) %>%
        as.data.frame()


      for (i in 5:ncol(dataBase)) {
        for (j in seq_len(nrow(dataBase))) {
          if (is.na(dataBase[j, i])) {
            dataBase[j, i] <- "-"
          }
        }
      }

      return(dataBase)
    }
  })

  #### Creating Table for Presentation ####
  output$mainPageTable <- renderDT(
    {
      data_want <- req(dataForTable())

      if (!is.null(data_want)) {
        index_hide <- which(colnames(data_want) %in% c("Source")) - 1

        index_center <- which(!(colnames(data_want) %in% c("Source", "Measure", "Disaggregation", "Breakdown"))) - 1

        index_wider <- which(colnames(data_want) %in% c("Measure")) - 1

        datatable1 <- datatable(
          data_want,
          selection = "single",
          extensions = "FixedHeader",
          rownames = FALSE,
          caption = tags$caption(paste("Source:", data_want %>% pull(Source) %>% unique() %>% paste(collapse = "; ")), style = "color: black; caption-side: bottom; text-align: left;"),
          class = "table",
          options = list(
            paging = FALSE,
            searching = FALSE,
            fixedHeader = TRUE,
            dom = "t",
            ordering = FALSE,
            scrollX = "300px",
            columnDefs = list(
              list(targets = index_hide, visible = FALSE),
              list(targets = index_center, className = "dt-center", width = "10%"),
              list(targets = index_wider, className = "dt-left", width = "100%")
            )
          )
        )

        showGraph <- case_when(
          ((input$graphOrTable %in% c("All", "Only Tables")) & (page_name == CurrentPage())) ~ TRUE,
          TRUE ~ FALSE
        )

        if (showGraph == TRUE) {
          return(datatable1)
        }
      }
    },
    server = TRUE
  )

  #### Getting the Graph Data for the International Graph (if there is any) ####
  intDataGraph <- reactive({
    if (!is.na(internationalData)) {
      dataInt <- internationalData %>%
        mutate_if(is.character, trimws) %>%
        mutate(Highlight = case_when(
          Country == "Scotland" ~ "1",
          TRUE ~ "0"
        ))
    }
  })

  output$internationalGraph <- renderGirafe({
    data_want <- req(intDataGraph())

    ## Title ##
    measureOrig <- colnames(data_want) [ncol(data_want) - 1]
    measure_name <- gsub(".", " ", measureOrig, fixed = TRUE)
    indicator_name <- dataset %>%
      pull(Indicator) %>%
      unique()

    scotland_ranking <- data_want %>%
      filter(toupper(Country) == "SCOTLAND") %>%
      pull(Quartile) %>%
      unique() %>%
      as.character()

    title <- case_when(
      indicator_name == "Spend on Research and Development" ~ paste("Scotland is in the ", tolower(scotland_ranking),
        " of OECD countries for spending on Research and Development as a % of GDP.",
        sep = ""
      ),
      TRUE ~ paste("Scotland is in the ", tolower(scotland_ranking), " of OECD countries for ",
        measure_name, ".",
        sep = ""
      )
    )

    title <- str_wrap(title, 150)

    ## Subtitle if needed ##
    subtitle <- NULL
    if (indicator_name == "Entrepreneurial activity") {
      subtitle <- "The percentage of 18-64 year olds in the process of starting a business or running businesses less than three and a half years old."
    }

    ## Y Min ##
    y_min <- 0

    ## Y Max ##
    figures <- data_want[, ncol(data_want)] %>%
      as.numeric() %>%
      na.omit()
    y_max <- NA
    if (length(figures) > 0) {
      y_max <- round_any((max(figures, na.rm = TRUE) + 5), 10, ceiling)
    }

    ## Source ##
    source_text <- "Source: OECD"

    #### Getting Data in order for the graph ####
    ## Ordering by Rank first ##
    data_want <- data_want[order(data_want[, as.numeric(ncol(data_want) - 1)]), ]
    data_want$Country <- factor(data_want$Country, levels = data_want$Country[order(data_want[, as.numeric(ncol(data_want) - 1)])])

    ## Making the Quartiles a factor that facet wraps are in the right order ##
    data_want$Quartile <- factor(data_want$Quartile, levels = c("Bottom Quartile", "Third Quartile", "Second Quartile", "Top Quartile"))

    ## Wrapping Y Label ##
    y_label <- str_wrap(measure_name, 40)

    data_want <- data_want %>%
      rename("Int.Variable" = contains(measureOrig)) %>%
      mutate(FigureFormat = case_when(
        indicator_name == "Entrepreneurial activity" ~ percent(Int.Variable, accuracy = 0.1, scale = 1),
        indicator_name == "Spend on Research and Development" ~ percent(Int.Variable, accuracy = 0.1, scale = 1),
        TRUE ~ Int.Variable %>% as.character()
      )) %>%
      mutate(HoverText = paste(
        "Country: ", Country, "\n",
        "Figure:", FigureFormat, "\n",
        "Quartile:", Quartile
      ))

    #### Creating Final Graph ####
    plot1 <- ggplot(data = data_want, aes(x = Country, y = Int.Variable, fill = Highlight)) +
      geom_col_interactive(aes(tooltip = HoverText)) +
      labs(caption = source_text, y = measure_name, x = "", title = title, subtitle = subtitle) +
      facet_wrap(facets = vars(Quartile), scales = "free_x", nrow = 1, strip.position = "bottom") +
      scale_fill_manual(guide = FALSE, values = c("lightblue2", "deepskyblue")) +
      theme(
        text = element_text(family = "Ariel", size = 14, color = "black", face = "bold"),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.line = element_line(colour = "grey50"),
        axis.ticks.length = unit(0.25, "cm"),
        panel.grid.major.y = element_line(color = "slategrey", linetype = "dotted"),
        panel.grid.major.x = element_line(colour = "gray98", linetype = "solid"),
        panel.background = element_rect(fill = "gray98"),
        panel.spacing = unit(0, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "black", fill = "white"),
        strip.text = element_text(family = "Ariel", size = 14),
        strip.placement = "Outside"
      )

    plot1 <- girafe(
      ggobj = plot1,
      width_svg = 12,
      height_svg = 6
    )
    plot1 <- girafe_options(
      plot1,
      opts_tooltip(css = "background-color: white; color: black; border-style: solid; border-color: black; border-width: 1px;"),
      opts_zoom(min = 0.7, max = 2),
      opts_toolbar(position = "bottomleft")
    )
  })

  output$intCaveat <- renderText({
    if (!is.na(internationalData)) {
      text <- NULL
      indicator_name <- dataset %>%
        pull(Indicator) %>%
        unique()

      if (indicator_name == "Entrepreneurial activity") {
        text <- paste("Caveats: This Data is from 2017 except where indicated by '*' where we have provided the most recent published data available (2013-2016)",
          "For New Zealand the mose recent data if from 2005 so it has been excluded from the chart.",
          sep = " "
        )
      } else if (indicator_name == "Spend on Research and Development") {
        text <- paste("Caveats: Data is from 2016 except where indicated  by *.",
          "Data for these countries is from 2015, this is the most recent published data available.",
          sep = " "
        )
      }

      if (!is.null(text)) {
        return(text)
      }
    }
  })
}
