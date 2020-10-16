#### Enterprise and Skills 2nd Page Module ####

#### UI Component ####
ES_secondPage_UI <- function(id) {
  ## Defining Namespace ##
  ns <- NS(id)
  
  ## Beginning of Layout ##
  fluidRow(
    h2(paste(id, "Summary Table"), style = "color: #DE3979;"),
    
    p("This page summarises the each of the indicators below and displays the short term trend, the longer term trend, how Scotland performs in comparison either internationally or within the UK and shows a trend graph for that indicator."),
    
    p(paste("You can hover over the 'Indicator' or 'Measure' columns below to find out the source of the data.",
    "Hovering over the 'Short Term Trends' column will show the latest year available for that indicator.",
    "Double clicking any of the rows below will take you to that indicator's page.")),
    
    p("Please note that not all the international comparators or UK comparators data are on this dashboard currently."),
    
    hr(),
    
    h5(paste(
             "Summary Table for",
             id,
             "Indicators"
             ) ,
    style = "text-align: center; color: #DE3979;"),
    
    img(src = 'Table Meanings.png', style = "height: 50%;"),
    
    column(12, 
           DTOutput(ns("summaryTable"), width = "100%") %>% withSpinner()
           )
  )
  
}

#### Server Component ####
ES_secondPage_Server <- function(input, output, session, dataset) {
  #### Data Manipulation ####
  dataset <- dataset %>%
    group_by(Indicator, Disaggregation) %>% 
    filter(Year == max(Year, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(Disaggregation == "Scotland") %>% 
    select(Source, Year, Indicator, Measure, Figure) %>% 
    distinct() %>% 
    dplyr::rename(
      "Short Term Trend" = Figure
    ) %>% 
    as.data.frame()
  
  #### Formatting Changes and Figures ####
  dataset <- formattingChanges(dataset)
  
  dataset <- formattingFigures(dataset, "Short Term Trend")
  
  #### Putting in smaller trend pictures ####
  dataset <- dataset %>% 
    mutate(`Trend Picture` = paste0(
      "<img src = '",
      gsub("'", "", Indicator, fixed = TRUE),
      ".png'>"
    ))
  
  #### Table Output ####
  output$summaryTable <- renderDT({
    index_hide <- which(colnames(dataset) %in% c("Source", "Year", "DifferenceShort", "changeFormattedShort","changeFormattedLong","Longer Term Trend Values")) - 1
    index_center <- which(colnames(dataset) %in% c("Short Term Trend", "Comparator Rank", "Longer Term Trend", "OECD/UK Comparison")) - 1
  
    index_left <-  which(!(colnames(dataset) %in% c("Short Term Trend", "Comparator Rank", "Longer Term Trend", "OECD/UK Comparison","Source", "Year", "DifferenceShort", "changeFormattedShort","changeFormattedLong","Longer Term Trend Values"))) - 1

    table1 <- datatable(
      dataset, 
      selection = "single",
      extensions = "FixedHeader",
      rownames = FALSE,
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
          list(targets = index_center, className = "dt-center"),
          list(targets = index_left, className = "dt-left")
        ),
        rowCallback = JS(
          ## Hover Information ##
          "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {", 
          "var YearText = 'Latest Figure from ' + aData[1];",
          "var SourceText = 'Source: ' + aData[0];",
          # Tooltip for the Year Context
          "$('td:eq(2)', nRow).attr('title', YearText);", 
          # Tooltip for the Source context
          "$('td:eq(0)', nRow).attr('title', SourceText);", 
          "$('td:eq(1)', nRow).attr('title', SourceText);", 
          # Showing a hand as a cursor
          "$('td', nRow).css('cursor', 'pointer');", 
          
          ## Colour Information for Short Change ##
          "if (parseFloat(aData[6]) == 1) {",
          "$('td:eq(2)', nRow).css('color', 'green');",
          "}",
          "else if (parseFloat(aData[6]) == 0) {",
          "$('td:eq(2)', nRow).css('color', 'orange');",
          "}",
          "else if (parseFloat(aData[6]) == -1) {",
          "$('td:eq(2)', nRow).css('color', 'red');",
          "}",
          
          "}"
          
          )
      ),
      callback = JS('
                                        /* Adding Double click ability to the data table (to take to appropriate tab) */
                                        table.on("dblclick.dt", "tr", function() {
                                        var data = table.row(this).data();
                                        Shiny.onInputChange("doubleClick", data)
                                        console.long(data)
                                        })
                                        '),
      escape = FALSE
    )
    
    return(table1)
  })
  outputOptions(output, "summaryTable", suspendWhenHidden = FALSE)
}