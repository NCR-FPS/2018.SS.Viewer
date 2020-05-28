###############################################################################
#Code for 2018 Sustainability Survey App

#Load and format data
{setwd(".")
  dat <- read.csv("2018_survey_data_en.csv", check.names = FALSE)
    dat[dat=="1"] <- "Yes"
      dat[dat=="0"] <- "No"
      dat$`Provide the link to the Scientific Report (if available)` <- paste0("<a href='",
        dat$`Provide the link to the Scientific Report (if available)`,"'target='_blank'>",
          dat$`Provide the link to the Scientific Report (if available)`,"</a>")
      dat$`Please provide the link for the online IFMP, or if an IFMP has not been developed, indicate when the IFMP will be posted online.` <- paste0("<a href='",                                                      dat$`Please provide the link for the online IFMP, or if an IFMP has not been developed, indicate when the IFMP will be posted online.`,"'target='_blank'>",                                                          dat$`Please provide the link for the online IFMP, or if an IFMP has not been developed, indicate when the IFMP will be posted online.`,"</a>")
        dat2 <- data.frame(t(dat[,-1]))
          colnames(dat2) <- dat[,1]
            x <- c("dplyr", "kableExtra", "knitr", "shiny", "data.table", "formattable",
                   "shinythemes", "ggplot2", "plotly", "reshape2", "shinyjs", "tidyr",
                   "ggthemes", "DT", "gridExtra", "shinycssloaders")
              packages <- x[!(x %in% installed.packages()[,"Package"])]
                if(length(packages)) install.packages(packages,
                    dependencies = TRUE)
                    lapply(x, require, character.only = TRUE)
                      as.data.table(dat2)
                        dtab <- dat[, c(1, 3, 78)]
                        PA <- dat2[c(1,2,15:19,66,70,77,79,82,84,86,88,90:93),]
                        batch1 <- dat[c(26,30,31,37,50,51,56,58,59,66,68,82,81,
                                        98,102,103,112,122,136,150,151,155,159,164),]
                        batch_flipped <- data.frame(t(batch1[,-1]))
                        colnames(batch_flipped) <- batch1[,1]
                        batch_PA <- batch_flipped[c(1,2,15:19,66,70,77,79,82,84,
                                                    86,88,90:93),]

    subheads <- c("General Information", "Scientific Reports", "Management Plans",
                  "Abundance and Mortality Information", "Limit Reference Point", "Upper Stock Reference",
                  "Stock Status", "Removal Reference", "Fishery Status", "Harvest Decision Rules",
                  "Management Measures", "Bycatch Information", "SARA and COSEWIC Considerations")
    dat_subhead <- dat
    dat_subhead[,subheads] <- ""
    dat_organized <- dat_subhead[, c(1, 138, 2:6, 16, 137, 139, 7:15, 140, 97:100, 104:112, 141, 74:77, 142, 17:20, 68:71, 143, 65:67, 144, 78:83, 145, 84:91, 146, 94:96, 147, 72, 73, 92, 93, 148, 21:64, 101, 102, 149, 103, 113:122, 150, 123:136)]
    dat_organized2 <- data.frame(t(dat_organized[,-1]))
    colnames(dat_organized2) <- dat_organized[,1]
    dat_PA <- dat_organized2[c(1:8, 38:75),]
    dat_batch <- dat_organized[c(26,30,31,37,50,51,56,58,59,66,68,82,81,98,102,103,112,122,136,150,151,155,159,164),]
    dat_batch2 <- data.frame(t(dat_batch[,-1]))
    colnames(dat_batch2) <- dat_batch[,1]
    dat_batch_PA <- dat_batch2[c(1:8, 38:75),]}
##############################################################################

#Construct User-Interface
ui <- bootstrapPage(theme = shinytheme("flatly"),
  tags$head(tags$style(HTML("hr {border-top: 2px solid #b2babb;}"))),
    navbarPage("2018 Sustainability Survey Viewer",
        
#Welcome Page UI
  tabPanel("Home",
    hr(),
    titlePanel(h1("Welcome to the 2018 Sustainability Survey Viewer", align = "center")),
    h4("Use this application to view, explore, compare, and summarise the results of the 2018
        Sustainability Survey", align = "center"),
    hr(),
    br(),
    mainPanel(width = 7, position = "left",
    h3("Instructions for Using this Application"),
    hr(),
    p("Using and navigating this application is simple. The application is divided into Tabs containing
        different functions and information. To navigate between Tabs, simply click the desired heading on
        the blue navigation bar along the top of the page. Instructions for interacting with specific Tabs
        are found below."),
    hr(),
    h4("Explore the Survey Tab:"),
    p("Clicking the Explore the Survey Tab will provide an interface for creating information tables for specific
        fish stocks."),
    p("Generating Information for Specific Stocks:"),
    tags$ol(
      tags$li("Use the input bar labeled [Include all Stocks or only Batch 1 Stocks] to select
              whether to include information on all 177 stocks in the 2018 Sustainability Survey or to
              filter the data to include only those stocks prescribed in Batch 1 under the Fish Stock
              Provisions."),
      tags$li("Further apply filters onto the data using the radio buttons under the label [Select a
              data filter]. Once a filter has been chosen, new dropdown menus will appear below with
              additional choices for selecting the stock you wish to view. Selecting [None] leaves keeps
              all stocks in the data, selecting [Filter by Region] will allow you to limit the available
              stocks to those found in a selected management region, and selecting [Filter by Species
              Group] limits the available stocks to only those in a selected species groups."),
      tags$li("Use the dropdown menus to apply any filters you desire then click the input bar labeled
              [Select a Stock Here] to select a specific stock by typing a stock name or selecting one
              from the dropdown list. If filters have been applied, the list of available stocks will be
              limited to only those in the selected region or species group."),
      tags$li("Once a stock has been selected, use the [Select Information to Include] input bar to
              select whether to include All the information in the 2018 Sustainability Survey or only information 
              on components of the Precautionary Approach."),
      tags$li("As soon as options for all the inputs have been selected, a formatted table with the desired
              information will immediately be generated below the user-interface.")),
    hr(),
    h4("Multi-Stock Outputs Tab:"),
    p("Clicking the Compare Stocks tab will provide an interface for creating tables containing all the
        results of the 2018 Sustainability Survey for multiple stocks at once. Using the dropdown menus
        and radiobuttons, you can filter the stocks included."),
    tags$ol(
      tags$li("First, select whether to use survey results for all the stocks in the survey or only
              those belonging to Batch 1 of the Fish Stock Provisions using the [Include the entire
              survey or only Batch 1] dropdown menu."),
      tags$li("You can then further filter the data using the radiobuttons labelled [Add a data filter:]. 
              Selecting None will generate a large table containing either all stocks in the
              survey or all stocks in Batch 1. Selecting filter by region or species group will
              produce new dropdown menus. Use these menus to narrow the stocks included to only those
              belonging to a specific region or species group"),
      tags$li("Upon selection of a data filter, a table will automatically be generated below the
              interface. Please note that selecting all stocks and no filters will produce a very large
              table and this may load slowly. Do not close the application or attempt to change
              your data selection while the table is loading")),
    hr(),
    h4("Precautionary Approach Components Tab:"),
    p("Clicking the Precautionary Approach Components tab will provide an interface for viewing
        summaries of six Precautionary Approach components across all the stocks in the 2018
        Sustainability Survey, with options to filter the data based on Region or Species Group. An
        option to exclude stocks that are missing PA components is also provided."),
    p("Generating Summaries for Precautionary Approach Components:"),
    tags$ol(
      tags$li("First, select using the [Include all stocks or only Batch 1 Stocks] dropdown whether
            you wish to limit the viewable information to only stocks scheduled to be prescribed in Batch
              1 of the Fish Stock Provisions or if all stocks should be viewable."),
      tags$li("Using the [Select a component of the Precautionary Approach:] dropdown menu, select the
                Precautionary Component you wish to have summarised."),
      tags$li("You can add filters to the stocks included in your summary using the [Add a data filter:]
              radio buttons. By default [None] is selected and all stocks from the 2018 Sustainability
              Survey are included. Selecting the [Filter by Region] or [Filter by Species Group] buttons
              will generate new dropdown menus below, allowing you to restrict the stocks included in
              the summary to only those belonging to the selected region or species group."),
      tags$li("Once a PA component has been selected and data filters applied, a formatted table will
              be generated automatically below the user-interface."),
      tags$li("If desired, the [Include stocks missing the selected PA component?:] buttons can be used
              to remove all stocks missing the selected Precautionary Approach component from the
              summary.")),
    hr(),
    h4("Summary Tab:"),
    p("On the Summary tab, you will find a brief description and summary of the 2018 Sustainability
        survey, along with several links to additional resources. On the right side of the page you can
        select between a series of exploratory plots using the upper tabs. Above each of the exploratory plots are
        dropdown menus that can be used to filter the date used to generate the plot by region, species group, 
        or Batch 1."),
    br()),
    br(),
    sidebarPanel(width = 5, position = "right",
      h3("About this Application"),
      hr(),
      h6("Created in R-Studio Version 1.1.463; R Version 3.6.1; Shiny Verision 1.4.0"),
      h6("Created: January 21, 2020"),
      h6("Last Edited: March 25, 2020"),
      hr(),
      h4("Description"),
      p("This application was designed to convert data included in the results of the 2018 Sustainability
        Survey into a more easily readable format that can be filtered by specific stocks, regions, and/or
        species groups."),
      br(),
      h4("About Shiny"),
      p("Shiny is an R package for easily developing interactive web applications within R. Shiny
        applications can be ran within the R-studio Viewer window, an external window, or in a user
        -selected web browser."),
      p("For more information about Shiny, see:"),
      a("https://shiny.rstudio.com/", href = "https://shiny.rstudio.com/", target="_blank"),
      br(),
      br(),
      h4("Printing"),
      p("Currently, this application lacks built-in functionality for printing tables and plots. However,
        tables generated on the Stocks, Regions, and Species Groups Tabs can be printed or converted to
        PDF by right-clicking and selecting print."),
      br(),
      h4("Auto-Fitting Issues"),
      p("This application will auto-size its contents to best fit different windows and screen sizes. Unfortunately,
        this can alter the layout and design of some elements, including cutting off parts of figures or causing
        figure components to overlap. If these problems arise, we recommend adjusting the zoom of your web browser to
        100%. Doing so should fix any issues, however, if problems persist, experiment with the zoom setting until
        satisfied with the results."),
      h6("*This application performs and looks best when run in Google Chrome."))),
                    
#Stocks Page UI
  tabPanel("Explore the Survey", 
    sidebarPanel(width = 12,
      tags$style(type="text/css", 
        ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }"),            
      h3("Explore the 2018 Sustainability Survey"),
        p("Use the filters below to select and view
          information from the 2018 Sustainability Survey for specific fish stocks."),
      selectizeInput(inputId = "inputbatch", 
                     label = "Include the entire survey or only information for stocks in Batch 1:", 
                     choices = c("All Stocks" = 1, "Batch 1 Stocks" = 2),
                     options = list(placeholder = "Include all Stocks or only Batch 1 Stocks",
                                    onInitialize = I('function() { this.setValue(""); }'))),
      radioButtons(inputId = "inputfilt", 
        label = "Select a data filter:", 
          choices = list("None" = 1, "Filter by Region" = 2, "Filter by Species Group" = 3),
            selected = 1, inline = T),
      fluidRow(
        column(4, uiOutput("reportfilter"), div(style = "margin: -1em")),
          column(4, offset = 0.5, uiOutput("stockfilter"), div(style = "margin: -1em")),
            column(4, offset = 0.5, uiOutput("infofilter"), div(style = "margin: -1em"))),
      hr(),
      tableOutput(outputId = "table1") %>% withSpinner(color="#2C3E50"))),

#Multi-Stock Comparisions
  tabPanel("Multi-Stock Outputs", 
    sidebarPanel(width = 12,
      h3("Compare Survey Results Within Batch 1, Regions, or Species Groups"),
      p("Use the filter below to generate a table comparing the results of the 2018 Sustainability Survey
        across stocks in the selected groupings."),
      h6("*Tables generated with all stocks selected and no filters applied may load very slowly."),
      selectizeInput(inputId = "comp1",
                     label = "Include the entire survey or only Batch 1:",
                     choices = c("All Stocks" = 1, "Batch 1 Stocks" = 2),
                     options = list(placeholder = "Include all Stocks or only Batch 1 Stocks",
                                    onInitialize = I('function() { this.setValue(""); }'))),
      fluidRow(
        column(width = 4,
      radioButtons(inputId = "filterc", 
                   label = "Add a data filter:", 
                   choices = list("None" = 1, "Filter by Region" = 2, "Filter by Species Group" = 3),
                   selected = 1, inline = T)),
        column(width = 4,
      uiOutput("outputfilterc"))),
      hr()),
    mainPanel(width = 12,
      dataTableOutput(outputId = "comptable") %>% withSpinner(color="#2C3E50"))),

#PA Components Page UI
  tabPanel("Precautionary Approach Components",
    sidebarPanel(width = 12,
     h3("Precautionary Approach Component Selection"),
      p("Use the dropdown menus below to view available information on components of the Precautionary
        Approach from the 2018 Sustainability Survey."),
          selectizeInput(inputId = "inputbatchB", 
                        label = "Include the entire survey or only information for stocks in Batch 1:", 
                        choices = c("All Stocks" = 1, "Batch 1 Stocks" = 2),
                        options = list(placeholder = "Include all Stocks or only Batch 1 Stocks",
                          onInitialize = I('function() { this.setValue(""); }'))),
          selectizeInput(inputId = "inputPA", 
            label = "Select a component of the Precautionary Approach:", 
              choices = c("Limit Reference Point (LRP)" = 1, "Upper Stock Reference (USR)" = 2,
                          "Harvest Control Rate (HCR)" = 3,
                          "Removal Reference (RR) for Healthy Zone " = 4, 
                          "Removal Reference (RR) for Cautious Zone " = 5, 
                          "Removal Reference (RR) for Critical Zone " = 6,
                          "Removal Reference (RR) when Status is Uncertain" = 7,
                          "Stock Status" = 8,
                          "Fishery Status" = 9),
                options = list(placeholder = "Select PA Component to view",
                  onInitialize = I('function() { this.setValue(""); }'))),
          fluidRow(
          column(width = 4,
          radioButtons(inputId = "inputReg", 
            label = "Add a data filter:", 
              choices = list("None" = 1, "Filter by Region" = 2, "Filter by Species Group" = 3),
                selected = 1, inline = T)),
          column(width = 3,
            radioButtons(inputId = "inputno", 
              label = "Include stocks missing the selected PA component?", 
                choices = list("Yes" = 1, "No" = 2),
                  selected = 1, inline = T))),
     uiOutput("outputfilter"),
      hr(),
        tableOutput(outputId = "PAtable") %>% withSpinner(color="#2C3E50"))),

#Summary Plots Page UI 
  tabPanel("Summary",
    fluidRow(
      sidebarPanel(width = 5,
        h2("The Sustainability Survey for Fisheries"), 
          p("The Sustainability Survey for Fisheries is conducted once a year by regional scientists and
            resource managers to track DFO progress and gather other information about major fish stocks.
            The results of the survey are then used to help set priorities for improving how fisheries 
            are managed."),
          p("The purpose of the Sustainability Survey is to (1) collect information about major fish
            stocks, (2) track the status of these stocks, and (3) track the progress to implement fishery
            policies and measures for these stocks."),
          a("More information on the Sustainability Survey", href = "https://www.dfo-mpo.gc.ca/reports-rapports/regs/sff-cpd/survey-sondage/about-propos-en.html#_Framework_and_survey", target="_blank"),
          br(),
          hr(),
        h4("Summary of 2018 Sustainability Survey"),
          p("The latest version of the sustainability survey is from 2018 and includes 
            177 major fish stocks:"),
          tags$ul(
          tags$li("79 stocks (45%) have Upper Stock Reference Points (USR)"),
          tags$li("104 stocks (59%) have Limit Reference Points (LRP)"),
          tags$li("106 stocks (60%) have Harvest Control Rules (HCR)"),
          tags$li("99 stocks (56%) have Removal References (RR) for the status zone the stock is 
                  currently in"),
          tags$li("19 stocks currently classified as in the Critical Zone"),
          tags$li("8 stocks in the Critical Zone have Rebuilding Plans and another 8 have Rebuilding
                  Plans under development"),
          tags$li("73 stocks currently classified as uncertain"),
          tags$li("3 uncertain stocks are at levels such that serious harm is likely, another 22 stocks
                  are at levels such that serious harm is possible")),
        hr(),
        h4("Links to Additional Resources:"),
        a("[1] Summary of the 2018 Sustainability Survey for Fisheries", href = "https://www.dfo-mpo.gc.ca/reports-rapports/regs/sff-cpd/survey-sondage/results-resultats-s-2018-en.html", target="_blank"),
        br(),
        a("[2] Sustainability Survey for Fisheries", href = "https://www.dfo-mpo.gc.ca/reports-rapports/regs/sff-cpd/survey-sondage/index-en.html", target="_blank"),
        br(),
        a("[3] Sustainability Survey for Fisheries Datasets", href = "https://open.canada.ca/data/en/dataset/49f25051-3946-426e-a589-b7063f75bbd5", target="_blank"),
        br(),
        a("[4] Comparision of 2018 Results to Previous Surveys", href = "https://www.dfo-mpo.gc.ca/reports-rapports/regs/sff-cpd/survey-sondage/comparison-comparaison-en.html", target="_blank")),
      mainPanel(width = 6, 
          h5("Exploratory Plots: Select tab to view"),
          tabsetPanel(
            tabPanel("Precautionary Approach Components",
              tags$style(HTML(".tabbable > .nav > li > a {background-color: #2C3E50; color: white")),     
              br(),
              fluidRow(
                column(width = 4,
                  selectizeInput(inputId = "plotfilt",
                                 label = "Select filters to apply to data:",
                                 choices = c("All Stocks" = 1, "Filter by Region" = 2,
                                       "Filter by Species Group" = 3, "Filter by Batch 1" = 4),
                                 selected = "1")),
                column(width = 4, 
              uiOutput("plotfilter"))),
              plotOutput(outputId = "plot1") %>% withSpinner(color="#2C3E50"),
              br(), 
              p("Figure 1. Percentage of fish stocks with five components required for
                the implementation of the Precautionary Approach.", style = "font-size:18px")),
            tabPanel("Stock Status",
              br(),
              fluidRow(
                column(width = 4,
              selectizeInput(inputId = "plotfilt2",
                             label = "Select information to plot:",
                             choices = c("All Stocks" = 1, "Filter by Region" = 2,
                                         "Filter by Species Group" = 3, "Filter by Batch 1" = 4))),
                column(width = 4,
              uiOutput("plotfilter2"))),
              h3("Percentage of stocks currently in each status zone:"),
              plotOutput(outputId = "plot3") %>% withSpinner(color="#2C3E50"),
              p("Figure 2. Summary of the stock status of fish stocks included in the 2018
                Sustainability Survey. Coloured bars shows the percentage of stocks in each status zone
                (Critical, Cautious, Healthy, and Uncertain).", style = "font-size:18px")),
            tabPanel("Fish Stocks Included in the 2018 Survey",
              DT::dataTableOutput(outputId = "dtable1") %>% withSpinner(color="#2C3E50"))))))))
     
##############################################################################

#Create App Functions
server <- function(input, output) {
  
#Stocks Page Functions  
  
  observeEvent(input$inputbatch, {
    
    if(input$inputbatch == "1") {
      dat <- dat_organized
        dat2 <- dat_organized2
          PA <- dat_PA}
    
   else if(input$inputbatch == "2") {
      dat <- dat_batch
        dat2 <- dat_batch2
          PA <- dat_batch_PA}
  
  observeEvent(input$inputfilt, {

    #No Filters
    if(input$inputfilt == "1") {
      
      output$reportfilter <- renderUI({
        selectizeInput(inputId = "dropdown", 
          label = "Select a Stock here:", 
            choices = dat[,1], 
              options = list(placeholder = "Enter a Stock Name", 
                onInitialize = I('function() { this.setValue(""); }'),
                  openOnFocus = FALSE))})
      
      output$stockfilter <- renderUI({
        selectizeInput(inputId = "check", 
          label = "Select information to include:", 
            choices = c("All Information" = 2, "Precautionary Approach Components" = 3), 
                options = list(placeholder = "Select Information to Include", 
                  onInitialize = I('function() { this.setValue(""); }')))})
      
      output$infofilter <- NULL
      
      output$table1 <- function(){
        req(input$inputbatch)
        req(input$check)
          req(input$dropdown)
            if(input$check == 2) {
              dat2 %>% select(input$dropdown) %>%
                kable(format = "html", escape = F) %>%
                  kable_styling(bootstrap_options = c("striped", "responsive","bordered"),
                                position = "center", fixed_thead = TRUE) %>% 
                    column_spec(1, bold = T, width = "35em", color = "black") %>%
                      column_spec(2, color = "black") %>%
                        row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>% 
                          row_spec(c(1,9,19,33,38,47,51,58,67,71,76,123,135), bold = T, color = "white",
                                   font_size = 18, background = "#56799c")}
  
            else if(input$check == 3) {
              req(input$check)
                req(input$dropdown)
                  PA %>% select(input$dropdown) %>%
                    kable(format = "html", escape = F) %>%
                      kable_styling(bootstrap_options = c("striped", "responsive","bordered"),
                                    position = "center", fixed_thead = TRUE) %>%
                        column_spec(1, bold = T, width = "35em", color = "black") %>%
                          column_spec(2, color = "black") %>%
                            row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>% 
                                row_spec(c(1, 9, 18, 22, 29, 38, 42), bold = T, color = "white",
                                    font_size = 18, background = "#56799c")}}}
    
    #Filter by Region
    else if(input$inputfilt == "2") {
    
      output$reportfilter <- renderUI({
        selectizeInput(inputId = "dropdown2",
          label = "Select a Region here:",
            choices = dat[,3], 
              options = list(placeholder = "Select a Region", 
                onInitialize = I('function() { this.setValue(""); }')))})
      
      output$stockfilter <- renderUI({
        Regdat <- filter(dat, Region == input$dropdown2)
          Regdat1 <- data.frame(t(Regdat[,-1]))
            colnames(Regdat1) <- Regdat[,1]   
          selectizeInput(inputId = "regstock1",
            label = "Select a Stock from the chosen Region:",
              choices = colnames(Regdat1),
                options = list(placeholder = "Select a Stock",
                  onInitialize = I('function() { this.setValue(""); }')))})
      
      output$infofilter <- renderUI({
        selectizeInput(inputId = "check2", 
          label = "Select information to include:",
            choices = c("All Information" = 2, "Precautionary Approach Components" = 3), 
                options = list(placeholder = "Select Information to Include", 
                  onInitialize = I('function() { this.setValue(""); }')))})
          
      output$table1 <- function(){
        Regdat <- filter(dat, Region == input$dropdown2)
          Regdat1 <- data.frame(t(Regdat[,-1]))
            colnames(Regdat1) <- Regdat[,1]
      
               if(input$check2 == 2) {
                Regdat1 <- Regdat1
                  Regdat1 %>% select(input$regstock1) %>% 
                    kable(table.attr = "style = \"color: black;\"", format = "html", escape = F) %>%
                      kable_styling(bootstrap_options = c("striped", "responsive", "bordered"),
                                   position = "center", fixed_thead = TRUE, full_width = TRUE) %>%
                        column_spec(1, bold = T, width = "35em", color = "black") %>%
                          row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>% 
                            row_spec(c(1,9,19,33,38,47,51,58,67,71,76,123,135), bold = T, color = "white",
                                font_size = 18, background = "#56799c")} 
          
              else if(input$check2 == 3) {
                Regdat1 <- Regdat1[c(1:8, 38:75),]
                  Regdat1 %>% select(input$regstock1) %>%
                    kable(table.attr = "style = \"color: black;\"", format = "html", escape = F) %>%
                      kable_styling(bootstrap_options = c("striped", "responsive", "bordered"),
                                    position = "center", fixed_thead = TRUE, full_width = TRUE) %>%
                        column_spec(1, bold = T, width = "35em", color = "black") %>%
                          row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>% 
                    row_spec(c(1, 9, 18, 22, 29, 38, 42), bold = T, color = "white",
                             font_size = 18, background = "#56799c")}}}

    #Filter by Species Group
    else if(input$inputfilt == "3") {
      
      output$reportfilter <- renderUI({
        selectizeInput(inputId = "dropdown3", 
          label = "Select a Species Group here:",
            choices = dat[,2], 
              options = list(placeholder = "Select a Species Group", 
                onInitialize = I('function() { this.setValue(""); }')))})
      
      output$stockfilter <- renderUI({
        taxdat <- rename(dat, "Species" = "Species Group")
          taxdat2 <- filter(taxdat, Species == input$dropdown3) 
            taxdat1 <- data.frame(t(taxdat2[,-1])) 
              colnames(taxdat1) <- taxdat2[,1]
          selectizeInput(inputId = "groupstock1",
            label = "Select a Stock from the chosen Species Group:",
              choices = colnames(taxdat1),
                options = list(placeholder = "Select a Stock",
                  onInitialize = I('function() { this.setValue(""); }')))}) 
      
      output$infofilter <- renderUI({
        selectizeInput(inputId = "check3", 
          label = "Select information to include:", 
            choices = c("All Information" = 2, "Precautionary Approach Components" = 3), 
                options = list(placeholder = "Select Information to Include", 
                  onInitialize = I('function() { this.setValue(""); }')))})
    
    output$table1 <- function(){
      req(input$check3)
        req(input$groupstock1)
     
      taxdat <- rename(dat, "Species" = "Species Group")
        taxdat2 <- filter(taxdat, Species == input$dropdown3) 
          taxdat1 <- data.frame(t(taxdat2[,-1])) 
            colnames(taxdat1) <- taxdat2[,1]
     
          if(input$check3 == 2) {
            taxdat1 <- taxdat1
              taxdat1 %>% select(input$groupstock1) %>%
                kable(table.attr = "style = \"color: black;\"", format = "html", escape = F) %>%
                  kable_styling(full_width = TRUE,
                                bootstrap_options = c("striped", "responsive", "bordered"), 
                                position = "center", fixed_thead = TRUE) %>%
                    column_spec(1, bold = T, width = "35em", color = "black") %>%
                      row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>% 
                row_spec(c(1,9,19,33,38,47,51,58,67,71,76,123,135), bold = T, color = "white",
                         font_size = 18, background = "#56799c")}
      
          else if(input$check3 == 3) {
            taxdat1 <- taxdat1[c(1:8, 38:75),]
              taxdat1 %>% select(input$groupstock1) %>%
                kable(table.attr = "style = \"color: black;\"", format = "html", escape = F) %>%
                  kable_styling(full_width = TRUE,
                              bootstrap_options = c("striped", "responsive", "bordered"),
                              position = "center", fixed_thead = TRUE) %>%
                    column_spec(1, bold = T, width = "35em", color = "black") %>%
                      row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>% 
                row_spec(c(1, 9, 18, 22, 29, 38, 42), bold = T, color = "white",
                         font_size = 18, background = "#56799c")}}}})})

######################################################
   
#Precautionary Approach Components Functions
  
  observeEvent(input$inputbatchB, {
    
    if(input$inputbatchB == "1") {
      dat <- dat_organized}
    
    else if(input$inputbatchB == "2") {
      dat <- dat_batch}
  
  observeEvent(input$inputReg, {
      
    if(input$inputReg == "1") {
      output$outputfilter <- NULL}
        
    else if(input$inputReg == "2") {
      output$outputfilter <- renderUI({
        selectizeInput(inputId = "regdrop", 
          label = "Select a Region here:",
            choices = dat[,3], 
              options = list(placeholder = "Select a Region", 
                onInitialize = I('function() { this.setValue(""); }')))})}

    else if(input$inputReg == "3") {
      output$outputfilter <- renderUI({
        selectizeInput(inputId = "sppdrop", 
          label = "Select a Species Group here:",
            choices = dat[,2], 
              options = list(placeholder = "Select a Species Group", 
                onInitialize = I('function() { this.setValue(""); }')))})}})
    
  output$PAtable <- function() {
    req(input$inputbatchB)
    if(input$inputPA == 1) {
      
      if(input$inputReg == "1") {
        dat3 <- dat}
      
      else if(input$inputReg == "2") {
        req(input$regdrop)
          dat3 <- filter(dat, Region == input$regdrop)}
      
      else if(input$inputReg == "3") {
        req(input$sppdrop)
          tempdat <- rename(dat, "Species" = "Species Group")
            dat3 <- filter(tempdat, Species == input$sppdrop)}
        
      LRPdat <- dat3[, c(1, 44, 47)]
    
      if(input$inputno == "1") {
        LRPdat <- LRPdat}
    
      else if(input$inputno == "2") {
        tempdat2 <- LRPdat
          tempdat2 <- mutate(tempdat2, PA = tempdat2$`Is there a Limit Reference Point (point dividing the critical and cautious zones)?`)
            tempdat2 <- tempdat2 %>% filter(PA == "Yes")
              LRPdat <- subset(tempdat2, select = c(1:3))}

          LRPdat %>% kable(table.attr = "style = \"color: black;\"", format = "html",
                           escape = F, row.names = FALSE) %>%
            kable_styling(full_width = TRUE,
                          bootstrap_options = c("striped", "responsive", "bordered"),
                          position = "center", fixed_thead = TRUE) %>%
              row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>%
                column_spec(1, width = "30em", bold = T) %>% column_spec(3, width = "60em")}
    
    else if(input$inputPA == 2) {
      
      if(input$inputReg == "1") {
        dat3 <- dat}
      
      else if(input$inputReg == "2") {
        req(input$regdrop)
          dat3 <- filter(dat, Region == input$regdrop)}
      
      else if(input$inputReg == "3") {
        req(input$sppdrop)
          tempdat <- rename(dat, "Species" = "Species Group")
            dat3 <- filter(tempdat, Species == input$sppdrop)}
      
              USRdat <- dat3[, c(1, 49, 51)] 
              
              if(input$inputno == "1") {
                USRdat <- USRdat}
              
              else if(input$inputno == "2") {
                tempdat2 <- USRdat
                  tempdat2 <- mutate(tempdat2, PA = tempdat2$`Is there an Upper Stock Reference (point dividing the cautious and healthy zones)?`)
                    tempdat2 <- tempdat2 %>% filter(PA == "Yes")
                      USRdat <- subset(tempdat2, select = c(1:3))}
              
                USRdat %>% kable(table.attr = "style = \"color: black;\"", format = "html",
                                 escape = F, row.names = FALSE) %>%
                  kable_styling(full_width = TRUE,
                                bootstrap_options = c("striped", "responsive", "bordered"),
                                position = "center", fixed_thead = TRUE) %>%
                    row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>%
                      column_spec(1, width = "30em", bold = T) %>% column_spec(3, width = "60em")}
      
    else if(input$inputPA == 3) {
      
      if(input$inputReg == "1") {
        dat3 <- dat}
      
      else if(input$inputReg == "2") {
        req(input$regdrop)
          dat3 <- filter(dat, Region == input$regdrop)}
      
      else if(input$inputReg == "3") {
        req(input$sppdrop)
          tempdat <- rename(dat, "Species" = "Species Group")
            dat3 <- filter(tempdat, Species == input$sppdrop)}
      
              HCRdat <- dat3[, c(1, 75, 76)]
              
              if(input$inputno == "1") {
                HCRdat <- HCRdat}
              
              else if(input$inputno == "2") {
                tempdat2 <- HCRdat
                  tempdat2 <- mutate(tempdat2, PA = tempdat2$`For this stock, harvest decision rules that aim to control exploitation:`)
                    tempdat2 <- tempdat2 %>% filter(PA == "Have been developed, implemented and evaluated"|
                                            PA == "Have been developed and implemented, but not evaluated"|
                                            PA == "Have been developed but not implemented")
                      HCRdat <- subset(tempdat2, select = c(1:3))}
              
                HCRdat %>% kable(table.attr = "style = \"color: black;\"", format = "html",
                                 escape = F, row.names = FALSE) %>%
                  kable_styling(full_width = TRUE,
                                bootstrap_options = c("striped", "responsive", "bordered"),
                                position = "center", fixed_thead = TRUE) %>%
                    row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>%
                      column_spec(1, width = "30em", bold = T) %>% column_spec(3, width = "60em")}
    
    else if(input$inputPA == 4) {
      
      if(input$inputReg == "1") {
        dat3 <- dat}
      
      else if(input$inputReg == "2") {
        req(input$regdrop)
          dat3 <- filter(dat, Region == input$regdrop)}
      
      else if(input$inputReg == "3") {
        req(input$sppdrop)
          tempdat <- rename(dat, "Species" = "Species Group")
            dat3 <- filter(tempdat, Species == input$sppdrop)}
      
              RRHdat <- dat3[, c(1, 60, 61)]
              
              if(input$inputno == "1") {
                RRHdat <- RRHdat}
              
              else if(input$inputno == "2") {
                tempdat2 <- RRHdat
                  tempdat2 <- mutate(tempdat2, PA = tempdat2$`Has a removal reference been identified when the stock is in the Healthy Zone?`)
                    tempdat2 <- tempdat2 %>% filter(PA == "Yes")
                      RRHdat <- subset(tempdat2, select = c(1:3))}
              
                RRHdat %>% kable(table.attr = "style = \"color: black;\"", format = "html", 
                                 escape = F, row.names = FALSE) %>%
                  kable_styling(full_width = TRUE,
                                bootstrap_options = c("striped", "responsive", "bordered"), 
                                position = "center", fixed_thead = TRUE) %>%
                    row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>%
                      column_spec(1, width = "30em", bold = T) %>% column_spec(3, width = "60em")}
    
    else if(input$inputPA == 5) {
      
      if(input$inputReg == "1") {
        dat3 <- dat}
      
      else if(input$inputReg == "2") {
        req(input$regdrop)
          dat3 <- filter(dat, Region == input$regdrop)}
      
      else if(input$inputReg == "3") {
        req(input$sppdrop)
          tempdat <- rename(dat, "Species" = "Species Group")
            dat3 <- filter(tempdat, Species == input$sppdrop)}
      
              RRCdat <- dat3[, c(1, 62, 63)]
              
              if(input$inputno == "1") {
                RRCdat <- RRCdat}
              
              else if(input$inputno == "2") {
                tempdat2 <- RRCdat
                  tempdat2 <- mutate(tempdat2, PA = tempdat2$`Has a removal reference been identified when the stock is in the Cautious Zone?`)
                    tempdat2 <- tempdat2 %>% filter(PA == "Yes")
                      RRCdat <- subset(tempdat2, select = c(1:3))}
              
                RRCdat %>% kable(table.attr = "style = \"color: black;\"", format = "html",
                                 escape = F, row.names = FALSE) %>%
                  kable_styling(full_width = TRUE,
                                bootstrap_options = c("striped", "responsive", "bordered"),
                                position = "center", fixed_thead = TRUE) %>%
                    row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>%
                      column_spec(1, width = "30em", bold = T) %>% column_spec(3, width = "60em")}

    else if(input$inputPA == 6) {
      
      if(input$inputReg == "1") {
        dat3 <- dat}
      
      else if(input$inputReg == "2") {
        req(input$regdrop)
          dat3 <- filter(dat, Region == input$regdrop)}
      
      else if(input$inputReg == "3") {
        req(input$sppdrop)
          tempdat <- rename(dat, "Species" = "Species Group")
            dat3 <- filter(tempdat, Species == input$sppdrop)}
      
              RRCrdat <- dat3[, c(1, 64, 65)]
              
              if(input$inputno == "1") {
                RRCrdat <- RRCrdat}
              
              else if(input$inputno == "2") {
                tempdat2 <- RRCrdat
                  tempdat2 <- mutate(tempdat2, PA = tempdat2$`Has a removal reference been identified when the stock is in the Critical Zone?`)
                    tempdat2 <- tempdat2 %>% filter(PA == "Yes")
                      RRCrdat <- subset(tempdat2, select = c(1:3))}

                RRCrdat %>% kable(table.attr = "style = \"color: black;\"", format = "html",
                                  escape = F, row.names = FALSE) %>%
                  kable_styling(full_width = TRUE,
                                bootstrap_options = c("striped", "responsive", "bordered"),
                                position = "center", fixed_thead = TRUE) %>%
                    row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>%
                      column_spec(1, width = "30em", bold = T) %>% column_spec(3, width = "60em")}

  
  else if(input$inputPA == 7) {
    
    if(input$inputReg == "1") {
      dat3 <- dat}
    
    else if(input$inputReg == "2") {
      req(input$regdrop)
      dat3 <- filter(dat, Region == input$regdrop)}
    
    else if(input$inputReg == "3") {
      req(input$sppdrop)
      tempdat <- rename(dat, "Species" = "Species Group")
      dat3 <- filter(tempdat, Species == input$sppdrop)}
    
    RRUdat <- dat3[, c(1, 66, 67)]
    
    if(input$inputno == "1") {
      RRUdat <- RRUdat}
    
    else if(input$inputno == "2") {
      tempdat2 <- RRUdat
      tempdat2 <- mutate(tempdat2, PA = tempdat2$`When listed as uncertain, is there a removal reference for this stock?`)
      tempdat2 <- tempdat2 %>% filter(PA == "Yes")
      RRUdat <- subset(tempdat2, select = c(1:3))}
    
    RRUdat %>% kable(table.attr = "style = \"color: black;\"", format = "html",
                      escape = F, row.names = FALSE) %>%
      kable_styling(full_width = TRUE,
                    bootstrap_options = c("striped", "responsive", "bordered"),
                    position = "center", fixed_thead = TRUE) %>%
      row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>%
      column_spec(1, width = "30em", bold = T) %>% column_spec(3, width = "60em")}
  
  else if(input$inputPA == 8) {
    
    if(input$inputReg == "1") {
      dat3 <- dat}
    
    else if(input$inputReg == "2") {
      req(input$regdrop)
      dat3 <- filter(dat, Region == input$regdrop)}
    
    else if(input$inputReg == "3") {
      req(input$sppdrop)
      tempdat <- rename(dat, "Species" = "Species Group")
      dat3 <- filter(tempdat, Species == input$sppdrop)}
    
    SSdat <- dat3[, c(1, 53, 54)]
    
    if(input$inputno == "1") {
      SSdat <- SSdat}
    
    else if(input$inputno == "2") {
      SSdat <- SSdat}
    
    SSdat %>% kable(table.attr = "style = \"color: black;\"", format = "html",
                     escape = F, row.names = FALSE) %>%
      kable_styling(full_width = TRUE,
                    bootstrap_options = c("striped", "responsive", "bordered"),
                    position = "center", fixed_thead = TRUE) %>%
      row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>%
      column_spec(1, width = "30em", bold = T) %>% column_spec(3, width = "60em")}
    
    else if(input$inputPA == 9) {
      
      if(input$inputReg == "1") {
        dat3 <- dat}
      
      else if(input$inputReg == "2") {
        req(input$regdrop)
        dat3 <- filter(dat, Region == input$regdrop)}
      
      else if(input$inputReg == "3") {
        req(input$sppdrop)
        tempdat <- rename(dat, "Species" = "Species Group")
        dat3 <- filter(tempdat, Species == input$sppdrop)}
      
      FSdat <- dat3[, c(1, 69, 70)]
      
      if(input$inputno == "1") {
        FSdat <- FSdat}
      
      else if(input$inputno == "2") {
        FSdat <- FSdat}
      
      FSdat %>% kable(table.attr = "style = \"color: black;\"", format = "html",
                       escape = F, row.names = FALSE) %>%
        kable_styling(full_width = TRUE,
                      bootstrap_options = c("striped", "responsive", "bordered"),
                      position = "center", fixed_thead = TRUE) %>%
        row_spec(0, bold = T, color = "white", font_size = 18, background = "#2C3E50") %>%
        column_spec(1, width = "30em", bold = T) %>% column_spec(3, width = "60em")}}})
  
######################################################
  
#Compare Stocks Tab
  
  observeEvent(input$comp1, {
    
    if (input$comp1 == "1") {
      
      dat <- dat
        dat2 <- dat2}
    
    else if (input$comp1 == "2") {
      
      dat <- batch1
        dat2 <- batch_flipped}
  
  observeEvent(input$filterc, {
    
    if (input$filterc == "1") {
      output$outputfilterc <- renderUI({NULL})}
    
    else if (input$filterc == "2") {
      output$outputfilterc <- renderUI({
        selectizeInput(inputId = "filtregion", label = "Select a Region: (Note - There are no stocks from the C&A
                       region in Batch 1)",
                       choices = dat[,3],
                       options = list(placeholder = "Select a Region",                                
                       onInitialize = I('function() { this.setValue(""); }')))})}
    
    else if (input$filterc == "3") {
      output$outputfilterc <- renderUI({
        selectizeInput(inputId = "filtspp", label = "Select a Species Group:", choices = dat[,2],
                       options = list(placeholder = "Select a Species Group",                           
                       onInitialize = I('function() { this.setValue(""); }')))})}}) 
  

  output$comptable <- DT::renderDataTable({  
    if (input$filterc == "1") {
      req(input$comp1)
      
      dat2 <- dat2

      DT::datatable(dat2, extensions = "FixedColumns", escape = FALSE, class = "cell-border stripe",
                    options = list(scrollX = TRUE, scrollY = "800px", lengthChange = FALSE, dom = "t",
                    autoWidth = TRUE, searching = FALSE, paging = FALSE, ordering = FALSE,
                    headerCallback = JS("function(settings, json) {", "$(this.api().table().header()).css
                                        ({'background-color': '#2C3E50', 'color': '#fff'});", "}"),
                    fixedColumns = list(leftColumns = 1)), rownames = TRUE) %>% 
      formatStyle(0, fontWeight = "bold", color = "black") %>% 
      formatStyle(1:ncol(dat2), color = "black")}
  
    else if (input$filterc == "2") {
      req(input$comp1)
      
      Regdat <- filter(dat, Region == input$filtregion) 
        Regdat1 <- data.frame(t(Regdat[,-1])) 
          colnames(Regdat1) <- Regdat[,1]
          
      DT::datatable(Regdat1, extensions = "FixedColumns", escape = FALSE, class = "cell-border stripe",
        options = list(scrollX = TRUE, scrollY = "800px", lengthChange = FALSE, autoWidth = T, dom = "t",
                  searching = FALSE, paging = FALSE, ordering = FALSE,
                  headerCallback = JS("function(settings, json) {", "$(this.api().table().header
                                      ()).css({'background-color': '#2C3E50', 'color': '#fff'});", "}"),
                  fixedColumns = list(leftColumns = 1)), rownames = TRUE) %>% 
      formatStyle(0, fontWeight = "bold", color = "black") %>% 
      formatStyle(1:ncol(Regdat1), color = "black")}
    
    else if (input$filterc == "3") {
      req(input$comp1)
      
      taxdat <- rename(dat, "Species" = "Species Group")
        taxdat2 <- filter(taxdat, Species == input$filtspp) 
          taxdat1 <- data.frame(t(taxdat2[,-1])) 
            colnames(taxdat1) <- taxdat2[,1]
      
      DT::datatable(taxdat1, extensions = "FixedColumns", escape = FALSE, class = "cell-border stripe",
        options = list(scrollX = TRUE, scrollY = "800px", lengthChange = FALSE, autoWidth = T,
                  searching = FALSE, paging = FALSE, ordering = FALSE, dom = "t",
                  headerCallback = JS("function(settings, json){", "$(this.api().table().header()).css
                                      ({'background-color':'#2C3E50', 'color': '#fff'});","}"),
                  fixedColumns = list(leftColumns = 1)), rownames = TRUE) %>% 
      formatStyle(0, fontWeight = "bold", color = "black") %>% 
      formatStyle(1:ncol(taxdat1), color = "black")}  })})
  
######################################################
      
#Summary Plots Page Functions
  
  #Precautionary Components Tab
  
  #Information Selection - PA Components Plot
  
observeEvent(input$plotfilt, {
    
  if(input$plotfilt == "1") {
    output$plotfilter <- renderUI({
       NULL})}
    
  else if (input$plotfilt == "2") {
    output$plotfilter <- renderUI({
        selectizeInput(inputId = "info1", 
          label = "Select a Region:", 
            choices = dat[,3], 
              options = list(placeholder = "Select a Region", 
                onInitialize = I('function() { this.setValue("Gulf"); }')))})}
                    
  else if (input$plotfilt == "3") {
    output$plotfilter <- renderUI({
      selectizeInput(inputId = "info2", 
        label = "Select a Species Group:", 
          choices = dat[,2], 
            options = list(placeholder = "Select a Species Group", 
              onInitialize = I('function() { this.setValue("Salmonids"); }')))})}
    
  else if (input$plotfilt == "4") {
    output$plotfilter <- renderUI({
      NULL})}})
  
  #Produce PA Components Plot
  
  output$plot1 <- renderPlot( {
    
  if (input$plotfilt == "1") {
    
   dat <- dat
      
  dat <- mutate(dat, HCR = dat$`For this stock, harvest decision rules that aim to control exploitation:`)
    levels(dat$HCR)[levels(dat$HCR) == "Have been developed but not implemented"] <- "No"
    levels(dat$HCR)[levels(dat$HCR) == "Have been developed and implemented, but not evaluated"] <- "Yes"
    levels(dat$HCR)[levels(dat$HCR) == "Have not been developed"] <- "No"
    levels(dat$HCR)[levels(dat$HCR) == "Have been developed, implemented and evaluated"] <- "Yes"
    
    USR <- dat$`Is there an Upper Stock Reference (point dividing the cautious and healthy zones)?`
      LRP <- dat$`Is there a Limit Reference Point (point dividing the critical and cautious zones)?`
        RRH <- dat$`Has a removal reference been identified when the stock is in the Healthy Zone?`
          RRC <- dat$`Has a removal reference been identified when the stock is in the Cautious Zone?`
            RRCr <- dat$`Has a removal reference been identified when the stock is in the Critical Zone?`
                HCR <- dat$HCR
            
    USR <- as.data.frame(USR)
      LRP <- as.data.frame(LRP)
        RRH <- as.data.frame(RRH)
          RRC <- as.data.frame(RRC) 
            RRCr <- as.data.frame(RRCr)
              HCR <- as.data.frame(HCR)
                datUSR <- cbind(USR, LRP, RRH, RRC, RRCr, HCR)
              
    datPA <- as.data.frame(do.call(cbind, lapply(datUSR, summary)))
      datPA$HCR <- rev(datPA$HCR)
        datPA <- as.data.frame(lapply(datPA, function(x) 100*(x/sum(x))))
          datPA <- round(datPA, 0)
            names(datPA)[names(datPA) == "USR"] <- "Upper Stock\nReference (USR)"
              names(datPA)[names(datPA) == "LRP"] <- "Limit Reference\nPoint (LRP)"
                names(datPA)[names(datPA) == "RRH"] <- "Removal Reference\n(Healthy Zone)"
                  names(datPA)[names(datPA) == "RRC"] <- "Removal Reference\n(Cautious Zone)"
                    names(datPA)[names(datPA) == "RRCr"] <- "Removal Reference\n(Critical Zone)"
                        names(datPA)[names(datPA) == "HCR"] <- "Harvest Control\nRule (HCR)"
                  
    rownames(datPA) <- c("No", "Yes")
      datPA2 <- data.frame(t(datPA[,-8])) 
        colnames(datPA2) <- rownames(datPA)
          datPA2 <- tibble::rownames_to_column(datPA2, "Point")

    data_long <- datPA2 %>% gather(Exist, Percent, Yes)
      data_long$Point <- factor(data_long$Point, levels = c("Harvest Control\nRule (HCR)", 
                           "Limit Reference\nPoint (LRP)", "Upper Stock\nReference (USR)",
                             "Removal Reference\n(Healthy Zone)", "Removal Reference\n(Critical Zone)",
                               "Removal Reference\n(Cautious Zone)"))

    ggplot(data_long, mapping = aes(fill = Exist, x = Point, y = Percent)) +
      geom_bar(stat = "identity") +
        geom_text(aes(label = ifelse(Exist == "No", "",
                        paste0(round(Percent/100 * length(dat$Region), 0), " Stocks"))),
                          position = position_nudge(y = 5), color = "#2C3E50",
                            size = 6) +
          geom_text(aes(label = paste0(Percent, "%")), position = position_nudge(y = -5), 
                      color = "white", size = 6) +
            theme_hc() +
              ylab("Percentage (%)") +
                scale_x_discrete(expand = c(0, 0)) +
                  scale_y_continuous(limits = c(0, 110), expand = c(0, 0),
                                       labels = function(Percent) paste0(Percent, "%")) +
                    scale_fill_manual(values = c("#FF7F0E"),
                                        labels = c("Stocks without Precautionary Approach Component",
                                          "Stocks with Precautionary Approach Component")) +
                      theme(axis.title.x = element_blank(), 
                              axis.title.y = element_blank(),
                                axis.text = element_text(color = "#2C3E50", size = 13),
                                  legend.position = "none", 
                                    plot.margin = unit(c(-6,4,0,0), "mm"))}  

  else if(input$plotfilt == "2") {
      
    dat <- dat %>% filter(Region == input$info1)
      
  dat <- mutate(dat, HCR = dat$`For this stock, harvest decision rules that aim to control exploitation:`)
    levels(dat$HCR)[levels(dat$HCR) == "Have been developed but not implemented"] <- "No"
    levels(dat$HCR)[levels(dat$HCR) == "Have been developed and implemented, but not evaluated"] <- "Yes"
    levels(dat$HCR)[levels(dat$HCR) == "Have not been developed"] <- "No"
    levels(dat$HCR)[levels(dat$HCR) == "Have been developed, implemented and evaluated"] <- "Yes"
      
    USR <- dat$`Is there an Upper Stock Reference (point dividing the cautious and healthy zones)?`
      LRP <- dat$`Is there a Limit Reference Point (point dividing the critical and cautious zones)?`
        RRH <- dat$`Has a removal reference been identified when the stock is in the Healthy Zone?`
          RRC <- dat$`Has a removal reference been identified when the stock is in the Cautious Zone?`
            RRCr <- dat$`Has a removal reference been identified when the stock is in the Critical Zone?`
              HCR <- dat$HCR
      
    USR <- as.data.frame(USR)
      LRP <- as.data.frame(LRP)
        RRH <- as.data.frame(RRH)
          RRC <- as.data.frame(RRC) 
            RRCr <- as.data.frame(RRCr)
              HCR <- as.data.frame(HCR)
                datUSR <- cbind(USR, LRP, RRH, RRC, RRCr, HCR)
      
    datPA <- as.data.frame(do.call(cbind, lapply(datUSR, summary)))
      datPA$HCR <- rev(datPA$HCR)
        datPA <- as.data.frame(lapply(datPA, function(x) 100*(x/sum(x))))
          datPA <- round(datPA, 0)
            names(datPA)[names(datPA) == "USR"] <- "Upper Stock\nReference (USR)"
              names(datPA)[names(datPA) == "LRP"] <- "Limit Reference\nPoint (LRP)"
                names(datPA)[names(datPA) == "RRH"] <- "Removal Reference\n(Healthy Zone)"
                  names(datPA)[names(datPA) == "RRC"] <- "Removal Reference\n(Cautious Zone)"
                    names(datPA)[names(datPA) == "RRCr"] <- "Removal Reference\n(Critical Zone)"
                      names(datPA)[names(datPA) == "HCR"] <- "Harvest Control\nRule (HCR)"
      
    rownames(datPA) <- c("No", "Yes")
      datPA2 <- data.frame(t(datPA[,-7])) 
        colnames(datPA2) <- rownames(datPA)
          datPA2 <- tibble::rownames_to_column(datPA2, "Point")
      
    data_long <- datPA2 %>% gather(Exist, Percent, Yes)
      data_long$Point <- factor(data_long$Point, levels = c("Harvest Control\nRule (HCR)", 
                                                            "Limit Reference\nPoint (LRP)",
                                                            "Upper Stock\nReference (USR)",
                                                            "Removal Reference\n(Healthy Zone)",
                                                            "Removal Reference\n(Critical Zone)",
                                                            "Removal Reference\n(Cautious Zone)"))

    ggplot(data_long, mapping = aes(fill = Exist, x = Point, y = Percent)) +
      geom_bar(stat = "identity") +
        geom_text(aes(label = ifelse(Exist == "No", "",
                        paste0(round(Percent/100 * length(dat$Region), 0), " Stocks"))), 
                          position = position_nudge(y = 5),
                            color = "#2C3E50", size = 6) +
          geom_text(aes(label = paste0(Percent, "%")), position = position_nudge(y = -5), 
                      color = "white", size = 6) +
            theme_hc() +
              ylab("Percentage (%)") +
                scale_x_discrete(expand = c(0, 0)) +
                  scale_y_continuous(limits = c(0, 110), expand = c(0, 0),
                                       labels = function(Percent) paste0(Percent, "%")) +
                    scale_fill_manual(values = c("#FF7F0E"),
                                        labels = c("Stocks without Precautionary Approach Component",
                                          "Stocks with Precautionary Approach Component")) +
                      theme(axis.title.x = element_blank(), 
                              axis.title.y = element_blank(),
                                axis.text = element_text(color = "#2C3E50", size = 13),
                                  legend.position = "none", 
                                    plot.margin = unit(c(-6,4,0,0), "mm"))}
  
  else if(input$plotfilt == "3") {
    
    tempdat <- rename(dat, "Species" = "Species Group")
      dat <- tempdat %>% filter(Species == input$info2)

  dat <- mutate(dat, HCR = dat$`For this stock, harvest decision rules that aim to control exploitation:`)
    levels(dat$HCR)[levels(dat$HCR) == "Have been developed but not implemented"] <- "No"
    levels(dat$HCR)[levels(dat$HCR) == "Have been developed and implemented, but not evaluated"] <- "Yes"
    levels(dat$HCR)[levels(dat$HCR) == "Have not been developed"] <- "No"
    levels(dat$HCR)[levels(dat$HCR) == "Have been developed, implemented and evaluated"] <- "Yes"
    
    USR <- dat$`Is there an Upper Stock Reference (point dividing the cautious and healthy zones)?`
      LRP <- dat$`Is there a Limit Reference Point (point dividing the critical and cautious zones)?`
        RRH <- dat$`Has a removal reference been identified when the stock is in the Healthy Zone?`
          RRC <- dat$`Has a removal reference been identified when the stock is in the Cautious Zone?`
            RRCr <- dat$`Has a removal reference been identified when the stock is in the Critical Zone?`
              HCR <- dat$HCR
    
    USR <- as.data.frame(USR)
      LRP <- as.data.frame(LRP)
        RRH <- as.data.frame(RRH)
          RRC <- as.data.frame(RRC) 
            RRCr <- as.data.frame(RRCr)
              HCR <- as.data.frame(HCR)
                datUSR <- cbind(USR, LRP, RRH, RRC, RRCr, HCR)
    
    datPA <- as.data.frame(do.call(cbind, lapply(datUSR, summary)))
      datPA$HCR <- rev(datPA$HCR)
        datPA <- as.data.frame(lapply(datPA, function(x) 100*(x/sum(x))))
          datPA <- round(datPA, 0)
            names(datPA)[names(datPA) == "USR"] <- "Upper Stock\nReference (USR)"
              names(datPA)[names(datPA) == "LRP"] <- "Limit Reference\nPoint (LRP)"
                names(datPA)[names(datPA) == "RRH"] <- "Removal Reference\n(Healthy Zone)"
                  names(datPA)[names(datPA) == "RRC"] <- "Removal Reference\n(Cautious Zone)"
                    names(datPA)[names(datPA) == "RRCr"] <- "Removal Reference\n(Critical Zone)"
                      names(datPA)[names(datPA) == "HCR"] <- "Harvest Control\nRule (HCR)"
    
    rownames(datPA) <- c("No", "Yes")
      datPA2 <- data.frame(t(datPA[,-7])) 
        colnames(datPA2) <- rownames(datPA)
          datPA2 <- tibble::rownames_to_column(datPA2, "Point")
    
    data_long <- datPA2 %>% gather(Exist, Percent, Yes)
      data_long$Point <- factor(data_long$Point, levels = c("Harvest Control\nRule (HCR)", 
                                                            "Limit Reference\nPoint (LRP)",
                                                            "Upper Stock\nReference (USR)",
                                                            "Removal Reference\n(Healthy Zone)",
                                                            "Removal Reference\n(Critical Zone)",
                                                            "Removal Reference\n(Cautious Zone)"))
    
    ggplot(data_long, mapping = aes(fill = Exist, x = Point, y = Percent)) +
      geom_bar(stat = "identity") +
        geom_text(aes(label = ifelse(Exist == "No", "",
                        paste0(round(Percent/100 * length(dat$Region), 0), " Stocks"))), 
                          position = position_nudge(y = 5), color = "#2C3E50", size = 6) +
          geom_text(aes(label = paste0(Percent, "%")), position = position_nudge(y = -5), 
                      color = "white", size = 6) +
            theme_hc() +
              ylab("Percentage (%)") +
                scale_x_discrete(expand = c(0, 0)) +
                  scale_y_continuous(limits = c(0, 110), expand = c(0, 0),
                                       labels = function(Percent) paste0(Percent, "%")) +
                    scale_fill_manual(values = c("#FF7F0E"),
                                        labels = c("Stocks without Precautionary Approach Component",
                                          "Stocks with Precautionary Approach Component")) +
                      theme(axis.title.x = element_blank(), 
                        axis.title.y = element_blank(),
                          axis.text = element_text(color = "#2C3E50", size = 13),
                            legend.position = "none", 
                              plot.margin = unit(c(-6,4,0,0), "mm"))}
  
  else if (input$plotfilt =="4") {
    
    dat <- batch1
      
  dat <- mutate(dat, HCR = dat$`For this stock, harvest decision rules that aim to control exploitation:`)
    levels(dat$HCR)[levels(dat$HCR) == "Have been developed but not implemented"] <- "No"
    levels(dat$HCR)[levels(dat$HCR) == "Have been developed and implemented, but not evaluated"] <- "Yes"
    levels(dat$HCR)[levels(dat$HCR) == "Have not been developed"] <- "No"
    levels(dat$HCR)[levels(dat$HCR) == "Have been developed, implemented and evaluated"] <- "Yes"
      
    USR <- dat$`Is there an Upper Stock Reference (point dividing the cautious and healthy zones)?`
      LRP <- dat$`Is there a Limit Reference Point (point dividing the critical and cautious zones)?`
        RRH <- dat$`Has a removal reference been identified when the stock is in the Healthy Zone?`
          RRC <- dat$`Has a removal reference been identified when the stock is in the Cautious Zone?`
            RRCr <- dat$`Has a removal reference been identified when the stock is in the Critical Zone?`
              HCR <- dat$HCR
      
    USR <- as.data.frame(USR)
      LRP <- as.data.frame(LRP)
        RRH <- as.data.frame(RRH)
          RRC <- as.data.frame(RRC) 
            RRCr <- as.data.frame(RRCr)
              HCR <- as.data.frame(HCR)
                datUSR <- cbind(USR, LRP, RRH, RRC, RRCr, HCR)
      
    datPA <- as.data.frame(do.call(cbind, lapply(datUSR, summary)))
      datPA$HCR <- rev(datPA$HCR)
        datPA <- as.data.frame(lapply(datPA, function(x) 100*(x/sum(x))))
          datPA <- round(datPA, 0)
            names(datPA)[names(datPA) == "USR"] <- "Upper Stock\nReference (USR)"
              names(datPA)[names(datPA) == "LRP"] <- "Limit Reference\nPoint (LRP)"
                names(datPA)[names(datPA) == "RRH"] <- "Removal Reference\n(Healthy Zone)"
                  names(datPA)[names(datPA) == "RRC"] <- "Removal Reference\n(Cautious Zone)"
                    names(datPA)[names(datPA) == "RRCr"] <- "Removal Reference\n(Critical Zone)"
                      names(datPA)[names(datPA) == "HCR"] <- "Harvest Control\nRule (HCR)"
      
    rownames(datPA) <- c("No", "Yes")
      datPA2 <- data.frame(t(datPA[,-7])) 
        colnames(datPA2) <- rownames(datPA)
          datPA2 <- tibble::rownames_to_column(datPA2, "Point")
      
    data_long <- datPA2 %>% gather(Exist, Percent, Yes)
      data_long$Point <- factor(data_long$Point, levels = c("Harvest Control\nRule (HCR)", 
                                                            "Limit Reference\nPoint (LRP)",
                                                            "Upper Stock\nReference (USR)",
                                                            "Removal Reference\n(Healthy Zone)",
                                                            "Removal Reference\n(Critical Zone)",
                                                            "Removal Reference\n(Cautious Zone)"))
      
    ggplot(data_long, mapping = aes(fill = Exist, x = Point, y = Percent)) +
      geom_bar(stat = "identity") +
        geom_text(aes(label = ifelse(Exist == "No", "",
                        paste0(round(Percent/100 * length(dat$Region), 0), " Stocks"))),
                          position = position_nudge(y = 5), color = "#2C3E50",
                            size = 6) +
          geom_text(aes(label = paste0(Percent, "%")), position = position_nudge(y = -5), 
                      color = "white", size = 6) +
            theme_hc() +
              ylab("Percentage (%)") +
                scale_x_discrete(expand = c(0, 0)) +
                  scale_y_continuous(limits = c(0, 110), expand = c(0, 0),
                                       labels = function(Percent) paste0(Percent, "%")) +
                    scale_fill_manual(values = c("#FF7F0E"),
                                        labels = c("Stocks without Precautionary Approach Component",
                                          "Stocks with Precautionary Approach Component")) +
                      theme(axis.title.x = element_blank(), 
                              axis.title.y = element_blank(),
                                axis.text = element_text(color = "#2C3E50", size = 13),
                                  legend.position = "none", 
                                    plot.margin = unit(c(-6,4,0,0), "mm")) }})
  
######################################################
  
  #Stock Status 
  
  observeEvent(input$plotfilt2, {  
  
    if(input$plotfilt2 == "1") {
      output$plotfilter2 <- renderUI({NULL})}

    else if (input$plotfilt2 == "2") {
      output$plotfilter2 <- renderUI({
        selectizeInput(inputId = "info1a", 
                       label = "Select a Region:", 
                         choices = dat[,3], 
                           options = list(placeholder = "Select a Region", 
                             onInitialize = I('function() { this.setValue("Gulf"); }')))})}

    else if (input$plotfilt2 == "3") {
      output$plotfilter2 <- renderUI({
        selectizeInput(inputId = "info2a", 
                       label = "Select a Species Group:", 
                         choices = dat[,2], 
                           options = list(placeholder = "Select a Species Group", 
                             onInitialize = I('function() { this.setValue("Salmonids"); }')))})}
    
    else if (input$plotfilt2 == "4") {
      output$plotfilter2 <- renderUI({NULL})}})
  
  output$plot3 <- renderPlot( {
    
    if (input$plotfilt2 == "1") {
      
      dat <- dat
        zone <- dat$`What is the current status zone for this stock?`
          zone <- as.data.frame(table(zone))
            zone <- zone %>% mutate(FreqNo = 177 - Freq)
              zone_long <- zone %>% gather(Exist, Stocks, Freq:FreqNo)
                zone_long$Stocks <- zone_long$Stocks/177 * 100
                  zone_long$Stocks <- round(zone_long$Stocks, 1)
                    healthy <- filter(zone_long, zone == "Healthy Zone")
                      cautious <- filter(zone_long, zone == "Cautious Zone")
                        critical <- filter(zone_long, zone == "Critical Zone")
                          uncertain <- filter(zone_long, zone == "Uncertain")
      
      Hplot <- ggplot(healthy, mapping = aes(fill = Exist, x = zone, y = Stocks)) +
        geom_bar(position = position_stack(reverse = TRUE), 
                 stat = "identity", color = "white", size = 1) +
          geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                      paste0(round(Stocks/100 * 177, 0), " Stocks"))),
                        position = position_nudge(y = 7), color = "black",
                          size = 8) +
          geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                      paste0(round(Stocks), "%"))),
                        position = position_nudge(y = -4), color = "white", size = 8) +
            labs(title = "Healthy Zone:") +
              theme_hc() +
                scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                  scale_fill_manual(values = c("#4bae2e", "grey")) +
                    coord_flip() +
                      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                              axis.text = element_blank(), plot.title = element_text(color = "#2C3E50", 
                                size = 16, face = "bold"), legend.position = "none",
                                  axis.ticks = element_blank()) 
      
      Cplot <- ggplot(cautious, mapping = aes(fill = Exist, x = zone, y = Stocks)) +
        geom_bar(position = position_stack(reverse = TRUE), 
                  stat = "identity", color = "white", size = 1) +
          geom_text(aes(label = ifelse(Exist == "FreqNo", "", 
                      paste0(round(Stocks/100 * 177, 0), " Stocks"))),
                        position = position_nudge(y = 7),
                          color = "black", size = 8) +
        geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                    paste0(round(Stocks), "%"))),
                      position = position_nudge(y = -4), color = "white", size = 8) +
            labs(title = "Cautious Zone:") +
              theme_hc() +
                scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                  scale_fill_manual(values = c("#FF7F0E", "grey")) +
                    coord_flip() +
                      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                              axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                                size = 16, face = "bold"), legend.position = "none",
                                  axis.ticks = element_blank())
      
      Crplot <- ggplot(critical, mapping = aes(fill = Exist, x = zone,y = Stocks)) +
        geom_bar(position = position_stack(reverse = TRUE),
                  stat = "identity", color = "white", size = 1) +
          geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                          paste0(round(Stocks/100 * 177, 0), " Stocks"))),
                            position = position_nudge(y = 7),
                              color = "black", size = 8) +
          geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                      paste0(round(Stocks), "%"))),
                        position = position_nudge(y = -4), color = "white", size = 8) +
            labs(title = "Critical Zone:") +
              theme_hc() +
                scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                  scale_fill_manual(values = c("#e30303", "grey")) +
                    coord_flip() +
                      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                              axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                                size = 16, face = "bold"), legend.position = "none",
                                  axis.ticks = element_blank())
      
      Uplot <- ggplot(uncertain, mapping = aes(fill = Exist, x = zone, y = Stocks)) +
        geom_bar(position = position_stack(reverse = TRUE), 
                  stat = "identity", color = "white", size = 1) +
          geom_text(aes(label = ifelse(Exist == "FreqNo", "", 
                      paste0(round(Stocks/100 * 177, 0), " Stocks"))),
                        position = position_nudge(y = 7),
                          color = "black", size = 8) +
          geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                      paste0(round(Stocks), "%"))),
                        position = position_nudge(y = -4), color = "white", size = 8) +
            labs(title = "Status Uncertain:") +
              theme_hc() +
                scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                  scale_fill_manual(values = c("#1F77B4", "grey")) +
                    coord_flip() +
                      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                              axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                                size = 16, face = "bold"), legend.position = "none", 
                                  axis.ticks = element_blank())
      
      grid.arrange(Crplot, Cplot, Hplot, Uplot, ncol = 1)}
    
    else if (input$plotfilt2 == "2") {
      
      dat <- dat %>% filter(Region == input$info1a)
        zone <- dat$`What is the current status zone for this stock?`
          zone <- as.data.frame(table(zone))
            zone <- zone %>% mutate(FreqNo = length(dat$Region) - Freq)
              zone_long <- zone %>% gather(Exist, Stocks, Freq:FreqNo)
                zone_long$Stocks <- zone_long$Stocks/length(dat$Region) * 100
                  zone_long$Stocks <- round(zone_long$Stocks, 1)
                    healthy <- filter(zone_long, zone == "Healthy Zone")
                      cautious <- filter(zone_long, zone == "Cautious Zone")
                        critical <- filter(zone_long, zone == "Critical Zone")
                          uncertain <- filter(zone_long, zone == "Uncertain")
    
      Hplot <- ggplot(healthy, mapping = aes(fill = Exist, x = zone, y = Stocks)) +
        geom_bar(position = position_stack(reverse = TRUE), 
                  stat = "identity", color = "white", size = 1) +
          geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                      paste0(round(Stocks/100 * length(dat$Region), 0), " Stocks"))),
                        position = position_nudge(y = 7), 
                          color = "black", size = 8) +
          geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                      paste0(round(Stocks), "%"))),
                        position = position_nudge(y = -4), color = "white", size = 8) +
            labs(title = "Healthy Zone:") +
              theme_hc() +
                scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                  scale_fill_manual(values = c("#4bae2e", "grey")) +
                    coord_flip() +
                      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                              axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                                size = 16, face = "bold"), legend.position = "none",
                                  axis.ticks = element_blank()) 
      
      Cplot <- ggplot(cautious, mapping = aes(fill = Exist, x = zone, y = Stocks)) +
        geom_bar(position = position_stack(reverse = TRUE), 
                  stat = "identity", color = "white", size = 1) +
          geom_text(aes(label = ifelse(Exist == "FreqNo", "", 
                      paste0(round(Stocks/100 * length(dat$Region), 0), " Stocks"))),
                        position = position_nudge(y = 7), 
                          color = "black", size = 8) +
            geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                        paste0(round(Stocks), "%"))),
                          position = position_nudge(y = -4), color = "white", size = 8) +
            labs(title = "Cautious Zone:") +
              theme_hc() +
                scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                  scale_fill_manual(values = c("#FF7F0E", "grey")) +
                    coord_flip() +
                      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                              axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                                size = 16, face = "bold"), legend.position = "none", 
                                  axis.ticks = element_blank())
      
      Crplot <- ggplot(critical, mapping = aes(fill = Exist, x = zone,y = Stocks)) +
        geom_bar(position = position_stack(reverse = TRUE),
                  stat = "identity", color = "white", size = 1) +
          geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                      paste0(round(Stocks/100 * length(dat$Region), 0), " Stocks"))),
                        position = position_nudge(y = 7),
                          color = "black", size = 8) +
            geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                        paste0(round(Stocks), "%"))),
                          position = position_nudge(y = -4), color = "white", size = 8) +
            labs(title = "Critical Zone:") +
              theme_hc() +
                scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                  scale_fill_manual(values = c("#e30303", "grey")) +
                    coord_flip() +
                      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                              axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                                size = 16, face = "bold"), legend.position = "none",
                                  axis.ticks = element_blank())
      
      Uplot <- ggplot(uncertain, mapping = aes(fill = Exist, x = zone, y = Stocks)) +
        geom_bar(position = position_stack(reverse = TRUE), 
                  stat = "identity", color = "white", size = 1) +
          geom_text(aes(label = ifelse(Exist == "FreqNo", "", 
                      paste0(round(Stocks/100 * length(dat$Region), 0), " Stocks"))),
                        position = position_nudge(y = 7),
                          color = "black", size = 8) +
            geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                        paste0(round(Stocks), "%"))),
                          position = position_nudge(y = -4), color = "white", size = 8) +
            labs(title = "Status Uncertain:") +
              theme_hc() +
                scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                  scale_fill_manual(values = c("#1F77B4", "grey")) +
                    coord_flip() +
                      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                              axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                                size = 16, face = "bold"), legend.position = "none",
                                  axis.ticks = element_blank())
      
      grid.arrange(Crplot, Cplot, Hplot, Uplot, ncol = 1)}      
    
  else if (input$plotfilt2 == "3") {
    
    tempdat <- rename(dat, "Species" = "Species Group")
      dat <- tempdat %>% filter(Species == input$info2a)
        zone <- dat$`What is the current status zone for this stock?`
          zone <- as.data.frame(table(zone))
            zone <- zone %>% mutate(FreqNo = length(dat$Region) - Freq)
              zone_long <- zone %>% gather(Exist, Stocks, Freq:FreqNo)
                zone_long$Stocks <- zone_long$Stocks/length(dat$Region) * 100
                  zone_long$Stocks <- round(zone_long$Stocks, 1)
                    healthy <- filter(zone_long, zone == "Healthy Zone")
                      cautious <- filter(zone_long, zone == "Cautious Zone")
                        critical <- filter(zone_long, zone == "Critical Zone")
                          uncertain <- filter(zone_long, zone == "Uncertain")
        
        Hplot <- ggplot(healthy, mapping = aes(fill = Exist, x = zone, y = Stocks)) +
          geom_bar(position = position_stack(reverse = TRUE), 
                    stat = "identity", color = "white", size = 1) +
            geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                        paste0(round(Stocks/100 * length(dat$Region), 0), " Stocks"))),
                          position = position_nudge(y = 7),
                            color = "black", size = 8) +
              geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                          paste0(round(Stocks), "%"))),
                            position = position_nudge(y = -4), color = "white", size = 8) +
              labs(title = "Healthy Zone:") +
                theme_hc() +
                  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                    scale_fill_manual(values = c("#4bae2e", "grey")) +
                      coord_flip() +
                        theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                                axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                                  size = 16, face = "bold"), legend.position = "none",
                                    axis.ticks = element_blank()) 
        
        Cplot <- ggplot(cautious, mapping = aes(fill = Exist, x = zone, y = Stocks)) +
          geom_bar(position = position_stack(reverse = TRUE), 
                    stat = "identity", color = "white", size = 1) +
            geom_text(aes(label = ifelse(Exist == "FreqNo", "", 
                        paste0(round(Stocks/100 * length(dat$Region), 0), " Stocks"))),
                          position = position_nudge(y = 7), 
                            color = "black", size = 8) +
              geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                          paste0(round(Stocks), "%"))),
                            position = position_nudge(y = -4), color = "white", size = 8) +
              labs(title = "Cautious Zone:") +
                theme_hc() +
                  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                    scale_fill_manual(values = c("#FF7F0E", "grey")) +
                      coord_flip() +
                        theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                                axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                                  size = 16, face = "bold"), legend.position = "none", 
                                    axis.ticks = element_blank())
        
        Crplot <- ggplot(critical, mapping = aes(fill = Exist, x = zone,y = Stocks)) +
          geom_bar(position = position_stack(reverse = TRUE),
                    stat = "identity", color = "white", size = 1) +
            geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                        paste0(round(Stocks/100 * length(dat$Region), 0), " Stocks"))),
                          position = position_nudge(y = 7), 
                            color = "black", size = 8) +
              geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                          paste0(round(Stocks), "%"))),
                            position = position_nudge(y = -4), color = "white", size = 8) +
              labs(title = "Critical Zone:") +
                theme_hc() +
                  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                    scale_fill_manual(values = c("#e30303", "grey")) +
                      coord_flip() +
                        theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                                axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                                  size = 16, face = "bold"), legend.position = "none",
                                    axis.ticks = element_blank())
        
        Uplot <- ggplot(uncertain, mapping = aes(fill = Exist, x = zone, y = Stocks)) +
          geom_bar(position = position_stack(reverse = TRUE), 
                    stat = "identity", color = "white", size = 1) +
            geom_text(aes(label = ifelse(Exist == "FreqNo", "", 
                        paste0(round(Stocks/100 * length(dat$Region), 0), " Stocks"))),
                          position = position_nudge(y = 7),
                            color = "black", size = 8) +
            geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                        paste0(round(Stocks), "%"))),
                          position = position_nudge(y = -4), color = "white", size = 8) +
              labs(title = "Status Uncertain:") +
                theme_hc() +
                  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                    scale_fill_manual(values = c("#1F77B4", "grey")) +
                      coord_flip() +
                        theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                                axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                                  size = 16, face = "bold"), legend.position = "none",
                                    axis.ticks = element_blank())
        
        grid.arrange(Crplot, Cplot, Hplot, Uplot, ncol = 1)}
    
    
  else if (input$plotfilt2 == "4") {

    dat <- batch1
      zone <- dat$`What is the current status zone for this stock?`
        zone <- as.data.frame(table(zone))
          zone <- zone %>% mutate(FreqNo = length(dat$Region) - Freq)
            zone_long <- zone %>% gather(Exist, Stocks, Freq:FreqNo)
              zone_long$Stocks <- zone_long$Stocks/length(dat$Region) * 100
                zone_long$Stocks <- round(zone_long$Stocks, 1)
                  healthy <- filter(zone_long, zone == "Healthy Zone")
                    cautious <- filter(zone_long, zone == "Cautious Zone")
                      critical <- filter(zone_long, zone == "Critical Zone")
                        uncertain <- filter(zone_long, zone == "Uncertain")
      
    Hplot <- ggplot(healthy, mapping = aes(fill = Exist, x = zone, y = Stocks)) +
      geom_bar(position = position_stack(reverse = TRUE), 
                stat = "identity", color = "white", size = 1) +
        geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                    paste0(round(Stocks/100 * length(dat$Region), 0), " Stocks"))),
                      position = position_nudge(y = 7),
                        color = "black", size = 8) +
        geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                    paste0(round(Stocks), "%"))),
                      position = position_nudge(y = -4), color = "white", size = 8) +
          labs(title = "Healthy Zone:") +
            theme_hc() +
              scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                scale_fill_manual(values = c("#4bae2e", "grey")) +
                  coord_flip() +
                    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                            axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                              size = 16, face = "bold"), legend.position = "none",
                                axis.ticks = element_blank()) 
      
    Cplot <- ggplot(cautious, mapping = aes(fill = Exist, x = zone, y = Stocks)) +
      geom_bar(position = position_stack(reverse = TRUE), 
                stat = "identity", color = "white", size = 1) +
        geom_text(aes(label = ifelse(Exist == "FreqNo", "", 
                    paste0(round(Stocks/100 * length(dat$Region), 0), " Stocks"))),
                      position = position_nudge(y = 7), 
                        color = "black", size = 8) +
        geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                    paste0(round(Stocks), "%"))),
                      position = position_nudge(y = -4), color = "white", size = 8) +
          labs(title = "Cautious Zone:") +
            theme_hc() +
              scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                scale_fill_manual(values = c("#FF7F0E", "grey")) +
                  coord_flip() +
                    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                            axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                              size = 16, face = "bold"), legend.position = "none", 
                                axis.ticks = element_blank())
      
    Crplot <- ggplot(critical, mapping = aes(fill = Exist, x = zone,y = Stocks)) +
      geom_bar(position = position_stack(reverse = TRUE),
                 stat = "identity", color = "white", size = 1) +
        geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                    paste0(round(Stocks/100 * length(dat$Region), 0), " Stocks"))),
                      position = position_nudge(y = 7), 
                        color = "black", size = 8) +
        geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                    paste0(round(Stocks), "%"))),
                      position = position_nudge(y = -4), color = "white", size = 8) +
          labs(title = "Critical Zone:") +
            theme_hc() +
              scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                scale_fill_manual(values = c("#e30303", "grey")) +
                  coord_flip() +
                    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                            axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                              size = 16, face = "bold"), legend.position = "none",
                                axis.ticks = element_blank())
      
    Uplot <- ggplot(uncertain, mapping = aes(fill = Exist, x = zone, y = Stocks)) +
      geom_bar(position = position_stack(reverse = TRUE), 
                stat = "identity", color = "white", size = 1) +
        geom_text(aes(label = ifelse(Exist == "FreqNo", "", 
                    paste0(round(Stocks/100 * length(dat$Region), 0), " Stocks"))),
                      position = position_nudge(y = 7),
                        color = "black", size = 8) +
        geom_text(aes(label = ifelse(Exist == "FreqNo", "",
                    paste0(round(Stocks), "%"))),
                      position = position_nudge(y = -4), color = "white", size = 8) +
          labs(title = "Status Uncertain:") +
            theme_hc() +
              scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
                scale_fill_manual(values = c("#1F77B4", "grey")) +
                  coord_flip() +
                    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                            axis.text = element_blank(), plot.title = element_text(color = "#2C3E50",
                              size = 16, face = "bold"), legend.position = "none",
                                axis.ticks = element_blank())
      
    grid.arrange(Crplot, Cplot, Hplot, Uplot, ncol = 1)} })
  
  #Fish stocks included in the 2018 survey datatable
  output$dtable1 <- DT::renderDataTable({
    DT::datatable(dtab, options = list(pageLength = 10,
      lengthChange = FALSE, searching = FALSE,
        rowCallback = JS("function(r,d) {$(r).attr('height', '50px')}"),
          columnDefs = list(list(width = '300px', targets = "_all"))),
            rownames = FALSE,
              caption = htmltools::tags$caption("Table 1. Fish stocks included in the 2018 Sustainability 
                                                Survey across management regions, and 
                                                their current status.",
                                                  style = "color: #2C3E50"))}) 
}

###############################################################################

#Launches App
shinyApp(ui = ui, server = server)
