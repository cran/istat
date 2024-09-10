#' shinyIstat
#'
#' @description
#' A graphic interface that makes searching, downloading and filtering data sets from Istat easier. Call shinyIstat() to get started. This shinyApp was built using the same functions of istat package but they have been adapted for the shiny. You will find additional information and help inside the app.
#'
#' @usage shinyIstat()
#'
#' @return It opens the app.
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom shiny downloadHandler fileInput fluidRow icon mainPanel observeEvent
#'   radioButtons actionButton reactive reactiveVal renderPrint renderTable renderUI renderPlot req selectInput
#'   showNotification sidebarLayout sidebarPanel tabPanel tabsetPanel uiOutput updateSelectInput br checkboxInput
#'   conditionalPanel downloadButton eventReactive observe shinyApp tableOutput textInput verbatimTextOutput outputOptions div
#'   plotOutput fluidPage column tagList
#'   h1 h2 h3 h4 p tags
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyhelper helper observe_helpers
#' @importFrom shinydashboard box dashboardBody dashboardHeader dashboardPage
#'   dashboardSidebar menuItem menuSubItem sidebarMenu tabItem tabItems
#' @importFrom utils write.csv
#' @importFrom writexl write_xlsx
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_excel
#' @importFrom rsdmx readSDMX
#' @importFrom htmltools HTML
#' @importFrom magrittr %>%
#' @importFrom shinyBS bsTooltip
#' @importFrom reactable reactableOutput
#' @importFrom datamods filter_data_ui filter_data_server
#' @import datasets
#' @import ggplot2
#' @import dplyr
#' @import shinyWidgets
#' @import httr

#' @export
#'
#' @note
#' Calling a ShinyApp equals to calling a R function. For this reason, once called shinyIstat(), R will be busy processing it until the app will be closed. As a consequence, all other R functionalities can be processed only when the app is closed.
#'
#' @examples
#' if(interactive()) {
#'   shinyIstat()
#' }
shinyIstat <- function(){

  env <- .GlobalEnv
  env[["iris"]] <- datasets::iris

#  if (interactive()) {

    ################ get_dataset (from enviroment)
    get_datasets <- function() {
      objs <- ls(envir = .GlobalEnv)
      datasets <- objs[sapply(objs, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]
      return(datasets)
    }
    ################

    header <- dashboardHeader(title = "shinyIstat")

    sidebar <- dashboardSidebar(
      sidebarMenu(
        menuItem("Welcome", tabName = "welcome", icon = icon("info-circle")),
        menuItem("Available datasets", icon = icon("table"),
                 menuSubItem("I.Stat", tabName = "search_i", icon = icon("magnifying-glass")),
                 menuSubItem("IstatData", tabName = "search_e", icon = icon("magnifying-glass"))),
        menuItem("Get dataset", tabName = "get", icon = icon("download")),
        menuItem("Filter", tabName = "filter", icon=icon("filter")),
        menuItem("Plots", tabName="grafici", icon = icon("chart-column"))
      )
    )

    body <- dashboardBody(
      useShinyjs(),
      tabItems(
        tabItem(tabName = "welcome",
                fluidRow(
                  box(
                      width = 12,
                      # status = "danger",
                      solidHeader = TRUE,
                      h2("Welcome to shinyIstat!", align = "center"),
                      br(),
                      h4("How to Use This App"),
                      p(HTML("<b>ShinyIstat</b> is a tool to explore and obtain data from <b>ISTAT</b> (the <i>Italian National Institute of Statistics</i>). As of September 2024, there are 2 sources of data:
                      <b>I.Stat</b> and <b>IstatData</b>. Istat is replacing I.Stat with IstatData  platform, but I.Stat
                      can still be used as a source.
                      Searching and downloading datasets from the new platform allows you to have access to more datasets
                      (they can be found at https://esploradati.istat.it/databrowser/).

                      This app aims to provide
                      a useful tool to search, get, filter and plot those datasets within R environment. Here are the main
                      features:"),
                        tags$ul(
                          tags$li(HTML("<b>List</b> available datasets or search for datasets using keywords by selecting <b>Available datasets</b> in the sidebar. You can choose to use <i>I.Stat</i> or <i>IstatData</i> as the source.")),
                          tags$li(HTML("<b>Download</b> datasets by providing an <i>ID</i> and selecting a date range in <b>Get dataset</b>. You can choose to use <i>I.Stat</i> or <i>IstatData</i> as the source." )),
                          tags$li(HTML("<b>Filter</b> datasets by selecting <b>Filter</b>. You can upload a <i>.xlsx</i> file or select a dataset from the environment.")),
                          tags$li(HTML("<b>Visualize</b> data by selecting <b>Plots</b>. You can choose between <i>scatter plot</i>, <i>bar plot</i> and <i>pie chart</i>. Note that this function it's intended to be used with exploratory purpose only."))
                        ),
                        p("Use the menu on the left to navigate through the app.
                      Inside each panel you will find further help."),
                        p(HTML("<b>NOTES</b>:")),
                        tags$ul(
                          tags$li("For a better experience, use the app in full-screen."),
                          tags$li("This is the first version of the app. Please report any bugs, errors or suggestions to the authors."),
                          tags$li(HTML("If you prefer to work in the console of RStudio, the same functions of <i>ShinyIstat</i> can be found in the r-package <i>istat</i>."))
                        )
                      )
                  )
                )
        ),

        tabItem(tabName = "search_i",
                h2("I.Stat"),
                tabsetPanel( type = "pills",
                             tabPanel("Complete list",
                                      fluidRow(
                                        box(width = 12,
                                            selectInput("langvi", "Language:", choices =  c("Italian" = "ita", "English" = "eng"))%>%
                                              helper( icon = "question",
                                                      colour = "green",
                                                      size = "m",
                                                      type = "inline",
                                                      content = c(paste(
                                                        "<h1 style='color:navy;'>Search for available datasets: <i>I.Stat</i></h1>
                                            <hr>
                                            <p><b>Complete list:</b> simply select the language and click on '<b>Load Data</b>' to obtain the complete list of available datasets from <i>I.Stat</i>.</p>
                                            <p><b>Search datasets:</b> insert keywords separated by commas (,) and select language. You will obtain a list of datasets containing all keywords inserted and the list of the datasets that contain the single keywords. Each dataset has its <b>id</b> and <b>name</b> of the dataset. You will need the id to download the dataset.</p>
                                            <p><b>NOTE</b>: if you searched the dataset through <i>I.Stat</i> and you want to download it, you have to click on <b>Get dataset -->I.Stat</b></p>" )),
                                                      fade = TRUE
                                              ),
                                            actionButton("load_data_i", "Load", styleclass = "primary", icon = NULL),
                                            verbatimTextOutput("message_view_i"),

                                            DT::DTOutput("istat_table")
                                        )
                                      )
                             ),

                             tabPanel("Search datasets",
                                      fluidRow(
                                        box(width = 12,
                                            textInput(
                                              inputId = "keywords",
                                              label = "Insert keywords separated by comma (,):",
                                              value = "popolazione, gennaio")%>%
                                              helper( icon = "question",
                                                      colour = "green",
                                                      size = "m",
                                                      type = "inline",
                                                      content = c(paste(
                                                        "<h1 style='color:navy;'>Search for available datasets: <i>I.Stat</i></h1>
                                            <hr>
                                            <p><b>Complete list:</b> simply select the language and click on '<b>Load</b>' to obtain the complete list of available datasets from <i>I.Stat</i>.</p>
                                            <p><b>Search datasets:</b> insert keywords separated by commas (,) and select language. You will obtain a list of datasets containing all keywords inserted and the list of the datasets that contain the single keywords. Each dataset has its <b>id</b> and <b>name</b> of the dataset. You will need the id to download the dataset.</p>
                                            <p><b>NOTE</b>: if you searched the dataset through <i>I.Stat</i> and you want to download it, you have to click on <b>Get dataset -->I.Stat</b></p>" )),
                                                      fade = TRUE
                                              ),
                                            selectInput("lang", "Language:",
                                                        choices = c("Italian" = "ita", "English" = "eng"),
                                                        selected = "ita"),
                                            actionButton("search_i", "Search", styleclass = "secondary", icon = NULL),
                                            verbatimTextOutput("message_search_i"),
                                            conditionalPanel(
                                              condition = "input.search_i",
                                              div(
                                                style = "font-weight: bold; margin-bottom: 10px;",
                                                "All Keywords Table"
                                              ),
                                              DT::DTOutput("results_all"),
                                              div(
                                                style = "font-weight: bold; margin-bottom: 10px;",  # Stile per il titolo
                                                "Some Keywords Table"  # Titolo della tabella
                                              ),
                                              DT::DTOutput("results_any")
                                            )
                                        )
                                      )
                             )
                )
        ),

        tabItem(tabName = "search_e",
                h2("IstatData"),
                tabsetPanel( type = "pills",
                             tabPanel("Complete list",
                                      fluidRow(
                                        box(width = 12,
                                            selectInput("langve", "Language:", choices = c("Italian" = "ita", "English" = "eng"))%>%
                                              helper( icon = "question",
                                                      colour = "green",
                                                      size = "m",
                                                      type = "inline",
                                                      content =c(paste(
                                                        "<h1 style='color:navy;'>Search for available datasets: <i>IstatData</i></h1>
                                            <hr>
                                            <p><b>Complete list:</b> simply select the language and click on '<i>Load </i>' to obtain the complete list of available datasets from <i>IstatData</i>.</p>
                                            <p><b>Search datasets:</b> insert keywords separated by commas (,) and select language. You will obtain a list of datasets containing all keywords inserted and the list of the datasets that contain the single keywords. Each dataset has its <b>agencyId</b>, <b>id</b>, <b>version</b> and <b>name</b> of the dataset. You will need these information to download the dataset.</p>
                                            <p><b>*NOTE</b>: if you searched the dataset through <i>IstatData</i> and you want to download it, you have to click on <b><i>Get dataset -->IstatData</i></b></p>" )),
                                                      fade = TRUE
                                              ),
                                            actionButton("load_data_e", "Load", styleclass = "primary", icon = NULL),
                                            verbatimTextOutput("message_view_e"),
                                            DT::DTOutput("esploradati_table")
                                        )
                                      )
                             ),
                             tabPanel("Search datasets",
                                      fluidRow(
                                        box( width = 12,
                                             textInput("keywords_e",
                                                       label = "Insert keywords separated by comma (,):",
                                                       value = "popolazione,gennaio")%>%
                                               helper( icon = "question",
                                                       colour = "green",
                                                       size = "m",
                                                       type = "inline",
                                                       content =c(paste(
                                                         "<h1 style='color:navy;'>Search for available datasets: <i>IstatData</i></h1>
                                              <hr>
                                              <p><b>Complete list:</b> simply select the language and click on '<i>Load Data</i>' to obtain the complete list of available datasets from <i>IstatData</i>.</p>
                                              <p><b>Search datasets:</b> insert keywords separated by commas (,) and select language. You will obtain a list of datasets containing all keywords inserted and the list of the datasets that contain the single keywords. Each dataset has its <b>agencyId</b>, <b>id</b>, <b>version</b> and <b>name</b> of the dataset. You will need these information to download the dataset.</p>
                                              <p><b>*NOTE</b>: if you searched the dataset through <i>IstatData</i> and you want to download it, you have to click on <b><i>Get dataset -->IstatData</i></b></p>" )),
                                                       fade = TRUE
                                               ),
                                             selectInput("lang", "Language:",
                                                         choices = c("Italian" = "ita", "English" = "eng"),
                                                         selected = "ita"),
                                             actionButton("search_e", "Search", styleclass = "secondary", icon = NULL),
                                             verbatimTextOutput("message_search_e"),
                                             conditionalPanel(
                                               condition = "input.search_e",
                                               div(
                                                 style = "font-weight: bold; margin-bottom: 10px;",
                                                 "All Keywords Table"
                                               ),
                                               DT::DTOutput("allKeywordsTable"),
                                               div(
                                                 style = "font-weight: bold; margin-bottom: 10px;",
                                                 "Some Keywords Table"
                                               ),
                                               DT::DTOutput("someKeywordsTable")
                                             )
                                        )
                                      )
                             )
                )
        ),

        tabItem(tabName = "get",
                h2("Download datasets"),
                tabsetPanel(type = "pills",
                            tabPanel("I.Stat",
                                     fluidRow(
                                       box(
                                         textInput("id_dataset", "Id dataset:", value = "12_60") %>%
                                           helper(icon = "question",
                                                  colour = "green",
                                                  size = "m",
                                                  type = "inline",
                                                  content = c(paste(
                                                    "<h1 style=\"color:navy;\"> Download datasets: <i>I.Stat</i> </h1>
                                             <p><b>Id dataset</b>: insert the id of the wanted dataset. You can find the id through <i> Available datasets -->I.stat -->Complete list</i> or <i>Search datasets</i> to find a dataset by keyword(s).</p>
                                             <p><b>Language</b>: select the language of the labels. You can choose between <i>Italian</i>, <i>English</i> or <i>both</i>.</p>
                                             <p>With <b><i>Time period</i></b> you can choose between:</p>
                                             <ul>
                                             <li><b>Complete dataset:</b> if selected, the complete dataset, without time limits, is retrieved.</li>
                                             <li><b>Recent data:</b> if selected, data from last 10 years (with respect to the system date) is retrieved.</li>
                                             <li><b>Select period</b>: if selected, you can choose a time period for which you want the data. If you want data from one year, write the same year on both <i>Start period</i> and <i>End period</i>.</li>
                                             </ul>
                                             <p><b>Then</b>, click on <b><i>Show dataset</i></b> to visualize the dataset (note that <i>Show dataset</i> does not save the dataset). You can choose to save it to your environment or to save it as .csv or .xlsx.</p>
                                             <p><b>*NOTE:</b> In this first version, when data frequency is not <i>Annual (A),</i> selecting the period may not work. In future versions, we will fix this issue.</p>"
                                                  )),
                                                  fade = TRUE
                                           ),
                                         selectInput("lang", "Language:", choices = c("ita", "eng", "both")),


                                         radioButtons("data_option_i", "Time period:",
                                                      choices = list('Complete dataset' = 'complete_i',
                                                                     'Recent data (last 10 years)' = 'recent_i',
                                                                     'Select period' = 'range_data_i'),
                                                      selected = "complete_i"),


                                         conditionalPanel(
                                           condition = "input.data_option_i == 'range_data_i'",
                                           textInput("start_period", "Start period", value = NULL),
                                           textInput("end_period", "End period", value = NULL)
                                         ),

                                         actionButton("show_i", "Show dataset", styleclass = "secondary", icon = NULL),
                                         br(),

                                         conditionalPanel(
                                           condition = "input.show_i",
                                           br(),
                                           textInput("save_name_i", "Save dataset as (name in environment):", value = "dataset_i"),
                                           actionButton("save_i", "Save to environment", styleclass="secondary", icon = icon("parachute-box"), class="download-btn1"),
                                           downloadButton("download_i.csv", "Download dataset .csv", class="download-btn1"),
                                           downloadButton("download_i.xlsx", "Download dataset .xlsx", class="download-btn1"),
                                           br(),
                                           verbatimTextOutput("variables_i")
                                         )),

                                       DT::DTOutput("dati_i")
                                     )
                            ),
                            tabPanel("IstatData",
                                     fluidRow(
                                       box(
                                         textInput("agencyId", "Id agency:", value = "IT1") %>%
                                           helper(icon = "question",
                                                  colour = "green",
                                                  size = "m",
                                                  type = "inline",
                                                  content = c(paste(
                                                    "<h1 style=\"color:navy;\">Download datasets: <i>IstatData</i></h1>
                                             <hr>
                                             <p><b>agencyID</b>, <b>id</b> and <b>version</b>: they can be found through <i>Available datasets -->IstatData -->Complete list</i> or <i>Search datasets</i> to find a dataset by keyword(s).</p>
                                             <p>With <b><i>Time period</i></b> you can choose between:</p>
                                             <ul>
                                             <li><b>Complete dataset:</b> if selected, the complete dataset, without time limits, is retrieved.</li>
                                             <li><b>Recent data:</b> if selected, data from last 10 years (with respect to the system date) is retrieved.</li>
                                             <li><b>Select period</b>: if selected, you can choose a time period for which you want the data. If you want data from one year, write the same year on both <i>Start period</i> and <i>End period</i>.</li>
                                             </ul>
                                             <p><b>Then</b>, click on <i>Show dataset</i> to visualize the dataset (note that <i>Show dataset</i> does not save the dataset). You can choose to save it to your environment or to save it as .csv or .xlsx.</p>
                                             <p><b>*NOTE</b> In this first version, when data frequency is not <i>Annual (A)</i>, selecting the period may not work. In future versions, we will fix this issue.</p>"
                                                  )),
                                                  fade = TRUE
                                           ),
                                         textInput("dataset_id", "Id dataset:", value = "12_60_DF_DCCV_CONSACQUA_2"),
                                         textInput("version", "Version:", value = "1.0"),

                                         # Aggiunta di un'opzione "Complete dataset"
                                         radioButtons("data_option_e", "Data option:",
                                                      choices = list('Complete dataset' = 'complete_e',
                                                                     'Recent data (last 10 years)' = 'recent_e',
                                                                     'Select period' = 'range_data_e'),
                                                      selected = "complete_e"),


                                         conditionalPanel(
                                           condition = "input.data_option_e == 'range_data_e'",
                                           textInput("start", "Start period", value = NULL),
                                           textInput("end", "End period", value = NULL)
                                         ),

                                         actionButton("show_e", "Show dataset", styleclass = "secondary", icon = NULL),
                                         br(),

                                         conditionalPanel(
                                           condition = "input.show_e",
                                           br(),
                                           textInput("save_name_e", "Save dataset as (name in environment):", value = "dataset_e"),
                                           actionButton("save_e", "Save to environment", styleclass='secondary', icon=icon('parachute-box'), class="download-btn1"),
                                           downloadButton("download_e.csv", "Download dataset .csv", class="download-btn1"),
                                           downloadButton("download_e.xlsx", "Download dataset .xlsx", class="download-btn1"),
                                           br(),
                                           verbatimTextOutput("variables_e")
                                         )),
                                       DT::DTOutput("dati_e")
                                     )
                            )
                )
        ),

        tabItem(tabName = "filter",
                h2("Filter your data"),
                fluidPage(
                  box(width = 12,
                      fluidRow(
                        column(6,

                               radioButtons("dataSource", "Choose the data source:",
                                            choices = list("R Environment" = "env",
                                                           "Load from computer" = "file"),
                                            selected = "env")%>%
                                 helper( icon = "question",
                                         colour = "green",
                                         size = "m",
                                         type = "inline",
                                         content = c(paste(
                                           "<h1 style='color:navy;'>Filter your data</h1>
<p>Click on <i>R environment</i> to select a dataframe from your environment or click on <i>Load from your computer -->browse</i> to select a .xlsx file from your device. If you downloaded the dataset in your environment during this session of <i>ShinyIstat</i>, <span style='color:red;'><b>you should close the app and open it again, in order to select it.</b></span></p>
<p>Once you selected the dataframe you want to filter, click on <i>Load</i>: on the left you will find the columns of the dataset, on the right you will find the dataset. You can then choose the column values for which you want to filter the data, and for each column you can choose for which values, both through the drop down menu. <b>Moreover</b>, a <i>progress bar</i> is shown on top of the dataframe to keep you updated on the number of rows as you apply filters, and at the bottom of it, you will find the corresponding code.</p>
<p><b>Then</b>, by clicking on <i class='fas fa-download'></i> you can save your dataset to your environment (you will be asked to name it) or to save it as .csv or .xlsx (note that <i>Load</i> does not save the dataset). </p>
<p><b>*NOTE:</b> <i>Filter</i> works with .xlsx files or dataframes, not necessarily ISTAT ones.</p> " )),
                                         fade = TRUE
                                 )),

                        column(6,
                               conditionalPanel(
                                 condition = "input.dataSource == 'env'",
                                 selectInput("datasetInput", "Choose a dataset from enviroment:",
                                             choices = get_datasets()),
                                 bsTooltip("datasetInput", "Try filter with iris data", "right", options = list(container = "body"))
                               ),

                               conditionalPanel(
                                 condition = "input.dataSource == 'file'",
                                 fileInput("file1", "Load excel file (.xlsx or .xls)",
                                           multiple = FALSE,
                                           accept = c(".xlsx", ".xls")),
                                 tags$hr(),
                                 checkboxInput("header", "Header", TRUE)
                               ),
                               div(class = "btn-container",
                                   actionButton("load_btn", "Load", class = "btn load-btn"),

                                   conditionalPanel(
                                     condition = "input.load_btn",
                                     dropdownButton(
                                       circle = FALSE,
                                       right = TRUE,
                                       tooltip = tooltipOptions(placement = "bottom", title = "Saving options"),
                                       icon = icon("download"),
                                       dropdown = tagList(
                                         actionButton("save_f", "Save to environment", icon = icon("parachute-box"), class = "download-btn"),
                                         br(),
                                         downloadButton("download_f.csv", "Download dataset .csv", class = "download-btn"),
                                         downloadButton("download_f.xlsx", "Download dataset .xlsx", class = "download-btn")
                                       )
                                       # ,
                                       # class = "btn download-btn"
                                     )
                                   )
                               ),
                               br(),
                               fluidRow(
                                 conditionalPanel(
                                   condition = "input.save_f",
                                   column(9,
                                          textInput("save_name_f", NULL, value = "Choose dataset name")),
                                   column(3,
                                          actionButton("save_f_env", NULL, icon = icon("parachute-box")),
                                          bsTooltip("save_f_env", "Save", "bottom", options = list(container = "body")))
                                 )
                               )


                        )
                      )),


                  conditionalPanel(
                    condition = "input.load_btn",

                    fluidRow(
                      box( width = 12,
                           column(width = 4,

                                  filter_data_ui("filtering")
                           ),
                           column(
                             width = 8,
                             progressBar(
                               id = "pbar", value = 100,
                               total = 100, display_pct = TRUE
                             ),
                             reactable::reactableOutput(outputId = "table"),
                             tags$b("Code dplyr:"),
                             verbatimTextOutput(outputId = "code_dplyr")
                           )
                      )
                    )
                  )
                )

        ),

        tabItem(tabName = "grafici",
                h2("Plot your data"),

                sidebarLayout(
                  sidebarPanel(
                    radioButtons("dataSourceg", "Choose the data source:",
                                 choices = list("R Environment" = "envg",
                                                "Load from computer" = "fileg"),
                                 selected = "envg")%>%

                      helper( icon = "question",
                              colour = "green",
                              size = "m",
                              type = "inline",
                              content = c(paste(
                                "<h1 style='color:navy;'>Plot your data </h1>
                                 <p>The drop down menu of <i>Select dataset</i> allows you to select a dataframe from your environment. If you downloaded the dataset in your environment during this session of <i>ShinyIstat</i>, <span style='color:red;'><b>you should close the app and open it again, in order to select it.</b></span></p>
                                 <p>Once the file is selected, you have to choose the column to be the <b>variable X</b> (x axis) and the column to be the <b>variable Y</b> (y axis).</p>
                                 <p><i>Optional:</i> select a <b>grouping variable</b>. Depending on your data, you may want to group your data by a certain column. You can select it through the drop down menu.</p>
                                 <p>Click on the boxes to visualize, respectively, the <b>scatter plot</b>, the <b>bar plot</b> or the <b>pie chart</b>. If you select the <i>pie chart</i>, you have to choose the variable (column of the dataset) for which you want to visualize it.</p>
                                 <p><b>*NOTE:</b>
                                 <i>Pie chart</i> works only with datasets downloaded from ISTAT, since it fills the pie with values from the column <i>obsValue</i> (a column with values that you can find in every ISTAT dataset).</p> " )),
                              fade = TRUE
                      ),

                    conditionalPanel(
                      condition = "input.dataSourceg == 'envg'",
                      selectInput("datasetInputg", "Choose a dataset from enviroment:",
                                  choices = get_datasets()),
                      bsTooltip("datasetInputg", "Try plot with iris data", "right", options = list(container = "body"))
                    ),

                    conditionalPanel(
                      condition = "input.dataSourceg == 'fileg'",
                      fileInput("file1g", "Load excel file (.xlsx or .xls)",
                                multiple = FALSE,
                                accept = c(".xlsx", ".xls")),
                      tags$hr(),
                      checkboxInput("header", "Header", TRUE)
                    ),
                    selectInput("xcol", "X variable", choices = NULL),
                    selectInput("ycol", "Y variable", choices = NULL),
                    selectInput("groupcol", "Grouping variable (optional)", choices = c("No grouping")),

                    h4("Plot type:"),
                    actionButton("scatter", label = NULL, icon = icon("braille"), style = "padding: 10px; width: 50px; height: 50px;"),
                    actionButton("bar", label = NULL, icon = icon("chart-column"), style = "padding: 10px; width: 50px; height: 50px;"),
                    actionButton("pie", label = NULL, icon = icon("chart-pie"), style = "padding: 10px; width: 50px; height: 50px;"),

                    conditionalPanel(
                      condition = "output.showPieInput",
                      selectInput("piecol", "Variable for pie chart", choices = NULL)
                    )
                  ),
                  mainPanel(
                    plotOutput("plot")
                  )
                )
        )
      )
    )

    ui <- dashboardPage(
      header,
      sidebar,
      body,
      tags$head(
        tags$style(HTML("
                body{
                  font-family:'Avenir', sans-serif;
                }
                h1, h2, h3, h4, h5, h6 {
                  font-family: 'Avenir', sans-serif;
                }

                .skin-blue .main-header .navbar {
                  background-color: #00295a;
                }


                .skin-blue .main-header .logo {
                    background-color: #00295a;
                }

               .nav-pills>li.active>a, .nav-pills>li.active>a:focus, .nav-pills>li.active>a:hover {
                     border-top-color: #808080;
                }

                .nav-pills>li.active>a, .nav-pills>li.active>a:focus, .nav-pills>li.active>a:hover {
                    color: #fff;
                    background-color: #808080;
                }


                .btn-container {
                  display: flex;
                  width: 100%;
                }

                .btn-container .btn {
                  flex: 1;
                  margin: 0;
                }
                .btn-container .download-btn {
                  width: 100%;
                  border-radius: 0;
                  display: flex;
                  align-items: center;
                  justify-content: center;
                  margin-left: 5px;
                }
                .btn-container .load-btn {
                  margin-right: 5px;
                }
                /* Stili per il menu a discesa */
                .dropdown-menu {
                  width: 200px;
                  box-sizing: border-box;
                }
                .dropdown-menu .action-button,
                .dropdown-menu .btn-download,
                .dropdown-menu .downloadButton {
                  width: 100%;

                }
                .dropdown-menu .btn-download {
                  white-space: nowrap;
                }

                .download-btn1 {
                  width: 70%;
                  border-radius: 0;
                  display: flex;
                  align-items: center;
                  justify-content: center;
                  margin-left: 5px;
                }

                "
        ))
      )
    )

    server <- function(input, output, session) {

      ################# list i.stat
      list_i_stat <- function(langvi = "ita") {

        istat.flvi <- rsdmx::readSDMX(providerId = "ISTAT", resource = "dataflow")
        istat.flvi <- as.data.frame(istat.flvi)

        #istat.flvi <- istat.flvi %>%
        #  filter(!id %in% "14_121")

        ID <- istat.flvi$id

        if (input$langvi == "ita") {
          if ("Name.it" %in% colnames(istat.flvi)) {
            Name <- istat.flvi$Name.it
          } else {
            stop("La colonna Name.it non esiste nel dataframe.")
          }
        } else if (input$langvi == "eng") {
          if ("Name.en" %in% colnames(istat.flvi)) {
            Name <- istat.flvi$Name.en
          } else {
            stop("La colonna Name.en non esiste nel dataframe.")
          }
        } else {
          stop("Lingua non supportata. Utilizza 'ita' per italiano o 'eng' per inglese.")
        }

        Data_i <- data.frame(ID, Name)

        return(Data_i)
      }

      ################# list istatdata
      list_istatdata <- function(langve = "ita") {
        link <- "https://esploradati.istat.it/SDMXWS/rest/dataflow"
        istat.flve <- rsdmx::readSDMX(link)
        istat.flve <- as.data.frame(istat.flve)

        # removal of unavailable datasets
        istat.flve <- istat.flve %>%
          filter(!id %in% c("152"
                            #                    "111_263_DF_DCSC_FIDIMPRMAN_11"
                            #                   ,"111_263_DF_DCSC_FIDIMPRMAN_12"
                            #                   ,"111_263_DF_DCSC_FIDIMPRMAN_13"
                            #                   ,"111_263_DF_DCSC_FIDIMPRMAN_14"
                            #                   ,"111_263_DF_DCSC_FIDIMPRMAN_15"
                            #                   ,"111_263_DF_DCSC_FIDIMPRMAN_16"
                            #                   ,"111_263_DF_DCSC_FIDIMPRMAN_9"
                            #                   ,"111_40_DF_DCSC_FIDIMPRCOSTR_4"
                            #                   ,"111_40_DF_DCSC_FIDIMPRCOSTR_5"
                            #                   ,"111_40_DF_DCSC_FIDIMPRCOSTR_6"
                            #                   ,"117_262_DF_DCSC_FIDIMPRSERV_5"
                            #                   ,"117_262_DF_DCSC_FIDIMPRSERV_6"
                            #                   ,"117_262_DF_DCSC_FIDIMPRSERV_7"
                            #                   ,"117_266_DF_DCSC_FIDCOMM_3"
                            #                   ,"117_266_DF_DCSC_FIDCOMM_4"
                            #                   ,"14_121_DF_DCCV_TRASPUB_1"
                            #                   ,"14_166_DF_DCCV_INTBONRUM_1"
                            #                   ,"14_167_DF_DCCV_MONINQACS_1"
                            #                   ,"172_926"
                            #                   ,"172_926_DF_DCCV_COMPL1_1"
                            #                   ,"172_926_DF_DCCV_COMPL1_12"
                            #                   ,"172_926_DF_DCCV_COMPL1_13"
                            #                   ,"172_926_DF_DCCV_COMPL1_14"
                            #                   ,"172_926_DF_DCCV_COMPL1_15"
                            #                   ,"172_926_DF_DCCV_COMPL1_16"
                            #                   ,"172_926_DF_DCCV_COMPL1_17"
                            #                   ,"172_926_DF_DCCV_COMPL1_18"
                            #                   ,"172_926_DF_DCCV_COMPL1_19"
                            #                   ,"172_926_DF_DCCV_COMPL1_2"
                            #                   ,"172_926_DF_DCCV_COMPL1_20"
                            #                   ,"172_926_DF_DCCV_COMPL1_21"
                            #                   ,"172_926_DF_DCCV_COMPL1_22"
                            #                   ,"172_926_DF_DCCV_COMPL1_23"
                            #                   ,"172_926_DF_DCCV_COMPL1_24"
                            #                   ,"172_926_DF_DCCV_COMPL1_25"
                            #                   ,"172_926_DF_DCCV_COMPL1_26"
                            #                   ,"172_926_DF_DCCV_COMPL1_3"
                            #                   ,"172_926_DF_DCCV_COMPL1_4"
                            #                   ,"172_926_DF_DCCV_COMPL1_5"
                            #                   ,"25_74_DF_DCIS_NATI1_10"
                            #                   ,"25_74_DF_DCIS_NATI1_11"
                            #                   ,"25_74_DF_DCIS_NATI1_12"
                            #                   ,"25_74_DF_DCIS_NATI1_13"
                            #                   ,"25_74_DF_DCIS_NATI1_14"
                            #                   ,"25_74_DF_DCIS_NATI1_15"
                            #                   ,"25_74_DF_DCIS_NATI1_16"
                            #                   ,"25_74_DF_DCIS_NATI1_17"
                            #                   ,"25_74_DF_DCIS_NATI1_7"
                            #                   ,"25_74_DF_DCIS_NATI1_8"
                            #                   ,"25_74_DF_DCIS_NATI1_9"
                            #                   ,"60_195_DF_DCIS_MUSVIS_5"
                            #                   ,"610_119_DF_DCCV_TRASPRIV_1"
                            #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_4"
                            #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_5"
                            #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_6"
                            #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_7"
                            #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_8"
                            #                   ,"73_1071_DF_DCCV_IMPUTATI_A_10"
                            #                   ,"73_1071_DF_DCCV_IMPUTATI_A_11"
                            #                   ,"73_1071_DF_DCCV_IMPUTATI_A_12"
                            #                   ,"73_1071_DF_DCCV_IMPUTATI_A_13"
                            #                   ,"73_1072_DF_DCCV_IMPUTATI_M_10"
                            #                   ,"73_1072_DF_DCCV_IMPUTATI_M_11"
                            #                   ,"73_1072_DF_DCCV_IMPUTATI_M_12"
                            #                   ,"73_1072_DF_DCCV_IMPUTATI_M_13"
                            #                   ,"73_230_DF_DCCV_AUTVITTPS_5"
                            #                   ,"73_230_DF_DCCV_AUTVITTPS_6"
                            #                   ,"73_230_DF_DCCV_AUTVITTPS_7"
                            #                   ,"73_230_DF_DCCV_AUTVITTPS_8"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_13"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_14"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_15"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_16"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_17"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_18"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_19"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_20"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_21"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_22"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_14"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_15"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_16"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_17"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_18"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_19"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_20"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_21"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_22"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_23"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_24"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_14"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_15"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_16"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_17"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_18"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_19"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_20"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_21"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_22"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_15"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_16"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_17"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_18"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_19"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_20"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_21"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_22"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_23"
                            #                   ,"83_63_DF_DCCV_AVQ_PERSONE_125"
                            #                   ,"92_504_NOTE_TIME"
                            ,"22_289_DF_DCIS_POPRES1_3"
                            ,"22_315_DF_DCIS_POPORESBIL1_10"
                            ,"22_315_DF_DCIS_POPORESBIL1_11"
                            ,"22_315_DF_DCIS_POPORESBIL1_12"
                            ,"22_315_DF_DCIS_POPORESBIL1_13"
                            ,"22_315_DF_DCIS_POPORESBIL1_14"
                            ,"22_315_DF_DCIS_POPORESBIL1_15"
                            ,"22_315_DF_DCIS_POPORESBIL1_16"
                            ,"22_315_DF_DCIS_POPORESBIL1_17"
                            ,"22_315_DF_DCIS_POPORESBIL1_18"
                            ,"22_315_DF_DCIS_POPORESBIL1_19"
                            ,"22_315_DF_DCIS_POPORESBIL1_20"
                            ,"22_315_DF_DCIS_POPORESBIL1_21"
                            ,"22_315_DF_DCIS_POPORESBIL1_22"
                            ,"22_315_DF_DCIS_POPORESBIL1_23"
                            ,"22_315_DF_DCIS_POPORESBIL1_3"
                            ,"22_315_DF_DCIS_POPORESBIL1_4"
                            ,"22_315_DF_DCIS_POPORESBIL1_5"
                            ,"22_315_DF_DCIS_POPORESBIL1_6"
                            ,"22_315_DF_DCIS_POPORESBIL1_7"
                            ,"22_315_DF_DCIS_POPORESBIL1_8"
                            ,"22_315_DF_DCIS_POPORESBIL1_9"
                            ,"DF_BULK_DCCN_FPA"
                            ,"DF_BULK_DCSC_CAPACOFTUR"
                            ,"DF_BULK_DCSC_OCCUPCOLLE"
                            ,"DF_BULK_DCSC_TURISAREA"
                            ,"DF_BULK_DCSP_FOI1B2010_TB4"
                            ,"DF_BULK_DCSP_FOI1B2015_NUTS3"
                            ,"DF_BULK_DCSP_FOI1B2015_TB4"
                            ,"DF_BULK_DCSP_NIC1B2015_NUTS1"
                            ,"DF_BULK_DCSP_NIC1B2015_NUTS2"
                            ,"DF_BULK_DCSP_NIC1B2015_NUTS3"
                            ,"DF_BULK_DCSP_NIC1B2015_RECONNIC"
                            ,"DF_BULK_DCSP_NIC1B2015_SPLICECO"
                            ,"DF_BULK_DCSP_NIC1B2015_SPLICI"
                            ,"DF_BULK_DCSP_NIC1B2015_TB2"
                            ,"DF_BULK_DCSP_NIC1B2015_TB3"
                            ,"DF_BULK_DCSP_NICDUEB2010_CPI"
                            ,"DF_BULK_DCSP_NICDUEB2010_TB2"
                            ,"DF_BULK_DCSP_NICDUEB2010_TB3"
                            ,"DF_BULK_DCSP_NICUNOBB2010_CPI"
                            ,"DF_BULK_DCSP_NICUNOBB2010_NIC"
                            ,"DF_BULK_DCSP_NICUNOBB2010_TB2"
                            ,"DF_BULK_DCSP_NICUNOBB2010_TB3"
                            ,"DF_BULK_DIVORCESSE"
                            ,"DF_BULK_ENTERPRISE"
                            ,"DF_BULK_FARMSTRUCT"
                            ,"DF_BULK_FISHERYBRO"
                            ,"DF_BULK_FLOWERSBRO"
                            ,"DF_BULK_FORESTRYBR"
                            ,"DF_BULK_HIGHSCHOOL"
                            ,"DF_BULK_HOURLYWAGE"
                            ,"DF_BULK_HUNTINGBRO"
                            ,"DF_BULK_LOCALUNITS"
                            ,"DF_BULK_PERMANENTC"
                            ,"DF_BULK_POPOLAZION"
                            ,"DF_BULK_PREVIOUSBA"
                            ,"DF_BULK_PROCEEDING"
                            ,"DF_BULK_PROTECTION"
                            ,"DF_BULK_PROTESTSSE"
                            ,"DF_BULK_PUBLISHEDB"
                            ,"DF_BULK_REPORTSOFP"
                            ,"DF_BULK_SCHOOLSSER"
                            ,"DF_BULK_SEEDSBROKE"
                            ,"DF_BULK_SEPARATION"
                            ,"DF_BULK_STATEARCHI"
                            ,"DF_BULK_STATEOWNED"
                            ,"DF_BULK_STIMEPERGR"
                            ,"DF_BULK_SUICIDESAN"
                            ,"DF_BULK_SUICIDI"
                            ,"DF_BULK_SURVEYCALE"
                            ,"DF_BULK_UNIVERSITY"
                            ,"DF_BULK_UNIVERSITY_DEGREE"
                            ,"DF_DCAT_CENSAGRIC2020_AGE_MAR"
                            ,"DF_DIFF_REPORT"
                            , "DF_TEST_DCIS_ABORTISPONTL"))

        agencyId <- istat.flve$agencyID
        ID <- istat.flve$id
        version <- istat.flve$version

        if (langve == "ita") {
          if ("Name.it" %in% colnames(istat.flve)) {
            Name <- istat.flve$Name.it
          } else {
            stop("La colonna Name.it non esiste nel dataframe.")
          }
        } else if (langve == "eng") {
          if ("Name.en" %in% colnames(istat.flve)) {
            Name <- istat.flve$Name.en
          } else {
            stop("La colonna Name.en non esiste nel dataframe.")
          }
        } else {
          stop("Wrong language parameter. Select 'ita' for Italian or 'eng' for English.")
        }

        Data_e <- data.frame(agencyId, ID, version, Name)

        return(Data_e)
      }

      ################# search i.stat
      search_i_stat <- function(keywords, lang = "ita") {

        istat.fl <- rsdmx::readSDMX(providerId = "ISTAT", resource = "dataflow")
        istat.fl <- as.data.frame(istat.fl)

        #istat.flvi <- istat.flvi %>%
        #  filter(!id %in% "14_121")

        if (lang == "ita") {
          name_column <- "Name.it"
        } else if (lang == "eng") {
          name_column <- "Name.en"
        } else {
          stop("Wrong language parameter. Use 'ita' for Italian and 'eng' for English.")
        }

        search_keyword <- function(keyword) {
          grepl(keyword, istat.fl[[name_column]], ignore.case = TRUE)
        }

        if (length(keywords) == 1) {

          matches_any <- search_keyword(keywords)
          id_any <- istat.fl$id[matches_any]
          name_any <- istat.fl[[name_column]][matches_any]
          match_any <- cbind(id_any, name_any)

          if (nrow(match_any) == 0 && lang == "ita") {
            message("Non ci sono dataset contenenti la parola chiave inserita.")
          } else if (nrow(match_any) == 0 && lang == "eng") {
            message("There are no dataset including that keyword.")
          }
          return(list(all_matches = match_any, any_matches = NULL))

        } else {

          index_list <- lapply(keywords, function(k) grep(k, istat.fl[[name_column]], ignore.case = TRUE))
          index_all <- Reduce(intersect, index_list)
          id_all <- istat.fl$id[index_all]
          name_all <- istat.fl[[name_column]][index_all]
          match_all <- cbind(id_all, name_all)

          matches_any <- sapply(keywords, search_keyword)
          matches_any <- apply(matches_any, 1, any)
          id_any <- istat.fl$id[matches_any]
          name_any <- istat.fl[[name_column]][matches_any]
          match_any <- cbind(id_any, name_any)

          if (nrow(match_all) == 0 && lang == "ita") {
            message("Non ci sono dataset contenenti la parola chiave inserita.")
          } else if (nrow(match_all) == 0 && lang == "eng") {
            message("There are no datasets including all keywords")
          }
          return(list(all_matches = match_all, any_matches = match_any))
        }
      }

      ################# search istatdata
      search_istatdata <- function(keywords, lang = "ita") {

        link <- "https://esploradati.istat.it/SDMXWS/rest/dataflow"
        istat.fl <- rsdmx::readSDMX(link)
        istat.fl <- as.data.frame(istat.fl)

        # removal of unavailable datasets
        istat.fl <- istat.fl %>%
          filter(!id %in% c("152"
                            #                    "111_263_DF_DCSC_FIDIMPRMAN_11"
                            #                   ,"111_263_DF_DCSC_FIDIMPRMAN_12"
                            #                   ,"111_263_DF_DCSC_FIDIMPRMAN_13"
                            #                   ,"111_263_DF_DCSC_FIDIMPRMAN_14"
                            #                   ,"111_263_DF_DCSC_FIDIMPRMAN_15"
                            #                   ,"111_263_DF_DCSC_FIDIMPRMAN_16"
                            #                   ,"111_263_DF_DCSC_FIDIMPRMAN_9"
                            #                   ,"111_40_DF_DCSC_FIDIMPRCOSTR_4"
                            #                   ,"111_40_DF_DCSC_FIDIMPRCOSTR_5"
                            #                   ,"111_40_DF_DCSC_FIDIMPRCOSTR_6"
                            #                   ,"117_262_DF_DCSC_FIDIMPRSERV_5"
                            #                   ,"117_262_DF_DCSC_FIDIMPRSERV_6"
                            #                   ,"117_262_DF_DCSC_FIDIMPRSERV_7"
                            #                   ,"117_266_DF_DCSC_FIDCOMM_3"
                            #                   ,"117_266_DF_DCSC_FIDCOMM_4"
                            #                   ,"14_121_DF_DCCV_TRASPUB_1"
                            #                   ,"14_166_DF_DCCV_INTBONRUM_1"
                            #                   ,"14_167_DF_DCCV_MONINQACS_1"
                            #                   ,"172_926"
                            #                   ,"172_926_DF_DCCV_COMPL1_1"
                            #                   ,"172_926_DF_DCCV_COMPL1_12"
                            #                   ,"172_926_DF_DCCV_COMPL1_13"
                            #                   ,"172_926_DF_DCCV_COMPL1_14"
                            #                   ,"172_926_DF_DCCV_COMPL1_15"
                            #                   ,"172_926_DF_DCCV_COMPL1_16"
                            #                   ,"172_926_DF_DCCV_COMPL1_17"
                            #                   ,"172_926_DF_DCCV_COMPL1_18"
                            #                   ,"172_926_DF_DCCV_COMPL1_19"
                            #                   ,"172_926_DF_DCCV_COMPL1_2"
                            #                   ,"172_926_DF_DCCV_COMPL1_20"
                            #                   ,"172_926_DF_DCCV_COMPL1_21"
                            #                   ,"172_926_DF_DCCV_COMPL1_22"
                            #                   ,"172_926_DF_DCCV_COMPL1_23"
                            #                   ,"172_926_DF_DCCV_COMPL1_24"
                            #                   ,"172_926_DF_DCCV_COMPL1_25"
                            #                   ,"172_926_DF_DCCV_COMPL1_26"
                            #                   ,"172_926_DF_DCCV_COMPL1_3"
                            #                   ,"172_926_DF_DCCV_COMPL1_4"
                            #                   ,"172_926_DF_DCCV_COMPL1_5"
                            #                   ,"25_74_DF_DCIS_NATI1_10"
                            #                   ,"25_74_DF_DCIS_NATI1_11"
                            #                   ,"25_74_DF_DCIS_NATI1_12"
                            #                   ,"25_74_DF_DCIS_NATI1_13"
                            #                   ,"25_74_DF_DCIS_NATI1_14"
                            #                   ,"25_74_DF_DCIS_NATI1_15"
                            #                   ,"25_74_DF_DCIS_NATI1_16"
                            #                   ,"25_74_DF_DCIS_NATI1_17"
                            #                   ,"25_74_DF_DCIS_NATI1_7"
                            #                   ,"25_74_DF_DCIS_NATI1_8"
                            #                   ,"25_74_DF_DCIS_NATI1_9"
                            #                   ,"60_195_DF_DCIS_MUSVIS_5"
                            #                   ,"610_119_DF_DCCV_TRASPRIV_1"
                            #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_4"
                            #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_5"
                            #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_6"
                            #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_7"
                            #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_8"
                            #                   ,"73_1071_DF_DCCV_IMPUTATI_A_10"
                            #                   ,"73_1071_DF_DCCV_IMPUTATI_A_11"
                            #                   ,"73_1071_DF_DCCV_IMPUTATI_A_12"
                            #                   ,"73_1071_DF_DCCV_IMPUTATI_A_13"
                            #                   ,"73_1072_DF_DCCV_IMPUTATI_M_10"
                            #                   ,"73_1072_DF_DCCV_IMPUTATI_M_11"
                            #                   ,"73_1072_DF_DCCV_IMPUTATI_M_12"
                            #                   ,"73_1072_DF_DCCV_IMPUTATI_M_13"
                            #                   ,"73_230_DF_DCCV_AUTVITTPS_5"
                            #                   ,"73_230_DF_DCCV_AUTVITTPS_6"
                            #                   ,"73_230_DF_DCCV_AUTVITTPS_7"
                            #                   ,"73_230_DF_DCCV_AUTVITTPS_8"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_13"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_14"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_15"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_16"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_17"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_18"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_19"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_20"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_21"
                            #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_22"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_14"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_15"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_16"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_17"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_18"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_19"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_20"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_21"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_22"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_23"
                            #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_24"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_14"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_15"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_16"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_17"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_18"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_19"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_20"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_21"
                            #                   ,"73_58_DF_DCCV_CONDCRIM1_22"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_15"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_16"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_17"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_18"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_19"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_20"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_21"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_22"
                            #                   ,"73_59_DF_DCCV_CONDGEO1_23"
                            #                   ,"83_63_DF_DCCV_AVQ_PERSONE_125"
                            #                   ,"92_504_NOTE_TIME"
                            ,"22_289_DF_DCIS_POPRES1_3"
                            ,"22_315_DF_DCIS_POPORESBIL1_10"
                            ,"22_315_DF_DCIS_POPORESBIL1_11"
                            ,"22_315_DF_DCIS_POPORESBIL1_12"
                            ,"22_315_DF_DCIS_POPORESBIL1_13"
                            ,"22_315_DF_DCIS_POPORESBIL1_14"
                            ,"22_315_DF_DCIS_POPORESBIL1_15"
                            ,"22_315_DF_DCIS_POPORESBIL1_16"
                            ,"22_315_DF_DCIS_POPORESBIL1_17"
                            ,"22_315_DF_DCIS_POPORESBIL1_18"
                            ,"22_315_DF_DCIS_POPORESBIL1_19"
                            ,"22_315_DF_DCIS_POPORESBIL1_20"
                            ,"22_315_DF_DCIS_POPORESBIL1_21"
                            ,"22_315_DF_DCIS_POPORESBIL1_22"
                            ,"22_315_DF_DCIS_POPORESBIL1_23"
                            ,"22_315_DF_DCIS_POPORESBIL1_3"
                            ,"22_315_DF_DCIS_POPORESBIL1_4"
                            ,"22_315_DF_DCIS_POPORESBIL1_5"
                            ,"22_315_DF_DCIS_POPORESBIL1_6"
                            ,"22_315_DF_DCIS_POPORESBIL1_7"
                            ,"22_315_DF_DCIS_POPORESBIL1_8"
                            ,"22_315_DF_DCIS_POPORESBIL1_9"
                            ,"DF_BULK_DCCN_FPA"
                            ,"DF_BULK_DCSC_CAPACOFTUR"
                            ,"DF_BULK_DCSC_OCCUPCOLLE"
                            ,"DF_BULK_DCSC_TURISAREA"
                            ,"DF_BULK_DCSP_FOI1B2010_TB4"
                            ,"DF_BULK_DCSP_FOI1B2015_NUTS3"
                            ,"DF_BULK_DCSP_FOI1B2015_TB4"
                            ,"DF_BULK_DCSP_NIC1B2015_NUTS1"
                            ,"DF_BULK_DCSP_NIC1B2015_NUTS2"
                            ,"DF_BULK_DCSP_NIC1B2015_NUTS3"
                            ,"DF_BULK_DCSP_NIC1B2015_RECONNIC"
                            ,"DF_BULK_DCSP_NIC1B2015_SPLICECO"
                            ,"DF_BULK_DCSP_NIC1B2015_SPLICI"
                            ,"DF_BULK_DCSP_NIC1B2015_TB2"
                            ,"DF_BULK_DCSP_NIC1B2015_TB3"
                            ,"DF_BULK_DCSP_NICDUEB2010_CPI"
                            ,"DF_BULK_DCSP_NICDUEB2010_TB2"
                            ,"DF_BULK_DCSP_NICDUEB2010_TB3"
                            ,"DF_BULK_DCSP_NICUNOBB2010_CPI"
                            ,"DF_BULK_DCSP_NICUNOBB2010_NIC"
                            ,"DF_BULK_DCSP_NICUNOBB2010_TB2"
                            ,"DF_BULK_DCSP_NICUNOBB2010_TB3"
                            ,"DF_BULK_DIVORCESSE"
                            ,"DF_BULK_ENTERPRISE"
                            ,"DF_BULK_FARMSTRUCT"
                            ,"DF_BULK_FISHERYBRO"
                            ,"DF_BULK_FLOWERSBRO"
                            ,"DF_BULK_FORESTRYBR"
                            ,"DF_BULK_HIGHSCHOOL"
                            ,"DF_BULK_HOURLYWAGE"
                            ,"DF_BULK_HUNTINGBRO"
                            ,"DF_BULK_LOCALUNITS"
                            ,"DF_BULK_PERMANENTC"
                            ,"DF_BULK_POPOLAZION"
                            ,"DF_BULK_PREVIOUSBA"
                            ,"DF_BULK_PROCEEDING"
                            ,"DF_BULK_PROTECTION"
                            ,"DF_BULK_PROTESTSSE"
                            ,"DF_BULK_PUBLISHEDB"
                            ,"DF_BULK_REPORTSOFP"
                            ,"DF_BULK_SCHOOLSSER"
                            ,"DF_BULK_SEEDSBROKE"
                            ,"DF_BULK_SEPARATION"
                            ,"DF_BULK_STATEARCHI"
                            ,"DF_BULK_STATEOWNED"
                            ,"DF_BULK_STIMEPERGR"
                            ,"DF_BULK_SUICIDESAN"
                            ,"DF_BULK_SUICIDI"
                            ,"DF_BULK_SURVEYCALE"
                            ,"DF_BULK_UNIVERSITY"
                            ,"DF_BULK_UNIVERSITY_DEGREE"
                            ,"DF_DCAT_CENSAGRIC2020_AGE_MAR"
                            ,"DF_DIFF_REPORT"
                            , "DF_TEST_DCIS_ABORTISPONTL"))

        if (lang == "ita") {
          name_column <- "Name.it"
        } else if (lang == "eng") {
          name_column <- "Name.en"
        } else {
          stop("Wrong language parameter. Use 'ita' for Italian and 'eng' for English.")
        }

        search_keyword <- function(keyword) {
          grepl(keyword, istat.fl[[name_column]], ignore.case = TRUE)
        }

        all_matches <- list()
        match_all_keywords <- NULL

        if (length(keywords) == 1) {
          matches <- grep(keywords, istat.fl[[name_column]], ignore.case = TRUE)
          if (length(matches) == 0) {
            if (lang == "ita") {
              message("Non ci sono dataset contenenti la parola chiave inserita.")
            } else {
              message("There are no datasets including that keyword.")
            }
            return(NULL)
          }
          agencyId <- istat.fl$agencyID[matches]
          id <- istat.fl$id[matches]
          version <- istat.fl$version[matches]
          name <- istat.fl[[name_column]][matches]
          match <- data.frame(agencyId = agencyId, id = id, version = version, name = name, stringsAsFactors = FALSE)
          return(list(all_matches = NULL, match_all_keywords = match))

        } else if (length(keywords) > 1) {
          index_list <- lapply(keywords, function(k) grep(k, istat.fl[[name_column]], ignore.case = TRUE))
          index_all_keywords <- Reduce(intersect, index_list)

          if (length(index_all_keywords) > 0) {
            agencyId_all_keywords <- istat.fl$agencyID[index_all_keywords]
            id_all_keywords <- istat.fl$id[index_all_keywords]
            version_all_keywords <- istat.fl$version[index_all_keywords]
            name_all_keywords <- istat.fl[[name_column]][index_all_keywords]
            match_all_keywords <- data.frame(agencyId = agencyId_all_keywords, id = id_all_keywords, version = version_all_keywords, name = name_all_keywords, stringsAsFactors = FALSE)
          }

          for (kw in keywords) {
            matches <- grep(kw, istat.fl[[name_column]], ignore.case = TRUE)
            agencyId <- istat.fl$agencyID[matches]
            id <- istat.fl$id[matches]
            version <- istat.fl$version[matches]
            name <- istat.fl[[name_column]][matches]
            match <- data.frame(agencyId = agencyId, id = id, version = version, name = name, stringsAsFactors = FALSE)
            all_matches[[kw]] <- match
          }

          # Controllo aggiuntivo per il caso in cui match_all_keywords sia NULL
          if (is.null(match_all_keywords) || nrow(match_all_keywords) == 0) {
            if (lang == "ita") {
              message("Non ci sono dataset contenenti tutte le parole chiave inserite.")
            } else {
              message("There are no datasets including all keywords.")
            }
            return(NULL)
          } else {
            return(list(all_matches = all_matches, match_all_keywords = match_all_keywords))
          }
        }
      }


      ################# get i.stat
      get_i_stat <- function(id_dataset,
                             start_period = NULL,
                             end_period = NULL,
                             recent = FALSE,
                             csv = FALSE,
                             xlsx = FALSE,
                             lang = "both") {

        if (!is.null(start_period) & !is.null(end_period) & isTRUE(recent)) {
          stop("Use recent = TRUE for data from last 10 years.")
        }

        dati<-NULL

        if (is.null(start_period) & is.null(end_period) & !isTRUE(recent)) {
          result <- try({
            dati <- rsdmx::readSDMX(providerId = "ISTAT", resource = "data",
                                    flowRef    = id_dataset,
                                    dsd        = TRUE)
            dati <- as.data.frame(dati, labels = TRUE)
          }, silent = FALSE)

        } else if (!is.null(start_period) & !is.null(end_period)) {
          result <- try({

            dati <- rsdmx::readSDMX(providerId = "ISTAT", resource = "data",
                                    flowRef  = id_dataset,
                                    dsd = TRUE, start = start_period, end = end_period)
            dati <- as.data.frame(dati, labels = TRUE)
          }, silent = FALSE)

        } else if (isTRUE(recent)) {

          result <- try({
            dati <- rsdmx::readSDMX(providerId = "ISTAT", resource = "data",
                                    flowRef       = id_dataset,
                                    dsd   = TRUE,
                                    start = as.numeric(format(Sys.Date(), "%Y")) - 10,
                                    end   = as.numeric(format(Sys.Date(), "%Y")))
            dati <- as.data.frame(dati, labels = TRUE)
          }, silent = FALSE)

        } else {
          stop("Please provide either a date range or set recent = TRUE.")
        }

        if (inherits(result, "try-error")) {
          warning("There is no dataset with the specified ID. If you found the ID through list or search functions, the dataset may not be available: it it exists, try with a similar ID.")
          return(NULL)
        }

        dati <- switch(lang,
                       "ita" = dati[, !grepl("\\.en$", names(dati))],
                       "eng" = dati[, !grepl("\\.it$", names(dati))],
                       dati)


        if (isTRUE(csv) & isTRUE(xlsx)) {
          stop("Choose csv OR xlsx")
        } else if (isTRUE(xlsx)) {
          write_xlsx(dati, "Dataset.xlsx")
        } else if (isTRUE(csv)) {
          write.csv(dati, "Dataset.csv")
        } else {
          return(dati)
        }
      }

      ################# get istatdata
      get_istatdata <- function(agencyId, dataset_id, version, start=NULL, end=NULL, recent_e=FALSE) {

        base_url <- "https://esploradati.istat.it/SDMXWS/rest"

        if (!is.null(start) & !is.null(end) & recent_e == FALSE){
          url <- paste0(base_url, "/data/", agencyId, ",", dataset_id, ",", version, "/ALL/?detail=full&startPeriod=", start, "-01-01&endPeriod=", end, "-12-31&dimensionAtObservation=TIME_PERIOD")
        } else if (is.null(start) & is.null(end) & recent_e == TRUE){
          start <- as.numeric(format(Sys.Date(), "%Y"))-10
          end <- as.numeric(format(Sys.Date(), "%Y"))
          url <- paste0(base_url, "/data/", agencyId, ",", dataset_id, ",", version, "/ALL/?detail=full&startPeriod=", start, "-01-01&endPeriod=", end, "-12-31&dimensionAtObservation=TIME_PERIOD")
        } else if (is.null(start) & is.null(end) & recent_e == FALSE){
          url <- paste0(base_url, "/data/",agencyId, ",", dataset_id, ",", version, "/ALL/?detail=full&dimensionAtObservation=TIME_PERIOD")
        } else {
          stop("Wrong parameters. Use start and end to define a period or recent = TRUE to obtain data from last 10 years.")
        }

        print(paste("Requesting URL:", url))

        result <- try({
          sdmx_data <- readSDMX(url)
          data <- as.data.frame(sdmx_data)
        }, silent = FALSE)

        if (inherits(result, "try-error")) {
          warning("There is no dataset with the specified ID. If you found the ID through list or search functions, the dataset may not be available: it it exists, try with a similar ID.")
          return(NULL)
        }
        return(data)
      }



      ################# get unique values
      get_unique_values <- function(df, exclude_columns = NULL) {
        unique_values_list <- list()

        columns_to_include <- setdiff(names(df), exclude_columns)

        for (colname in columns_to_include) {

          if (length(df[[colname]]) > 0) {

            unique_values <- unique(df[[colname]])

            unique_values_list[[colname]] <- unique_values
          } else {

            unique_values_list[[colname]] <- NA
          }
        }


        return(unique_values_list)
      }

      ################# check availability (check url status)
      check_availability <- function(url_a) {
        response <- try(GET(url_a), silent = TRUE)
        if (inherits(response, "try-error")) {
          return(FALSE)
        } else {
          return(status_code(response) == 200)
        }
      }

      ################################## OBSERVE EVENT ##################################

      results_i <- reactiveVal(NULL)
      results_e <- reactiveVal(NULL)
      istat_data <- reactiveVal(NULL)
      istat_esploradati <- reactiveVal(NULL)
      dataset <- reactiveVal(NULL)
      dataset_e <- reactiveVal(NULL)
      result_df <- reactiveVal(NULL)

      ################ List

      observeEvent(input$load_data_i, {
        shinyjs::html("message_view_i", "")
        shinyjs::show("message_view_i")
        shinyjs::html("message_view_i", "<span style='color: maroon; font-size: 16px;'>Loading... (it may take a while)</span>")

        data_i <- list_i_stat(input$langvi)

        istat_data(data_i)

        shinyjs::html("message_view_i", "<span style='color: forestgreen; font-size: 16px;'>Loading completed</span>")
      })

      output$istat_table <- DT::renderDT({
        DT::datatable(istat_data(), options = list(pageLength = 10, scrollX = TRUE, dom="tp"))
      })

      observeEvent(input$load_data_e, {
        shinyjs::html("message_view_e", "")
        shinyjs::show("message_view_e")
        shinyjs::html("message_view_e", "<span style='color: maroon; font-size: 16px;'>Loading... (it may take a while)</span>")

        data_e <- list_istatdata(input$langve)
        istat_esploradati(data_e)

        shinyjs::html("message_view_e", "<span style='color: forestgreen; font-size: 16px;'>Loading completed</span>")

      })
      output$esploradati_table <- DT::renderDT({
        DT::datatable(istat_esploradati(), options = list(pageLength = 10, scrollX = TRUE, dom="tp"))
      })

      ################# Search

      observeEvent(input$search_i, {

        shinyjs::html("message_search_i", "")
        shinyjs::show("message_search_i")
        shinyjs::html("message_search_i", "<span style='color: maroon; font-size: 16px;'>Searching... (it may take a while)</span>")

        output$results_all <-DT::renderDT({data.frame(Result="...")})
        output$results_any <-DT::renderDT({data.frame(Result="...")})

        keywords <- strsplit(input$keywords, ",\\s*")[[1]]
        lang <- input$lang

        result <- search_i_stat(keywords, lang)

        output$results_all <- DT::renderDT({
          DT::datatable(
            if (is.null(result$all_matches) || nrow(result$all_matches) == 0) {

              if (lang == "ita") {
                data.frame(Results = "Non ci sono dataset contenenti tutte le parole chiave inserite.")
              }else if(lang=="eng"){
                data.frame(Results = "There are no datasets containing all the entered keywords.")
              }
            } else {
              as.data.frame(result$all_matches)
            },
            options = list(pageLength = 10, scrollX = TRUE, dom="tip"),
            colnames = c("ID", "Name"))

        })

        output$results_any <-DT::renderDT({

          DT::datatable(

            if (is.null(result$any_matches) || nrow(result$any_matches) == 0) {
              if (lang == "ita") {
                data.frame(Results = "Non ci sono dataset contenenti almeno una delle parole chiave inserite.")
              }else if(lang=="eng"){
                data.frame(Results = "There are no datasets containing at least one of the entered keywords.")
              }

            } else {
              as.data.frame(result$any_matches)
            },
            options = list(pageLength = 10, scrollX = TRUE, dom="tip"),
            colnames = c("ID", "Name"))

        })

        shinyjs::html("message_search_i", "<span style='color: forestgreen; font-size: 16px;'>Search completed. </span>")
      })

      observeEvent(input$search_e, {
        result_df <- NULL
        shinyjs::html("message_search_e", "")
        shinyjs::show("message_search_e")
        shinyjs::html("message_search_e", "<span style='color: maroon; font-size: 16px;'>Searching... (it may take a while)</span>")

        output$allKeywordsTable <- DT::renderDT({ data.frame(Result = "...") })
        output$someKeywordsTable <- DT::renderDT({ data.frame(Result = "...") })

        keywords <- unlist(strsplit(input$keywords_e, ",\\s*"))
        lang <- input$lang

        results <- search_istatdata(keywords, lang)

        # Controllo per le parole chiave in match_all_keywords
        output$allKeywordsTable <- DT::renderDT({
          if (is.null(results) || is.null(results$match_all_keywords)) {

            # Messaggio di assenza di risultati
            if (lang == "ita") {
              data.frame(Results = "Non ci sono dataset contenenti tutte le parole chiave inserite.")
            } else if (lang == "eng") {
              data.frame(Results = "There are no datasets containing all the entered keywords.")
            }
          } else {
            as.data.frame(results$match_all_keywords)
          }
        })

        output$someKeywordsTable <- DT::renderDT({
          if (is.null(results) || is.null(results$all_matches) || length(results$all_matches) == 0) {
            if (lang == "ita") {
              data.frame(Results = "Non ci sono dataset contenenti almeno una delle parole chiave inserite.")
            } else if (lang == "eng") {
              data.frame(Results = "There are no datasets containing at least one of the entered keywords.")
            }
          } else {
            all_matches <- results$all_matches

            # Assicurati che all_matches sia una lista e non NULL
            if (is.list(all_matches) && length(all_matches) > 0) {
              result_df <- do.call(rbind, all_matches)

              if (!is.null(results$match_all_keywords)) {
                all_ids <- unique(results$match_all_keywords$id)
                result_df <- result_df[!result_df$id %in% all_ids, ]
              }

              DT::datatable(
                result_df,
                options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
                colnames = c("agencyId", "ID", "version", "Name")
              )
            } else {
              data.frame(Results = "Non ci sono risultati da mostrare.")
            }
          }
        })

        shinyjs::html("message_search_e", "<span style='color: forestgreen; font-size: 16px;'>Search completed. </span>")
      })


      ################# Get_i

      observeEvent(input$show_i, {
        shinyjs::html("variables_i", "")
        shinyjs::show("variables_i")
        shinyjs::html("variables_i", "<span style='color: maroon; font-size: 16px;'>Downloading... (downloading may take a while)</span>")

        dataset(NULL)
        get_i <- NULL


        if (input$data_option_i == "range_data_i") {
          get_i <- get_i_stat(trimws(input$id_dataset),
                              start_period = input$start_period,
                              end_period = input$end_period,
                              lang = input$lang)

        } else if (input$data_option_i == "recent_i") {

          get_i <- get_i_stat(trimws(input$id_dataset),
                              recent = TRUE,
                              lang = input$lang)

        } else if (input$data_option_i == "complete_i") {
          get_i <- get_i_stat(trimws(input$id_dataset), lang = input$lang)
        }
        dataset(get_i)

        if (is.null(dataset())) {
          shinyjs::html("variables_i", "<span style='color: red; font-size: 16px;'>There is no dataset with the specified ID. If you found the ID through list or search functions, the dataset may not be available: it it exists, try with a similar ID.</span>")
        } else {
          output$variables_i <- renderPrint({

            df_columns_i <- get_unique_values(dataset(), exclude_columns = c("ITTER107", "ITTER107_label.it", "ITTER107_label.en", "obsValue"))
            unique_ref_area_i <- length(unique(dataset()$ITTER107))

            results_combined_i <- list(
              "dataset" = df_columns_i,
              "Count of unique values in 'ITTER107'" = unique_ref_area_i
            )
            results_combined_i
          })
        }

      })

      output$dati_i <- DT::renderDT({
        datatable(dataset(), options = list(pageLength = 10, scrollX = TRUE))
      })

      observeEvent(input$save_i, {
        data_to_save <- dataset()
        save_name <- input$save_name_i

        if (is.null(data_to_save)) {
          showNotification("No dataset to save", type = "error")
          return()
        }

        env <- .GlobalEnv
        env[[save_name]] <- data_to_save
        showNotification(paste("Dataset saved as", save_name), type = "message")
      })

      output$download_i.csv <- downloadHandler(

        filename = function() {
          paste("dataset", ".csv", sep="")
        },
        content = function(file) {
          write.csv(dataset(), file)
        } )

      output$download_i.xlsx <- downloadHandler(

        filename = function() {
          paste("dataset", ".xlsx", sep="")
        },
        content = function(file) {
          write.xlsx(dataset(), file)
        } )


      #################### Get_e

      observeEvent(input$show_e, {
        shinyjs::html("variables_e", "")
        shinyjs::show("variables_e")
        shinyjs::html("variables_e", "<span style='color: maroon; font-size: 16px;'>Downloading... (downloading may take a while)</span>")

        dataset_e(NULL)
        get_e <- NULL

        if (input$data_option_e == "range_data_e") {
          get_e <- get_istatdata(input$agencyId,
                                 input$dataset_id,
                                 input$version,
                                 start = input$start,
                                 end = input$end)

        } else if (input$data_option_e == "recent_e") {

          get_e <- get_istatdata(input$agencyId,
                                 input$dataset_id,
                                 input$version,
                                 recent_e = TRUE
          )

        } else if (input$data_option_e == "complete_e") {
          get_e <- get_istatdata(input$agencyId,
                                 input$dataset_id,
                                 input$version)
        }
        dataset_e(get_e)


        if (is.null(dataset_e())) {
          shinyjs::html("variables_e", "<span style='color: red; font-size: 16px;'>There is no dataset with the specified ID. If you found the ID through list or search functions, the dataset may not be available: it it exists, try with a similar ID.</span>")
        }else{

          output$variables_e <- renderPrint({
            df_columns_e <- get_unique_values(dataset_e(), exclude_columns = c("REF_AREA", "obsValue"))
            unique_ref_area_e <- length(unique(dataset_e()$REF_AREA))

            results_combined_e <- list(
              "dataset" = df_columns_e,
              "Count of unique values in 'REF_AREA'" = unique_ref_area_e
            )
            results_combined_e
          })
        }

      })

      observeEvent(input$save_e, {
        data_to_save <- dataset_e()
        save_name <- input$save_name_e

        if (is.null(data_to_save)) {
          showNotification("No dataset to save", type = "error")
          return()
        }

        env <- .GlobalEnv
        env[[save_name]] <- data_to_save
        showNotification(paste("Dataset saved as", save_name), type = "message")
      })

      output$download_e.csv <- downloadHandler(

        filename = function() {
          paste("dataset", ".csv", sep="")
        },
        content = function(file) {
          write.csv(dataset_e(), file)
        } )

      output$download_e.xlsx <- downloadHandler(

        filename = function() {
          paste("dataset", ".xlsx", sep="")
        },
        content = function(file) {
          write.xlsx(dataset_e(), file)
        } )

      output$dati_e <- DT::renderDT({
        datatable(dataset_e(), options = list(pageLength = 10, scrollX = TRUE))
      })



      ################# Filter

      name_dataset <- reactive({
        if (input$dataSource == "file") {
          req(input$file1)
          inFile <- input$file1
          read_excel(inFile$datapath)
        } else if (input$dataSource == "env") {
          req(input$datasetInput)
          dataset_name <- input$datasetInput
          get(dataset_name, envir = .GlobalEnv)
        }
      })

      res_filter <- filter_data_server(
        id = "filtering",
        data = reactive(name_dataset()),
        drop_ids = FALSE,
        #name = reactive("dataset_i"),  # Assume the filtered dataset name
        vars = reactive({
          if (is.null(name_dataset())) return(NULL)
          names(name_dataset())
        }),
        #defaults = reactive(NULL),
        widget_char = "picker",
        widget_num = "slider",
        widget_date = "slider",
        label_na = "Missing"
      )

      observeEvent(res_filter$filtered(), {
        updateProgressBar(
          session = session, id = "pbar",
          value = nrow(res_filter$filtered()), total = nrow(name_dataset())
        )
      })

      output$table <- reactable::renderReactable({
        reactable::reactable(res_filter$filtered())
      })


      output$code_dplyr <- renderPrint({
        res_filter$code()
      })



      # Logica per il salvataggio del dataset filtrato
      observeEvent(input$save_f_env, {
        data_to_save <- res_filter$filtered()
        save_name <- input$save_name_f

        if (is.null(data_to_save)) {
          showNotification("No dataset to save", type = "error")
          return()
        }

        env <- .GlobalEnv
        env[[save_name]] <- data_to_save
        showNotification(paste("Dataset saved as", save_name), type = "message")
      })

      output$download_f.csv <- downloadHandler(
        filename = function() {
          paste("dataset", ".csv", sep = "")
        },
        content = function(file) {
          write.csv(res_filter$filtered(), file)
        }
      )

      output$download_f.xlsx <- downloadHandler(
        filename = function() {
          paste("dataset", ".xlsx", sep = "")
        },
        content = function(file) {
          write.xlsx(res_filter$filtered(), file)
        }
      )

      ###################### Plots
      observe({
        updateSelectInput(session, "datasetPlot", choices = get_datasets())
      })

      datasetPlot <- reactive({
        if (input$dataSourceg == "fileg") {
          req(input$file1g)
          inFile <- input$file1g
          read_excel(inFile$datapath)
        } else if (input$dataSourceg == "envg") {
          req(input$datasetInputg)
          dataset_name <- input$datasetInputg
          get(dataset_name, envir = .GlobalEnv)
        }
      })

      observe({
        df_p <- datasetPlot()
        updateSelectInput(session, "xcol", choices = names(df_p))
        updateSelectInput(session, "ycol", choices = names(df_p))
        updateSelectInput(session, "groupcol", choices = c("No grouping", names(df_p)))
        updateSelectInput(session, "piecol", choices = names(df_p))
      })

      selected_chart <- reactiveVal("")

      observeEvent(input$scatter, {
        selected_chart("scatter")
      })

      observeEvent(input$bar, {
        selected_chart("bar")
      })

      observeEvent(input$pie, {
        selected_chart("pie")
      })

      output$showPieInput <- reactive({
        selected_chart() == "pie"
      })
      outputOptions(output, "showPieInput", suspendWhenHidden = FALSE)

      output$plot <- renderPlot({
        req(input$xcol, input$ycol)

        df_p <- datasetPlot()

        if (input$groupcol != "No grouping" && input$groupcol != "") {
          df_p <- df_p %>% group_by(across(all_of(input$groupcol)))
        }

        p <- NULL

        if (selected_chart() == "scatter") {
          p <- ggplot(df_p, aes_string(x = input$xcol, y = input$ycol, color = if (input$groupcol != "No grouping") input$groupcol else NULL)) +
            geom_point() +
            theme_minimal()+
            labs(title = paste("Scatter plot of", input$xcol, "and", input$ycol))

        } else if (selected_chart() == "bar") {
          p <- ggplot(df_p, aes_string(x = input$xcol, y = input$ycol, fill = if (input$groupcol != "No grouping") input$groupcol else NULL)) +
            geom_bar(stat = "identity", position = if (input$groupcol != "No grouping") "dodge" else "stack") +
            theme_minimal()+
            labs(title = paste("Bar plot of", input$xcol, "and", input$ycol))

        } else if (selected_chart() == "pie") {
          req(input$piecol)
          df_pie <- df_p %>%
            group_by(across(all_of(input$piecol))) %>%
            summarise(total_value = sum(get("obsValue"), na.rm = TRUE))


          p <- ggplot(df_pie, aes_string(x = "''", y = "total_value", fill = input$piecol)) +
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y", start = 0) +
            theme_void() +
            labs(title = paste("Pie chart of", input$piecol, "(filled with 'obsValue')"))


        }
        print(p)
      })
      ######################
      observe_helpers()
    }
    shinyApp(ui, server)
#  }
}

