library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  title="Odyssey",
  skin="green",
  dashboardHeader(title = "Odyssey", titleWidth = 220),
  dashboardSidebar(width=220,
      sidebarMenu(
        #sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Search..."),
        menuItem("Files",tabName="files",icon=shiny::icon("upload")),
        menuItem("View Intervals",tabName="data",icon=shiny::icon("database")),
        menuItem("IdeoViz",tabName="ideoviz",icon=shiny::icon("line-chart")),
        menuItem("Gviz",tabName="gviz",icon=shiny::icon("line-chart")),
        menuItem("Seqplots",tabName="seqplots",icon=shiny::icon("line-chart")),
        menuItem("Genomation",tabName="genomation",icon=shiny::icon("line-chart")),
        menuItem("Help",tabName="help",icon=shiny::icon("question"))
      )
  ),
  dashboardBody(
   includeCSS("www/custom.css"),
   tabItems(
     tabItem(tabName="files",
               fluidRow(
                 box(
                   title="Load Files",width = NULL,status="primary",solidHeader=TRUE,
                   #htmlOutput("fileUI"),
                   #fileInput("file", "Input File",multiple = FALSE) ##May add upload file option)
                   textInput("dir","Select file directory:",value="/homes/swebb/interactive_plotting/odyssey/inst/odyssey/test_data"),
                   checkboxInput("recursive",label = "Check folder recursively",value = F),
                   actionButton("list_dir","List",icon = shiny::icon("folder-open"))
                 ),
                 box(
                   title="Select files",width = NULL,status="warning",solidHeader=TRUE,
                   uiOutput("inFiles")
                 ),
                 box(
                   title="Relabel files",width = NULL,status="warning",solidHeader=TRUE,
                   uiOutput("labels"),
                   actionButton("saveLabels","Save",icon = shiny::icon("refresh")),
                   textOutput("lsave")
                 )
             )
     ),
     tabItem(tabName="data",
             fluidRow(
               box(
                 title="Table",width = NULL,status="primary",solidHeader=TRUE,
                 div(style = 'overflow-x: scroll', dataTableOutput('table'))   
               ),
               box(
                 title="Data",width = 6,status="warning",solidHeader=TRUE,
                 div(style = 'overflow-y: scroll',uiOutput("bedFiles"),
                     textInput("save_name","Name current table",value = ""),
                     actionButton("save","Save current table",icon = shiny::icon("save"))
                 ) 
               ),
               box(
                 title="R Code",width = 6,status="danger",collapsible=TRUE,collapsed = TRUE,solidHeader=TRUE,
                 #tags$textarea(id="code", rows=6, cols=40,""),
                 HTML('<textarea id="code" rows="3" cols="40"></textarea>'),
                 checkboxInput("apply_code",label = "Apply R code",value = F),
                 helpText("See help tab for examples")
               )
             )
     ),
     tabItem(tabName="gviz",
             fluidRow(
               box(
                 title="Gviz",width = 9,status="primary",solidHeader=TRUE,
                 plotOutput("gviz_plot")   
               ),
               box(
                 title="Controls",width = 3,status="warning",solidHeader=TRUE,
                 div(style = 'overflow-y: scroll',uiOutput("gviz_controls")) 
               )
             )
     ),
     tabItem(tabName="seqplots",
             fluidRow(
               box(
                 title="Seqplots",width = 9,status="primary",solidHeader=TRUE,
                 plotOutput("seqplots_plot")
               ),
               tabBox(
                 width = 3,
                 tabPanel("Data",uiOutput("seqplots_controls")),
                 tabPanel("Parameters",
                      div(style = 'overflow-y: scroll; max-height: 600px; max-width: 400px',
                          selectInput("seqplots_output","Plot type:",choices=c("profile","heatmap")),
                          selectInput("seqplots_type","Select region to plot around",choices=c("Start of feature","Midpoint","End of feature","Anchor feature")),
                          numericInput("seqplots_xmin","Upstream:",1000),
                          numericInput("seqplots_xmax","Downstream:",1000),
                          numericInput("seqplots_anchored","Anchored size:",1000),
                          numericInput("seqplots_bin","Bin size:",100),
                          checkboxInput("seqplots_ignorestrand","Ignore strand:",F),
                          checkboxInput("seqplots_rm","Remove 0s:",F),
                          selectInput("seqplots_stat","Stat:",choices=c("mean","median"))
                      )
                 ),
                 tabPanel("Layout",
                          div(style = 'overflow-y: scroll; max-height: 600px; max-width: 400px',
                              textInput("seqplots_main","Plot title",""),
                              textInput("seqplots_xlab","X-axis label",""),
                              textInput("seqplots_ylab","Y-axis label",""),
                              checkboxInput("seqplots_manual","Set Y-axis manually:",F),
                              conditionalPanel(
                                condition = "input.seqplots_manual == true",
                                numericInput("seqplots_ylim_min","Y-axis minimum",NULL),
                                numericInput("seqplots_ylim_max","Y-axis maximum",NULL)
                              ),
                              selectInput("seqplots_scale","Plot scale",choices=c("linear","log2","zscore"),selected = "linear"),
                              checkboxInput("seqplots_keepratio","Keep ratio 1:1:",F),
                              checkboxInput("seqplots_error","Plot error estimates",T),
                              selectInput("seqplots_leg","Legend position",choices=c("topleft","topright","bottomleft","bottomright"),selected = "topleft"),
                              checkboxInput("seqplots_setlabels","Use custom legend labels",F),
                              textInput("seqplots_labels","Enter legend labels",""),
                              helpText("Enter labels separated by commas"),
                              checkboxInput("seqplots_vl","Show vertical guidelines",T),
                              numericInput("seqplots_hl","Include horizontal guideline",NULL),
                              numericInput("seqplots_point","Plot point size",12)
                          )
                 )
               )
             )
     )
   )
  )
)
)
