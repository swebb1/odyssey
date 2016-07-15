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
   #tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
   #),
   tabItems(
     tabItem(tabName="files",
               fluidRow(
                 box(
                   title="Load Files",width = NULL,status="primary",solidHeader=TRUE,
                   #htmlOutput("fileUI"),
                   #fileInput("file", "Input File",multiple = FALSE) ##May add upload file option)
                   textInput("dir","Select file directory:",value="/homes/swebb/interactive_plotting/Odyssey/test_data"),
                   actionButton("list_dir","List",icon = shiny::icon("folder-open"))
                 ),
                 box(
                   title="Select files",width = NULL,status="warning",solidHeader=TRUE,
                   uiOutput("inFiles")
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
                 title="Seqplots",width = 12,status="primary",solidHeader=TRUE,
                 plotOutput("seqplots_plot")   
               )
             ),
             fluidRow(
               box(
                 title="Data",width = 6,status="warning",solidHeader=TRUE,
                 div(style = 'overflow-y: scroll',uiOutput("seqplots_controls"))
               ),
               box(
                 title="Controls",width = 6,status="warning",solidHeader=TRUE,
                 div(style = 'overflow-y: scroll',
                   selectInput("seqplots_output","Plot type:",choices=c("profile","heatmap")),
                   selectInput("seqplots_type","Select region to plot around",choices=c("pf","ef","mf","af")),
                   numericInput("seqplots_xmin","Upstream:",1000),
                   numericInput("seqplots_xmax","Downstream:",1000),
                   numericInput("seqplots_anchored","Anchored size:",1000),
                   numericInput("seqplots_bin","Bin size:",100),
                   selectInput("seqplots_strand","Ignore strand:",choices=c("no","yes")),
                   selectInput("seqplots_rm0","Remove 0s:",choices=c("no","yes")), #redo as check box
                   selectInput("seqplots_stat","Stat:",choices=c("mean","median"))
                 )
               )
             )
     )
  )
  )
)
)
