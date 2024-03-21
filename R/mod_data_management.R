#' data_management UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_management_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabItem(tabName = "data_management", uiOutput(ns("tab4UI")))
}

#' data_management Server Functions
#'
#' @noRd
mod_data_management_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$tab4UI <- renderUI({
      shinydashboard::box(width = NULL, status = "primary",
        sidebarLayout(
          sidebarPanel(
            textInput(ns("table_name"), "Table Name"),
            textInput(ns("columns"), "Columns (e.g., col1 INT, col2 VARCHAR(255))"),
            actionButton(ns("create_table_btn"), "Create Table"),
            actionButton(ns("refresh_tables_btn"), "Refresh Tables"),
            selectInput(ns("tables_list"), "Select Table:", choices = NULL),
            actionButton(ns("show_metadata_btn"), "Show Metadata"),
            actionButton(ns("delete_table_btn"), "Delete Table")
          ),
          mainPanel(
            verbatimTextOutput("metadata_output")
          )
        )
      )
    })
  })
}

## To be copied in the UI
# mod_data_management_ui("data_management")

## To be copied in the server
# mod_data_management_server("data_management_1")
