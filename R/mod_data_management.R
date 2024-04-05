#' data_management UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyalert
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
            HTML("<h3>Create table</h3>"),
            textInput(ns("table_name"), "Table Name"),
            textInput(ns("columns"), "Columns (e.g., col1 INT, col2 VARCHAR(255))"),
            actionButton(ns("create_table_btn"), "Create Table"),
            HTML("<h3>Delete table</h3>"),
            selectInput(ns("tables_list"), "Select Table:", choices = NULL),
            actionButton(ns("show_metadata_btn"), "Show Metadata"),
            actionButton(ns("delete_table_btn"), "Delete Table"),
            actionButton(ns("refresh_tables_btn"), "Refresh Tables"),
            HTML("<h4>Reset data base and delete all</h4>"),
            actionButton(ns("delete_all_tables_btn"), "Delete all Tables")
          ),
          mainPanel(
            verbatimTextOutput(ns("metadata_output"))
          )
        )
      )
    })

    tables <- reactive({show_all_tables(con)})

    observeEvent(input$refresh_tables_btn, {
      updateSelectInput(session, "tables_list", choices = tables())
    })

    observeEvent(input$show_metadata_btn, {
      meta <- show_table_metadata(con, input$tables_list)
      output$metadata_output <- renderPrint(meta)
    })

    observeEvent(input$create_table_btn, {
      Sys.sleep(2)
      create_table(con, input$table_name, input$columns)
      updateSelectInput(session, "tables_list", choices = show_all_tables(con))
      shinyalert("Success", "The table was successfully created!", type = "success")
    })

    observeEvent(input$delete_table_btn, {
      confirmation_message <- paste("Are you sure you want to delete the table", input$tables_list, "?", sep = " ")

      # Öffnen Sie das Modalfenster
      showModal(modalDialog(
        title = "Confirm Deletion",
        confirmation_message,
        footer = tagList(
          modalButton(ns("Cancel")),
          actionButton(ns("confirm_delete_btn"), "Delete", class = "btn-danger")
        )
      ))

      # Funktion zum Löschen der Tabelle, wenn der Benutzer bestätigt
      observeEvent(input$confirm_delete_btn, {
        delete_table(con, input$tables_list)
        updateSelectInput(session, "tables_list", choices = show_all_tables(con))
        removeModal()
      })
    })

    observeEvent(input$delete_all_tables_btn, {
      confirmation_message <- "Are you sure you want to delete all tables"

      # Öffnen Sie das Modalfenster
      showModal(modalDialog(
        title = "Confirm Deletion",
        confirmation_message,
        footer = tagList(
          modalButton(ns("Cancel")),
          actionButton(ns("confirm_delete_all_btn"), "Delete", class = "btn-danger")
        )
      ))

      # Funktion zum Löschen der Tabelle, wenn der Benutzer bestätigt
      observeEvent(input$confirm_delete_all_btn, {
        delete_tables_in_order()
        updateSelectInput(session, "tables_list", choices = NULL)
        removeModal()
      })
    })

    session$onSessionEnded(function() {
      dbDisconnect(con)
    })
  })
}

## To be copied in the UI
# mod_data_management_ui("data_management")

## To be copied in the server
# mod_data_management_server("data_management_1")
