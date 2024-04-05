#' data_editor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyalert
mod_data_editor_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabItem(tabName = "data_editor", uiOutput(ns("tab5UI")))
}

#' data_editor Server Functions
#'
#' @noRd
mod_data_editor_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tab5UI <- renderUI({
      shinydashboard::box(width = NULL, status = "primary",
        sidebarLayout(
          sidebarPanel(
            selectInput(ns("tables_list"), "Select Table:", choices = show_all_tables(con)),
            actionButton(ns("refresh_tables_btn"), "Refresh Tables"),
#            actionButton(ns("save_btn"), "Save changes")
          ),
          mainPanel(
            DTOutput(ns("table"))
          )
        )
      )
    })

    table_data <- reactive({
      dbReadTable(con, input$tables_list)
    })

    # Tabelle rendern
    output$table <- renderDT({
      table_data()
    }, editable = TRUE
    )

    # Änderungen speichern
    observeEvent(input$table_cell_edit, {
      edited_data <- input$table_cell_edit
      if (!is.null(edited_data)) {
        # Geänderte Daten in die Datenbank schreiben
        for (i in 1:nrow(edited_data)) {
          row <- edited_data[i, ]
          col_name <- colnames(table_data())[as.integer(row$col)]
          id <- table_data()[row$row,c(paste0(input$tables_list, "_id"))]
          dbExecute(con, paste0("UPDATE ", input$tables_list, " SET ", col_name, " = '", row$value, "' WHERE ", input$tables_list, "_id = ", id))
          print(paste0("UPDATE ", input$tables_list, " SET ", col_name, " = '", row$value, "' WHERE ", input$tables_list, "_id = ", id))
        }
      }
    })





  })
}

## To be copied in the UI
# mod_data_editor_ui("data_editor_1")

## To be copied in the server
# mod_data_editor_server("data_editor_1")
