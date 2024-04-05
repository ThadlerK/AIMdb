#' update_values UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_update_values_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabItem(tabName = "update_values", uiOutput(ns("tab6UI")))
}

#' update_values Server Functions
#'
#' @noRd
mod_update_values_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tab6UI <- renderUI({
      shinydashboard::box(width = NULL, status = "primary",
        tagList(
        selectInput(ns("table"), "Table", choices = show_all_tables(con)),
        selectInput(ns("column"), "Column", choices = NULL),
        textInput(ns("new_value"), "New value"),
        textInput(ns("where_clause"), "WHERE-clause (optional)"),
        actionButton(ns("update_btn"), "Execute UPDATE")
        )
      )
    })


    observeEvent(input$table, {
      if(!is.null(input$table)){
        columns <- show_all_col(con, input$table)
        updateSelectInput(session, "column", choices = columns)
      }
    })

    observeEvent(input$update_btn, {
      table <- input$table
      column <- input$column
      new_value <- input$new_value
      where_clause <- input$where_clause

      if (table != "" && column != "" && new_value != "") {
        # Erstelle das UPDATE-Statement
        sql <- paste0("UPDATE ", table, " SET ", column, " = ", "'", new_value ,"'")
        if (where_clause != "") {
          sql <- paste(sql, "WHERE", where_clause)
        }
        print(sql)
        dbExecute(con, sql)

        # Ausgabe einer Bestätigungsmeldung oder Fehlermeldung
        showModal(modalDialog(
          title = "UPDATE-Statement ausgeführt",
          paste("The UPDATE-statement has been executed successful:\n", sql),
          easyClose = TRUE
        ))
      } else {
        showModal(modalDialog(
          title = "Error",
          "Please choose a table and column and type in a new value",
          easyClose = TRUE
        ))
      }
    })

  })
}

## To be copied in the UI
# mod_update_values_ui("update_values_1")

## To be copied in the server
# mod_update_values_server("update_values_1")
