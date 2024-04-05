#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import magrittr
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT JS
#' @noRd



app_server <- function(input, output, session) {

  # set maximum upload size
  options(shiny.maxRequestSize = 100 * 1024^2)

  user_base <- data.frame(
    username = c("jerome"),
    password = c("aimdb"),
    password_hash = sapply(c("aimdb"), sodium::password_store),
    permissions = c("admin")
  )

  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })


  con <<- con_db.f()


  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    reactive(credentials()$user_auth))

  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = username,
    pwd_col = password_hash,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init()))

  output$user_table <- renderUI({
    if(credentials()$user_auth) {return(NULL)}
    else{
      fluidRow(
        column(4, p("Please use the usernames and passwords ...",
                  class = "text-center", style = "font-size: 15px;"),
                  br(),
                  renderTable({user_base[, -3]}), offset = 4
        )
      )
    }
  })



  output$dynamic_tabs <- renderUI({
    if(credentials()$user_auth) {
      shinydashboard::tabItems(
        mod_upload_data_ui(id = "upload_data"),
        mod_search_data_ui(id = "search_data"),
        mod_graphs_ui(id = "graphs"),
        mod_data_management_ui(id = "data_management"),
        mod_data_editor_ui("data_editor"),
        mod_update_values_ui("update_values")
      )
    }
  })








  #  autocomplete_list <- autocomplete_list.f()
  #  observe({
  #    shinyjs::runjs(
  #      sprintf("$('#%s').autocomplete({source: %s});",
  #              input$search_term,
  #              jsonlite::toJSON(autocomplete_list))
  #    )
  #  })

  #  plot_type <- reactive({
  #    if(input$y_axis !="None") {
  #
  #    }

  #  })

  mod_upload_data_server(id = "upload_data")
  mod_search_data_server("search_data")
  mod_graphs_server(id = "graphs")
  mod_data_management_server(id = "data_management")
  mod_data_editor_server("data_editor")
  mod_update_values_server("update_values")

}
