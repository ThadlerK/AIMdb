#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(
          title = "Search engine Thaddeus",
          tags$li(class = "dropdown", shinyauthr::logoutUI("logout"))
        ),
        shinydashboard::dashboardSidebar(
          collapsed = TRUE,
          div(htmlOutput("welcome"), style = "padding: 20px"),
          shinydashboard::sidebarMenu(
            shinydashboard::menuItem("Upload data", tabName = "upload_data", icon = icon("upload")),
            shinydashboard::menuItem("Search data", tabName = "search_data", icon = icon("search")),
            shinydashboard::menuItem("Create plot", tabName = "create_plot", icon = icon("chart-bar")),
            shinydashboard::menuItem("Create/Delete Tables", tabName = "data_management", icon = icon("plus-square")),
            shinydashboard::menuItem("View/Edit Tables", tabName = "data_editor", icon = icon("edit")),
            shinydashboard::menuItem("Update clause", tabName = "update_values", icon = icon("edit"))
          )
        ),
        shinydashboard::dashboardBody(
          shinyauthr::loginUI(id = "login"),
          tableOutput("user_table"),
          uiOutput("dynamic_tabs")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AIMdb"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
