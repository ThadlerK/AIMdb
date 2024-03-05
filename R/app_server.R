#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT JS
#' @noRd



app_server <- function(input, output, session) {
  user_base <- data.frame(
    username = c("user1", "user2"),
    password = c("pass1", "pass2"),
    password_hash = sapply(c("pass1", "pass2"), sodium::password_store),
    permissions = c("manager", "admin")
  )

  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })

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
    if(credentials()$user_auth) return(NULL)
    fluidRow(column(4,
                    p("Please use the usernames and passwords ...",
                      class = "text-center", style = "font-size: 15px;"),
                    br(),
                    renderTable({user_base[, -3]}), offset = 4
    )
    )
  })



  output$tab2UI <- renderUI({
    shinydashboard::box(width = NULL, status = "primary",
        sidebarLayout(
          sidebarPanel(
            textInput("search_term", "Search term", value =  "Cecidomyiidae"),
            HTML("<h3>Search configuration</h3>"),
            checkboxGroupInput("selected_attributes", "Choose attributes to display", choices = c("project_name", "habitat", "location_name", "sample_name", "date", "month", "year", "customer", "BOLD_BIN_uri", "BOLD_Grade_ID", "BOLD_HIT_ID", "BIN_sharing", "BIN_species", "adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD", "consensus_Domain", "consensus_Phylum", "consensus_Class", "consensus_Order", "consensus_Family", "consensus_Genus", "consensus_Species", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI", "abs_reads", "norm_reads")),
            uiOutput("dynamic_attributes"),
            actionButton("search_button", "Suchen"),
            tags$hr(),
            textOutput("result_count"),
          ),
          mainPanel(
            DT::DTOutput("result_table"),
            HTML("<h4>Summary table</h4>"),
            DT::DTOutput("summary_table")
          )
        )
    )
  })

  output$dynamic_tabs <- renderUI({
    if(credentials()$user_auth) {
      shinydashboard::tabItems(
        mod_upload_data_ui(id = "upload_data"),
        shinydashboard::tabItem(tabName = "search_data", uiOutput("tab2UI")),
        mod_graphs_ui(id = "graphs"),
        shinydashboard::tabItem(tabName = "del_table", uiOutput("tab4UI")),
        shinydashboard::tabItem(tabName = "update_table", uiOutput("tab5UI")),
        shinydashboard::tabItem(tabName = "create_table", uiOutput("tab6UI")),
        shinydashboard::tabItem(tabName = "insert_value", uiOutput("tab7UI")),
        shinydashboard::tabItem(tabName = "about", uiOutput("tab8UI"))
      )
    }
  })






  selected_attributes <- reactive(input$selected_attributes)

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
  mod_graphs_server(id = "graphs")


  observeEvent(input$search_button, {
    require(magrittr)
    search_results <- reactive({search_function(input$search_term) %>%
        clean_search_results.f()})


    if (is.null(search_results())) {
      showModal(modalDialog(
        title = "No results found",
        "There were no results found for your search term",
        easyClose = TRUE
      )
      )
    }
    else{
      if (length(selected_attributes()) > 0) {filtered_search_results <- filter_search_results.f(selected_attributes(), search_results())}
      else {filtered_search_results <- search_results() %>% dplyr::select(c(BOLD_BIN_uri, NCBI_tax_ID))}


      output$result_table <- DT::renderDT({
        filtered_search_results %>%
          DT::datatable(
            options = list(order = list(1, 'asc'),
                           dom = 'Bfrtip',
                           buttons = c('csv', 'excel')),
            extensions = 'Buttons',
            editable = TRUE,
            filter = 'bottom'
          )
      })

      summary_table <- create_summary_table.f(selected_attributes(), search_results())

      output$summary_table <- DT::renderDT({
        #        table_ncol = ncol(summary_table())/2
        data = summary_table
        DT::datatable(data,
                  options = list(
                    columnDefs = list(
                      list(visible=FALSE, targets = (which(grepl("\\.u", names(data)))-1))
                    ),

                    rowCallback = JS(
                      "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                      "for(var i = 1; i < ", ceiling(ncol(data)/2), "; i++) {",
                      "var full_text = aData[i + ", floor(ncol(data)/2), "];",
                      "$('td:eq('+i+')', nRow).attr('title', full_text);",
                      "}",
                      "}")
                  ),
                  rownames = FALSE,
                  editable = TRUE)


      })
      output$result_count <- renderText({
        paste0("Total results: ", "\n", nrow(search_results()))
      })
    }
  })
}
