#' search_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom DT datatable
mod_search_data_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabItem(tabName = "search_data", uiOutput(ns("tab2UI")))
}

#' search_data Server Functions
#'
#' @noRd
mod_search_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tab2UI <- renderUI({
    shinydashboard::box(width = NULL, status = "primary",
        sidebarLayout(
          sidebarPanel(
            textInput(ns("search_term"), "Search term", value =  "Cecidomyiidae"),
            HTML("<h3>Search configuration</h3>"),
            checkboxGroupInput(ns("selected_attributes"), "Choose attributes to display", choices = c("project_name", "habitat", "location_name", "sample_name", "date", "month", "year", "customer", "BOLD_BIN_uri", "BOLD_Grade_ID", "BOLD_HIT_ID", "BIN_sharing", "BIN_species", "adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD", "consensus_Domain", "consensus_Phylum", "consensus_Class", "consensus_Order", "consensus_Family", "consensus_Genus", "consensus_Species", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI", "abs_reads", "norm_reads", "OTU_fasta_sequence")),
            uiOutput(ns("dynamic_attributes")),
            actionButton(ns("search_button"), "Suchen"),
            tags$hr(),
            textOutput(ns("result_count")),
          ),
          mainPanel(
            DT::DTOutput(ns("result_table")),
            HTML("<h4>Summary table</h4>"),
            DT::DTOutput(ns("summary_table"))
          )
        )
      )
    })

    selected_attributes <- reactive(input$selected_attributes)

    observeEvent(input$search_button, {
      tryCatch( expr = {
        con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = "HIPPDatenbank.db")
        if (dbExistsTable(con, "reads")) {

          if (is_dna_sequence(input$search_term) == TRUE) {
            search_results <- reactive({search_sequences.f(input$search_term) %>%
                clean_search_results.f()})
          } else {
            search_results <- reactive({search_function(input$search_term) %>%
                clean_search_results.f()})
          }

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
            else {filtered_search_results <- search_results() %>% dplyr::select(.data$BOLD_BIN_uri, .data$NCBI_tax_ID)}


            output$result_table <- DT::renderDT({
              filtered_search_results %>%
                DT::datatable(
                  options = list(order = list(1, 'asc'),
                                 dom = 'Blfrtip',
                                 buttons = c('csv', 'excel')),
                  extensions = 'Buttons',
                  editable = FALSE,
                  filter = 'bottom'
                )
            })

            summary_table <- create_summary_table.f(selected_attributes(), search_results())

            output$summary_table <- DT::renderDT({
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
                            editable = FALSE)


            })
            output$result_count <- renderText({
              paste0("Total results: ", "\n", nrow(search_results()))
            })
          }
        } else {
            showModal(
              modalDialog(
              title = "No connection",
              "Could not connect to data base",
              easyClose = TRUE
              )
            )
          }
        dbDisconnect(con)
      },
      error = function(e){
        showModal(
          modalDialog(
            title = "Error",
            "Error connecting to database or no results found for your search_term",
            easyClose = TRUE
          )
        )
      })
    })

  })
}


