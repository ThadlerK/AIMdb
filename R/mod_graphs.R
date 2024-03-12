#' graphs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import plotly dplyr
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom DT datatable
mod_graphs_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabItem(tabName = "create_plot", uiOutput(ns("tab3UI")), selected = TRUE)
}

#' graphs Server Functions
#'
#' @noRd
mod_graphs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$tab3UI <- renderUI({
      shinydashboard::box(width = NULL, status = "primary",
          sidebarLayout(
            sidebarPanel(
              HTML("<h3>Plot configuration</h3>"),
              checkboxGroupInput(ns("plot_data"), "Choose the project data:", choices = RSQLite::dbGetQuery(con_db.f(), projects_query.f())$project_name),
              selectInput(ns("plot_type"), "Choose a plot type", choices = c("None","Bar chart", "Box plot", "Line plot")),
              uiOutput(ns("plot_config")),
            ),
            mainPanel(
              plotly::plotlyOutput(ns("results_plot")),
              plotly::plotlyOutput(ns("total_plot"))
            )
          )
      )
    })

    observeEvent(input$plot_data,{
      if (!is.null(input$plot_data)) {
        output$plot_config <- renderUI({
          req(input$plot_type)

          tagList(
            if (input$plot_type == "Line plot"){
              tagList(
                selectInput(ns("x_var"), "x-axis", choices = c("date", "month", "year"), selected = NULL),
                selectInput(ns("y_var"), "y-axis", choices = c("date", "month", "year", "customer", "project_name", "location_name", "habitat", "adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD", "BOLD_Process_ID", "BOLD_BIN_uri", "BOLD_Grade_ID", "BOLD_HIT_ID", "BIN_sharing", "BIN_species", "consensus_Domain", "consensus_Phylum", "consensus_Class", "consensus_Order", "consensus_Family", "consensus_Genus", "consensus_Species", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI", "NCBI_Accession_ID", "NCBI_tax_ID", "sample_name", "customer_sample_id", "abs_reads", "norm_reads"), selected = NULL)
              )
            },
            if(input$plot_type == 'Bar chart'){
              tagList(selectInput(ns("x_var"), "x-axis", choices = c("date", "month", "year", "customer", "project_name", "location_name", "habitat", "adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD", "BOLD_Process_ID", "BOLD_BIN_uri", "BOLD_Grade_ID", "BOLD_HIT_ID", "BIN_sharing", "BIN_species", "consensus_Domain", "consensus_Phylum", "consensus_Class", "consensus_Order", "consensus_Family", "consensus_Genus", "consensus_Species", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI", "NCBI_Accession_ID", "NCBI_tax_ID", "sample_name", "customer_sample_id", "abs_reads", "norm_reads"), selected = NULL),
                      selectInput(ns("y_var"), "y-axis", choices = c("BOLD_BIN_uri", "NCBI_tax_ID", "sample_name","customer_sample_id", "abs_reads", "reads_id", "project_name", "BOLD_Process_ID")),
                      selectInput(ns("color_var"), "Color variable", choices = c("None","date", "month", "year", "customer", "project_name", "location_name", "habitat", "sample_name"), selected = NULL),
                      selectInput(ns("count_typ"), "Count typ", choices = c("unique_counts", "read_counts")),
                      shinyWidgets::prettySwitch(
                        inputId = ns("percent_id"),
                        label = "percent",
                        fill = TRUE,
                        status = "primary"
                      ),
                      shinyWidgets::prettySwitch(
                        inputId = ns("filter_taxon_id"),
                        label = "Turn on taxon filter",
                        fill = TRUE,
                        status = "primary"
                      ),
                      uiOutput(ns("filter_taxon_config"))
              )},
            if(input$plot_type == 'Box plot'){
              tagList(selectInput(ns("x_var"), "x-axis", choices = c("date", "month", "year", "customer", "project_name", "location_name", "habitat", "adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD", "BOLD_Process_ID", "BOLD_BIN_uri", "BOLD_Grade_ID", "BOLD_HIT_ID", "BIN_sharing", "BIN_species", "consensus_Domain", "consensus_Phylum", "consensus_Class", "consensus_Order", "consensus_Family", "consensus_Genus", "consensus_Species", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI", "NCBI_Accession_ID", "NCBI_tax_ID", "sample_name", "customer_sample_id", "abs_reads", "norm_reads"), selected = NULL),
                      selectInput(ns("y_var"), "y-axis", choices = c("date", "month", "year", "customer", "project_name", "location_name", "habitat", "adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD", "BOLD_Process_ID", "BOLD_BIN_uri", "BOLD_Grade_ID", "BOLD_HIT_ID", "BIN_sharing", "BIN_species", "consensus_Domain", "consensus_Phylum", "consensus_Class", "consensus_Order", "consensus_Family", "consensus_Genus", "consensus_Species", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI", "NCBI_Accession_ID", "NCBI_tax_ID", "sample_name", "customer_sample_id", "abs_reads", "norm_reads"), selected = NULL)
              )},
            actionButton(
              inputId = ns("get_graph"),
              label = "Print graph"
            )
          )
        })
      } else{
        tagList(
          p("Please select a plot type ")
        )
      }
    })

    observeEvent(input$filter_taxon_id, {
      if(input$filter_taxon_id == TRUE){
        output$filter_taxon_config = renderUI({
          req(input$filter_taxon_id)
          tagList(
            selectInput(ns("taxonomy_type"), "Choose taxonomy to be filtered", choices = c("BOLD_BIN_uri","NCBI_tax_ID","consensus_Domain", "consensus_Phylum", "consensus_Class", "consensus_Order", "consensus_Family", "consensus_Genus", "consensus_Species")),
            textInput(ns("filter_term"), "Enter comma-separated values:")
          )
        })
      }
    })


    observeEvent(input$get_graph,{
      if (input$plot_type != "None"){
        df <- plot_data_query.f(input$plot_data) %>%
          clean_search_results.f()
        if (input$plot_type == "Bar chart"){
          output$results_plot <- renderPlotly({
            if (input$filter_taxon_id == FALSE){
              if (input$percent_id == FALSE) {
                create_plot_barchart.f(df, input$x_var, input$y_var, input$color_var, input$count_typ)
              } else {
                create_plot_barchart_percent.f(df, input$x_var, input$y_var, input$color_var, input$count_typ)
              }
            } else {
              req(input$filter_term)
              filter_values <- reactive({
                strsplit(gsub(" ", "", trimws(input$filter_term)), ",")[[1]]
              })
              if (input$percent_id == FALSE) {
                create_barchart_tax_filtered.f(df, input$x_var, input$y_var, input$color_var, input$taxonomy_type, taxon_term = filter_values(), input$count_typ)
              } else {
                create_barchart_tax_filtered_percent.f(df, input$x_var, input$y_var, input$color_var, input$taxonomy_type, taxon_term = filter_values(), input$count_typ)
              }
            }
          })
        }
      }
    })
  })
}

## To be copied in the UI
# mod_graphs_ui("graphs_1")

## To be copied in the server
# mod_graphs_server("graphs_1")
