#' graphs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graphs_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabItem(tabName = "create_plot", uiOutput(ns("tab3UI")))
  )
}

#' graphs Server Functions
#'
#' @noRd
mod_graphs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$tab3UI <- renderUI({
      box(width = NULL, status = "primary",
          sidebarLayout(
            sidebarPanel(
              HTML("<h3>Plot configuration</h3>"),
              checkboxGroupInput(ns("plot_data"), "Choose the project data:", choices = RSQLite::dbGetQuery(con_db.f(), projects_query.f())$project_name),
              selectInput(ns("plot_type"), "Choose a plot type", choices = c("None","Bar chart", "Box plot", "Line plot")),
              uiOutput(ns("plot_config")),
            ),
            mainPanel(
              plotlyOutput(ns("results_plot")),
              plotlyOutput(ns("total_plot"))
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
                      selectInput(ns("color_var"), "Color variable", choices = c("None","date", "month", "year", "customer", "project_name", "location_name", "habitat", "sample_name"), selected = NULL),
                      prettySwitch(
                        inputId = ns("percent_id"),
                        label = "percent",
                        fill = TRUE,
                        status = "primary"
                      ),
                      prettySwitch(
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
            textInput(ns("filter_term"), "Type in")
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
                create_plot_barchart.f(df, input$x_var, input$color_var)
              } else {
                create_plot_barchart_percent.f(df, input$x_var, input$color_var)
              }
            } else {
              if (input$percent_id == FALSE) {
                create_barchart_tax_filtered.f(df, input$x_var, input$taxonomy_type, input$filter_term)
              } else {
                create_barchart_tax_filtered_percent.f(df, input$x_var, input$taxonomy_type, input$filter_term)
              }
            }
          })
        }
      }
    })

    create_plot_barchart.f <- function(df, x_var, color_var){
      if (color_var != "None") {
        df %>%
          dplyr::distinct(BOLD_BIN_uri, !!sym(x_var), !!sym(color_var)) %>%
          group_by(!!sym(x_var), !!sym(color_var)) %>%
          summarise(n = n()) %>%
          group_by(!!sym(x_var)) %>%
          mutate(total = sum(n)) %>%
          plot_ly(x = ~get(x_var),
                  y = ~n,
                  color = ~get(color_var),
                  type = "bar") %>%
          add_text( x = ~get(x_var),
                    y = ~total,
                    text = ~scales::comma(total),
                    textposition = "top middle",
                    cliponaxis = FALSE,
                    textfont = list(color = "black")
          ) %>%
          layout(yaxis = list(title = 'count'), barmode = "stack")
      } else {
        df %>%
          dplyr::distinct(BOLD_BIN_uri, !!sym(x_var)) %>%
          group_by(!!sym(x_var)) %>%
          summarise(n = n()) %>%
          plot_ly(x = ~get(x_var),
                  y = ~n,
                  type = "bar") %>%
          add_text(
            text = ~scales::comma(n), y = ~n,
            textposition = "top middle",
            cliponaxis = FALSE,
            textfont = list(color = "black")
          ) %>%
          layout(yaxis = list(title = 'count'), barmode = "stack")

      }
    }

    create_plot_barchart_percent.f <- function(df, x_var, color_var){
      if (color_var != "None") {
        df %>%
          dplyr::distinct(BOLD_BIN_uri, !!sym(x_var), !!sym(color_var)) %>%
          group_by(!!sym(x_var), !!sym(color_var)) %>%
          summarise(n = n()) %>%
          group_by(!!sym(x_var)) %>%
          mutate(total = sum(n)) %>%
          mutate(percent = n/total * 100) %>%
          plot_ly(x = ~get(x_var),
                  y = ~percent,
                  color = ~get(color_var),
                  type = "bar") %>%
          layout(yaxis = list(title = 'percentage'), barmode = "stack")
      } else {
        df %>%
          dplyr::distinct(BOLD_BIN_uri, !!sym(x_var)) %>%
          group_by(!!sym(x_var)) %>%
          summarise(n = n(), total = sum(n)) %>%
          mutate(percent = n/total * 100) %>%
          plot_ly(x = ~get(x_var),
                  y = ~percent,
                  type = "bar") %>%
          layout(yaxis = list(title = 'percentage'), barmode = "stack")

      }
    }

    create_barchart_tax_filtered.f <- function(df, x_var, filter_taxon, taxon_term) {
      df %>%
        filter(!!sym(filter_taxon) == {{taxon_term}}) %>%
        dplyr::distinct(BOLD_BIN_uri, !!sym(x_var)) %>%
        group_by(!!sym(x_var)) %>%
        summarise(n = n()) %>%
        plot_ly(x = ~get(x_var),
                y = ~n,
                type = "bar") %>%
        layout(yaxis = list(title = 'count'), barmode = "stack")
    }

    create_barchart_tax_filtered_percent.f <- function(df, x_var, filter_taxon, taxon_term) {
      counter <- unique(df[[x_var]])
      results_df <- bind_rows(lapply(counter, function(m) {
        df_filtered <- df %>%
          filter(!!sym(x_var) == m, !!sym(filter_taxon) == {{taxon_term}}) %>%
          distinct(BOLD_BIN_uri, !!sym(x_var))

        percent <- nrow(df_filtered) / nrow(df %>% filter(!!sym(x_var) == m) %>% distinct(BOLD_BIN_uri, !!sym(x_var))) * 100

        return(data.frame(x_var = m, percent = percent))
      }))

      results_df %>%
        plot_ly(x = ~x_var, y = ~percent, type = "bar")
    }
  })
}

## To be copied in the UI
# mod_graphs_ui("graphs_1")

## To be copied in the server
# mod_graphs_server("graphs_1")
