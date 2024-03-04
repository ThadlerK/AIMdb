#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# clean the search results
clean_search_results.f <- function(search_results){
  if (!is.null(search_results)) {
    res <- search_results[, !duplicated(colnames({{search_results}}))] %>%
      filter(BOLD_Grade_ID > 0.95) %>%
      mutate(month = factor(month, levels = month.name)) %>%
      mutate(date = as.Date(date)) %>%
      mutate(year = as.integer(year))
    print("Search results cleaned")
    return(res)
  }
}

# create a bar chart (with or without color variable)
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

# create a bar chart with percentage (with or without color variable)
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

# create a bar chart for a specific taxon
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

# create a bar chart for a specific taxon with percentage
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

# loads data from the data base for a specific search term
search_function <- function(search_term) {
  con <- dbConnect(RSQLite::SQLite(), dbname = "HIPPDatenbank.db")
  tables <- dbListTables(con)
  # Durch alle Tabellen iterieren
  for (table in tables) {
    # SQL-Abfrage erstellen
    rel_field = setdiff(dbListFields(con, table), paste(table,"_id", sep = ""))
    for (table_col in rel_field) {
      query <- glue::glue("
            SELECT *
            FROM {table}
            WHERE {table_col} = '{search_term}';")

      # Ergebnisse der Abfrage hinzufügen
      table_results <- dbGetQuery(con, query)
      if (nrow(table_results) > 0) {
        query <- glue::glue("
                SELECT *
                FROM reads
                  INNER JOIN sample ON reads.sample_id = sample.sample_id
                  INNER JOIN BOLD_tax ON BOLD_db.BOLD_tax_id = BOLD_tax.BOLD_tax_id
                  INNER JOIN BOLD_db ON reads.BOLD_db_id = BOLD_db.BOLD_db_id
                  INNER JOIN date ON sample.date_id = date.date_id
                  INNER JOIN project ON sample.project_id = project.project_id
                  INNER JOIN location ON sample.location_id = location.location_id
                  INNER JOIN NCBI_tax ON NCBI_gb.taxonomy_id = NCBI_tax.taxonomy_id
                  INNER JOIN NCBI_gb ON reads.NCBI_gb_id = NCBI_gb.NCBI_gb_id
                  INNER JOIN consensus_taxonomy ON reads.ct_id = consensus_taxonomy.ct_id
                WHERE {table}.{table_col} = '{search_term}';")
        res = unique(dbGetQuery(con, query))
        return(res)
      }
    }
  }
  dbDisconnect(con)
}

# sum the reads per BOLD_BIN, NCBI_tax_ID and the selected atributes to display
sum_abs_reads_function <- function(selected_attributes, search_results){
  invalid_attributes <- setdiff(selected_attributes, colnames(search_results))
  if (length(invalid_attributes) > 0) {
    stop(paste("Die folgenden ausgewählten Attribute sind nicht in den search_results vorhanden:", paste(invalid_attributes, collapse = ", ")))
  }
  return(search_results %>%
           select(c(BOLD_BIN_uri, NCBI_tax_ID, all_of(selected_attributes), abs_reads)) %>%
           group_by(BOLD_BIN_uri, NCBI_tax_ID, !!!syms(selected_attributes)) %>%
           summarize(abs_reads = sum(abs_reads)))
}

# filter for BOLD_Grade > 0.95
# remove duplicate columns
# alter datatype of month, data, year

clean_search_results.f <- function(search_results){
  if (!is.null(search_results)) {
    search_results <- search_results[, !duplicated(colnames({{search_results}}))] %>%
      filter(BOLD_Grade_ID > 0.95) %>%
      mutate(month = factor(month, levels = month.name)) %>%
      mutate(date = as.Date(date)) %>%
      mutate(year = as.integer(year))
    print("Search results cleaned")
    return(search_results)
  }
}


# filter data for selected attributes and remove rows with only NA
filter_search_results.f <- function(selected_attributes, search_results){
  search_results <- search_results %>%
    select(c(BOLD_BIN_uri, NCBI_tax_ID, all_of(selected_attributes)))

  if(length(selected_attributes) > 1){
    search_results <-search_results %>%
      filter(rowSums(!is.na(.[, !(names(.) %in% c("BOLD_BIN_uri", "NCBI_tax_ID"))])) > 0)
  }

  if ("abs_reads" %in% selected_attributes) {
    search_results <- sum_abs_reads_function(selected_attributes, search_results)
  }
  print("search results filtered")
  return(search_results)
}

# creates table with a summary for each attribute
create_summary_table.f = function(selected_attributes, search_results) {
  ph1 <- search_results %>%
    select(c(BOLD_BIN_uri, NCBI_tax_ID, all_of(selected_attributes)))

  unique_counts_per_bin <- ph1 %>%
    group_by(BOLD_BIN_uri) %>%
    summarise_all(~ n_distinct(., na.rm = TRUE))

  ph2 <- ph1 %>%
    distinct() %>%
    group_by(BOLD_BIN_uri) %>%
    summarise_all( ~ paste(unique(.), collapse = ", "))


  total_unique_counts <- ph1 %>%
    summarise_all(~ n_distinct(., na.rm = TRUE)) %>%
    unlist()

  summary_table <- unique_counts_per_bin %>%
    mutate(across(
      .cols = -BOLD_BIN_uri,
      .fns = ~ paste0(., "/", total_unique_counts[as.character(cur_column())])
    ))
  res <- inner_join(summary_table, ph2, by = c("BOLD_BIN_uri"), suffix = c("", ".u"))
  return(res)

}
