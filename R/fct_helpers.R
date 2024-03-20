#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom utils globalVariables
#' @import dplyr

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# clean the search results
clean_search_results.f <- function(search_results){
  if (!is.null(search_results)) {
    res <- search_results[, !duplicated(colnames({{search_results}}))] %>%
      dplyr::filter(bold_grade_id > 0.95) %>%
      dplyr::mutate(month = factor(month, levels = month.name)) %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::mutate(year = as.integer(year))
    print("Search results cleaned")
    return(res)
  }
}

# create a bar chart (with or without color variable)
create_plot_barchart.f <- function(df, x_var, y_var, color_var, typ){
  if (color_var != "none") {
    df %>%
      dplyr::group_by(!!sym(x_var), !!sym(color_var)) %>%
      dplyr::summarise(n = ifelse(typ == "unique_counts", n_distinct(!!sym(y_var), !!sym(x_var), !!sym(color_var)), sum(!!sym(y_var)))) %>%
      dplyr::group_by(!!sym(x_var)) %>%
      dplyr::mutate(total = sum(n)) %>%
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
      dplyr::group_by(!!sym(x_var)) %>%
      dplyr::summarise(n = ifelse(typ == "unique_counts", n_distinct(!!sym(y_var), !!sym(x_var)), sum(!!sym(y_var)))) %>%
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
create_plot_barchart_percent.f <- function(df, x_var, y_var, color_var, typ){
  if (color_var != "none") {
    df %>%
      dplyr::group_by(!!sym(x_var), !!sym(color_var)) %>%
      dplyr::summarise(n = ifelse(typ == "unique_counts", n_distinct(!!sym(y_var), !!sym(x_var), !!sym(color_var)), sum(!!sym(y_var)))) %>%
      dplyr::group_by(!!sym(x_var)) %>%
      dplyr::mutate(total = sum(n)) %>%
      dplyr::mutate(percent = n/.data$total * 100) %>%
      plot_ly(x = ~get(x_var),
              y = ~percent,
              color = ~get(color_var),
              type = "bar") %>%
      layout(yaxis = list(title = 'percentage'), barmode = "stack")
  } else {
    df %>%
      dplyr::group_by(!!sym(x_var)) %>%
      dplyr::summarise(n = ifelse(typ == "unique_counts", n_distinct(!!sym(y_var), !!sym(x_var)), sum(!!sym(y_var))), total = sum(n)) %>%
      dplyr::mutate(percent = n/.data$total * 100) %>%
      plot_ly(x = ~get(x_var),
              y = ~percent,
              type = "bar") %>%
      layout(yaxis = list(title = 'percentage'), barmode = "stack")

  }
}

# create a bar chart for a specific taxon (with or without color variable)
create_barchart_tax_filtered.f <- function(df, x_var, y_var, color_var, filter_taxon, taxon_term, typ) {
  if (color_var != "none") {
    df %>%
      dplyr::filter(!!sym(filter_taxon) %in% taxon_term) %>%
      dplyr::group_by(!!sym(x_var), !!sym(color_var)) %>%
      dplyr::summarise(n = ifelse(typ == "unique_counts", n_distinct(!!sym(y_var), !!sym(x_var), !!sym(color_var)), sum(!!sym(y_var)))) %>%
      plot_ly(x = ~get(x_var),
              y = ~n,
              color = ~get(color_var),
              type = "bar") %>%
      layout(yaxis = list(title = 'count'), barmode = "stack")
  } else {
    df %>%
      dplyr::filter(!!sym(filter_taxon) %in% taxon_term) %>%
      dplyr::group_by(!!sym(x_var)) %>%
      dplyr::summarise(n = ifelse(typ == "unique_counts", n_distinct(!!sym(y_var), !!sym(x_var)), sum(!!sym(y_var)))) %>%
      plot_ly(x = ~get(x_var),
              y = ~n,
              type = "bar") %>%
      layout(yaxis = list(title = 'count'), barmode = "stack")
  }
}

# create a bar chart for a specific taxon with percentage (with or without color variable)
create_barchart_tax_filtered_percent.f <- function(df, x_var, y_var, color_var, filter_taxon, taxon_term, typ) {
  if (color_var != "none") {
    df_filtered <- df %>%
      dplyr::filter(!!sym(filter_taxon) %in% taxon_term) %>%
      dplyr::group_by(!!sym(x_var), !!sym(color_var)) %>%
      dplyr::summarise(n = ifelse(typ == "unique_counts", n_distinct(!!sym(y_var), !!sym(x_var), !!sym(color_var)), sum(!!sym(y_var))))

    df_total <- df %>%
      dplyr::group_by(!!sym(x_var), !!sym(color_var)) %>%
      dplyr::summarise(nn = ifelse(typ == "unique_counts", n_distinct(!!sym(y_var), !!sym(x_var), !!sym(color_var)), sum(!!sym(y_var))))

      full_join(df_filtered, df_total, by = c({{x_var}}, {{color_var}})) %>%
      mutate(prop = n/.data$nn * 100) %>%
      plot_ly(x = ~get(x_var), y = ~prop, color = ~get(color_var), type = "bar")
  } else {
    counter <- unique(df[[x_var]])
    results_df <- bind_rows(lapply(counter, function(m) {
      df_filtered <- df %>%
        dplyr::filter(!!sym(x_var) == m, !!sym(filter_taxon) %in% taxon_term)

      if (typ == "unique_counts") {
        df_filtered <- distinct(df_filtered, !!sym(y_var), !!sym(x_var))
        total_rows <- nrow(df %>% filter(!!sym(x_var) == m) %>% distinct(!!sym(y_var), !!sym(x_var)))
      } else if (typ == "read_counts") {
        total_rows <- nrow(df %>% filter(!!sym(x_var) == m))
      } else {
        stop("Invalid value for 'typ'")
      }

      percent <- nrow(df_filtered) / total_rows * 100

      return(data.frame(x_var = m, percent = percent))
    }))

    results_df %>%
      plot_ly(x = ~x_var, y = ~percent, type = "bar")
  }
}


# sum the reads per BOLD_BIN, NCBI_tax_ID and the selected atributes to display
sum_abs_reads_function <- function(selected_attributes, search_results){
  invalid_attributes <- setdiff(selected_attributes, colnames(search_results))
  if (length(invalid_attributes) > 0) {
    stop(paste(
      "The following selected attributes are not present in the search_results:", paste(invalid_attributes, collapse = ", ")))
  }
  return(search_results %>%
           dplyr::select(.data$bold_bin_uri, .data$ncbi_tax_id, all_of(selected_attributes), .data$abs_reads) %>%
           dplyr::group_by(.data$bold_bin_uri, .data$ncbi_tax_id, !!!syms(selected_attributes)) %>%
           dplyr::summarize(abs_reads = sum(.data$abs_reads)))
}

# filter for BOLD_Grade > 0.95
# remove duplicate columns
# alter datatype of month, data, year

clean_search_results.f <- function(search_results){
  if (!is.null(search_results)) {
    search_results <- search_results[, !duplicated(colnames({{search_results}}))] %>%
      dplyr::filter(.data$bold_grade_id > 0.95) %>%
      dplyr::mutate(month = factor(.data$month, levels = month.name)) %>%
      dplyr::mutate(date = as.Date(.data$date)) %>%
      dplyr::mutate(year = as.integer(.data$year))
    print("Search results cleaned")
    return(search_results)
  }
}


# filter data for selected attributes and remove rows with only NA
filter_search_results.f <- function(selected_attributes, search_results){
  search_results <- search_results %>%
    dplyr::select(.data$bold_bin_uri, .data$ncbi_tax_id, all_of(selected_attributes))

  if(length(selected_attributes) > 1){
    search_results <-search_results %>%
      dplyr::filter(rowSums(!is.na(.[, !(names(.) %in% c("bold_bin_uri", "ncbi_tax_id"))])) > 0)
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
    dplyr::select(.data$bold_bin_uri, .data$ncbi_tax_id, all_of(selected_attributes))

  unique_counts_per_bin <- ph1 %>%
    dplyr::group_by(.data$bold_bin_uri) %>%
    dplyr::summarise_all(~ n_distinct(., na.rm = TRUE))

  ph2 <- ph1 %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$bold_bin_uri) %>%
    dplyr::summarise_all( ~ paste(unique(.), collapse = ", "))


  total_unique_counts <- ph1 %>%
    summarise_all(~ n_distinct(., na.rm = TRUE)) %>%
    unlist()

  summary_table <- unique_counts_per_bin %>%
    dplyr::mutate(across(
      .cols = -.data$bold_bin_uri,
      .fns = ~ paste0(., "/", total_unique_counts[as.character(cur_column())])
    ))
  res <- inner_join(summary_table, ph2, by = c("bold_bin_uri"), suffix = c("", ".u"))
  return(res)

}

#test if a vector could be a DNA sequence
is_dna_sequence <- function(query) {
  # Regulärer Ausdruck, der nach möglichen DNA-Basen sucht (A, T, C, G)
  dna_pattern <- "^[ATCGatcg]+$"

  # Überprüfung des Suchbegriffs mit dem regulären Ausdruck
  if (grepl(dna_pattern, query)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# reverse a DNA sequence
reverse_complement <- function(seq) {
  # Umkehrung der Sequenz
  reversed_seq <- rev(strsplit(seq, "")[[1]])

  # Ersetzen der Basen durch ihre Komplementäre
  complement <- function(base) {
    switch(base,
           "A" = "T",
           "T" = "A",
           "C" = "G",
           "G" = "C",
           base)
  }
  complemented_seq <- sapply(reversed_seq, complement)

  # Verbinden der komplementären Basen zu einer Zeichenfolge
  complemented_seq <- paste(complemented_seq, collapse = "")

  return(complemented_seq)
}
