#' queries
#'
#' @description Shiny module for View/Edit Tables tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import RPostgreSQL
#' @import DBI
#' @importFrom shiny NS
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom RSQLite dbGetQuery
#' @importFrom RSQLite dbExecute
#' @importFrom dplyr bind_rows


# Get Project names
projects_query.f <- function(){

  res <- paste0("SELECT project_name FROM project")
  return(res)
}

#Get plot data
plot_data_query.f <- function(project){
  con <- con_db.f("PostgreSQL")
  project_names <- paste0("'", project, "'", collapse = ", ")
  query <- glue::glue("
                SELECT *
                FROM public.reads
                  JOIN public.sample ON reads.sample_id = sample.sample_id
                  JOIN public.BOLD_db ON reads.BOLD_db_id = BOLD_db.BOLD_db_id
                  JOIN public.BOLD_tax ON BOLD_db.BOLD_tax_id = BOLD_tax.BOLD_tax_id
                  JOIN public.date ON sample.date_id = date.date_id
                  JOIN public.project ON sample.project_id = project.project_id
                  JOIN public.location ON sample.location_id = location.location_id
                  JOIN public.NCBI_gb ON reads.NCBI_gb_id = NCBI_gb.NCBI_gb_id
                  JOIN public.NCBI_tax ON NCBI_gb.taxonomy_id = NCBI_tax.taxonomy_id
                  JOIN public.consensus_taxonomy ON reads.ct_id = consensus_taxonomy.ct_id
                WHERE project.project_name IN ({project_names});")
  res = unique(DBI::dbGetQuery(con, query))
  return(res)

}

#try if data base connection is still valid
db_con_valid <- function(con) {
  tryCatch({
    DBI::dbGetQuery(con, "SELECT 1")
    TRUE
  }, error = function(e) {
    FALSE
  })
}

#connect to db
con_db.f <- function(){
  if (exists("con")){
    print("already connected")
    return(con)
  } else {
    tryCatch(expr = {
      print("Connecting to PostgreSQL...")
      dsn_database = "AIM_db"
      dsn_hostname = "127.0.0.1"
      dsn_port = 5432
      dsn_uid = "postgres"
      dsn_pwd = "Ö+87=V"
      con <- dbConnect(dbDriver("PostgreSQL"),
                       dbname = dsn_database,
                       port = dsn_port,
                       user = dsn_uid,
                       #rstudioapi::askForPassword("Database user"),
                       password = dsn_pwd
                       #rstudioapi::askForPassword("Database password")
      )
      print("connected")
      return(con)

    },
    error = function(e){
      return(paste("connect did not work"))
    })
  }
}

# loads data from the data base for a specific search term
search_function_old <- function(search_term) {
  con <- con_db.f("PostgreSQL")
  tables <- dbListTables(con)
  # Durch alle Tabellen iterieren
  for (table in tables) {
    # SQL-Abfrage erstellen
    rel_field = setdiff(dbListFields(con, table), paste(table,"_id", sep = ""))
    for (table_col in rel_field) {
      query <- glue::glue("
            SELECT *
            FROM public.{table}
            WHERE {table_col} = '{search_term}';")

      # Ergebnisse der Abfrage hinzufuegen
      table_results <- dbGetQuery(con, query)
      if (nrow(table_results) > 0) {
        query <- glue::glue("
                SELECT *
                FROM public.reads
                  INNER JOIN public.sample ON reads.sample_id = sample.sample_id
                  INNER JOIN public.BOLD_tax ON BOLD_db.BOLD_tax_id = BOLD_tax.BOLD_tax_id
                  INNER JOIN public.BOLD_db ON reads.BOLD_db_id = BOLD_db.BOLD_db_id
                  INNER JOIN public.date ON sample.date_id = date.date_id
                  INNER JOIN public.project ON sample.project_id = project.project_id
                  INNER JOIN public.location ON sample.location_id = location.location_id
                  INNER JOIN public.NCBI_tax ON NCBI_gb.taxonomy_id = NCBI_tax.taxonomy_id
                  INNER JOIN public.NCBI_gb ON reads.NCBI_gb_id = NCBI_gb.NCBI_gb_id
                  INNER JOIN public.consensus_taxonomy ON reads.ct_id = consensus_taxonomy.ct_id
                WHERE public.{table}.{table_col} = '{search_term}';")
        res = unique(dbGetQuery(con, query))
        return(res)
      }
    }
  }
}

search_function_2 <- function(search_term) {
  con <- con_db.f("PostgreSQL")
  tables <- dbListTables(con)
  # Ergebnisliste initialisieren
  res <- list()

  # Durch alle Tabellen iterieren
  for (table in tables) {
    # Spaltennamen der aktuellen Tabelle abrufen
    rel_field <- setdiff(dbListFields(con, table), paste(table, "_id", sep = ""))

    # Durch alle Spaltennamen iterieren
    for (table_col in rel_field) {
      # SQL-Abfrage erstellen
      query <- glue::glue("
            SELECT *
            FROM public.{table}
            WHERE {table_col} = '{search_term}'
          ")

      # Ergebnisse der Abfrage hinzufügen
      table_results <- dbGetQuery(con, query)
      if (nrow(table_results) > 0) {
        res[[table]] <- table_results
      }
    }
  }



  # Ergebnisse zurückgeben
  return(res)
}


search_function <- function(search_term){
  con <- con_db.f("PostgreSQL")
  query <- glue::glue("
    SELECT table_name, column_name
    FROM information_schema.columns
    WHERE table_schema = 'public'
      AND data_type = 'character varying';
  ")
  char_info <- dbGetQuery(con, query)
  search_columns <- paste(char_info$table_name, char_info$column_name, sep = ".")
  query <- glue::glue("
                SELECT *
                FROM public.reads
                  JOIN public.sample ON reads.sample_id = sample.sample_id
                  JOIN public.BOLD_db ON reads.BOLD_db_id = BOLD_db.BOLD_db_id
                  JOIN public.BOLD_tax ON BOLD_db.BOLD_tax_id = BOLD_tax.BOLD_tax_id
                  JOIN public.date ON sample.date_id = date.date_id
                  JOIN public.project ON sample.project_id = project.project_id
                  JOIN public.location ON sample.location_id = location.location_id
                  JOIN public.NCBI_gb ON reads.NCBI_gb_id = NCBI_gb.NCBI_gb_id
                  JOIN public.NCBI_tax ON NCBI_gb.taxonomy_id = NCBI_tax.taxonomy_id
                  JOIN public.consensus_taxonomy ON reads.ct_id = consensus_taxonomy.ct_id
                WHERE '{search_term}' IN ({toString(search_columns)})
  ")
  dbGetQuery(con, query)
}

#search DNA seq
search_sequences.f <- function(sequences) {
  # Öffnen der Datenbankverbindung
  con <- con_db.f("PostgreSQL")

  # Durchlaufen der übergebenen Sequenzen
#  for (seq in sequences) {
    # Erstellen des reverse komplementären Strings
    rev_seq <- reverse_complement(seq)

    # Suchabfrage für die Originalsequenz
    query_original <- paste0("
      SELECT *
      FROM public.reads
        JOIN public.sample ON reads.sample_id = sample.sample_id
        JOIN public.BOLD_db ON reads.BOLD_db_id = BOLD_db.BOLD_db_id
        JOIN public.BOLD_tax ON BOLD_db.BOLD_tax_id = BOLD_tax.BOLD_tax_id
        JOIN public.date ON sample.date_id = date.date_id
        JOIN public.project ON sample.project_id = project.project_id
        JOIN public.location ON sample.location_id = location.location_id
        JOIN public.NCBI_gb ON reads.NCBI_gb_id = NCBI_gb.NCBI_gb_id
        JOIN public.NCBI_tax ON NCBI_gb.taxonomy_id = NCBI_tax.taxonomy_id
        JOIN public.consensus_taxonomy ON reads.ct_id = consensus_taxonomy.ct_id
      WHERE seq = '", seq, "'")
    result_original <- unique(dbGetQuery(con, query_original))

    # Suchabfrage für das reverse komplementäre Sequenz
    query_reverse <- paste0("
      SELECT *
      FROM public.reads
        INNER JOIN public.sample ON reads.sample_id = sample.sample_id
        INNER JOIN public.BOLD_tax ON BOLD_db.BOLD_tax_id = BOLD_tax.BOLD_tax_id
        INNER JOIN public.BOLD_db ON reads.BOLD_db_id = BOLD_db.BOLD_db_id
        INNER JOIN public.date ON sample.date_id = date.date_id
        INNER JOIN public.project ON sample.project_id = project.project_id
        INNER JOIN public.location ON sample.location_id = location.location_id
        INNER JOIN public.NCBI_tax ON NCBI_gb.taxonomy_id = NCBI_tax.taxonomy_id
        INNER JOIN public.NCBI_gb ON reads.NCBI_gb_id = NCBI_gb.NCBI_gb_id
        INNER JOIN public.consensus_taxonomy ON reads.ct_id = consensus_taxonomy.ct_id
      WHERE seq = '", rev_seq, "'")
    result_reverse <- unique(dbGetQuery(con, query_reverse))

    # Verarbeiten der Suchergebnisse
    if (nrow(result_original) > 0 && nrow(result_reverse) > 0){
      res <- bind_rows(result_original, result_reverse)
    } else if (nrow(result_original) > 0) {
      res <- result_original
    } else if (nrow(result_reverse) > 0) {
      res <- result_reverse
    } else {
      res <- NULL
    }
    return(res)
#  }


}
