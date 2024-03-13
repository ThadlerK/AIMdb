#' queries
#'
#' @description Shiny module for View/Edit Tables tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import RPostgreSQL
#' @importFrom shiny NS
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom RSQLite dbGetQuery
#' @importFrom RSQLite dbExecute
#' @importFrom dplyr bind_rows


# Get Project names
projects_query.f <- function(){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = "HIPPDatenbank.db")
  res <- paste0("SELECT project_name FROM project")
  return(res)
  dbDisconnect(con)
}

#Get plot data
plot_data_query.f <- function(project){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = "HIPPDatenbank.db")
  project_names <- paste0("'", project, "'", collapse = ", ")
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
                WHERE project.project_name IN ({project_names});")
  res = unique(RSQLite::dbGetQuery(con, query))
  RSQLite::dbDisconnect(con)
  return(res)

}

#connect to db
con_db.f <- function(SQL_typ){
    if (SQL_typ == "RSQLite") {
      RSQLite::dbConnect(RSQLite::SQLite(), dbname = "HIPPDatenbank.db")
    } else if (SQL_typ == "PostgreSQL") {
      dsn_database = "AIM_db"
      dsn_hostname = "127.0.0.1"
      dsn_port = 5432
      dsn_uid = "postgres"
      dsn_pwd = "Ö+87=V"
      dbConnect(dbDriver("PostgreSQL"),
                       dbname = dsn_database,
                       port = dsn_port,
                       user = dsn_uid,
#                  rstudioapi::askForPassword("Database user"),
                       password = dsn_pwd
#  rstudioapi::askForPassword("Database password")
      )
    }
  }



#search DNA seq
search_sequences.f <- function(sequences) {
  # Öffnen der Datenbankverbindung
  con <- con_db.f()

  # Durchlaufen der übergebenen Sequenzen
#  for (seq in sequences) {
    # Erstellen des reverse komplementären Strings
    rev_seq <- reverse_complement(seq)

    # Suchabfrage für die Originalsequenz
    query_original <- paste0("
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
      WHERE seq = '", seq, "'")
    result_original <- unique(RSQLite::dbGetQuery(con, query_original))

    # Suchabfrage für das reverse komplementäre Sequenz
    query_reverse <- paste0("
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
      WHERE seq = '", rev_seq, "'")
    result_reverse <- unique(RSQLite::dbGetQuery(con, query_reverse))

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

  # Schließen der Datenbankverbindung
  RSQLite::dbDisconnect(con)
}
