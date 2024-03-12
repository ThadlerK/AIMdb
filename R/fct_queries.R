#' queries
#'
#' @description Shiny module for View/Edit Tables tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom RSQLite dbGetQuery
#' @importFrom RSQLite dbExecute


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
con_db.f <- function(){
  RSQLite::dbConnect(RSQLite::SQLite(), dbname = "HIPPDatenbank.db")
  }
