#' upload_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @importFrom tibble rownames_to_column
#' @import RSQLite dplyr
#'
mod_upload_data_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabItem(tabName = "upload_data", uiOutput(ns("tab1UI")))
}

#' upload_data Server Functions
#'
#' @noRd
mod_upload_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(input$meata_data, {
      info = input$meta_data
      row  <- as.integer(info$row)
      clmn <- as.integer(info$col)
      v$meta_data[row, clmn] <- info$value
    })

    observeEvent(input$saveButton, {
      updated_data <- v$meta_data
      print(updated_data)
    })

    output$tab1UI <- renderUI({
      shinydashboard::box(width = NULL, status = "primary",
        sidebarLayout(
          sidebarPanel(
            fileInput(ns("file"), "File input", accept = c(".xlsx")),
            actionButton(ns("data_read"), "Load data"),
          ),
          mainPanel(
            textInput(ns("project"), "Project name"),
            textInput(ns("customer"), "Customer name"),
#            DT::DTOutput("meta_data"),
            uiOutput(ns("saveButton_ui")),
            textOutput(ns("file_path")),
          )
        )
      )
    })

    observeEvent(input$data_read, {
      if (input$project != "" && input$customer != ""){
        if (!is.null(input$file) && input$file$size > 0) {
          file_path <- input$file$datapath
          upload_success <- data_read.f(input$file$datapath, input$project, input$customer)
          output$file_path <- renderText({
            upload_success
          })
        } else {
          output$file_path <- renderText({"error"})
        }
      } else {
        showModal(modalDialog(
          title = "Warining",
          "Pleas fill out all input fields",
          easyClose = TRUE
        )
        )
      }
    })


    data_read.f <- function(file_path, project.v, customer.v){
      tryCatch( expr = {
    ########################################################### Data read in and formatting ###########################################################
    #file_name = readline("File name: ")
    #args <- commandArgs(trailingOnly = T)

    #if (length(args) < 3) {
    #  stop("Zu wenige Argumente. Benutzung: Rscript DataRead.R")
    #} else{
    #  file_path <- args[1]
    #  project.v <- args[2]
    #  customer.v <- args[3]
      raw_data = read_excel(file_path, col_names = F, trim_ws = T)
    #}


    #formatting dataframe
    raw_data = data.frame(raw_data)
    r_indice_BOLD_Process_ID = grep("BOLD_Process_ID", raw_data[,1], ignore.case = TRUE)
    r_indice_date = unique(unlist(sapply(raw_data[c(1:r_indice_BOLD_Process_ID),], function(column) grep("date", column, ignore.case = TRUE))))
    r_indice_habitat = unique(unlist(sapply(raw_data[c(1:r_indice_BOLD_Process_ID),], function(column) grep("habitat", column, ignore.case = TRUE))))
    r_indice_customer_sample_id = unique(unlist(sapply(raw_data[c(1:r_indice_BOLD_Process_ID),], function(column) grep("customer_sampleID", column, ignore.case = TRUE))))
    col_names = unlist(raw_data[r_indice_BOLD_Process_ID,])
    names(raw_data) = col_names
    colnames(raw_data) = gsub("%", "_", colnames(raw_data))
    print("cleaning done")
    ########################################################### create date relation ###########################################################
    date.df = data.frame()
    start.col = grep("date", raw_data[c(1:r_indice_BOLD_Process_ID),], ignore.case = TRUE)+1
    end.col = grep("NCBI_Accession_ID", raw_data[r_indice_BOLD_Process_ID,], ignore.case = TRUE)-1
    date.df = as.data.frame(t(raw_data[r_indice_date, start.col: end.col ]))
    colnames(date.df) = "date"
    date.df$date = as.numeric(date.df$date)
    date.df$date = as.Date(date.df$date, origin = "1899-12-30")
    date.df$month = month.name[as.numeric(format(date.df$date, "%m"))]
    date.df$year = format(date.df$date, "%Y")
    date.df$date = as.character(date.df$date)
    date.df = unique(date.df)
    row.names(date.df) = NULL

    con = dbConnect(SQLite(), dbname = "HIPPDatenbank.db")
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS date(
        date_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        date DATE,
        month CHARACTER,
        year CHARACTER
      )
    ")

    dbWriteTable(con, "date", date.df, append = TRUE, overwrite = FALSE, row.names = FALSE)
    rel_field = setdiff(dbListFields(con, "date"), "date_id")
    query <- glue::glue("
          DELETE FROM date
          WHERE date_id NOT IN (
            SELECT MIN(date_id)
            FROM date
            GROUP BY {paste(rel_field, collapse = ', ')}
        )"
    )
    dbExecute(con, query)
    print("date relation done")
    ########################################################### create project relation ###########################################################
    project.df = data.frame()
    project.df = data.frame(customer = customer.v, project_name = project.v)

    con = dbConnect(SQLite(), dbname = "HIPPDatenbank.db")
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS project(
      project_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
      customer CHARACTER,
      project_name CHARACTER
      )
    ")

    dbWriteTable(con, "project", project.df, append = TRUE, overwrite = FALSE, row.names = FALSE)
    rel_field = setdiff(dbListFields(con, "project"), "project_id")
    query <- glue::glue("
          DELETE FROM project
          WHERE project_id NOT IN (
            SELECT MIN(project_id)
            FROM project
            GROUP BY {paste(rel_field, collapse = ', ')}
        )"
    )
    dbExecute(con, query)
    dbDisconnect(con)
    print("project relation done")
    ########################################################### create location relation ###########################################################
    location.df = data.frame()
    start.col = grep("date", raw_data[c(1:r_indice_BOLD_Process_ID),], ignore.case = TRUE)+1
    end.col = grep("NCBI_Accession_ID", raw_data[r_indice_BOLD_Process_ID,], ignore.case = TRUE)-1
    if (length(r_indice_habitat) == 0) {
      location.df = as.data.frame(t(raw_data[r_indice_customer_sample_id,c(start.col:end.col)]))
      location.df$habitat <- NA
    } else {
      location.df = as.data.frame(t(raw_data[c(r_indice_customer_sample_id, r_indice_habitat),c(start.col:end.col)]))
    }
    colnames(location.df) = c("location_name", "habitat")
    location.df$location_name = gsub("[0-9\\s]", "", location.df$location_name)
    location.df = location.df %>%
      distinct(.data$location_name,.data$habitat, .keep_all = T)




    #location.df$location_id = as.integer(factor(paste(location.df$location_name, location.df$habitat), levels = unique(paste(location.df$location_name, location.df$habitat))))
    row.names(location.df) = NULL
    location.df = location.df[,c("location_name", "habitat")]

    con = dbConnect(SQLite(), dbname = "HIPPDatenbank.db")
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS location (
        location_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        location_name CHARACTER,
        habitat CHARACTER
      )
    ")

    #for (i in 1:nrow(location.df)) {
    #  query <- glue::glue("
    #    INSERT INTO location (location_name, habitat)
    #    SELECT '{location.df[i, 'location_name']}', '{location.df[i, 'habitat']}'
    #    WHERE NOT EXISTS (
    #      SELECT 1 FROM location
    #      WHERE location_name = '{location.df[i, 'location_name']}' AND habitat = '{location.df[i, 'habitat']}'
    #    )
    #  ")

    #  dbExecute(con, query)
    #}
    dbWriteTable(con, "location", location.df, append = TRUE, overwrite = FALSE, row.names = FALSE)
    rel_field = setdiff(dbListFields(con, "location"), "location_id")
    query <- glue::glue("
          DELETE FROM location
          WHERE location_id NOT IN (
            SELECT MIN(location_id)
            FROM location
            GROUP BY {paste(rel_field, collapse = ', ')}
        )"
    )
    dbExecute(con, query)
    print("location relation done")
    ########################################################### create BOLD taxonomy relation ###########################################################

    BOLD_tax.df = as.data.frame(raw_data[,c("adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD")])
    BOLD_tax.df = BOLD_tax.df[c(c(r_indice_BOLD_Process_ID+1):nrow(BOLD_tax.df)),]

    BOLD_tax.df = unique(BOLD_tax.df)

    con = dbConnect(SQLite(), dbname = "HIPPDatenbank.db")
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS BOLD_tax(
        BOLD_tax_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        adjusted_Phylum_BOLD CHARACTER,
        adjusted_Class_BOLD CHARACTER,
        adjusted_Order_BOLD CHARACTER,
        adjusted_Family_BOLD CHARACTER,
        adjusted_Genus_BOLD CHARACTER,
        adjusted_Species_BOLD CHARACTER
      )
    ")
    #for (i in 1:nrow(BOLD_tax.df)) {
    #  query <- glue::glue("
    #    INSERT INTO BOLD_tax (adjusted_Phylum_BOLD,
    #    adjusted_Class_BOLD,
    #    adjusted_Order_BOLD,
    #    adjusted_Family_BOLD,
    #    adjusted_Genus_BOLD,
    #    adjusted_Species_BOLD)
    #    SELECT '{BOLD_tax.df[i, 'adjusted_Phylum_BOLD']}', '{BOLD_tax.df[i, 'adjusted_Class_BOLD']}', '{BOLD_tax.df[i, #'adjusted_Order_BOLD']}', '{BOLD_tax.df[i, 'adjusted_Family_BOLD']}', '{BOLD_tax.df[i, 'adjusted_Genus_BOLD']}', '{BOLD_tax.df[i, #'adjusted_Species_BOLD']}'
    #    WHERE NOT EXISTS (
    #      SELECT 1 FROM BOLD_tax
    #      WHERE adjusted_Phylum_BOLD = '{BOLD_tax.df[i, 'adjusted_Phylum_BOLD']}' AND adjusted_Class_BOLD = '{BOLD_tax.df[i, #'adjusted_Class_BOLD']}' AND  adjusted_Order_BOLD = '{BOLD_tax.df[i, 'adjusted_Order_BOLD']}' AND adjusted_Family_BOLD = #'{BOLD_tax.df[i, 'adjusted_Family_BOLD']}' AND adjusted_Genus_BOLD = '{BOLD_tax.df[i, 'adjusted_Genus_BOLD']}' AND #adjusted_Species_BOLD = '{BOLD_tax.df[i, 'adjusted_Species_BOLD']}'
    #    )
    #  ")
    #
    #  dbExecute(con, query)
    #}
    dbWriteTable(con, "BOLD_tax", BOLD_tax.df, append = TRUE, overwrite = FALSE, row.names = FALSE)
    rel_field = setdiff(dbListFields(con, "BOLD_tax"), "BOLD_tax_id")
    query <- glue::glue("
          DELETE FROM BOLD_tax
          WHERE BOLD_tax_id NOT IN (
            SELECT MIN(BOLD_tax_id)
            FROM BOLD_tax
            GROUP BY {paste(rel_field, collapse = ', ')}
        )"
    )
    dbExecute(con, query)
    print("BOLD taxonomy relation done")
    ########################################################### create BOLD database BLAST relation ###########################################################
    BOLD_db.df = raw_data[,c("BOLD_Process_ID", "BOLD_BIN_uri", "BOLD_Grade_ID", "BOLD_HIT_ID", "BIN sharing?", "BIN species", "adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD")]
    BOLD_db.df = BOLD_db.df[c(c(r_indice_BOLD_Process_ID+1):nrow(BOLD_db.df)),]
    BOLD_db.df$`BOLD_HIT_ID` = as.numeric(sub("%", "", BOLD_db.df$`BOLD_HIT_ID`))/100
    BOLD_db.df$`BOLD_Grade_ID` = as.numeric(sub("%", "", BOLD_db.df$`BOLD_Grade_ID`))/100
    colnames(BOLD_db.df)[colnames(BOLD_db.df) == "BIN sharing?"] = "BIN_sharing"
    colnames(BOLD_db.df)[colnames(BOLD_db.df) == "BIN species"] = "BIN_species"
    #BOLD_db.df$BOLD_db_id = as.integer(factor(paste(BOLD_db.df$BOLD_Process_ID, BOLD_db.df$BOLD_BIN_uri, BOLD_db.df$BOLD_Grade_ID, BOLD_db.df$BOLD_HIT_ID, BOLD_db.df$BIN_sharing, BOLD_db.df$BIN_species), levels = unique(paste(BOLD_db.df$BOLD_Process_ID, BOLD_db.df$BOLD_BIN_uri, BOLD_db.df$BOLD_Grade_ID, BOLD_db.df$BOLD_HIT_ID, BOLD_db.df$BIN_sharing, BOLD_db.df$BIN_species))))
    BOLD_db.df = unique(BOLD_db.df)

    #get BOLD_tax_id from the databank
    con = dbConnect(SQLite(), dbname = "HIPPDatenbank.db")
    BOLD_tax_wid.df = dbGetQuery(con,"
      SELECT BOLD_tax_id, adjusted_Phylum_BOLD, adjusted_Class_BOLD, adjusted_Order_BOLD, adjusted_Family_BOLD, adjusted_Genus_BOLD, adjusted_Species_BOLD
      FROM BOLD_tax
    ")
    BOLD_db.df = left_join(BOLD_db.df, BOLD_tax_wid.df, by = c("adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD"))
    BOLD_db.df = BOLD_db.df[,c("BOLD_tax_id", "BOLD_Process_ID", "BOLD_BIN_uri", "BOLD_Grade_ID", "BOLD_HIT_ID", "BIN_sharing", "BIN_species")]

    #create BOLD_db relation
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS BOLD_db(
        BOLD_db_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        BOLD_tax_id INTEGER,
        BOLD_Process_ID CHARACTER,
        BOLD_BIN_uri CHARACTER,
        BOLD_HIT_ID FLOAT,
        BOLD_Grade_ID FLOAT,
        BIN_sharing BOOLEAN,
        BIN_species CHARACTER,
        CONSTRAINT BOLD_tax_BOLD_db
          FOREIGN KEY (BOLD_tax_id) REFERENCES BOLD_tax (BOLD_tax_id)
      )
    ")

    #insert data into data bank
    #for (i in 1:nrow(BOLD_db.df)) {
    #  query <- glue::glue("
    #    INSERT INTO BOLD_db(BOLD_tax_id, BOLD_Process_ID,
    #    BOLD_BIN_uri,
    #    BOLD_HIT_ID,
    #    BOLD_Grade_ID,
    #    BIN_sharing,
    #    BIN_species)
    #    SELECT
    #      '{BOLD_db.df[i, 'BOLD_tax_id']}',
    #      '{BOLD_db.df[i, 'BOLD_Process_ID']}',
    #      '{BOLD_db.df[i, 'BOLD_BIN_uri']}',
    #      '{BOLD_db.df[i, 'BOLD_HIT_ID']}',
    #      '{BOLD_db.df[i, 'BOLD_Grade_ID']}',
    #      '{BOLD_db.df[i, 'BIN_sharing']}',
    #      '{BOLD_db.df[i, 'BIN_species']}'
    #    WHERE NOT EXISTS (
    #      SELECT 1 FROM BOLD_db
    #      WHERE BOLD_tax_id = '{BOLD_db.df[i, 'BOLD_tax_id']}' AND
    #      BOLD_Process_ID = '{BOLD_db.df[i, 'BOLD_Process_ID']}' AND
    #      BOLD_BIN_uri = '{BOLD_db.df[i, 'BOLD_BIN_uri']}' AND
    #      BOLD_HIT_ID = '{BOLD_db.df[i, 'BOLD_HIT_ID']}' AND
    #      BOLD_Grade_ID = '{BOLD_db.df[i, 'BOLD_Grade_ID']}' AND
    #      BIN_sharing = '{BOLD_db.df[i, 'BIN_sharing']}' AND
    #      BIN_species = '{BOLD_db.df[i, 'BIN_species']}'
    #    )
    #  ")
    #
    #  dbExecute(con, query)
    #}
    dbWriteTable(con, "BOLD_db", BOLD_db.df, append = TRUE, overwrite = FALSE, row.names = FALSE)
    rel_field = setdiff(dbListFields(con, "BOLD_db"), "BOLD_db_id")
    query <- glue::glue("
          DELETE FROM BOLD_db
          WHERE BOLD_db_id NOT IN (
            SELECT MIN(BOLD_db_id)
            FROM BOLD_db
            GROUP BY {paste(rel_field, collapse = ', ')}
        )"
    )
    dbExecute(con, query)
    print("BOLD database BLAST relation done")
    ########################################################### create consensus taxonomy relation ###########################################################
    ct.df = raw_data[,c("consensus_Domain", "consensus_Phylum", "consensus_Class", "consensus_Order", "consensus_Family", "consensus_Genus", "consensus_Species")]
    ct.df = ct.df[c(c(r_indice_BOLD_Process_ID+1):nrow(ct.df)),]
    #ct.df$ct_id = as.integer(factor(paste(ct.df$consensus_Domain, ct.df$consensus_Phylum, ct.df$consensus_Class, ct.df$consensus_Order, ct.df$consensus_Family, ct.df$consensus_Genus, ct.df$consensus_Species), levels = unique(paste(ct.df$consensus_Domain, ct.df$consensus_Phylum, ct.df$consensus_Class, ct.df$consensus_Order, ct.df$consensus_Family, ct.df$consensus_Genus, ct.df$consensus_Species))))
    ct.df = unique(ct.df)

    con = dbConnect(SQLite(), dbname = "HIPPDatenbank.db")
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS consensus_taxonomy(
        ct_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        consensus_Domain CHARACTER,
        consensus_Phylum CHARACTER,
        consensus_Class CHARACTER,
        consensus_Order CHARACTER,
        consensus_Family CHARACTER,
        consensus_Genus CHARACTER,
        consensus_Species CHARACTER
      )
    ")

    #for (i in 1:nrow(ct.df)) {
    #  query <- glue::glue("
    #    INSERT INTO consensus_taxonomy(
    #    consensus_Domain,
    #    consensus_Phylum,
    #    consensus_Class,
    #    consensus_Order,
    #    consensus_Family,
    #    consensus_Genus,
    #    consensus_Species)
    #    SELECT '{ct.df[i, 'consensus_Domain']}', '{ct.df[i, 'consensus_Phylum']}', '{ct.df[i, 'consensus_Class']}', '{ct.df[i, #'consensus_Order']}', '{ct.df[i, 'consensus_Family']}', '{ct.df[i, 'consensus_Genus']}', '{ct.df[i, 'consensus_Species']}'
    #    WHERE NOT EXISTS (
    #      SELECT 1 FROM consensus_taxonomy
    #      WHERE consensus_Domain = '{ct.df[i, 'consensus_Domain']}' AND consensus_Phylum = '{ct.df[i, 'consensus_Phylum']}' AND  #consensus_Class = '{ct.df[i, 'consensus_Class']}' AND consensus_Order = '{ct.df[i, 'consensus_Order']}' AND consensus_Family = #'{ct.df[i, 'consensus_Family']}' AND consensus_Genus = '{ct.df[i, 'consensus_Genus']}' AND consensus_Species = '{ct.df[i, #'consensus_Species']}'
    #    )
    #  ")
    #
    #  dbExecute(con, query)
    #}
    dbWriteTable(con, "consensus_taxonomy", ct.df, append = TRUE, overwrite = FALSE, row.names = FALSE)
    rel_field = setdiff(dbListFields(con, "consensus_taxonomy"), "ct_id")
    query <- glue::glue("
          DELETE FROM consensus_taxonomy
          WHERE ct_id NOT IN (
            SELECT MIN(ct_id)
            FROM consensus_taxonomy
            GROUP BY {paste(rel_field, collapse = ', ')}
        )"
    )
    dbExecute(con, query)
    print("consensus taxonomy relation done")
    ########################################################### create NCBI taxonomy relation ###########################################################
    NCBI_tax.df = raw_data[,c("adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI")]
    NCBI_tax.df = NCBI_tax.df[c(c(r_indice_BOLD_Process_ID+1):nrow(NCBI_tax.df)),]
    #NCBI_tax.df$taxonomy_id = as.integer(factor(paste(NCBI_tax.df$adjusted_Domain_NCBI, NCBI_tax.df$adjusted_Phylum_NCBI, NCBI_tax.df$adjusted_Class_NCBI, NCBI_tax.df$adjusted_Order_NCBI, NCBI_tax.df$adjusted_Family_NCBI, NCBI_tax.df$adjusted_Genus_NCBI, NCBI_tax.df$adjusted_Species_NCBI), levels = unique(paste(NCBI_tax.df$adjusted_Domain_NCBI, NCBI_tax.df$adjusted_Phylum_NCBI, NCBI_tax.df$adjusted_Class_NCBI, NCBI_tax.df$adjusted_Order_NCBI, NCBI_tax.df$adjusted_Family_NCBI, NCBI_tax.df$adjusted_Genus_NCBI, NCBI_tax.df$adjusted_Species_NCBI))))
    NCBI_tax.df = unique(NCBI_tax.df)

    con = dbConnect(SQLite(), dbname = "HIPPDatenbank.db")
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS NCBI_tax(
        taxonomy_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        adjusted_Domain_NCBI CHARACTER,
        adjusted_Phylum_NCBI CHARACTER,
        adjusted_Class_NCBI CHARACTER,
        adjusted_Order_NCBI CHARACTER,
        adjusted_Family_NCBI CHARACTER,
        adjusted_Genus_NCBI CHARACTER,
        adjusted_Species_NCBI CHARACTER
      )
    ")

    #for (i in 1:nrow(NCBI_tax.df)) {
    #  query <- glue::glue("
    #    INSERT INTO NCBI_tax(
    #      adjusted_Domain_NCBI,
    #      adjusted_Phylum_NCBI,
    #      adjusted_Class_NCBI,
    #      adjusted_Order_NCBI,
    #      adjusted_Family_NCBI,
    #      adjusted_Genus_NCBI,
    #      adjusted_Species_NCBI)
    #    SELECT
    #      '{NCBI_tax.df[i, 'adjusted_Domain_NCBI']}',
    #      '{NCBI_tax.df[i, 'adjusted_Phylum_NCBI']}',
    #      '{NCBI_tax.df[i, 'adjusted_Class_NCBI']}',
    #      '{NCBI_tax.df[i, 'adjusted_Order_NCBI']}',
    #      '{NCBI_tax.df[i, 'adjusted_Family_NCBI']}',
    #      '{NCBI_tax.df[i, 'adjusted_Genus_NCBI']}',
    #      '{NCBI_tax.df[i, 'adjusted_Species_NCBI']}'
    #    WHERE NOT EXISTS (
    #      SELECT 1 FROM NCBI_tax
    #      WHERE
    #        adjusted_Domain_NCBI = '{NCBI_tax.df[i, 'adjusted_Domain_NCBI']}' AND
    #        adjusted_Phylum_NCBI = '{NCBI_tax.df[i, 'adjusted_Phylum_NCBI']}' AND
    #        adjusted_Class_NCBI = '{NCBI_tax.df[i, 'adjusted_Class_NCBI']}' AND
    #        adjusted_Order_NCBI = '{NCBI_tax.df[i, 'adjusted_Order_NCBI']}' AND
    #        adjusted_Family_NCBI = '{NCBI_tax.df[i, 'adjusted_Family_NCBI']}' AND
    #        adjusted_Genus_NCBI = '{NCBI_tax.df[i, 'adjusted_Genus_NCBI']}' AND
    #        adjusted_Species_NCBI = '{NCBI_tax.df[i, 'adjusted_Species_NCBI']}'
    #    )
    #  ")
    #
    #  dbExecute(con, query)
    #}
    dbWriteTable(con, "NCBI_tax", NCBI_tax.df, append = TRUE, overwrite = FALSE, row.names = FALSE)
    rel_field = setdiff(dbListFields(con, "NCBI_tax"), "taxonomy_id")
    query <- glue::glue("
          DELETE FROM NCBI_tax
          WHERE taxonomy_id NOT IN (
            SELECT MIN(taxonomy_id)
            FROM NCBI_tax
            GROUP BY {paste(rel_field, collapse = ', ')}
        )"
    )
    dbExecute(con, query)
    print("NCBI taxonomy relation done")
    ########################################################### create NCBI Genbank BLAST relation ###########################################################
    NCBI_gb.df = raw_data[,c("NCBI_Accession_ID", "NCBI_tax_ID", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI")]
    NCBI_gb.df = NCBI_gb.df[c(c(r_indice_BOLD_Process_ID+1):nrow(NCBI_gb.df)),]

    #get taxonomy_id from the databank
    con = dbConnect(SQLite(), dbname = "HIPPDatenbank.db")
    NCBI_tax_wid.df = dbGetQuery(con,"
      SELECT taxonomy_id, adjusted_Domain_NCBI, adjusted_Phylum_NCBI, adjusted_Class_NCBI, adjusted_Order_NCBI, adjusted_Family_NCBI, adjusted_Genus_NCBI, adjusted_Species_NCBI
      FROM NCBI_tax
    ")
    NCBI_gb.df = left_join(NCBI_gb.df, NCBI_tax_wid.df, by = c("adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI"))

    NCBI_gb.df = NCBI_gb.df[,c("NCBI_Accession_ID", "NCBI_tax_ID", "taxonomy_id")]
    #NCBI_gb.df$NCBI_gb_id = as.integer(factor(paste(NCBI_gb.df$NCBI_Accession_ID, NCBI_gb.df$NCBI_tax_ID, NCBI_gb.df$taxonomy_id), levels = unique(paste(NCBI_gb.df$NCBI_Accession_ID, NCBI_gb.df$NCBI_tax_ID, NCBI_gb.df$taxonomy_id))))
    NCBI_gb.df = unique(NCBI_gb.df)

    con = dbConnect(SQLite(), dbname = "HIPPDatenbank.db")
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS NCBI_gb(
        NCBI_gb_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        NCBI_Accession_ID CHARACTER,
        NCBI_tax_ID CHARACTER,
        taxonomy_id INTEGER NOT NULL,
        CONSTRAINT NCBI_tax_NCBI_gb
          FOREIGN KEY (taxonomy_id) REFERENCES NCBI_tax (taxonomy_id)
      )
    ")

    #insert data into data bank
    #for (i in 1:nrow(NCBI_gb.df)) {
    #  query <- glue::glue("
    #    INSERT INTO NCBI_gb(
    #      NCBI_Accession_ID,
    #      NCBI_tax_ID,
    #      taxonomy_id)
    #    SELECT
    #      '{NCBI_gb.df[i, 'NCBI_Accession_ID']}',
    #      '{NCBI_gb.df[i, 'NCBI_tax_ID']}',
    #      '{NCBI_gb.df[i, 'taxonomy_id']}'
    #    WHERE NOT EXISTS (
    #      SELECT 1 FROM NCBI_gb
    #      WHERE
    #        NCBI_Accession_ID = '{NCBI_gb.df[i, 'NCBI_Accession_ID']}' AND
    #        NCBI_tax_ID = '{NCBI_gb.df[i, 'NCBI_tax_ID']}' AND
    #        NCBI_tax_ID = '{NCBI_gb.df[i, 'NCBI_tax_ID']}'
    #    )
    #  ")
    #
    #  dbExecute(con, query)
    #}
    dbWriteTable(con, "NCBI_gb", NCBI_gb.df, append = TRUE, overwrite = FALSE, row.names = FALSE)
    rel_field = setdiff(dbListFields(con, "NCBI_gb"), "NCBI_gb_id")
    query <- glue::glue("
          DELETE FROM NCBI_gb
          WHERE NCBI_gb_id NOT IN (
            SELECT MIN(NCBI_gb_id)
            FROM NCBI_gb
            GROUP BY {paste(rel_field, collapse = ', ')}
        )"
    )
    dbExecute(con, query)
    print("NCBI genbank BLAST relation done")
    ########################################################### create sample relation ###########################################################
    #read in sample metadata
    sample.df = data.frame()
    start.col = grep("date", raw_data[c(1:r_indice_BOLD_Process_ID),], ignore.case = TRUE)+1
    end.col = grep("NCBI_Accession_ID", raw_data[r_indice_BOLD_Process_ID,], ignore.case = TRUE)-1
    if (length(r_indice_habitat) == 0) {
      sample.df = as.data.frame(t(raw_data[c(r_indice_date, r_indice_customer_sample_id, r_indice_BOLD_Process_ID),start.col: end.col ]))
      sample.df$habitat <- NA
    } else {
      sample.df = as.data.frame(t(raw_data[c(r_indice_date, r_indice_customer_sample_id, r_indice_BOLD_Process_ID, r_indice_habitat),start.col: end.col ]))
    }
    colnames(sample.df) = c("date", "customer_sample_id", "sample_name", "habitat")
    sample.df$date = as.numeric(sample.df$date)
    sample.df$date = as.Date(sample.df$date, origin = "1899-12-30")
    sample.df$date = as.character(sample.df$date)
    sample.df$customer = rep(customer.v, times = nrow(sample.df))
    sample.df$project_name = rep(project.v, times = nrow(sample.df))
    sample.df$location_name = gsub("[0-9\\s]", "", sample.df$customer_sample_id)



    con = dbConnect(SQLite(), dbname = "HIPPDatenbank.db")
    location_wid.df = dbGetQuery(con,"
      SELECT location_id, location_name, habitat
      FROM location
    ")
    sample.df = left_join(sample.df, location_wid.df, by = c("location_name", "habitat"))

    date_wid.df = dbGetQuery(con,"
      SELECT date_id, date
      FROM date
    ")
    sample.df = left_join(sample.df, date_wid.df, by = "date")

    project_wid.df = dbGetQuery(con,"
      SELECT project_id, project_name, customer
      FROM project
    ")
    sample.df = left_join(sample.df, project_wid.df, by = c("project_name", "customer"))

    rownames(sample.df) = NULL
    #sample.df$sample_id = as.integer(factor(paste(sample.df$sample_name, sample.df$location_id, sample.df$date_id, sample.df$customer_sample_id, sample.df$project_id),levels = unique(paste(sample.df$sample_name, sample.df$location_id, sample.df$date_id, sample.df$customer_sample_id, sample.df$project_id))))
    sample.df = unique(sample.df)
    sample.df = sample.df[,c("date_id", "project_id", "location_id", "sample_name", "customer_sample_id")]

    #creat sample table in data bank
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS sample(
        sample_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        date_id INTEGER,
        project_id INTEGER,
        location_id INTEGER NOT NULL,
        sample_name CHARACTER,
        customer_sample_id CHARACTER,
        CONSTRAINT project_sample
          FOREIGN KEY (project_id) REFERENCES project (project_id),
        CONSTRAINT date_sample
          FOREIGN KEY (date_id) REFERENCES date (date_id),
        CONSTRAINT location_sample
          FOREIGN KEY (location_id) REFERENCES location (location_id)
      )
    ")

    #for (i in 1:nrow(sample.df)) {
    #  query <- glue::glue("
    #    INSERT INTO sample(
    #      date_id,
    #      project_id,
    #      location_id,
    #      sample_name,
    #      customer_sample_id)
    #    SELECT
    #      '{sample.df[i, 'date_id']}',
    #      '{sample.df[i, 'project_id']}',
    #      '{sample.df[i, 'location_id']}',
    #      '{sample.df[i, 'sample_name']}',
    #      '{sample.df[i, 'customer_sample_id']}'
    #    WHERE NOT EXISTS (
    #      SELECT 1 FROM sample
    #      WHERE
    #        date_id = '{sample.df[i, 'date_id']}' AND
    #        project_id = '{sample.df[i, 'project_id']}' AND
    #        location_id = '{sample.df[i, 'location_id']}' AND
    #        sample_name = '{sample.df[i, 'sample_name']}' AND
    #        customer_sample_id = '{sample.df[i, 'customer_sample_id']}'
    #    )
    #  ")
    #
    #  dbExecute(con, query)
    #}
    dbWriteTable(con, "sample", sample.df, append = TRUE, overwrite = FALSE, row.names = FALSE)
    rel_field = setdiff(dbListFields(con, "sample"), "sample_id")
    query <- glue::glue("
          DELETE FROM sample
          WHERE sample_id NOT IN (
            SELECT MIN(sample_id)
            FROM sample
            GROUP BY {paste(rel_field, collapse = ', ')}
        )"
    )
    dbExecute(con, query)
    print("sample relation done")
    ########################################################### create reads table ###########################################################
    reads.df = data.frame()
    start.col = grep("date", raw_data[c(1:r_indice_BOLD_Process_ID),], ignore.case = TRUE)+1
    end.col = grep("NCBI_Accession_ID", raw_data[r_indice_BOLD_Process_ID,], ignore.case = TRUE)-1
    reads.df = cbind(raw_data[,c("NCBI_Accession_ID", "NCBI_tax_ID", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI" , "consensus_Domain", "consensus_Class", "consensus_Order", "consensus_Phylum", "consensus_Family", "consensus_Genus", "consensus_Species", "BOLD_Process_ID", "BOLD_BIN_uri", "BOLD_Grade_ID", "BOLD_HIT_ID","BIN sharing?","BIN species", "adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD")],raw_data[start.col: end.col ])
    if ("OTU_fasta_sequence" %in% colnames(raw_data)) {
      reads.df = cbind(reads.df, raw_data[,"OTU_fasta_sequence"])
      colnames(reads.df)[ncol(reads.df)] <-"OTU_fasta_sequence"
    } else {
      reads.df$OTU_fasta_sequence <- NA
    }
    reads.df = reads.df[c(c(r_indice_BOLD_Process_ID+1):nrow(reads.df)),]
    reads.df$BOLD_HIT_ID = as.numeric(sub("%", "", reads.df$BOLD_HIT_ID))/100
    reads.df$BOLD_Grade_ID = as.numeric(sub("%", "", reads.df$BOLD_Grade_ID))/100
    colnames(reads.df)[colnames(reads.df) == "BIN sharing?"] = "BIN_sharing"
    colnames(reads.df)[colnames(reads.df) == "BIN species"] = "BIN_species"
    reads.df = pivot_longer(reads.df, cols = colnames(raw_data[start.col: end.col ]), names_to = "sample_name", values_to = "abs_reads")
    reads.df$abs_reads = as.integer(reads.df$abs_reads)
    reads.df$customer = rep(customer.v, times = nrow(reads.df))
    reads.df$project_name = rep(project.v, times = nrow(reads.df))
    reads.df = reads.df %>%
      group_by(.data$sample_name) %>%
      mutate(
        sum_raw_reads = sum(.data$abs_reads),
        norm_reads = .data$abs_reads / .data$sum_raw_reads
      ) %>%
      ungroup()
    meta_data.df = as.data.frame(t(raw_data[c(r_indice_date, r_indice_customer_sample_id),start.col: end.col]))
    colnames(meta_data.df) = c("date","customer_sample_id")
    meta_data.df$date = as.numeric(meta_data.df$date)
    meta_data.df$date = as.Date(meta_data.df$date, origin = "1899-12-30")
    meta_data.df$date = as.character(meta_data.df$date)
    meta_data.df = meta_data.df %>%
      rownames_to_column(var = "sample_name")

    reads.df = left_join(reads.df, meta_data.df, by = "sample_name")

    reads.df = reads.df %>%
      filter(.data$abs_reads != 0)
    #reads.df$abs_reads = as.integer(reads.df$abs_reads)
    #reads.df = merge(reads.df, sample.df[,c()])
    #  reads.df %>%
    #  left_join(select(sample.df, sample_name, sample_id), by = "sample_name") %>%
    #  mutate(sample_id = coalesce(sample_id, NA))

    ##########################################################################################
    con = dbConnect(SQLite(), dbname = "HIPPDatenbank.db")
    BOLD_db_wid.df = dbGetQuery(con,"
      SELECT *
      FROM BOLD_db
    ")
    BOLD_tax_wid.df = dbGetQuery(con,"
      SELECT *
      FROM BOLD_tax
    ")
    compare.df = full_join(BOLD_db_wid.df, BOLD_tax_wid.df, by = c("BOLD_tax_id"))
    reads.df = left_join(reads.df, compare.df[,c("BOLD_db_id","BOLD_BIN_uri","BOLD_Process_ID", "BOLD_Grade_ID", "BOLD_HIT_ID","BIN_sharing","BIN_species", "adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD")], by = c("BOLD_BIN_uri","BOLD_Process_ID", "BOLD_Grade_ID", "BOLD_HIT_ID", "BIN_sharing","BIN_species", "adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD"))
    ##########################################################################################
    NCBI_gb_wid.df = dbGetQuery(con,"
      SELECT *
      FROM NCBI_gb
    ")
    NCBI_tax_wid.df = dbGetQuery(con,"
      SELECT *
      FROM NCBI_tax
    ")
    compare.df = full_join(NCBI_gb_wid.df, NCBI_tax_wid.df, by = c("taxonomy_id"))
    reads.df = left_join(reads.df, compare.df[,c("NCBI_gb_id","NCBI_Accession_ID", "NCBI_tax_ID", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI")], by = c( "NCBI_Accession_ID", "NCBI_tax_ID", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI"))
    ###########################################################################################
    ct_wid.df = dbGetQuery(con,"
      SELECT *
      FROM consensus_taxonomy
    ")
    reads.df = left_join(reads.df, ct_wid.df[,c("ct_id","consensus_Domain", "consensus_Class", "consensus_Order", "consensus_Phylum", "consensus_Family", "consensus_Genus", "consensus_Species")], by = c("consensus_Domain", "consensus_Class", "consensus_Order", "consensus_Phylum", "consensus_Family", "consensus_Genus", "consensus_Species"))
    ###########################################################################################
    sample_date_wid.df = dbGetQuery(con, "
      SELECT sample.sample_id, sample_name, customer_sample_id, date.date_id, date, project.project_id, project_name, customer
      FROM sample
      INNER JOIN date ON sample.date_id = date.date_id
      INNER JOIN project ON sample.project_id = project.project_id
    ")
    reads.df = left_join(reads.df, sample_date_wid.df[,c("sample_id", "sample_name", "customer_sample_id", "date", "project_name", "customer")], by = c("sample_name", "customer_sample_id", "date", "project_name", "customer"))
    rownames(reads.df) = NULL
    reads.df = reads.df[,c("sample_id", "BOLD_db_id", "NCBI_gb_id","ct_id", "abs_reads", "norm_reads", "OTU_fasta_sequence")]

    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS reads (
        reads_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        sample_id INTEGER NOT NULL,
        BOLD_db_id INTEGER NOT NULL,
        NCBI_gb_id INTEGER NOT NULL,
        ct_id INTEGER NOT NULL,
        abs_reads INTEGER,
        norm_reads NUMERIC,
        OTU_fasta_sequence CHARACTER,
        CONSTRAINT BOLD_reads_fk
          FOREIGN KEY (BOLD_db_id) REFERENCES BOLD_db (BOLD_db_id),
        CONSTRAINT NCBI_reads_fk
          FOREIGN KEY (NCBI_gb_id) REFERENCES NCBI_gb (NCBI_gb_id),
        CONSTRAINT consensus_taxonomy_reads_fk
          FOREIGN KEY (ct_id) REFERENCES consensus_taxonomy (ct_id),
        CONSTRAINT sample_reads_fk
          FOREIGN KEY (sample_id) REFERENCES sample (sample_id)
      )
    ")

    #for (i in 1:nrow(reads.df)) {
    #  query <- glue::glue("
    #    INSERT INTO reads(
    #      sample_id,
    #      BOLD_db_id,
    #      NCBI_gb_id,
    #      ct_id,
    #      abs_reads,
    #      norm_reads)
    #    SELECT
    #      '{reads.df[i, 'sample_id']}',
    #      '{reads.df[i, 'BOLD_db_id']}',
    #      '{reads.df[i, 'NCBI_gb_id']}',
    #      '{reads.df[i, 'ct_id']}',
    #      '{reads.df[i, 'abs_reads']}',
    #      '{reads.df[i, 'norm_reads']}'
    #    WHERE NOT EXISTS (
    #      SELECT 1 FROM reads
    #      WHERE
    #        sample_id = '{reads.df[i, 'sample_id']}' AND
    #        BOLD_db_id = '{reads.df[i, 'BOLD_db_id']}' AND
    #        NCBI_gb_id = '{reads.df[i, 'NCBI_gb_id']}' AND
    #        ct_id = '{reads.df[i, 'ct_id']}' AND
    #        abs_reads = '{reads.df[i, 'abs_reads']}' AND
    #        norm_reads = '{reads.df[i, 'norm_reads']}'
    #    )
    #  ")
    #
    #  dbExecute(con, query)
    #}
    dbWriteTable(con, "reads", reads.df, append = TRUE, overwrite = FALSE, row.names = FALSE)
    rel_field = setdiff(dbListFields(con, "reads"), "reads_id")
    query <- glue::glue("
          DELETE FROM reads
          WHERE reads_id NOT IN (
            SELECT MIN(reads_id)
            FROM reads
            GROUP BY {paste(rel_field, collapse = ', ')}
        )"
    )
    dbExecute(con, query)
    print("reads relation done")
    dbDisconnect(con)

    print("done")
    return(paste("Die Datei wurde erfolgreich hochgeladen. Pfad:", file_path))
      },
        error = function(e){
          return(paste("Data upload did not work"))
        }
      )
    }




  })
}

## To be copied in the UI
# mod_upload_data_ui("upload_data_1")

## To be copied in the server
# mod_upload_data_server("upload_data_1")
