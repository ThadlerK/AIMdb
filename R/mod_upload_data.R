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
#' @importFrom glue glue
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
    df_clean <- raw_data[r_indice_BOLD_Process_ID:nrow(raw_data),]
    df_clean <- df_clean[complete.cases(df_clean$BOLD_Process_ID, df_clean$NCBI_Accession_ID),]
    raw_data <- rbind(raw_data[1:(r_indice_BOLD_Process_ID-1),], df_clean)
    print("cleaning done")
    ########################################################### create date relation ###########################################################
    delete_temp_tables()

    date.df = data.frame()
    start.col = grep("date", raw_data[c(1:r_indice_BOLD_Process_ID),], ignore.case = TRUE)+1
    end.col = grep("NCBI_Accession_ID", raw_data[r_indice_BOLD_Process_ID,], ignore.case = TRUE)-1
    if (length(r_indice_date) == 0) {
      date.df <- data.frame(date = as.Date(NA), month = NA_character_, year = NA_character_)
    } else {
      date.df = as.data.frame(t(raw_data[r_indice_date, start.col: end.col ]))
      colnames(date.df) = "date"
      date.df$date = as.numeric(date.df$date)
      date.df$date = as.Date(date.df$date, origin = "1899-12-30")
      date.df$month = month.name[as.numeric(format(date.df$date, "%m"))]
      date.df$year = format(date.df$date, "%Y")
    }
    date.df = unique(date.df)
    row.names(date.df) = NULL

    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS public.date(
        date_id serial NOT NULL,
        date DATE,
        month character varying,
        year character varying,
        PRIMARY KEY (date_id)
      )
    ")

    DBI::dbWriteTable(con, "temp_date", date.df, temporary = TRUE)
    query <- glue::glue("
      INSERT INTO public.date (date, month, year)
      SELECT date, month, year
      FROM temp_date td
      WHERE NOT EXISTS (
          SELECT 1
          FROM public.date
          WHERE COALESCE(date, NULL) = COALESCE(td.date, NULL) AND COALESCE(month, '') = COALESCE(td.month, '') AND COALESCE(year, '') = COALESCE(td.year, '')
      )
    ")
    dbExecute(con, query)
    DBI::dbRemoveTable(con, "temp_date")
    print("date relation done")
    ########################################################### create project relation ###########################################################
    project.df = data.frame()
    project.df = data.frame(customer = customer.v, project_name = project.v)

    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS public.project(
        project_id serial NOT NULL,
        customer character varying,
        project_name character varying,
        PRIMARY KEY (project_id)
      )
    ")

    DBI::dbWriteTable(con, "temp_project", project.df, temporary = TRUE)
    query <- glue::glue("
      INSERT INTO public.project (customer, project_name)
      SELECT customer, project_name
      FROM temp_project d
      WHERE NOT EXISTS (
          SELECT 1
          FROM public.project
          WHERE customer = d.customer AND project_name = d.project_name
      )
    ")
    dbExecute(con, query)
    DBI::dbRemoveTable(con, "temp_project")
    print("project relation done")
    ########################################################### create location relation ###########################################################
    location.df = data.frame()
    start.col = grep("customer_sampleID", raw_data[c(1:r_indice_BOLD_Process_ID),], ignore.case = TRUE)+1
    end.col = grep("NCBI_Accession_ID", raw_data[r_indice_BOLD_Process_ID,], ignore.case = TRUE)-1
    if (length(r_indice_habitat) == 0) {
      location.df = as.data.frame(t(raw_data[r_indice_customer_sample_id,c(start.col:end.col)]))
      location.df$habitat <- NA
    } else {
      location.df = as.data.frame(t(raw_data[c(r_indice_customer_sample_id, r_indice_habitat),c(start.col:end.col)]))
    }
    colnames(location.df) = c("location_name", "habitat")
    location.df$habitat = as.character(location.df$habitat)
#    location.df$location_name = gsub("[0-9\\s]", "", location.df$location_name)
    location.df = location.df %>%
      distinct(.data$location_name,.data$habitat, .keep_all = T)


    row.names(location.df) = NULL
    location.df = location.df[,c("location_name", "habitat")]

    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS public.location (
        location_id serial NOT NULL,
        location_name character varying,
        habitat character varying,
        PRIMARY KEY (location_id)
      )
    ")


    DBI::dbWriteTable(con, "temp_location", location.df, temporary = TRUE)
    query <- glue::glue("
      INSERT INTO public.location (location_name, habitat)
      SELECT location_name, habitat
      FROM temp_location d
      WHERE NOT EXISTS (
        SELECT 1
        FROM public.location
        WHERE location_name = d.location_name AND COALESCE(habitat, '') = COALESCE(d.habitat, '')
      )
    ")
    dbExecute(con, query)
    DBI::dbRemoveTable(con, "temp_location")
    print("location relation done")
    ########################################################### create BOLD taxonomy relation ###########################################################

    BOLD_tax.df = as.data.frame(raw_data[,c("adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD")])
    BOLD_tax.df = BOLD_tax.df[c(c(r_indice_BOLD_Process_ID+1):nrow(BOLD_tax.df)),]
    BOLD_tax.df = unique(BOLD_tax.df)
    colnames(BOLD_tax.df) = tolower((colnames(BOLD_tax.df)))

    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS public.bold_tax(
        BOLD_tax_id serial NOT NULL,
        adjusted_phylum_bold character varying,
        adjusted_class_bold character varying,
        adjusted_order_bold character varying,
        adjusted_family_bold character varying,
        adjusted_genus_bold character varying,
        adjusted_species_bold character varying,
        PRIMARY KEY (BOLD_tax_id)
      )
    ")

    DBI::dbWriteTable(con, "temp_bold_tax", BOLD_tax.df, temporary = TRUE)
    query <- glue::glue("
      INSERT INTO public.bold_tax (adjusted_phylum_bold, adjusted_class_bold, adjusted_order_bold, adjusted_family_bold, adjusted_genus_bold, adjusted_species_bold)
      SELECT adjusted_phylum_bold, adjusted_class_bold, adjusted_order_bold, adjusted_family_bold, adjusted_genus_bold, adjusted_species_bold
      FROM temp_bold_tax tbt
      WHERE NOT EXISTS (
        SELECT 1
        FROM public.BOLD_tax
        WHERE COALESCE(adjusted_Phylum_BOLD, '') = COALESCE(tbt.adjusted_Phylum_BOLD, '') AND
              COALESCE(adjusted_Class_BOLD, '') = COALESCE(tbt.adjusted_Class_BOLD, '') AND
              COALESCE(adjusted_Order_BOLD, '') = COALESCE(tbt.adjusted_Order_BOLD, '') AND
              COALESCE(adjusted_Family_BOLD, '') = COALESCE(tbt.adjusted_Family_BOLD, '') AND
              COALESCE(adjusted_Genus_BOLD, '') = COALESCE(tbt.adjusted_Genus_BOLD, '') AND
              COALESCE(adjusted_Species_BOLD, '') = COALESCE(tbt.adjusted_Species_BOLD, '')
      )
    ")
    dbExecute(con, query)
    DBI::dbRemoveTable(con, "temp_bold_tax")
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
    colnames(BOLD_db.df) = tolower((colnames(BOLD_db.df)))

    #get BOLD_tax_id from the databank
    DBI::dbWriteTable(con, "temp_bold_db", BOLD_db.df, temporary = TRUE, row.names = FALSE,
                      field.types = list(bold_process_id = "VARCHAR",
                                         bold_bin_uri = "VARCHAR",
                                         bold_grade_id = "NUMERIC",
                                         bold_hit_id = "NUMERIC",
                                         bin_sharing = "VARCHAR",
                                         bin_species = "VARCHAR",
                                         adjusted_phylum_bold = "VARCHAR",
                                         adjusted_class_bold = "VARCHAR",
                                         adjusted_order_bold = "VARCHAR",
                                         adjusted_family_bold = "VARCHAR",
                                         adjusted_genus_bold = "VARCHAR",
                                         adjusted_species_bold = "VARCHAR"))
    BOLD_tax_wid.df = unique(dbGetQuery(con,"
      SELECT bt.BOLD_tax_id, bt.adjusted_Phylum_BOLD, bt.adjusted_Class_BOLD, bt.adjusted_Order_BOLD, bt.adjusted_Family_BOLD, bt.adjusted_Genus_BOLD, bt.adjusted_Species_BOLD
      FROM BOLD_tax bt
      INNER JOIN temp_bold_db tbd ON COALESCE(bt.adjusted_Phylum_BOLD, '') = COALESCE(tbd.adjusted_Phylum_BOLD, '')
                             AND COALESCE(bt.adjusted_Class_BOLD, '') = COALESCE(tbd.adjusted_Class_BOLD, '')
                             AND COALESCE(bt.adjusted_Order_BOLD, '') = COALESCE(tbd.adjusted_Order_BOLD, '')
                             AND COALESCE(bt.adjusted_Family_BOLD, '') = COALESCE(tbd.adjusted_Family_BOLD, '')
                             AND COALESCE(bt.adjusted_Genus_BOLD, '') = COALESCE(tbd.adjusted_Genus_BOLD, '')
                             AND COALESCE(bt.adjusted_Species_BOLD, '') = COALESCE(tbd.adjusted_Species_BOLD, '')
    "))
    DBI::dbRemoveTable(con, "temp_bold_db")
    BOLD_db.df = left_join(BOLD_db.df, BOLD_tax_wid.df, by = tolower(c("adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD")))
    BOLD_db.df = BOLD_db.df[,tolower(c("BOLD_tax_id", "BOLD_Process_ID", "BOLD_BIN_uri", "BOLD_Grade_ID", "BOLD_HIT_ID", "BIN_sharing", "BIN_species"))]

    #create BOLD_db relation
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS public.BOLD_db(
        BOLD_db_id serial NOT NULL,
        BOLD_tax_id integer NOT NULL,
        BOLD_Process_ID character varying,
        BOLD_BIN_uri character varying,
        BOLD_HIT_ID numeric,
        BOLD_Grade_ID numeric,
        BIN_sharing character varying,
        BIN_species character varying,
        PRIMARY KEY (BOLD_db_id),
        CONSTRAINT BOLD_tax_BOLD_db
          FOREIGN KEY (bold_tax_id) REFERENCES BOLD_tax (bold_tax_id)
      )
    ")


    DBI::dbWriteTable(con, "temp_bold_db", BOLD_db.df, temporary = TRUE)
    query <- glue::glue("
      INSERT INTO public.BOLD_db (BOLD_tax_id, BOLD_Process_ID, BOLD_BIN_uri, BOLD_Grade_ID, BOLD_HIT_ID, BIN_sharing, BIN_species)
      SELECT BOLD_tax_id, BOLD_Process_ID, BOLD_BIN_uri, BOLD_Grade_ID, BOLD_HIT_ID, BIN_sharing, BIN_species
      FROM temp_BOLD_db tbd
      WHERE NOT EXISTS (
        SELECT 1
        FROM public.BOLD_db
        WHERE BOLD_tax_id = tbd.BOLD_tax_id AND
              BOLD_Process_ID = tbd.BOLD_Process_ID AND
              COALESCE(BOLD_BIN_uri, '') = COALESCE(tbd.BOLD_BIN_uri, '') AND
              BOLD_Grade_ID = tbd.BOLD_Grade_ID AND
              BOLD_HIT_ID = tbd.BOLD_HIT_ID AND
              COALESCE(BIN_sharing, '') = COALESCE(tbd.BIN_sharing, '') AND
              COALESCE(BIN_species, '') = COALESCE(tbd.BIN_species, '')
      )
    ")
    dbExecute(con, query)
    DBI::dbRemoveTable(con, "temp_bold_db")
    print("BOLD database BLAST relation done")
    ########################################################### create consensus taxonomy relation ###########################################################
    ct.df = raw_data[,c("consensus_Domain", "consensus_Phylum", "consensus_Class", "consensus_Order", "consensus_Family", "consensus_Genus", "consensus_Species")]
    ct.df = ct.df[c(c(r_indice_BOLD_Process_ID+1):nrow(ct.df)),]
    #ct.df$ct_id = as.integer(factor(paste(ct.df$consensus_Domain, ct.df$consensus_Phylum, ct.df$consensus_Class, ct.df$consensus_Order, ct.df$consensus_Family, ct.df$consensus_Genus, ct.df$consensus_Species), levels = unique(paste(ct.df$consensus_Domain, ct.df$consensus_Phylum, ct.df$consensus_Class, ct.df$consensus_Order, ct.df$consensus_Family, ct.df$consensus_Genus, ct.df$consensus_Species))))
    ct.df = unique(ct.df)
    colnames(ct.df) = tolower((colnames(ct.df)))

    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS public.consensus_taxonomy(
        ct_id serial NOT NULL,
        consensus_Domain character varying,
        consensus_Phylum character varying,
        consensus_Class character varying,
        consensus_Order character varying,
        consensus_Family character varying,
        consensus_Genus character varying,
        consensus_Species character varying,
        PRIMARY KEY (ct_id)
      )
    ")

    DBI::dbWriteTable(con, "temp_consensus_taxonomy", ct.df, temporary = TRUE)
    query <- glue::glue("
      INSERT INTO public.consensus_taxonomy (consensus_Domain, consensus_Phylum, consensus_Class, consensus_Order, consensus_Family, consensus_Genus, consensus_Species)
      SELECT consensus_Domain, consensus_Phylum, consensus_Class, consensus_Order, consensus_Family, consensus_Genus, consensus_Species
      FROM temp_consensus_taxonomy tct
      WHERE NOT EXISTS (
        SELECT 1
        FROM public.consensus_taxonomy
        WHERE COALESCE(consensus_Domain, '') = COALESCE(tct.consensus_Domain, '') AND
              COALESCE(consensus_Phylum, '') = COALESCE(tct.consensus_Phylum, '') AND
              COALESCE(consensus_Class, '') = COALESCE(tct.consensus_Class, '') AND
              COALESCE(consensus_Order, '') = COALESCE(tct.consensus_Order, '') AND
              COALESCE(consensus_Family, '') = COALESCE(tct.consensus_Family, '') AND
              COALESCE(consensus_Genus, '') = COALESCE(tct.consensus_Genus, '') AND
              COALESCE(consensus_Species, '') = COALESCE(tct.consensus_Species, '')
      )
    ")
    dbExecute(con, query)
    DBI::dbRemoveTable(con, "temp_consensus_taxonomy")
    print("consensus taxonomy relation done")
    ########################################################### create NCBI taxonomy relation ###########################################################
    NCBI_tax.df = raw_data[,c("adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI")]
    NCBI_tax.df = NCBI_tax.df[c(c(r_indice_BOLD_Process_ID+1):nrow(NCBI_tax.df)),]
    #NCBI_tax.df$taxonomy_id = as.integer(factor(paste(NCBI_tax.df$adjusted_Domain_NCBI, NCBI_tax.df$adjusted_Phylum_NCBI, NCBI_tax.df$adjusted_Class_NCBI, NCBI_tax.df$adjusted_Order_NCBI, NCBI_tax.df$adjusted_Family_NCBI, NCBI_tax.df$adjusted_Genus_NCBI, NCBI_tax.df$adjusted_Species_NCBI), levels = unique(paste(NCBI_tax.df$adjusted_Domain_NCBI, NCBI_tax.df$adjusted_Phylum_NCBI, NCBI_tax.df$adjusted_Class_NCBI, NCBI_tax.df$adjusted_Order_NCBI, NCBI_tax.df$adjusted_Family_NCBI, NCBI_tax.df$adjusted_Genus_NCBI, NCBI_tax.df$adjusted_Species_NCBI))))
    NCBI_tax.df = unique(NCBI_tax.df)
    colnames(NCBI_tax.df) = tolower((colnames(NCBI_tax.df)))

    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS public.NCBI_tax(
        taxonomy_id serial NOT NULL,
        adjusted_Domain_NCBI character varying,
        adjusted_Phylum_NCBI character varying,
        adjusted_Class_NCBI character varying,
        adjusted_Order_NCBI character varying,
        adjusted_Family_NCBI character varying,
        adjusted_Genus_NCBI character varying,
        adjusted_Species_NCBI character varying,
        PRIMARY KEY (taxonomy_id)
      )
    ")

    DBI::dbWriteTable(con, "temp_ncbi_tax", NCBI_tax.df, temporary = TRUE)
    query <- glue::glue("
      INSERT INTO public.NCBI_tax (adjusted_Domain_NCBI, adjusted_Phylum_NCBI, adjusted_Class_NCBI, adjusted_Order_NCBI, adjusted_Family_NCBI, adjusted_Genus_NCBI, adjusted_Species_NCBI)
      SELECT adjusted_Domain_NCBI, adjusted_Phylum_NCBI, adjusted_Class_NCBI, adjusted_Order_NCBI, adjusted_Family_NCBI, adjusted_Genus_NCBI, adjusted_Species_NCBI
      FROM temp_NCBI_tax tnt
      WHERE NOT EXISTS (
        SELECT 1
        FROM public.NCBI_tax
        WHERE COALESCE(adjusted_Domain_NCBI, '') = COALESCE(tnt.adjusted_Domain_NCBI, '') AND
              COALESCE(adjusted_Phylum_NCBI, '') = COALESCE(tnt.adjusted_Phylum_NCBI, '') AND
              COALESCE(adjusted_Class_NCBI, '') = COALESCE(tnt.adjusted_Class_NCBI, '') AND
              COALESCE(adjusted_Order_NCBI, '') = COALESCE(tnt.adjusted_Order_NCBI, '') AND
              COALESCE(adjusted_Family_NCBI, '') = COALESCE(tnt.adjusted_Family_NCBI, '') AND
              COALESCE(adjusted_Genus_NCBI, '') = COALESCE(tnt.adjusted_Genus_NCBI, '') AND
              COALESCE(adjusted_Species_NCBI, '') = COALESCE(tnt.adjusted_Species_NCBI, '')
      )
    ")
    dbExecute(con, query)
    DBI::dbRemoveTable(con, "temp_ncbi_tax")
    print("NCBI taxonomy relation done")
    ########################################################### create NCBI Genbank BLAST relation ###########################################################
    NCBI_gb.df = raw_data[,c("NCBI_Accession_ID", "NCBI_tax_ID", "NCBI_Grade_ID", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI")]
    NCBI_gb.df = NCBI_gb.df[c(c(r_indice_BOLD_Process_ID+1):nrow(NCBI_gb.df)),]
    NCBI_gb.df$`NCBI_Grade_ID` = as.numeric(sub("%", "", NCBI_gb.df$`NCBI_Grade_ID`))/100
    colnames(NCBI_gb.df) = tolower((colnames(NCBI_gb.df)))

    #get taxonomy_id from the databank")
    DBI::dbWriteTable(con, "temp_ncbi_gb", NCBI_gb.df, temporary = TRUE)
    NCBI_tax_wid.df = unique(dbGetQuery(con,"
      SELECT nt.taxonomy_id, nt.adjusted_Domain_NCBI, nt.adjusted_Phylum_NCBI, nt.adjusted_Class_NCBI, nt.adjusted_Order_NCBI, nt.adjusted_Family_NCBI, nt.adjusted_Genus_NCBI, nt.adjusted_Species_NCBI
      FROM NCBI_tax nt
      INNER JOIN temp_NCBI_gb tng ON COALESCE(nt.adjusted_Domain_NCBI, '') = COALESCE(tng.adjusted_Domain_NCBI, '')
                             AND COALESCE(nt.adjusted_Phylum_NCBI, '') = COALESCE(tng.adjusted_Phylum_NCBI, '')
                             AND COALESCE(nt.adjusted_Class_NCBI, '') = COALESCE(tng.adjusted_Class_NCBI, '')
                             AND COALESCE(nt.adjusted_Order_NCBI, '') = COALESCE(tng.adjusted_Order_NCBI, '')
                             AND COALESCE(nt.adjusted_Family_NCBI, '') = COALESCE(tng.adjusted_Family_NCBI, '')
                             AND COALESCE(nt.adjusted_Genus_NCBI, '') = COALESCE(tng.adjusted_Genus_NCBI, '')
                             AND COALESCE(nt.adjusted_Species_NCBI, '') = COALESCE(tng.adjusted_Species_NCBI, '')
    "))
    DBI::dbRemoveTable(con, "temp_ncbi_gb")
    NCBI_gb.df = left_join(NCBI_gb.df, NCBI_tax_wid.df, by = tolower(c("adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI")))

    NCBI_gb.df = NCBI_gb.df[,tolower(c("taxonomy_id", "NCBI_Accession_ID", "NCBI_tax_ID", "NCBI_Grade_ID"))]
    NCBI_gb.df = unique(NCBI_gb.df)

    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS public.NCBI_gb(
        NCBI_gb_id serial NOT NULL,
        taxonomy_id integer NOT NULL,
        NCBI_Accession_ID character varying,
        NCBI_tax_ID character varying,
        NCBI_Grade_ID numeric,
        PRIMARY KEY (NCBI_gb_id),
        CONSTRAINT NCBI_tax_NCBI_gb
          FOREIGN KEY (taxonomy_id) REFERENCES NCBI_tax (taxonomy_id)
      )
    ")

    DBI::dbWriteTable(con, "temp_ncbi_gb", NCBI_gb.df, temporary = TRUE, row.names = FALSE,
                            field.types = list(taxonomy_id = "INTEGER",
                                               ncbi_accession_id = "VARCHAR",
                                               ncbi_tax_id = "VARCHAR",
                                               ncbi_grade_id = "NUMERIC"
                                               ))

    query <- glue::glue("
      INSERT INTO public.NCBI_gb (taxonomy_id, NCBI_Accession_ID, NCBI_tax_ID, NCBI_Grade_ID)
      SELECT taxonomy_id, NCBI_Accession_ID, NCBI_tax_ID, NCBI_Grade_ID
      FROM temp_NCBI_gb tng
      WHERE NOT EXISTS (
        SELECT 1
        FROM public.NCBI_gb
        WHERE taxonomy_id = tng.taxonomy_id AND
              NCBI_Accession_ID = tng.NCBI_Accession_ID AND
              COALESCE(NCBI_tax_ID, '') = COALESCE(tng.NCBI_tax_ID, '') AND
              NCBI_Grade_ID = tng.NCBI_Grade_ID
      )
    ")
    dbExecute(con, query)
    DBI::dbRemoveTable(con, "temp_ncbi_gb")
    print("NCBI genbank BLAST relation done")
    ########################################################### create sample relation ###########################################################
    #read in sample metadata
    sample.df = data.frame()
    start.col = grep("customer_sampleID", raw_data[c(1:r_indice_BOLD_Process_ID),], ignore.case = TRUE)+1
    end.col = grep("NCBI_Accession_ID", raw_data[r_indice_BOLD_Process_ID,], ignore.case = TRUE)-1
    if(length(r_indice_habitat) == 0 && length(r_indice_date) == 0){
      sample.df = as.data.frame(t(raw_data[c(r_indice_customer_sample_id, r_indice_BOLD_Process_ID),start.col: end.col]))
      colnames(sample.df) = c("customer_sample_id", "sample_name")
      sample.df$habitat <- NA_character_
      sample.df$date <- as.Date(NA)
    } else if (length(r_indice_habitat) == 0) {
      sample.df = as.data.frame(t(raw_data[c(r_indice_date, r_indice_customer_sample_id, r_indice_BOLD_Process_ID),start.col: end.col ]))
      colnames(sample.df) = c("date", "customer_sample_id", "sample_name")
      sample.df$habitat <- NA_character_
      sample.df$date = as.numeric(sample.df$date)
      sample.df$date = as.Date(sample.df$date, origin = "1899-12-30")
    } else if (length(r_indice_date) == 0) {
      sample.df = as.data.frame(t(raw_data[c(r_indice_customer_sample_id, r_indice_BOLD_Process_ID, r_indice_habitat),start.col: end.col ]))
      colnames(sample.df) = c("customer_sample_id", "sample_name", "habitat")
      sample.df$date <- as.Date(NA)
    } else {
      sample.df = as.data.frame(t(raw_data[c(r_indice_date, r_indice_customer_sample_id, r_indice_BOLD_Process_ID, r_indice_habitat),start.col: end.col ]))
      colnames(sample.df) = c("date", "customer_sample_id", "sample_name", "habitat")
      sample.df$date = as.numeric(sample.df$date)
      sample.df$date = as.Date(sample.df$date, origin = "1899-12-30")
    }
    sample.df$habitat = as.character(sample.df$habitat)
    sample.df$customer = rep(customer.v, times = nrow(sample.df))
    sample.df$project_name = rep(project.v, times = nrow(sample.df))
    sample.df$location_name = sample.df$customer_sample_id


    DBI::dbWriteTable(con, "temp_sample", sample.df, temporary = TRUE)
    location_wid.df = unique(dbGetQuery(con,"
      SELECT l.location_id, l.location_name, l.habitat
      FROM location l
      INNER JOIN temp_sample ts ON COALESCE(l.location_name, '') = COALESCE(ts.location_name, '')
                             AND COALESCE(l.habitat, '') = COALESCE(ts.habitat, '')
    "))
    sample.df = left_join(sample.df, location_wid.df, by = c("location_name", "habitat"))

    if (length(r_indice_date) == 0) {
      date_wid.df = unique(dbGetQuery(con,"
      SELECT d.date_id, d.date
      FROM date d
      INNER JOIN temp_sample ts ON d.date IS NULL AND ts.date IS NULL
    "))
    } else {
      date_wid.df = unique(dbGetQuery(con,"
      SELECT d.date_id, d.date
      FROM date d
      INNER JOIN temp_sample ts ON d.date = ts.date
    "))
    }
    sample.df = left_join(sample.df, date_wid.df, by = "date")

    project_wid.df = unique(dbGetQuery(con,"
      SELECT p.project_id, p.project_name, p.customer
      FROM project p
      INNER JOIN temp_sample ts ON p.project_name = ts.project_name
                             AND p.customer = ts.customer
    "))
    DBI::dbRemoveTable(con, "temp_sample")
    sample.df = left_join(sample.df, project_wid.df, by = c("project_name", "customer"))

    rownames(sample.df) = NULL
    sample.df = unique(sample.df)
    sample.df = sample.df[,c("date_id", "project_id", "location_id", "sample_name", "customer_sample_id")]

    #creat sample table in data bank
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS public.sample(
        sample_id serial NOT NULL,
        date_id integer NOT NULL,
        project_id integer NOT NULL,
        location_id integer NOT NULL,
        sample_name character varying,
        customer_sample_id character varying,
        PRIMARY KEY (sample_id),
        CONSTRAINT project_sample
          FOREIGN KEY (project_id) REFERENCES project (project_id),
        CONSTRAINT date_sample
          FOREIGN KEY (date_id) REFERENCES date (date_id),
        CONSTRAINT location_sample
          FOREIGN KEY (location_id) REFERENCES location (location_id)
      )
    ")


    DBI::dbWriteTable(con, "temp_sample", sample.df, temporary = TRUE)
    query <- glue::glue("
      INSERT INTO public.sample (date_id, project_id, location_id, sample_name, customer_sample_id)
      SELECT date_id, project_id, location_id, sample_name, customer_sample_id
      FROM temp_sample ts
      WHERE NOT EXISTS (
        SELECT 1
        FROM public.sample
        WHERE date_id = ts.date_id AND
              project_id = ts.project_id AND
              location_id = ts.location_id AND
              sample_name = ts.sample_name AND
              customer_sample_id = ts.customer_sample_id
      )
    ")
    dbExecute(con, query)
    DBI::dbRemoveTable(con, "temp_sample")
    print("sample relation done")

    ########################################################### create seq table ############################################################

    if ("OTU_fasta_sequence" %in% colnames(raw_data)) {
      seq.df = data.frame(raw_data$OTU_fasta_sequence)
      seq.df = as.data.frame(seq.df[c(c(r_indice_BOLD_Process_ID+1):nrow(seq.df)),])
    } else {
      seq.df <- data.frame(otu_fasta_sequence = NA_character_)
    }

    colnames(seq.df) = "otu_fasta_sequence"

    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS public.seq(
        seq_id serial NOT NULL,
        otu_fasta_sequence character varying,
        PRIMARY KEY (seq_id)
      )
    ")

    DBI::dbWriteTable(con, "temp_seq", seq.df, temporary = TRUE)
    query <- glue::glue("
      INSERT INTO public.seq (otu_fasta_sequence)
      SELECT otu_fasta_sequence
      FROM temp_seq ts
      WHERE NOT EXISTS (
        SELECT 1
        FROM public.seq
        WHERE COALESCE(otu_fasta_sequence, '') = COALESCE(ts.otu_fasta_sequence, '')
      )
    ")
    dbExecute(con, query)
    DBI::dbRemoveTable(con, "temp_seq")
    print("seq relation done")
    ########################################################### create reads table ###########################################################
    reads.df = data.frame()
    start.col = grep("customer_sampleID", raw_data[c(1:r_indice_BOLD_Process_ID),], ignore.case = TRUE)+1
    end.col = grep("NCBI_Accession_ID", raw_data[r_indice_BOLD_Process_ID,], ignore.case = TRUE)-1
    reads.df = cbind(raw_data[,c("NCBI_Accession_ID", "NCBI_tax_ID", "NCBI_Grade_ID", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI" , "consensus_Domain", "consensus_Class", "consensus_Order", "consensus_Phylum", "consensus_Family", "consensus_Genus", "consensus_Species", "BOLD_Process_ID", "BOLD_BIN_uri", "BOLD_Grade_ID", "BOLD_HIT_ID","BIN sharing?","BIN species", "adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD")],raw_data[start.col: end.col ])
    reads.df = reads.df[c(c(r_indice_BOLD_Process_ID+1):nrow(reads.df)),]
    reads.df$BOLD_HIT_ID = as.numeric(sub("%", "", reads.df$BOLD_HIT_ID))/100
    reads.df$BOLD_Grade_ID = as.numeric(sub("%", "", reads.df$BOLD_Grade_ID))/100
    reads.df$`NCBI_Grade_ID` = as.numeric(sub("%", "", reads.df$`NCBI_Grade_ID`))/100
    colnames(reads.df)[colnames(reads.df) == "BIN sharing?"] = "BIN_sharing"
    colnames(reads.df)[colnames(reads.df) == "BIN species"] = "BIN_species"
    if ("OTU_fasta_sequence" %in% colnames(raw_data)) {
      reads.df = cbind(reads.df, raw_data[c(c(r_indice_BOLD_Process_ID+1):nrow(raw_data)),"OTU_fasta_sequence"])
      colnames(reads.df)[ncol(reads.df)] <-"OTU_fasta_sequence"
    } else {
      reads.df$OTU_fasta_sequence <- NA_character_
    }
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
    if(length(r_indice_habitat) == 0 && length(r_indice_date) == 0){
      meta_data.df = as.data.frame(t(raw_data[c(r_indice_customer_sample_id),start.col: end.col]))
      colnames(meta_data.df) = c("customer_sample_id")
      meta_data.df$habitat <- NA_character_
      meta_data.df$date <- as.Date(NA)
    } else if (length(r_indice_habitat) == 0) {
      meta_data.df = as.data.frame(t(raw_data[c(r_indice_date, r_indice_customer_sample_id),start.col: end.col ]))
      colnames(meta_data.df) = c("date", "customer_sample_id")
      meta_data.df$habitat <- NA_character_
      meta_data.df$date = as.numeric(meta_data.df$date)
      meta_data.df$date = as.Date(meta_data.df$date, origin = "1899-12-30")
    } else if (length(r_indice_date) == 0) {
      meta_data.df = as.data.frame(t(raw_data[c(r_indice_customer_sample_id, r_indice_habitat),start.col: end.col ]))
      colnames(meta_data.df) = c("customer_sample_id", "habitat")
      meta_data.df$date <- as.Date(NA)
    } else {
      meta_data.df = as.data.frame(t(raw_data[c(r_indice_date, r_indice_customer_sample_id, r_indice_habitat),start.col: end.col ]))
      colnames(meta_data.df) = c("date", "customer_sample_id", "habitat")
      meta_data.df$date = as.numeric(meta_data.df$date)
      meta_data.df$date = as.Date(meta_data.df$date, origin = "1899-12-30")
    }
    meta_data.df = meta_data.df %>%
      rownames_to_column(var = "sample_name")

    reads.df = left_join(reads.df, meta_data.df, by = "sample_name")
    reads.df$BOLD_HIT_ID <- signif(reads.df$BOLD_HIT_ID, 4)
    reads.df$BOLD_Grade_ID <- signif(reads.df$BOLD_Grade_ID, 4)
    reads.df$NCBI_Grade_ID <- signif(reads.df$NCBI_Grade_ID, 4)

    reads.df = reads.df %>%
      filter(.data$abs_reads != 0)
    colnames(reads.df) = tolower((colnames(reads.df)))


    ##########################################################################################
    DBI::dbWriteTable(con, "temp_reads", reads.df, temporary = TRUE)
    BOLD_db_wid.df = unique(dbGetQuery(con,"
      SELECT bd.BOLD_db_id, bd.BOLD_tax_id, bd.BOLD_Process_ID, bd.BOLD_BIN_uri, bd.BOLD_Grade_ID, bd.BOLD_HIT_ID, bd.BIN_sharing, bd.BIN_species
      FROM BOLD_db bd
      INNER JOIN temp_reads tr ON bd.BOLD_Process_ID = tr.BOLD_Process_ID
                             AND COALESCE(bd.BOLD_BIN_uri, '') = COALESCE(tr.BOLD_BIN_uri, '')
                             AND bd.BOLD_Grade_ID = tr.BOLD_Grade_ID
                             AND bd.BOLD_HIT_ID = tr.BOLD_HIT_ID
                             AND COALESCE(bd.BIN_sharing, '') = COALESCE(tr.BIN_sharing, '')
                             AND COALESCE(bd.BIN_species, '') = COALESCE(tr.BIN_species, '')
    "))

    BOLD_tax_wid.df = unique(dbGetQuery(con,"
      SELECT bt.BOLD_tax_id, bt.adjusted_Phylum_BOLD, bt.adjusted_Class_BOLD, bt.adjusted_Order_BOLD, bt.adjusted_Family_BOLD, bt.adjusted_Genus_BOLD, bt.adjusted_Species_BOLD
      FROM BOLD_tax bt
      INNER JOIN temp_reads tbd ON COALESCE(bt.adjusted_Phylum_BOLD, '') = COALESCE(tbd.adjusted_Phylum_BOLD, '')
                             AND COALESCE(bt.adjusted_Class_BOLD, '') = COALESCE(tbd.adjusted_Class_BOLD, '')
                             AND COALESCE(bt.adjusted_Order_BOLD, '') = COALESCE(tbd.adjusted_Order_BOLD, '')
                             AND COALESCE(bt.adjusted_Family_BOLD, '') = COALESCE(tbd.adjusted_Family_BOLD, '')
                             AND COALESCE(bt.adjusted_Genus_BOLD, '') = COALESCE(tbd.adjusted_Genus_BOLD, '')
                             AND COALESCE(bt.adjusted_Species_BOLD, '') = COALESCE(tbd.adjusted_Species_BOLD, '')
    "))
    compare.df = full_join(BOLD_db_wid.df, BOLD_tax_wid.df, by = c("bold_tax_id"))
    reads.df = left_join(reads.df, compare.df[,tolower(c("BOLD_db_id","BOLD_BIN_uri","BOLD_Process_ID", "BOLD_Grade_ID", "BOLD_HIT_ID","BIN_sharing","BIN_species", "adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD"))], by = tolower(c("BOLD_BIN_uri","BOLD_Process_ID", "BOLD_Grade_ID", "BOLD_HIT_ID", "BIN_sharing","BIN_species", "adjusted_Phylum_BOLD", "adjusted_Class_BOLD", "adjusted_Order_BOLD", "adjusted_Family_BOLD", "adjusted_Genus_BOLD", "adjusted_Species_BOLD")))
    ##########################################################################################

    NCBI_gb_wid.df = unique(dbGetQuery(con,"
      SELECT ng.NCBI_gb_id, ng.taxonomy_id, ng.NCBI_Accession_ID, ng.NCBI_tax_ID, ng.NCBI_Grade_ID
      FROM NCBI_gb ng
      INNER JOIN temp_reads tr ON ng.NCBI_Accession_ID = tr.NCBI_Accession_ID
                             AND COALESCE(ng.NCBI_tax_ID, '') = COALESCE(tr.NCBI_tax_ID, '')
                             AND ng.NCBI_Grade_ID = tr.NCBI_Grade_ID
    "))
    NCBI_tax_wid.df = unique(dbGetQuery(con,"
      SELECT nt.taxonomy_id, nt.adjusted_Domain_NCBI, nt.adjusted_Phylum_NCBI, nt.adjusted_Class_NCBI, nt.adjusted_Order_NCBI, nt.adjusted_Family_NCBI, nt.adjusted_Genus_NCBI, nt.adjusted_Species_NCBI
      FROM NCBI_tax nt
      INNER JOIN temp_reads tr ON COALESCE(nt.adjusted_Domain_NCBI, '') = COALESCE(tr.adjusted_Domain_NCBI, '')
                             AND COALESCE(nt.adjusted_Phylum_NCBI, '') = COALESCE(tr.adjusted_Phylum_NCBI, '')
                             AND COALESCE(nt.adjusted_Class_NCBI, '') = COALESCE(tr.adjusted_Class_NCBI, '')
                             AND COALESCE(nt.adjusted_Order_NCBI, '') = COALESCE(tr.adjusted_Order_NCBI, '')
                             AND COALESCE(nt.adjusted_Family_NCBI, '') = COALESCE(tr.adjusted_Family_NCBI, '')
                             AND COALESCE(nt.adjusted_Genus_NCBI, '') = COALESCE(tr.adjusted_Genus_NCBI, '')
                             AND COALESCE(nt.adjusted_Species_NCBI, '') = COALESCE(tr.adjusted_Species_NCBI, '')
    "))
    compare.df = full_join(NCBI_gb_wid.df, NCBI_tax_wid.df, by = c("taxonomy_id"))
    reads.df = left_join(reads.df, compare.df[,tolower(c("NCBI_gb_id", "NCBI_Grade_ID","NCBI_Accession_ID", "NCBI_tax_ID", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI"))], by = tolower(c( "NCBI_Accession_ID", "NCBI_Grade_ID", "NCBI_tax_ID", "adjusted_Domain_NCBI", "adjusted_Phylum_NCBI", "adjusted_Class_NCBI", "adjusted_Order_NCBI", "adjusted_Family_NCBI", "adjusted_Genus_NCBI", "adjusted_Species_NCBI")))
    ###########################################################################################
    ct_wid.df = unique(dbGetQuery(con,"
      SELECT ct.ct_id, ct.consensus_Domain, ct.consensus_Phylum, ct.consensus_Class, ct.consensus_Order, ct.consensus_Family, ct.consensus_Genus, ct.consensus_Species
      FROM consensus_taxonomy ct
      INNER JOIN temp_reads tr ON COALESCE(ct.consensus_Domain, '') = COALESCE(tr.consensus_Domain, '')
                             AND COALESCE(ct.consensus_Phylum, '') = COALESCE(tr.consensus_Phylum, '')
                             AND COALESCE(ct.consensus_Class, '') = COALESCE(tr.consensus_Class, '')
                             AND COALESCE(ct.consensus_Order, '') = COALESCE(tr.consensus_Order, '')
                             AND COALESCE(ct.consensus_Family, '') = COALESCE(tr.consensus_Family, '')
                             AND COALESCE(ct.consensus_Genus, '') = COALESCE(tr.consensus_Genus, '')
                             AND COALESCE(ct.consensus_Species, '') = COALESCE(tr.consensus_Species, '')
    "))
    reads.df = left_join(reads.df, ct_wid.df[,tolower(c("ct_id","consensus_Domain", "consensus_Class", "consensus_Order", "consensus_Phylum", "consensus_Family", "consensus_Genus", "consensus_Species"))], by = tolower(c("consensus_Domain", "consensus_Class", "consensus_Order", "consensus_Phylum", "consensus_Family", "consensus_Genus", "consensus_Species")))
    ###########################################################################################
    seq_wid.df = unique(dbGetQuery(con,"
                                   SELECT s.seq_id, s.otu_fasta_sequence
                                   FROM seq s
                                   INNER JOIN temp_reads tr ON COALESCE(s.otu_fasta_sequence, '') = COALESCE(tr.otu_fasta_sequence, '')
                                   "))
    reads.df = left_join(reads.df, seq_wid.df[,c("seq_id", "otu_fasta_sequence")], by = c("otu_fasta_sequence"))
    ###########################################################################################
    sample_date_wid.df = unique(dbGetQuery(con, "
      SELECT s.sample_id, s.sample_name, s.customer_sample_id, date.date_id, date.date, project.project_id, project.project_name, project.customer, location.location_id, location.habitat
      FROM sample s
      INNER JOIN date ON s.date_id = date.date_id
      INNER JOIN project ON s.project_id = project.project_id
      INNER JOIN location ON s.location_id = location.location_id
      INNER JOIN temp_reads tr ON s.sample_name = tr.sample_name
                             AND s.customer_sample_id = tr.customer_sample_id
    "))
    DBI::dbRemoveTable(con, "temp_reads")
    reads.df = left_join(reads.df, sample_date_wid.df[,c("sample_id", "sample_name", "customer_sample_id", "date", "project_name", "customer", "habitat")], by = c("sample_name", "customer_sample_id", "date", "project_name", "customer", "habitat"))
    rownames(reads.df) = NULL
    reads.df = reads.df[,tolower(c("sample_id", "BOLD_db_id", "NCBI_gb_id","ct_id","seq_id", "abs_reads", "norm_reads"))]

    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS public.reads (
        reads_id serial NOT NULL,
        sample_id integer NOT NULL,
        bold_db_id integer NOT NULL,
        ncbi_gb_id integer NOT NULL,
        ct_id integer NOT NULL,
        seq_id integer NOT NULL,
        abs_reads integer,
        norm_reads numeric,
        PRIMARY KEY (reads_id),
        CONSTRAINT bold_id
          FOREIGN KEY (bold_db_id) REFERENCES public.bold_db (bold_db_id),
        CONSTRAINT ncbi_id
          FOREIGN KEY (ncbi_gb_id) REFERENCES public.ncbi_gb (ncbi_gb_id),
        CONSTRAINT consensus_taxonomy_reads_fk
          FOREIGN KEY (ct_id) REFERENCES public.consensus_taxonomy (ct_id),
        CONSTRAINT sample_id
          FOREIGN KEY (sample_id) REFERENCES public.sample (sample_id),
        CONSTRAINT seq_id
          FOREIGN KEY (seq_id) REFERENCES public.seq (seq_id)
      )
    ")

    DBI::dbWriteTable(con, "temp_reads", reads.df, temporary = TRUE)
    query <- glue::glue("
      INSERT INTO public.reads (sample_id, BOLD_db_id, NCBI_gb_id,ct_id, seq_id, abs_reads, norm_reads)
      SELECT sample_id, BOLD_db_id, NCBI_gb_id, ct_id, seq_id, abs_reads, norm_reads
      FROM temp_reads tr
      WHERE NOT EXISTS (
        SELECT 1
        FROM public.reads
        WHERE sample_id = tr.sample_id AND
              BOLD_db_id = tr.BOLD_db_id AND
              NCBI_gb_id = tr.NCBI_gb_id AND
              ct_id = tr.ct_id AND
              seq_id = tr.seq_id AND
              abs_reads = tr.abs_reads AND
              norm_reads = tr.norm_reads
        )
    ")
    dbExecute(con, query)
    DBI::dbRemoveTable(con, "temp_reads")
    print("reads relation done")


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
