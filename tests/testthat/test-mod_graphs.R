testServer(
  mod_graphs_server,
  # Add here your module params
  args = list()
  , {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
#    session$setInputs(get_graph = TRUE)
#    session$setInputs(plot_type = "Bar chart")
#    session$setInputs(filter_taxon_id = TRUE)
#    session$setInputs(filter_term = "Cecidomyiidae, Noctuidae")
#    stopifnot(
#      filter_values() == c("Cecidomyiidae", "Noctuidae")
#    )
    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
})

test_that("module ui works", {
  ui <- mod_graphs_ui(id = "test")
  golem::expect_shinytag(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_graphs_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

