# Example Shiny app using standalone BOLD API functions
# No BOLDconnectR dependency required.
#
# Required packages: shiny, httr, jsonlite, dplyr, data.table, DT
#
# Run with: shiny::runApp("inst/shiny-standalone")

library(shiny)
library(DT)

source("bold_api_functions.R")

ui <- fluidPage(
  titlePanel("BOLD Database Search"),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      h4("Public Search"),
      textAreaInput("species", "Species (one per line)",
                    value = "Panthera leo\nPanthera uncia\nPanthera tigris",
                    rows = 5),
      textInput("geography", "Geography filter (optional)",
                placeholder = "e.g. India"),
      actionButton("search_btn", "Search BOLD", class = "btn-primary"),

      hr(),

      h4("Fetch Full BCDM Data"),
      textInput("api_key", "BOLD API Key", placeholder = "Your API key"),
      actionButton("fetch_btn", "Fetch BCDM Data", class = "btn-success"),

      hr(),

      h4("Export"),
      downloadButton("download_btn", "Download CSV")
    ),

    mainPanel(
      width = 8,
      verbatimTextOutput("status"),
      DT::dataTableOutput("results_table")
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(data = NULL, status = "Ready.")

  # --- Public Search (batch, per-species) ---
  observeEvent(input$search_btn, {
    species_list <- trimws(unlist(strsplit(input$species, "\n")))
    species_list <- species_list[nchar(species_list) > 0]

    if (length(species_list) == 0) {
      rv$status <- "Please enter at least one species name."
      return()
    }

    geo <- if (nchar(trimws(input$geography)) > 0) list(trimws(input$geography)) else NULL

    rv$status <- paste0("Searching for ", length(species_list), " species...")

    withProgress(message = "Searching BOLD...", value = 0, {
      results <- bold_public_search_batch(
        species_list = species_list,
        geography = geo,
        quiet = FALSE
      )
    })

    rv$data <- results$data
    rv$status <- results$summary
  })

  # --- Fetch full BCDM data ---
  observeEvent(input$fetch_btn, {
    if (is.null(rv$data)) {
      rv$status <- "Run a search first to get processids."
      return()
    }
    if (nchar(trimws(input$api_key)) == 0) {
      rv$status <- "Please enter your BOLD API key."
      return()
    }

    rv$status <- paste0("Fetching BCDM data for ", nrow(rv$data), " records...")

    withProgress(message = "Fetching BCDM data...", value = 0, {
      tryCatch({
        data <- bold_fetch(
          api_key = trimws(input$api_key),
          get_by = "processid",
          identifiers = rv$data$processid
        )
        rv$data <- data
        rv$status <- paste0("BCDM data retrieved: ", nrow(data), " records, ",
                            ncol(data), " fields.")
      }, error = function(e) {
        rv$status <- paste0("Fetch failed: ", e$message)
      })
    })
  })

  # --- Results table ---
  output$results_table <- DT::renderDataTable({
    req(rv$data)
    DT::datatable(rv$data,
                  options = list(scrollX = TRUE, pageLength = 25),
                  rownames = FALSE)
  })

  # --- Status ---
  output$status <- renderText({
    rv$status
  })

  # --- CSV download ---
  output$download_btn <- downloadHandler(
    filename = function() {
      paste0("bold_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$data)
      write.csv(rv$data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
