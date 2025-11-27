library(shiny)
library(bslib)
library(PubMatrixR)

# UI
ui <- page_fillable(
  theme = bs_theme(preset = "flatly"),
  
  div(
    style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 20px; padding: 20px; background-color: #f8f9fa; border-radius: 8px;",
    h1("PubMatrix - Literature Co-occurrence Analysis", style = "margin: 0; flex: 1;"),
    img(
      src = "https://toledoem.github.io/img/LogoPubmatrix.png",
      width = "150px",
      style = "margin-left: 20px;"
    )
  ),
  
  layout_sidebar(
    # Sidebar with inputs
    sidebar = sidebar(
      # API Key Input (Required)
      textInput(
        "api_key",
        label = "NCBI API Key (Required)",
        placeholder = "Enter your NCBI API key here"
      ),
      
      helpText(
        HTML(
          "Get a free API key at ",
          "<a href='https://support.nlm.nih.gov/kbArticle/?pn=KA-05317' target='_blank'>",
          "NCBI API Key Management</a>"
        )
      ),
      
      hr(),
      
      # List A Input
      h5("List A (First set of search terms)"),
      textAreaInput(
        "list_a",
        label = NULL,
        placeholder = "Enter search terms, one per line\nExample:\nWNT1\nWNT2\nWNT3",
        rows = 6,
        width = "100%"
      ),
      
      hr(),
      
      # List B Input
      h5("List B (Second set of search terms)"),
      textAreaInput(
        "list_b",
        label = NULL,
        placeholder = "Enter search terms, one per line\nExample:\nFZD1\nFZD2\nFZD3",
        rows = 6,
        width = "100%"
      ),
      
      hr(),
      
      # Database Selection
      radioButtons(
        "database",
        label = "Select Database",
        choices = c("PubMed" = "pubmed", "PMC" = "pmc"),
        selected = "pubmed"
      ),
      
      # Date Range
      h5("Date Range (Optional)"),
      numericInput(
        "start_year",
        label = "Start Year",
        value = 2020,
        min = 1900,
        max = as.numeric(format(Sys.Date(), "%Y"))
      ),
      numericInput(
        "end_year",
        label = "End Year",
        value = as.numeric(format(Sys.Date(), "%Y")),
        min = 1900,
        max = as.numeric(format(Sys.Date(), "%Y"))
      ),
      
      hr(),
      
      # Submit Button
      actionButton(
        "run_analysis",
        label = "Run Analysis",
        class = "btn-primary",
        width = "100%"
      ),
      
      width = 350
    ),
    
    # Main content area
    navset_card_tab(
      title = "Results",
      nav_panel(
        "Heatmap",
        div(
          style = "padding: 20px;",
          plotOutput("heatmap_plot", height = "600px"),
          downloadButton("download_heatmap", "Download Heatmap (PNG)", class = "btn-success")
        )
      ),
      nav_panel(
        "Results Table",
        div(
          style = "padding: 20px;",
          tableOutput("results_table"),
          downloadButton("download_csv", "Download Results (CSV)", class = "btn-success")
        )
      ),
      nav_panel(
        "Links",
        div(
          style = "padding: 20px;",
          htmlOutput("links_table"),
          downloadButton("download_links_csv", "Download Links (CSV)", class = "btn-success")
        )
      ),
      nav_panel(
        "Status",
        div(
          style = "padding: 20px;",
          verbatimTextOutput("status_log")
        )
      )
    )
  ),
  
  # Footer with citation and repo link
  hr(),
  div(
    style = "padding: 20px; text-align: center; color: #666; font-size: 0.9em;",
    p(
      "Based on: ",
      a("PubMatrix: a tool for multiplex literature mining", 
        href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC317283/",
        target = "_blank"),
      " - Becker KG et al. BMC Bioinformatics. 2003 Dec 10;4:61."
    ),
    p(
      "Repository: ",
      a("https://github.com/ToledoEM/PubMatrixR-v2", 
        href = "https://github.com/ToledoEM/PubMatrixR-v2",
        target = "_blank")
    ),
    p("Get your free NCBI API key: ",
      a("NCBI API Key Management", 
        href = "https://support.nlm.nih.gov/kbArticle/?pn=KA-05317",
        target = "_blank")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store results
  results <- reactiveValues(
    matrix_result = NULL,
    search_details = NULL,
    status_message = "Waiting for analysis to run..."
  )
  
  # Parse input lists
  parse_list <- function(text) {
    if (text == "" || is.null(text)) {
      return(character(0))
    }
    terms <- strsplit(text, "\n")[[1]]
    terms <- trimws(terms)
    terms <- terms[terms != ""]
    return(terms)
  }
  
  # Run PubMatrix Analysis
  observeEvent(input$run_analysis, {
    # Validate inputs
    if (input$api_key == "" || is.null(input$api_key)) {
      results$status_message <- "ERROR: Please enter your NCBI API key"
      showNotification("Please enter your NCBI API key", type = "error")
      return()
    }
    
    list_a <- parse_list(input$list_a)
    list_b <- parse_list(input$list_b)
    
    if (length(list_a) == 0) {
      results$status_message <- "ERROR: List A is empty"
      showNotification("Please enter at least one term in List A", type = "error")
      return()
    }
    
    if (length(list_b) == 0) {
      results$status_message <- "ERROR: List B is empty"
      showNotification("Please enter at least one term in List B", type = "error")
      return()
    }
    
    if (input$start_year > input$end_year) {
      results$status_message <- "ERROR: Start year must be before end year"
      showNotification("Start year must be before end year", type = "error")
      return()
    }
    
    # Show progress notification
    withProgress(
      message = "Running PubMatrix Analysis",
      detail = "Searching NCBI databases...",
      value = 0,
      {
        tryCatch(
          {
            # Run PubMatrix
            results$status_message <- paste(
              "Starting analysis...",
              "List A terms:", length(list_a),
              "List B terms:", length(list_b),
              "Total searches:", length(list_a) * length(list_b),
              sep = "\n"
            )
            
            incProgress(0.3)
            
            matrix_result <- PubMatrix(
              A = list_a,
              B = list_b,
              API.key = input$api_key,
              Database = input$database,
              daterange = c(input$start_year, input$end_year),
              outfile = NULL
            )
            
            incProgress(0.7)
            
            # Store results
            results$matrix_result <- matrix_result
            results$search_details <- data.frame(
              A_Terms = list_a,
              B_Terms = list_b,
              Database = input$database,
              Start_Year = input$start_year,
              End_Year = input$end_year
            )
            
            results$status_message <- paste(
              "SUCCESS: Analysis completed!",
              "Matrix dimensions:", nrow(matrix_result), "x", ncol(matrix_result),
              "Total publications found:", sum(matrix_result),
              sep = "\n"
            )
            
            incProgress(1.0)
            showNotification("Analysis completed successfully!", type = "message")
          },
          error = function(e) {
            results$status_message <- paste(
              "ERROR:",
              conditionMessage(e)
            )
            showNotification(
              paste("Error:", conditionMessage(e)),
              type = "error",
              duration = NULL
            )
          }
        )
      }
    )
  })
  
  # Display status
  output$status_log <- renderText({
    results$status_message
  })
  
  # Display heatmap
  output$heatmap_plot <- renderPlot({
    if (is.null(results$matrix_result)) {
      plot(0, 0, type = "n", xlab = "", ylab = "", main = "No data available")
      text(0.5, 0.5, "Run analysis to generate heatmap", cex = 1.2)
      return()
    }
    
    tryCatch(
      {
        plot_pubmatrix_heatmap(
          results$matrix_result,
          title = paste(
            "PubMatrix Co-occurrence Heatmap",
            paste(input$start_year, "-", input$end_year),
            sep = "\n"
          ),
          cluster_rows = TRUE,
          cluster_cols = TRUE,
          show_numbers = TRUE
        )
      },
      error = function(e) {
        plot(0, 0, type = "n", xlab = "", ylab = "", main = "Error creating heatmap")
        text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 0.8)
      }
    )
  })
  
  # Display results table
  output$results_table <- renderTable({
    if (is.null(results$matrix_result)) {
      return(data.frame(Message = "Run analysis to see results"))
    }
    
    df <- as.data.frame(results$matrix_result)
    rownames(df) <- rownames(results$matrix_result)
    df
  }, rownames = TRUE)
  
  # Display links table
  output$links_table <- renderText({
    if (is.null(results$matrix_result)) {
      return("<p>Run analysis to see links</p>")
    }
    
    tryCatch(
      {
        matrix_result <- results$matrix_result
        list_a <- parse_list(input$list_a)
        list_b <- parse_list(input$list_b)
        
        # Create HTML table with links
        html <- "<table class='table table-striped'><thead><tr><th>Term A</th><th>Term B</th><th>Publications</th><th>Link</th></tr></thead><tbody>"
        
        for (i in seq_along(list_b)) {
          for (j in seq_along(list_a)) {
            count <- matrix_result[i, j]
            term_a <- list_a[j]
            term_b <- list_b[i]
            
            # Create search URL
            search_query <- paste(term_a, "AND", term_b)
            search_query <- gsub(" ", "+", search_query)
            
            if (input$database == "pubmed") {
              url <- paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", search_query)
            } else {
              url <- paste0("https://www.ncbi.nlm.nih.gov/pmc/?term=", search_query)
            }
            
            html <- paste0(
              html,
              "<tr><td>", term_a, "</td><td>", term_b, "</td><td>", count, "</td>",
              "<td><a href='", url, "' target='_blank'>View on NCBI</a></td></tr>"
            )
          }
        }
        
        html <- paste0(html, "</tbody></table>")
        HTML(html)
      },
      error = function(e) {
        HTML(paste("<p class='text-danger'>Error generating links:", conditionMessage(e), "</p>"))
      }
    )
  })
  
  # Download heatmap
  output$download_heatmap <- downloadHandler(
    filename = function() {
      paste0("PubMatrix_heatmap_", Sys.Date(), ".png")
    },
    content = function(file) {
      if (is.null(results$matrix_result)) {
        showNotification("No data to download", type = "error")
        return()
      }
      
      tryCatch(
        {
          png(file, width = 1000, height = 800)
          plot_pubmatrix_heatmap(
            results$matrix_result,
            title = paste(
              "PubMatrix Co-occurrence Heatmap",
              paste(input$start_year, "-", input$end_year),
              sep = "\n"
            ),
            cluster_rows = TRUE,
            cluster_cols = TRUE,
            show_numbers = TRUE
          )
          dev.off()
        },
        error = function(e) {
          showNotification(paste("Error saving heatmap:", conditionMessage(e)), type = "error")
        }
      )
    }
  )
  
  # Download CSV results
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("PubMatrix_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (is.null(results$matrix_result)) {
        showNotification("No data to download", type = "error")
        return()
      }
      
      write.csv(results$matrix_result, file)
    }
  )
  
  # Download links CSV
  output$download_links_csv <- downloadHandler(
    filename = function() {
      paste0("PubMatrix_links_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (is.null(results$matrix_result)) {
        showNotification("No data to download", type = "error")
        return()
      }
      
      tryCatch(
        {
          matrix_result <- results$matrix_result
          list_a <- parse_list(input$list_a)
          list_b <- parse_list(input$list_b)
          
          # Create dataframe with links
          links_df <- data.frame(
            Term_A = character(),
            Term_B = character(),
            Publications = numeric(),
            NCBI_Link = character(),
            stringsAsFactors = FALSE
          )
          
          for (i in seq_along(list_b)) {
            for (j in seq_along(list_a)) {
              count <- matrix_result[i, j]
              term_a <- list_a[j]
              term_b <- list_b[i]
              
              # Create search URL
              search_query <- paste(term_a, "AND", term_b)
              search_query <- gsub(" ", "+", search_query)
              
              if (input$database == "pubmed") {
                url <- paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", search_query)
              } else {
                url <- paste0("https://www.ncbi.nlm.nih.gov/pmc/?term=", search_query)
              }
              
              links_df <- rbind(
                links_df,
                data.frame(
                  Term_A = term_a,
                  Term_B = term_b,
                  Publications = count,
                  NCBI_Link = url,
                  stringsAsFactors = FALSE
                )
              )
            }
          }
          
          write.csv(links_df, file, row.names = FALSE)
        },
        error = function(e) {
          showNotification(paste("Error saving links:", conditionMessage(e)), type = "error")
        }
      )
    }
  )
}

# Run app
shinyApp(ui, server)
