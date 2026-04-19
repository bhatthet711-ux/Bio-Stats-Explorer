# ==============================================================================
# THE BIO-STATS EXPLORER
# A Single-File Shiny Application for Interactive Biological Data Analysis
# ==============================================================================
# HOW TO RUN THIS APP:
#   1. Make sure you have R and RStudio installed.
#   2. Install the required packages by running this in your R console:
#      install.packages(c("shiny", "dplyr", "ggplot2", "DT", "bslib"))
#   3. Open this file (app.R) in RStudio and click the "Run App" button.
# ==============================================================================


# --- LOAD LIBRARIES -----------------------------------------------------------
# We load every package we need at the top. Think of these as "toolboxes"
# that give us access to special functions.

library(shiny)   # The core framework for building the interactive web app
library(dplyr)   # For clean, readable data manipulation (filtering, summarizing)
library(ggplot2) # For creating beautiful, publication-quality plots
library(DT)      # For rendering interactive, sortable, searchable data tables
library(bslib)   # For applying a modern Bootstrap theme to our UI


# ==============================================================================
# SECTION 1: USER INTERFACE (UI)
# ==============================================================================
# The UI function defines everything the USER SEES in the browser.
# Think of it as the "blueprint" or "layout" of your app.
# We use bslib's `page_sidebar()` for a clean, modern two-column layout.

ui <- page_sidebar(

  # --- App Title & Theme ---
  title = "The Bio-Stats Explorer",

  # `bs_theme()` from bslib applies a professional visual style.
  # `bootswatch` themes are pre-built color schemes. Try: "flatly", "cosmo", "lux"
  theme = bs_theme(
    bootswatch  = "flatly",   # A clean, professional green/teal theme
    base_font   = font_google("Inter"),  # A modern, readable font from Google
    heading_font = font_google("Inter"),
    primary     = "#2E8B57",  # Sea Green - our main accent color
    secondary   = "#5F9EA0"   # Cadet Blue - a complementary accent
  ),

  # =============================================================================
  # SIDEBAR PANEL (Left Column - Contains all the INPUTS)
  # =============================================================================
  sidebar = sidebar(
    width = 300, # Width of the sidebar in pixels

    # --- Section Header ---
    tags$h5(
      "Analysis Controls",
      style = "color: #2E8B57; font-weight: 700; margin-bottom: 15px;"
    ),

    tags$hr(), # A horizontal dividing line

    # --- INPUT 1: File Upload ---
    # `fileInput()` creates a "Browse..." button for uploading files.
    # `accept = ".csv"` tells the browser to only show CSV files.
    fileInput(
      inputId  = "uploaded_file",   # Unique ID - we use this in the server to access the file
      label    = "Upload Your Dataset (.csv)",
      accept   = ".csv",
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    ),

    tags$hr(),

    # --- INPUT 2: Dynamic Dropdown A (Numeric Variable) ---
    # `uiOutput()` is a PLACEHOLDER. It tells Shiny: "Render this UI element
    # dynamically from the server once we have data to work with."
    # This is how we make the dropdown populate with actual column names.
    uiOutput("ui_numeric_var"),

    tags$br(), # A small vertical spacer

    # --- INPUT 3: Dynamic Dropdown B (Categorical Variable) ---
    # Same concept as above - a server-rendered dynamic dropdown.
    uiOutput("ui_categorical_var"),

    tags$hr(),

    # --- Info Box at the bottom of the sidebar ---
    div(
      style = "background-color: #e8f5e9; border-left: 4px solid #2E8B57;
               padding: 10px 12px; border-radius: 4px; font-size: 0.82em;
               color: #333;",
      tags$b("Tip:"),
      " Upload any CSV with numeric columns (e.g., gene expression,
       measurements, concentrations) to get started."
    )

  ), # End of sidebar


  # =============================================================================
  # MAIN PANEL (Right Column - Contains all the OUTPUTS / Results)
  # =============================================================================
  # We use `navset_card_tab()` to create a tabbed interface inside a card.
  navset_card_tab(

    # ---- TAB 1: RAW DATA -------------------------------------------------------
    nav_panel(
      title = "Raw Data",

      # `uiOutput("table_or_message")` is another placeholder.
      # The server will render EITHER the data table OR a welcome message here.
      uiOutput("table_or_message")
    ),

    # ---- TAB 2: SUMMARY STATISTICS --------------------------------------------
    nav_panel(
      title = "Summary Statistics",
      uiOutput("summary_or_message")
    ),

    # ---- TAB 3: VISUALIZATIONS ------------------------------------------------
    nav_panel(
      title = "Visualizations",
      uiOutput("plots_or_message")
    )

  ) # End of navset_card_tab

) # End of page_sidebar (UI)


# ==============================================================================
# SECTION 2: SERVER LOGIC
# ==============================================================================
# The server function contains all the REACTIVE LOGIC - the code that runs
# when a user interacts with the app (uploads a file, changes a dropdown, etc.).
#
# Key concept: `input$<id>` reads a value from a UI input widget.
#              `output$<id>` sends a rendered result to a UI output placeholder.
#              `reactive({})` creates a "reactive expression" - a cached,
#              re-usable chunk of code that automatically re-runs whenever
#              any of the `input$` values it depends on change.

server <- function(input, output, session) {


  # --- REACTIVE: Load and Validate Uploaded Data -------------------------------
  # This reactive block reads the uploaded CSV file.
  # It returns NULL if no file has been uploaded yet (preventing crashes).
  # Any other part of the server that needs the data calls `loaded_data()`.

  loaded_data <- reactive({

    # `input$uploaded_file` is NULL until the user uploads something.
    # `req()` is short for "require" - it STOPS execution silently if the
    # value is NULL/empty. This is our primary crash-prevention mechanism.
    req(input$uploaded_file)

    # `read.csv()` reads the file the user uploaded into a data frame (a table).
    # `input$uploaded_file$datapath` is the temporary path Shiny saves the file to.
    df <- read.csv(input$uploaded_file$datapath, stringsAsFactors = FALSE)

    return(df) # Return the data frame so other reactives can use it
  })


  # --- REACTIVE UI: Render the Numeric Variable Dropdown ----------------------
  # `renderUI()` generates UI elements dynamically from the server.
  # This runs AFTER data is loaded, so it can use actual column names.

  output$ui_numeric_var <- renderUI({

    req(loaded_data()) # Only run if data is available

    df <- loaded_data()

    # Find columns that contain numbers (numeric or integer type)
    numeric_cols <- names(df)[sapply(df, is.numeric)]

    # `selectInput()` creates a standard dropdown menu.
    selectInput(
      inputId  = "numeric_var",
      label    = "Select Numeric Variable",
      choices  = numeric_cols,
      selected = numeric_cols[1] # Pre-select the first numeric column
    )
  })


  # --- REACTIVE UI: Render the Categorical Variable Dropdown ------------------

  output$ui_categorical_var <- renderUI({

    req(loaded_data())

    df <- loaded_data()

    # Find columns that are character or factor type (categorical)
    cat_cols <- names(df)[sapply(df, function(col) is.character(col) | is.factor(col))]

    # We add "None" as the first option, making this dropdown truly optional.
    choices_with_none <- c("None (no grouping)" = "none", cat_cols)

    selectInput(
      inputId  = "categorical_var",
      label    = "Group By (Categorical) — Optional",
      choices  = choices_with_none,
      selected = "none"
    )
  })


  # ===========================================================================
  # HELPER: A Reusable Welcome / No-Data Message
  # ===========================================================================
  # This is a regular R function (not reactive) that returns a styled HTML
  # message. We call it from multiple outputs to avoid repeating code.

  welcome_message <- function() {
    div(
      tags$h4("No Data Loaded Yet", style = "color: #555; font-weight: 600;"),
      tags$p(
        "Please upload a biological dataset (CSV) in the sidebar to begin.",
        style = "font-size: 1em; max-width: 380px;"
      )
    )
  }


  # ===========================================================================
  # TAB 1 OUTPUT: Raw Data Table (or Welcome Message)
  # ===========================================================================

  output$table_or_message <- renderUI({

    # If no data is loaded, show the friendly welcome message.
    if (is.null(loaded_data())) {
      return(welcome_message())
    }

    # Otherwise, render the interactive DT table.
    # `DTOutput()` is the placeholder, `renderDT()` fills it.
    # We nest renderDT inside renderUI to keep the logic in one place.
    tagList(
      tags$h5(
        paste("Showing", nrow(loaded_data()), "rows ×",
              ncol(loaded_data()), "columns"),
        style = "color: #555; margin-bottom: 10px; font-size: 0.9em;"
      ),
      DTOutput("raw_data_table")
    )
  })

  # The actual DT rendering is separate because DTOutput needs renderDT.
  output$raw_data_table <- renderDT({

    req(loaded_data())

    datatable(
      loaded_data(),
      options = list(
        pageLength  = 15,         # Show 15 rows per page by default
        scrollX     = TRUE,       # Enable horizontal scrolling for wide tables
        dom         = 'Bfrtip',   # Show Buttons, Filter, Records, Table, Info, Pager
        buttons     = c('csv', 'excel'), # Download buttons
        autoWidth   = TRUE
      ),
      extensions = 'Buttons',   # Enable the download buttons extension
      rownames    = FALSE,        # Don't show row numbers on the left
      class       = 'stripe hover cell-border' # Nice alternating row styling
    )
  })


  # ===========================================================================
  # TAB 2 OUTPUT: Summary Statistics Table (or Welcome Message)
  # ===========================================================================

  output$summary_or_message <- renderUI({

    if (is.null(loaded_data())) {
      return(welcome_message())
    }

    req(input$numeric_var) # Also require the user to have selected a variable

    df      <- loaded_data()
    num_var <- input$numeric_var
    values  <- df[[num_var]] # Extract the selected column as a vector

    # --- Calculate Statistics using dplyr's `summarise()` --------------------
    # `summarise()` collapses the data into a single row of calculated values.
    # `na.rm = TRUE` tells R to ignore any missing (NA) values in calculations.
    stats_df <- data.frame(
      Statistic = c("Mean", "Median", "Variance",
                    "Std. Deviation", "Minimum", "Maximum",
                    "Missing Values (NA)"),
      Value = c(
        round(mean(values,   na.rm = TRUE), 4),
        round(median(values, na.rm = TRUE), 4),
        round(var(values,    na.rm = TRUE), 4),
        round(sd(values,     na.rm = TRUE), 4),
        round(min(values,    na.rm = TRUE), 4),
        round(max(values,    na.rm = TRUE), 4),
        sum(is.na(values))
      )
    )

    # Render the stats as a clean, styled DT table
    tagList(
      tags$h5(
        paste("Summary Statistics for:", tags$b(num_var)),
        style = "color: #2E8B57; margin-bottom: 14px;"
      ),
      renderDT({
        datatable(
          stats_df,
          rownames = FALSE,
          options  = list(
            dom        = 't',     # 't' = table only, no search/pagination needed
            ordering   = FALSE,   # Don't allow re-sorting this table
            pageLength = 10
          ),
          class = 'stripe cell-border'
        ) |>
          # `formatStyle()` highlights alternate rows for readability
          formatStyle(
            'Statistic',
            fontWeight    = 'bold',
            color         = '#2E8B57',
            backgroundColor = '#f0f9f4'
          )
      })
    )
  })


  # ===========================================================================
  # TAB 3 OUTPUT: Visualizations (or Welcome Message)
  # ===========================================================================

  output$plots_or_message <- renderUI({

    if (is.null(loaded_data())) {
      return(welcome_message())
    }

    req(input$numeric_var)

    # Stack the two plots vertically using a fluid row layout
    tagList(
      # Row 1: Histogram
      fluidRow(
        column(12, plotOutput("histogram_plot", height = "320px"))
      ),
      tags$hr(),
      # Row 2: Boxplot
      fluidRow(
        column(12, plotOutput("boxplot_plot", height = "320px"))
      )
    )
  })


  # --- PLOT 1: Histogram -------------------------------------------------------
  # `renderPlot()` renders a ggplot2 (or base R) plot to the UI.

  output$histogram_plot <- renderPlot({

    req(loaded_data(), input$numeric_var)

    df      <- loaded_data()
    num_var <- input$numeric_var

    # --- Build the ggplot2 Histogram ---
    # ggplot2 works in layers. We start with `ggplot()`, then add layers with `+`.

    ggplot(df, aes(x = .data[[num_var]])) + # `.data[[]]` safely accesses column by string name

      # `geom_histogram` draws the bars
      geom_histogram(
        aes(y = after_stat(density)), # Scale y-axis to density (not raw count)
        bins     = 30,
        fill     = "#2E8B57",
        color    = "white",
        alpha    = 0.85              # Slight transparency
      ) +

      # Overlay a smooth density curve for a professional look
      geom_density(
        color     = "#1a5c35",
        linewidth = 1.1,
        linetype  = "dashed"
      ) +

      # `labs()` sets all text labels
      labs(
        title    = paste("Distribution of", num_var),
        subtitle = paste("n =", sum(!is.na(df[[num_var]])), "non-missing observations"),
        x        = num_var,
        y        = "Density",
        caption  = "Dashed line = Kernel Density Estimate"
      ) +

      # Add a vertical line for the mean
      geom_vline(
        aes(xintercept = mean(.data[[num_var]], na.rm = TRUE)),
        color     = "#e74c3c",
        linewidth = 1,
        linetype  = "solid"
      ) +

      # Annotate the mean line
      annotate(
        "text",
        x      = mean(df[[num_var]], na.rm = TRUE),
        y      = Inf, # Place at top of plot
        label  = paste(" Mean =", round(mean(df[[num_var]], na.rm = TRUE), 2)),
        hjust  = 0,
        vjust  = 2,
        color  = "#e74c3c",
        size   = 3.5,
        fontface = "bold"
      ) +

      # `theme_minimal()` is a clean, grid-based theme
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(face = "bold", color = "#2c3e50", size = 14),
        plot.subtitle = element_text(color = "#666", size = 10),
        plot.caption  = element_text(color = "#999", size = 9, hjust = 1),
        panel.grid.minor = element_blank()
      )
  })


  # --- PLOT 2: Boxplot ----------------------------------------------------------

  output$boxplot_plot <- renderPlot({

    req(loaded_data(), input$numeric_var)

    df      <- loaded_data()
    num_var <- input$numeric_var
    cat_var <- input$categorical_var # This may be "none" or a real column name

    # --- Decide whether to group the boxplot or not ---------------------------

    # CASE A: User selected "none" OR the categorical dropdown hasn't rendered yet
    if (is.null(cat_var) || cat_var == "none") {

      p <- ggplot(df, aes(x = "", y = .data[[num_var]])) +
        geom_boxplot(
          fill     = "#5F9EA0",
          color    = "#2c3e50",
          width    = 0.4,
          outlier.color = "#e74c3c",
          outlier.shape = 16,
          outlier.size  = 2.5
        ) +
        # Overlay individual data points with a jitter for transparency
        geom_jitter(
          width = 0.08,
          alpha = 0.3,
          color = "#2E8B57",
          size  = 1.5
        ) +
        labs(
          title    = paste("Boxplot of", num_var),
          subtitle = "No grouping variable selected",
          x        = "",
          y        = num_var
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title    = element_text(face = "bold", color = "#2c3e50"),
          plot.subtitle = element_text(color = "#666", size = 10),
          panel.grid.minor = element_blank(),
          axis.text.x  = element_blank()
        )

    } else {
      # CASE B: User selected a real categorical grouping variable

      # Convert the grouping column to a factor for proper ggplot handling
      df[[cat_var]] <- as.factor(df[[cat_var]])

      # Use a color palette that scales with the number of groups
      n_groups <- length(levels(df[[cat_var]]))

      p <- ggplot(df, aes(x = .data[[cat_var]],
                          y = .data[[num_var]],
                          fill = .data[[cat_var]])) +
        geom_boxplot(
          color         = "#2c3e50",
          outlier.color = "#e74c3c",
          outlier.shape = 16,
          outlier.size  = 2.5,
          alpha         = 0.75
        ) +
        geom_jitter(
          width = 0.15,
          alpha = 0.25,
          size  = 1.5,
          color = "#333"
        ) +
        # Use a colorblind-friendly palette from ggplot2
        scale_fill_brewer(palette = "Set2") +
        labs(
          title    = paste("Boxplot of", num_var, "Grouped by", cat_var),
          subtitle = paste(n_groups, "groups detected"),
          x        = cat_var,
          y        = num_var,
          fill     = cat_var
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title    = element_text(face = "bold", color = "#2c3e50"),
          plot.subtitle = element_text(color = "#666", size = 10),
          panel.grid.minor = element_blank(),
          legend.position  = "none" # Remove legend since x-axis labels it already
        )
    }

    return(p) # Return the final plot object

  }) # End of renderPlot for boxplot

} # End of server function


# ==============================================================================
# SECTION 3: RUN THE APPLICATION
# ==============================================================================
# This final line stitches together the UI and Server and launches the app.
# `shinyApp()` is always the last line in a single-file Shiny app.

shinyApp(ui = ui, server = server)
