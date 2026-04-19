# The Bio-Stats Explorer 

A fast, interactive, and user-friendly single-file Shiny application designed for biological data analysis. 

The **Bio-Stats Explorer** provides an intuitive interface for researchers and students to quickly upload datasets, view interactive tables, compute summary statistics, and generate publication-ready visualizations—all without writing a single line of code.

##  Features

- **Easy Data Upload:** Upload any `.csv` dataset. The app is optimized for biological datasets (e.g., gene expression, concentrations, measurements).
- **Interactive Data Tables:** View, search, sort, and download your raw data using the highly responsive `DT` package interface.
- **Automated Statistics:** Dynamically calculate key summary statistics (Mean, Median, Variance, Std Deviation, Min, Max, and Missing Values) for selected numeric variables.
- **Publication-Quality Visualizations:**
  - **Distribution Histograms:** Overlayed with Kernel Density Estimates and mean annotations.
  - **Boxplots:** Visualize distributions across categorical groups with underlying jittered data points for better data density representation.
- **Modern UI:** Built with `bslib` implementing a professional, modern, responsive theme using the Inter font.

##  Getting Started

### Prerequisites

Ensure you have [R](https://cran.r-project.org/) and optionally [RStudio](https://posit.co/download/rstudio-desktop/) installed on your system.

You will need the following R packages installed. Run this command in your R console:

```R
install.packages(c("shiny", "dplyr", "ggplot2", "DT", "bslib"))
```

### Running the App

1. Clone or download this repository so that you have the `app.R` file.
2. Open `app.R` in RStudio.
3. Click the **"Run App"** button in the top right corner of the script editor.

Alternatively, you can run the app directly from your R console:

```R
shiny::runApp("path/to/directory/containing/app.R")
```

##  Usage Flow

1. **Upload:** Use the sidebar to upload a `.csv` file. 
2. **Select Variables:** Choose a numeric variable. Optionally, select a categorical variable to group the data.
3. **Explore:** 
   - Tab 1: **Raw Data** - Inspect the loaded dataset.
   - Tab 2: **Summary Statistics** - Review calculated metrics.
   - Tab 3: **Visualizations** - Examine distributions visually.

##  Dependencies

- `shiny` - Core framework
- `dplyr` - Data manipulation
- `ggplot2` - Data visualization 
- `DT` - Interactive datatables
- `bslib` - Bootstrap styling

##  License

This project is open-source and available for research and educational purposes.
