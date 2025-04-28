# Shiny.EQ5D

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15296116.svg)](https://doi.org/10.5281/zenodo.15296116)

An R package wrapping a Shiny application for interactive EQ-5D data visualization.

------------------------------------------------------------------------

## Table of Contents

-   [Description](#description)
-   [Installation](#installation)
-   [Usage](#usage)
-   [Features](#features)
-   [Citation](#citation)
-   [Dependencies](#dependencies)
-   [Development](#development)
-   [License](#license)

------------------------------------------------------------------------

## Description 

**Shiny.EQ5D** provides an easy-to-install R package that launches a  Shiny app for exploring and visualizing EQ-5D descriptive system and VAS (Visual Analogue Scale) data. Whether you’re working with built-in simulated data or your own clinical/research dataset, the app offers flexible plotting options, language toggles (English/Norwegian), and publication-quality downloadable figures.

------------------------------------------------------------------------

## Installation 

Install the development version directly from GitHub:

``` r
install.packages("devtools")
devtools::install_github("SondreNilsen/Shiny.EQ5D")
```

------------------------------------------------------------------------

## Usage 

Once installed, load the package and run the app with:

``` r
library(Shiny.EQ5D)
runEQ5DApp()
```

------------------------------------------------------------------------

## Features 

-   **Data Source**
    -   Simulated EQ-5D data bundled with the package
    -   Upload your own SPSS (.sav), Stata (.dta), Excel (.xls/.xlsx) or CSV files
-   **EQ-5D Profiles**
    -   Bar charts, density curves, stacked bar plots per EQ-5D dimension
    -   Compare groups, show percentages, toggle coordinate flips
-   **EQ VAS**
    -   Histograms, density plots, raincloud, boxplots, violin, trend lines
    -   Faceting, reference lines (mean/median), and custom appearance controls
-   **Language Support**
    -   English and Norwegian labels for facets and response levels
-   **Downloadable Figures**
    -   PDF or high-resolution PNG export with custom dimensions and DPI
    

------------------------------------------------------------------------
  
## Citation 

If you use **Shiny.EQ5D**, please cite it as:

> Nilsen, S. A. (2025). *Shiny.EQ5D: Interactive EQ-5D Data Visualizer* (Version 0.1.0) [Software]. Zenodo. https://doi.org/10.5281/zenodo.15296116

### BibTeX

```bibtex
@software{Nilsen2025ShinyEQ5D,
  author       = {Nilsen, Sondre Aasen},
  title        = {{Shiny.EQ5D: Interactive EQ-5D Data Visualizer}},
  version      = {0.1.0},
  year         = {2025},
  publisher    = {Zenodo},
  doi          = {10.5281/zenodo.15296116},
  url          = {https://doi.org/10.5281/zenodo.15296116}
}
```

------------------------------------------------------------------------

## Dependencies

The package imports these core R packages: shiny, shinythemes, shinyWidgets, ggplot2, ggpp, hrbrthemes, ggthemes, ggsci, RColorBrewer, viridis, dplyr, tidyr, scales, DT, colourpicker, haven, readxl, readr, ggrain

------------------------------------------------------------------------

## Development

To contribute or customize:

1.  Fork the repository on GitHub.
2.  Clone locally:

``` bash
git clone git@github.com:SondreNilsen/Shiny.EQ5D.git
```

3.  Open the RStudio project, install dependencies, and edit.
4.  Document changes with roxygen2 and run `devtools::check()` before committing.
5.  Submit a pull request!

------------------------------------------------------------------------

## License

This project is licensed under the **MIT License** – see the [LICENSE](LICENSE) file for details.

------------------------------------------------------------------------

*Created by Sondre Aasen Nilsen, Center for Patient-Reported Data, Haukeland University Hospital*
