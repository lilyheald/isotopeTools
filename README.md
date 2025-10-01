# isotopeTools

**R package for processing, analyzing, and visualizing isotope data from archaeological enamel samples.**  

`isotopeTools` provides functions for:  
- Cleaning and standardizing isotope datasets (`clean_isotope_data`)  
- Generating summary statistics and saving as PNG (`statistics_results`)  
- Plotting δ13C and δ18O per tooth (`dual_axes`)  
- Creating species-specific δ13C plots (`save_all_indiv_carbon_plots`)  

---

## Installation

You can install the development version directly from GitHub using **devtools**:

```r
# install devtools if not already installed
install.packages("devtools")

# install isotopeTools from local GitHub folder
devtools::install_github("yourusername/isotopeTools")
```

## Dependencies

The package requires the following R packages:

- `dplyr`  
- `ggplot2`  
- `kableExtra`  
- `webshot2`  
- `stats` (base R)  

Install them with:

```r
install.packages(c("dplyr", "ggplot2", "kableExtra", "webshot2"))
```


