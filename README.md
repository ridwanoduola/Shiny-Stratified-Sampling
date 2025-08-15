# Two-Stage Stratified Sampling Shiny App

An interactive R Shiny web application for visualizing histograms, density plots, and computing statistical estimates for **two-stage stratified sampling**.  
Developed by **Ridwan Oduola @Moor Metrics**.

## Features
- Histogram and density plots for selected strata and substrata
- Estimates for population mean and population total
- Confidence intervals for estimates
- Adjustable sampling sizes for primary and secondary units
- Interactive controls for number of bins and substratum selection

## Project Structure
`app.R` # Main Shiny application script
`sdt.csv` # Sample dataset used in the app
`README.md` # Project documentation
`.gitignore` # Ignored files for Git tracking
## Data
The application uses a dataset (`sdt.csv`) where:
- **First column:** Stratum ID
- **Columns 2–6:** Observations for different substrata

## How to Run Locally
1. **Clone this repository:**
   ```bash
   git clone https://github.com/ridwanoduola/Shiny-Stratified-Sampling.git
   cd Shiny-Stratified-Sampling
```
2. **Install R and RStudio (if not already installed).**

3. **Install required R packages:**
`install.packages(c("shiny", "dplyr", "ggplot2", "tidyr"))`

4. Run the app in RStudio:

- Open `app.R`

- Click **Run App**

Or, from R console:

`shiny::runApp("path/to/Shiny-Stratified-Sampling")`

## Deployment

You can deploy the app to shinyapps.io for free:

`library(rsconnect)`
`rsconnect::deployApp("path/to/Shiny-Stratified-Sampling")`

----
Author: Ridwan Oduola
Organisation: Moor Metrics