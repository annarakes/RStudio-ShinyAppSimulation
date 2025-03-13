# R-Studio Shiny App P-Value Calculator and Simulation
## Authors: Anna Rakes and Lexi Waller

**Link to [Shiny App Simulation](https://annarakes.shinyapps.io/ShinyAppProject/)**


### Overview

This project is a Shiny web application developed by Anna Rakes and Lexi Waller for the Statistical Computing for Simulation Theory class at Elon University. The application allows users to visualize and calculate p-values for normal and t-distributions based on user-defined test statistics and directionality.

### Features

**Distribution Selection:** Choose between a Normal or T-distribution.

**Custom Inputs:**

Define test statistics.

Select one-tailed (left or right) or two-tailed tests.

Set degrees of freedom for t-distribution.

**Graphical Visualization:** Displays a probability distribution plot with shaded critical regions.

**P-Value Calculation:** Outputs the corresponding p-value based on the selected inputs.

### Technologies Used

Shiny for interactive web applications.

ggplot2 for data visualization.

data.table for data handling.

### Installation

To run the application locally, ensure you have R and the required packages installed.

install.packages("shiny")
install.packages("ggplot2")
install.packages("data.table")

### Running the Application

Save the script as app.R and run it in RStudio or directly in R using:

shiny::runApp("app.R")

### Usage

Select a distribution type.

Input relevant parameters (test statistic, direction, and degrees of freedom if applicable).

View the plotted distribution and computed p-value.
