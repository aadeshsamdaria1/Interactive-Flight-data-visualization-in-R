# Interactive-Flight-data-visualization-in-R
# Assignment 2 - Interactive Data Visualization in R

## Introduction

This assignment focuses on the interactive data visualization of the performance of the air travel network in the United States during the year 2015. The primary goal is to provide users with a comprehensive dashboard for exploring and understanding various aspects of air travel, including airline performance, delays, cancellations, and geographical insights. The dashboard leverages R and Shiny to create a user-friendly and interactive interface for data exploration.

## Problem Statement

The air travel network plays a crucial role in modern transportation, connecting people across the United States. Understanding the performance and trends in this network can provide valuable insights for passengers, airlines, and policymakers. The dashboard aims to address the following questions:

- How do different airlines perform in terms of arrival and departure delays?
- What are the monthly delay trends for specific airlines?
- What are the reasons behind delays for a chosen airline?
- Which airports have the highest total delays for a selected airline?
- What are the geographical insights into air travel statistics across states in the United States?

## Methodology

The dashboard uses data from Kaggle, consisting of three primary datasets: flights, airlines, and airports. It offers three main tabs for data exploration:

1. **Graphs Tab**: This tab provides insights into airline performance. It includes both static and dynamic charts. The static chart displays average arrival and departure performance for 14 airlines across all airports. The dynamic charts allow users to explore monthly delay trends for specific airlines, examine the reasons behind delays, and access a list of the top airports with the highest total delays for a selected airline. Users can customize the number of airports displayed and download chart images.

2. **Map Tab**: Users can visually analyze air travel statistics for the entire United States. The tab includes a choropleth map representing total flight counts, percentages of flights with arrival and departure delays, and percentages of canceled flights. Users can apply filters by selecting specific months and download the dataset for further analysis.

3. **Data Summary**: This section offers detailed information about the datasets used in the dashboard. Users can download these datasets directly from the dashboard.

Core packages and versions used in this assignment:
- Shiny
- Plotly
- DT
- shinydashboard
- dplyr

## How to Run the Notebook
1. Download the datasets (flights.csv, airports.csv, airlines.csv) from [Kaggle - Flight Delays Dataset](https://www.kaggle.com/datasets/usdot/flight-delays)
2. Open the R environment or RStudio.

3. Install the required packages if you haven't already:

```R
install.packages("shiny")
install.packages("plotly")
install.packages("DT")
install.packages("shinydashboard")
install.packages("dplyr")
```

4. Download and run the Shiny app R script (GEOM90007_Assignment2_AadeshSamdaria_1363757.R).

5. The Shiny dashboard should open in your web browser, allowing you to interact with the visualizations and explore the air travel network's performance.

6. Use the filters and options in the dashboard to customize your data exploration.

7. You can also download chart images and datasets directly from the dashboard for further analysis.

Enjoy exploring the air travel network's performance in the United States!

## Sources and References
- Dataset Source: [Kaggle - Flight Delays Dataset](https://www.kaggle.com/datasets/usdot/flight-delays)
- Map Tab Idea Reference: <https://github.com/yyeva022/dashboard_flight>
