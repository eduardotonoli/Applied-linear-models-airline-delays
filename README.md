# Applied Linear Models – Airline Delays

This repository contains a university project developed for the Applied Linear Models course at Università Cattolica del Sacro Cuore.

## Project overview
The aim of the project is to analyze airline arrival delays using linear regression techniques, with a focus on the relationship between total arrival delays and several operational delay components.

## Dataset
The analysis is based on the **Airline On-Time Statistics and Delay Causes** dataset from the U.S. Bureau of Transportation Statistics.

The sample was restricted to:
- year 2019
- airports: JFK, LAX, ORD, ATL
- carriers: AA, DL, UA

Since the data are aggregated, each observation represents an airport–carrier–month combination rather than an individual flight.

## Methods
The project includes:
- exploratory data analysis
- linear regression modeling
- best subset selection
- model comparison using AIC, BIC, adjusted R² and LOOCV
- diagnostic analysis

## Repository contents
- `final_report.pdf` – final written report
- `presentation.pdf` – presentation slides
- `analysis.R` – R script used for the analysis

## Tools
- R
- LaTeX

## Author
Eduardo Tonoli
