# Replication Files for *Rebel Groups and Human Rights Violations*

## Overview
This repository replicates the analysis for *Rebel Groups and Human Rights 
Violations*, examining how rebel organizational origins shape patterns of 
human rights abuses against civilians during civil conflict.
The analysis covers the period 1990–2018 and combines multiple datasets 
on rebel organizations, conflict behavior, and country-level characteristics.

## Folder Structure
- `data/raw/` — original input data (see note on large files below)
- `data/processed/` — cleaned and merged datasets created by scripts
- `R/` — R scripts for data preparation and analysis

## Data Sources
- **FORGE** — Rebel founding organizations and origins
- **RHRV** — Rebel human rights violations against civilians
- **NSA** — Non-state actor characteristics (`nsa_.asc`)
- **WDI** — World Bank GDP per capita data
- **V-Dem** — Democracy and institutional indicators

## Note on Large Data Files
The V-Dem dataset (`V-Dem-CY-Core-v15.dta`, 411MB) exceeds GitHub's 
file size limit and is not included in this repository.
Please download the **Country-Year V-Dem Core** dataset in Stata format 
directly from the V-Dem website:

👉 https://v-dem.net/data/the-v-dem-dataset/

Place the downloaded file in `data/raw/` before running the scripts.

## Replication Instructions
1. Download V-Dem from the link above and place in `data/raw/`
2. Run `analysis.R` to prepare data and run analysis
3. Output tables and figures will be generated in the `output/` directory

## Software
Analysis conducted in **R**. Required packages:
`tidyverse`, `haven`, `janitor`, `lmtest`, `sandwich`, 
`MASS`, `marginaleffects`, `ggplot2`, `patchwork`

## Author
**Olusegun Somide**
PhD Candidate — Political Science
University of Tennessee, Knoxville
