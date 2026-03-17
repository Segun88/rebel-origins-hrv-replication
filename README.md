# Rebel Group Origins and Human Rights Violations: Replication Files

## Overview

This repository contains the data, code, and documentation for replicating the analysis in **"Rebel Groups' Foundation and Human Rights Violations: Why Do Some Rebel Groups Commit Human Rights Abuses More Intensively While Others Do Not?"** by Olusegun Nicholas Somide (University of Tennessee, Knoxville).

The project examines how the organizational origins of rebel groups shape their patterns of human rights violations against civilians during civil conflict. Using an original integrated dataset covering more than 700 armed group observations across civil conflicts from 1990 to 2018, the analysis demonstrates that a group's founding structure — whether rooted in civil society networks, political parties, military organizations, violent predecessors, or no identifiable parent organization — systematically predicts both the composition and character of its human rights abuses.

### Key Findings

- **Organizational origins shape the composition of violations** — influencing which types of abuses groups commit rather than simply their overall frequency.
- **Civil society-origin groups** display distinctive selectivity in violence, engaging in fewer indiscriminate abuses compared to groups with militarized or violent origins.
- **Groups without identifiable parent organizations** show substantially higher annual probabilities of ceasing abusive behavior (~15.3%) compared to violent-origin groups (~2.7%).
- **Origins predict violation patterns** across multiple model specifications, analytic samples, and dependent variable operationalizations, suggesting robust relationships.

## Repository Structure

```
├── README.md                 # This file
├── CODEBOOK.md               # Variable definitions and coding decisions
├── LICENSE                   # License information
├── Analysis-script.R         # Full analysis script (data preparation + models)
└── data/
    └── raw/                  # Original source datasets (see Data Sources below)
```

## Data Sources

This project integrates five major international datasets into a single group-year analytical panel:

| Dataset | Description | Source |
|---------|-------------|--------|
| **FORGE** | Foundations of Rebel Group Emergence — rebel founding organizations and origins | [Braithwaite & Cunningham (2020)](https://www.jessicamaves.com/forge.html) |
| **RHRV** | Rebel Human Rights Violations — eight types of violations against civilians | [Walsh, Conrad & Whitaker (2024)](https://doi.org/10.1177/00223433221147940) |
| **NSA** | Non-State Actor Dataset — rebel group capabilities and characteristics | [Cunningham, Gleditsch & Salehyan (2013)](http://ksgleditsch.com/eacd.html) |
| **WDI** | World Development Indicators — GDP per capita (constant 2015 USD) | [World Bank](https://databank.worldbank.org/) |
| **V-Dem** | Varieties of Democracy — polyarchy and institutional indicators | [V-Dem Institute](https://v-dem.net/data/the-v-dem-dataset/) |

### Large File Note

The V-Dem dataset (`V-Dem-CY-Core-v15.dta`, ~411MB) exceeds GitHub's file size limit and is **not included** in this repository. To replicate the analysis:

1. Download the **Country-Year V-Dem Core** dataset in Stata format from: https://v-dem.net/data/the-v-dem-dataset/
2. Place the downloaded file in `data/raw/` before running the script.

## Methodology

The analysis constructs mutually exclusive origin categories from FORGE parent-organization indicators and employs multiple empirical approaches:

- **Fractional logit models** for compositional outcomes (discriminatory violence share)
- **Binomial composition models** for the ratio of discriminatory to indiscriminate violations
- **Negative binomial regression** for violation counts and variety measures
- **Logistic regression** for binary violation outcomes
- **Discrete-time survival models** for violation cessation analysis

All models use **clustered standard errors** at the rebel group level to account for within-group correlation across years. Two analytic samples are constructed using mutually exclusive origin categories to ensure clean theoretical comparisons.

## Replication Instructions

1. Clone or download this repository
2. Download V-Dem from the link above and place in `data/raw/`
3. Open `Analysis-script.R` in R or RStudio
4. Run the script — it will load all data, construct the merged panel, create variables, and estimate all models
5. The script is organized into clearly labeled sections (Parts A through N) corresponding to each stage of the data pipeline

## Software Requirements

Analysis conducted in **R** (version 4.3+). Required packages:

```r
# Data management
tidyverse, haven, janitor, lubridate, countrycode, WDI

# Statistical modeling
lmtest, sandwich, MASS, car

# Presentation
modelsummary, marginaleffects, ggplot2
```

Install all required packages:

```r
install.packages(c("tidyverse", "haven", "janitor", "lubridate", "countrycode",
                   "WDI", "lmtest", "sandwich", "MASS", "car",
                   "modelsummary", "marginaleffects", "ggplot2"))
```

## Citation

If you use this data or code, please cite:

```
Somide, Olusegun Nicholas. 2026. "Rebel Groups' Foundation and Human Rights
Violations: Why Do Some Rebel Groups Commit Human Rights Abuses More Intensively
While Others Do Not?" Working Paper, University of Tennessee, Knoxville.
```

## Contact

**Olusegun Nicholas Somide**  
Ph.D. Candidate, Department of Political Science  
The University of Tennessee, Knoxville  
Email: osomide@vols.utk.edu

## License

This work is licensed under the [MIT License](LICENSE). You are free to use, modify, and distribute the code with attribution. Please cite the original data sources when using the integrated dataset.
