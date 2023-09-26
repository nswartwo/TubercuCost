# TubercuCost

## Project Goals
The goal of the TubercuCost package is to provide a single source for cost-effective analyses of tuberculosis interventions in the United States. All estimates are based on United States parameter values. Support is currently provided for years 2023 - 2100. 

### Key function inputs 
The functions in TubercuCost require a dataframe of: 

* TB cases
* TB deaths
* LTBI treatment initiations 

by single year of age (0 - 100 years) and year of occurrence (2023 - 2100). If evaluating the effectiveness of a particular intervention, it could be expedient to use dataframes of averted measures.

Support for age groups coming soon! 

### Key function outputs 
Functions in TubercuCost generate outputs as follows: 

`calculateHealthCost` generates a list with entries for costs associated with with LTBI, TB, nonTB, and total health services. TB costs are exclusive of LTBI cost estimates.

`calculateProdCost` generates a list with entries for costs associated with with LTBI morbidity, TB morbidity, TB mortality, and total productivity losses. TB costs are exclusive of LTBI cost estimates.

`calculateQALYs` generates a list with entries for quality adjusted life years (QALYs) associated with LTBI morbidity, TB morbidity, TB mortality and total impacts. TB costs are exclusive of LTBI cost estimates.

### Methodology 
TubercuCost provides estimates of the economic impact of TB from the health services and societal perspective of economic evaluation. The health services perspective includes TB health services costs and future non-TB healthcare spending. The societal perspective includes all costs in the health services perspective plus future productivity estimates, which have been adjusted for further non-healthcare spending. Per-patient TB treatment costs were based on cost analyses conducted by the CDC. Annual total healthcare expenditures by age were taken from estimates calculated from Medical Expenditure Panel Survey data. Annual total non-healthcare expenditures were calculated from the U.S. Bureau of Labor Statistics’ Consumer Expenditure Surveys. Annual and lifetime productivity estimates were based on an analysis of U.S. market and non-market productivity. All costs are reported in 2020 USD. Future costs can be discounted to present day value at a fixed annual rate of 3%. More details available in the Economic Methodology file. 

#### Current limitations
In order to be as inclusive as possible, TubercuCost does not provide estimates of costs associated with the identification of individuals for LTBI or TB treatment. This can be updated based on locale or analysis specific values.  

## Installation 
Currently, TubercuCost is available for download with an authentication key. In order to gain access to the package, please [email](nswartwood@hsph.harvard.edu) with a detailed description of your interest in the package. 
```
# install.packages("devtools")
devtools::install_github("PPML/TubercuCost", auth_token='TOKEN')
```

## Code Examples 
Key vignettes coming soon! 
