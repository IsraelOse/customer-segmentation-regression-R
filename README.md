# Customer Segmentation & Sales Regression (R)

Two linked pieces of customer analytics in R on a **5,000-row customer dataset**: segmenting customers by behaviour, then modelling purchasing with multiple linear regression.

## Approach
- **Segmentation:** clustering customers and profiling the resulting segments (`clustered_data.csv`, `cluster_profile.csv`), with an external-validation check (`external_validation.csv`)
- **Regression:** multiple linear regression on purchasing/sales data (`Regression_Sales_Data.R`), diagnostics in `Rplots.pdf`
- Findings written up in `Purchasing_Data_Report.docx`

## Repo layout
| Path | What it is |
|------|-----------|
| `Assignment_v2.R` | Main segmentation script |
| `Regression_Sales_Data.R` | Regression modelling |
| `Purchasing_data.xlsx` | Source data |
| `clustered_data.csv`, `cluster_profile.csv`, `external_validation.csv` | Segmentation outputs |
| `Regression_Analysis.csv` | Regression outputs |
| `Purchasing_Data_Report.docx` | Report |

## Tech
R · tidyverse · cluster/factoextra · base stats (lm)

*DAMO-520 Marketing Analytics, Master of Data Analytics.*
