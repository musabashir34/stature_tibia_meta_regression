# Global regression coefficient for predicting stature from tibial length

Data and code for the systematic review and meta-regression *"Global Regression Coefficient for Predicting Stature from Tibial Length: Evidence from Systematic Review and Meta-Regression"*.

## Reproducing the manuscript

Requirements: R (≥ 4.3) with `metafor`, `ggplot2`, `dplyr`, `tidyr`, `forcats`, `patchwork`, `knitr` and `rmarkdown`, plus pandoc (bundled with RStudio).

```r
rmarkdown::render("article.Rmd", output_format = "word_document")   # or "html_document"
```

`article.Rmd` runs `MASTER_analysis_script.R`, which fits every model, writes every figure and table, and stores every number quoted in the text, so the manuscript text cannot drift from the analysis. `build_manuscript.sh` does the same, adds A4 page setup and line numbers to the Word output, and (where `title_page.md` is present) builds the separate title page required for double-anonymized review. Every figure is written both as a vector PDF (fonts embedded) and as a 300-dpi PNG. Running `MASTER_analysis_script.R` on its own produces all figures and tables plus `manuscript_key_numbers.csv`.

## Files

| File | Contents |
|---|---|
| `article.Rmd` | Reproducible manuscript (anonymized for peer review) |
| `MASTER_analysis_script.R` | All analyses, figures and tables |
| `meta_analysis_dataset.csv` | 95 equations pooled in the meta-regression (all risk-of-bias levels) |
| `primary_dataset_exclHighRoB.csv` | 58 equations in the primary analysis (high analysis-domain risk of bias excluded) |
| `offset_analysis_dataset_corrected.csv` | 86 equations with a reported or derivable SEE (secondary analysis) |
| `probast_domain_detail.csv` | PROBAST judgements for all 99 extracted equations |
| `excluded_from_synthesis.csv` | Extracted equations not pooled, with reasons |
| `prisma_counts.csv` | Record counts for the PRISMA flow diagram (including `not_retrieved`, reports whose full text could not be obtained) |
| `Supplementary_Table_S1_Study_Characteristics.csv` | Characteristics of all 99 extracted equations |
| `included_studies_references.csv` | Citation of each included study, mapped to its equations |
| `references.bib`, `vancouver.csl`, `reference.docx` | Bibliography, citation style and Word template |
| `validation_status.csv` | Validation reported for each of the 99 extracted equations, classified by TRIPOD definitions (external, internal, other check, none) |
| `S2_fulltext_exclusions.csv`, `S2_not_retrieved.csv` | Reports excluded at full text (with reasons) and reports not retrieved |

## Data dictionary

| Column | Description |
|---|---|
| `model` | Equation number (1–100; numbering of the extraction workbook; model 53 was excluded as ineligible) |
| `author_sex` | First author and sex of the equation (M/F) |
| `year` | Year of publication (print year where online and print years differ) |
| `ethnicity` | Population or ethnic group as described in the source |
| `sex_field`, `sex_cat` | Sex of the sample (`Male`, `Female`) |
| `N` | Sample size for the equation |
| `b` | Unstandardized regression coefficient (cm stature per cm tibial length) |
| `SEb` | Standard error of `b`, reported or derived (see Methods) |
| `RoB_Domain4` | PROBAST analysis-domain risk of bias (`Low RoB`, `High RoB`, `Unclear`) |
| `country`, `continent` | Country of the sample and regional grouping |
| `age_lower17` | `TRUE` if the sample's lower age bound was 17 years |
| `SEE`, `SEE_method` | Standard error of the estimate (cm) and how it was obtained (offset dataset only) |
| `D1_RoB`…`D4_RoB`, `Overall_RoB` | PROBAST risk of bias by domain and overall (`Low`, `High`, `Unclear`) |
| `D1_App`…`D3_App`, `Overall_App` | PROBAST applicability concerns by domain and overall |

## Licence

Code: MIT licence. Data: CC BY 4.0. See `LICENSE`.
