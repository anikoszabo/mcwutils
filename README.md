# mcwutils

Small, tested helpers for biostatistical data management created at the 
Medical College of Wisconsin Biostatistical Consulting Service.

## Installation

```r
# install.packages("remotes")
remotes::install_github("anikoszabo/mcwutils")  
```

## Functions

| Area | Function | Purpose |
|---|---|---|
| Flow table | `start_flow()`, `add_to_flow()`, `show_flow()` | Track exclusions and excluded IDs; format as a `gt` table (needs gt, janitor) |
| Strings | `paste_na()` | Paste ignoring `NA` |
| | `str_capitalize_first()` | Upper-case first character only |
| | `str_ascii()` | Transliterate or drop non-ASCII characters |
| Summaries | `safe_max()`, `safe_min()` | Max/min returning typed `NA` for all-`NA` input |
| | `first_non_na()` | First non-missing value |
| Lab data | `lod_to_numeric()` | `"<0.5"` to numeric (default half the limit) |
| Survival | `admin_censor()` | Administrative censoring at a fixed time |
| Data dictionary | `dict_parse_choices()`, `dict_recode_var()`, `copy_factor_labels()` | REDCap choices to factors; copy labels to `.factor` columns |
| Labels | `set_label()`, `set_label_tibble()` | Variable labels (base R) |
| Sourcing | `source_skip()`, `source_rmd()` | Source a script skipping lines; source an Rmd's code |

## Example

```r
library(mcwutils)
flow <- data_merge |>
  start_flow("Enrolled in study", idvar = "REDCap_ID") |>
  add_to_flow(age >= 18, "Adults")
flow$data
show_flow(flow)
```

## License

MIT (c) Aniko Szabo
