# Coding Conventions & Development Guide

## Naming Conventions

### Reactive Functions
- Prefix: `rct_` (e.g., `rct_df_data`, `rct_compare_ls`)
- Descriptive suffixes indicate return type:
  - `_df`: tibble/data frame
  - `_ls`: list
  - `_p` or `_pv`: p-value related

### UI Components
- Input IDs use snake_case: `data_source`, `sw_signif_level`
- Output IDs use snake_case: `df_dist_n_method`, `gg_post_hoc`
- Dynamic inputs use prefix: `var_` + variable name

## Code Organization

### server.R Sections
The file is organized into numbered sections with comment blocks:
```r
########################################
# 1. Section description
#    This reactive function is called
#    rct_function_name
#    return: type
#    render: output type
########################################
```

### Adding New Features
1. Add reactive function with proper documentation header
2. Add corresponding render function
3. Update UI if needed
4. Add translations to `resource/i18n/translation.json`

## Internationalization

### Adding Translations
1. Open `resource/i18n/translation.json`
2. Add new entry to the `translation` array:
```json
{
  "English": "New text",
  "简体中文": "新文本"
}
```
3. Use in UI: `i18n$t("New text")`

### Supported Languages
- English (default)
- 简体中文 (Simplified Chinese)

## Testing

### Local Testing
1. Open `moreThanANOVA.Rproj` in RStudio
2. Open `ui.R` or `server.R`
3. Click "Run App" button

### Demo Datasets
- **Iris**: Multi-group comparison (3 species)
- **ToothGrowth**: Two-group comparison (2 supplements)

## Deployment

### shinyapps.io
```r
rsconnect::deployApp()
```

Configuration stored in `rsconnect/` (gitignored)

## Dependencies

Key packages to install:
```r
install.packages(c(
  "shiny", "shinythemes", "shinyjs",
  "tidyverse", "DT",
  "ggplot2", "ggpubr", "ggsci",
  "rstatix", "coin", "broom",
  "shiny.i18n", "shinyWidgets",
  "showtext", "sysfonts"
))
```
