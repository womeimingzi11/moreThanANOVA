# Project Structure

## Directory Layout

```
moreThanANOVA/
├── server.R              # Main server logic (768 lines)
├── ui.R                  # UI definition (354 lines)
├── R/
│   └── glance_coin.R     # Utility for Monte Carlo permutation tests
├── resource/
│   ├── i18n/
│   │   └── translation.json   # English/Chinese translations
│   ├── page/
│   │   ├── overview.md        # App overview page content
│   │   └── acknowledgements.md # Credits and references
│   └── figure/
│       └── table_str.png      # Example data structure image
├── www/
│   └── table_str.png      # Static web assets
├── archive/
│   ├── app.R              # Legacy combined app file (deprecated)
│   └── ...                # Old documentation
├── .agent/                # Agent documentation (this folder)
├── README.md              # GitHub README
├── LICENSE.md             # MIT License
└── moreThanANOVA.Rproj    # RStudio project file
```

## Key Files

### Core Application Files

| File | Purpose | Lines |
|------|---------|-------|
| `server.R` | Server-side logic: data processing, statistical tests, plot generation | 768 |
| `ui.R` | User interface: tabs, inputs, outputs | 354 |
| `R/glance_coin.R` | Helper function for `coin` package permutation test results | 7 |

### Resource Files

| File | Purpose |
|------|---------|
| `resource/i18n/translation.json` | Bilingual translations (English + 简体中文) |
| `resource/page/overview.md` | Content for the Overview tab |
| `resource/page/acknowledgements.md` | Credits and package references |

### Configuration

| File | Purpose |
|------|---------|
| `.gitignore` | Excludes `.Rproj.user`, `.RData`, `rsconnect/`, `test/` |
| `moreThanANOVA.Rproj` | RStudio project configuration |
| `rsconnect/` | shinyapps.io deployment config (gitignored) |

## Code Architecture

### Server Logic Flow (`server.R`)

1. **Data Input** (`rct_df_data`)
   - Accepts demo datasets (iris, ToothGrowth) or uploaded CSV
   - Renames first column to "Treatment"

2. **Distribution Analysis**
   - `rct_df_sw_test_pv`: Shapiro-Wilk test p-values
   - `rct_condition_ls`: Determines test conditions (paired, equal samples, etc.)
   - `rct_analysis_method`: Recommends appropriate statistical method

3. **Variance Testing**
   - `rct_levene_p`: Levene's test for variance homogeneity

4. **Statistical Comparisons**
   - `rct_compare_ls`: Executes selected statistical tests
   - Supports: t-test, ANOVA, Wilcoxon, Kruskal-Wallis, permutation tests

5. **Visualization**
   - `rct_ggplot_hist`: Density/histogram plots
   - `rct_ggplot_qq`: Q-Q plots
   - Post-hoc comparison plots with ggpubr

### UI Structure (`ui.R`)

- **navbarPage** with three tabs:
  1. **Overview**: Project introduction and usage guide
  2. **Analysis**: Main analysis interface
     - Sidebar: Data upload, test options, p-value adjustment
     - Main: Data viewer, EDA plots, comparison results
  3. **Acknowledgements**: Credits and references
