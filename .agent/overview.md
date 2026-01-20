# moreThanANOVA - Project Overview

## Project Summary

**moreThanANOVA** is a bilingual (English/Chinese) R Shiny web application designed to help researchers perform appropriate statistical comparisons between groups. The app guides users to choose the right statistical test based on their data distribution, moving "beyond ANOVA" when data doesn't meet normality assumptions.

### Citation
> Jiang W, Chen H, Yang L, Pan X (2022) moreThanANOVA: A user-friendly Shiny/R application for exploring and comparing data with interactive visualization. PLOS ONE 17(7): e0271185. https://doi.org/10.1371/journal.pone.0271185

## Core Features

1. **Data Distribution Detection**
   - Shapiro-Wilk normality test for each variable
   - Automatic method recommendation based on distribution
   - Visual aids: density plots and Q-Q plots

2. **Statistical Testing**
   - Parametric: T-test, One-way ANOVA
   - Non-parametric: Wilcoxon tests, Kruskal-Wallis H test
   - Permutation tests (Monte Carlo)
   - Levene's test for variance homogeneity

3. **Post-Hoc Analysis**
   - Multiple comparison corrections (Bonferroni, Holm, etc.)
   - Publication-ready comparison plots with significance annotations

4. **Internationalization**
   - Full English and Simplified Chinese support via `shiny.i18n`

## Technology Stack

- **Framework**: R Shiny
- **UI Theme**: shinythemes (flatly)
- **Key Packages**: 
  - `tidyverse` (data manipulation)
  - `ggplot2`, `ggpubr`, `ggsci` (visualization)
  - `rstatix`, `coin` (statistical tests)
  - `DT` (interactive tables)
  - `shiny.i18n` (internationalization)
  - `showtext` (custom fonts)

## Deployment

- **Live Demo**: https://hanchen.shinyapps.io/moreThanANOVA/
- **Platform**: shinyapps.io

## Data Privacy

The application does not store any user data. All uploaded files are only used during the session and discarded upon exit.
