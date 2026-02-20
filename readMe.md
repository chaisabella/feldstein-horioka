# Feldstein–Horioka Revisited (Interactive)
### SAIS Spring 2026 - Independent Study
Isabella Cha  

---

## Overview

This project revisits the Feldstein–Horioka (1980) saving–investment correlation using updated IMF World Economic Outlook (WEO) data.

The central contribution of this project is not replication alone, but interactivity.

Rather than presenting a single regression estimate, this project transforms the Feldstein–Horioka puzzle into an interactive empirical tool that allows users to:

- Toggle balanced vs. unbalanced panels  
- Restrict to the classic OECD-21 sample  
- Select custom year windows  
- Dynamically recompute regression coefficients  
- Visually inspect country-level observations  

The result is a transparent and exploratory framework for engaging with one of international macroeconomics’ most persistent empirical puzzles.

---

## Why This Matters

The original Feldstein–Horioka result suggested limited international capital mobility based on a high correlation between domestic saving and investment across OECD countries.

However, the magnitude of the coefficient depends critically on:

- Sample selection  
- Time period  
- Panel balance  
- Cross-sectional vs. panel estimation  

By making these choices interactive, this project highlights how empirical conclusions can shift under different assumptions.

---

## Data

Source:
- IMF World Economic Outlook (WEO)

Variables:
- Gross national savings (% of GDP)
- Gross capital formation (% of GDP)

Sample:
- OECD-21 countries  
- Annual data  
- 1980–2025  

The raw WEO dataset is excluded from this repository.  
Cleaned datasets are included for reproducibility.

---

## Methodology

Baseline specification:

Investment/GDP = α + β × Saving/GDP

Estimated under:

- Unbalanced panel  
- Balanced panel (complete 1980–2025 coverage)  
- Cross-sectional country averages  

Regression coefficients update dynamically within the Shiny dashboard.
