# Feldstein–Horioka Revisited (Interactive)

Last run on: 2/20/2026, R version 4.5.2

🔗 **Live App:**\
<https://isabella-cha.shinyapps.io/feldstein-horioka/>

------------------------------------------------------------------------

## Project Overview

Feldstein and Horioka (1980) found a surprisingly high correlation between domestic saving and domestic investment across OECD countries. Under perfect international capital mobility, domestic investment should not be strongly tied to domestic saving. The persistence of a high coefficient raised questions about the extent of global financial integration.

This project reconstructs the empirical relationship using modern data and allows users to explore its sensitivity.

### Interactive Features

The application allows users to:

-   Toggle between balanced and unbalanced samples
-   Select/de-select individual countries using a searchable dropdown menu
-   Adjust the time window dynamically
-   Hover over scatter plot observations to view country-level values
-   View regression coefficients (β, standard error, R², and sample size) updated in real time

------------------------------------------------------------------------

## Data

Source: - IMF World Economic Outlook (WEO)

Variables: - Gross national savings (% of GDP) - Gross capital formation (% of GDP)

Sample: - OECD-21 countries - Annual data - 1980–2025

The raw WEO dataset is excluded from this repository. Cleaned datasets are included.

------------------------------------------------------------------------

## Methodology

Baseline specification:

Investment/GDP = α + β × Saving/GDP

Estimated under:

-   Unbalanced panel
-   Balanced panel (complete 1980–2025 coverage)
-   Cross-sectional country averages

Regression coefficients (β, SE, R², N) update dynamically within the Shiny dashboard.

------------------------------------------------------------------------

## Author

Isabella Cha\
Johns Hopkins SAIS\
Spring 2026
