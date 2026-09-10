Inflation Inequality in Spain (2006-2025)

This repository contains the R code developed for my undergraduate thesis (Trabajo de Fin de Grado), "Desigualdad inflacionaria en España (2006-2025): mecanismo composicional, sesgo de agregación y efecto distributivo de la rebaja de IVA en alimentos", submitted for the degree in Business Intelligence and Analytics (Inteligencia y Analítica de Negocios) at the Universitat de València.

What this project does

The thesis studies how inflation affects Spanish households differently depending on their level of spending, using household-level microdata from the Encuesta de Presupuestos Familiares (EPF) and detailed price indices from the Índice de Precios de Consumo (IPC), both published by Spain's National Statistics Institute (INE). Specifically, the project:

Measures household-specific inflation for the period 2006-2025, combining each household's own spending basket (at the most detailed product classification available, COICOP-4) with the corresponding category-level inflation rates, following a Laspeyres-type approach.
Documents the inflation gap between the lowest- and highest-spending quintiles, and tests its statistical significance with fixed-effects regression models.
Identifies the mechanism behind this gap: since price inflation is common to all households within a category, differences arise entirely from differences in consumption basket composition (Housing and Food driving the gap for lower-spending households; Transportation working in the opposite direction).
Quantifies the aggregation bias: comparing results at the detailed (COICOP-4) versus aggregate (COICOP-1) level of product classification, following the approach of Jaravel (2021).
Simulates the distributional effect of a real fiscal policy: the temporary VAT reduction on basic food items (Real Decreto-ley 20/2022 and its subsequent extensions), building a counterfactual scenario of what inflation would have looked like without the measure.
Repository contents
ESP_pipeline_limpio.R - Full data-cleaning and analysis pipeline: reading raw EPF/IPC files, harmonizing COICOP classifications across survey waves, constructing household-level inflation, building expenditure quintiles, running the econometric models, and simulating the VAT policy counterfactual.
Data availability

The raw microdata used in this project (EPF and IPC files from the INE) are not included in this repository due to their size and licensing terms. Some files require direct request to the INE; others are derived/processed extracts prepared specifically for this thesis.

If you would like access to the underlying data or processed intermediate files, please contact me at agathadelolmo@gmail.com.

Citation

If you use or reference this code, please cite:

Del Olmo Tirado, Á. (2026). Desigualdad inflacionaria en España (2006-2025): mecanismo composicional, sesgo de agregación y efecto distributivo de la rebaja de IVA en alimentos [Undergraduate thesis]. Universitat de València.

Disclaimer

This code was developed as part of an undergraduate research project. It is shared for transparency and reproducibility purposes; it has not been optimized or documented to production standards.
