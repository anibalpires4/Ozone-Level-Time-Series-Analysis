# Ozone Level Time Series Analysis

This repository contains the analysis and modelling of ground-level ozone (O3) concentrations across five regions in the Lisbon metropolitan area for the year 2021. The study utilizes Seasonal Autoregressive Integrated Moving Average (SARIMA) models to analyze time series data, identify seasonal patterns, and forecast future ozone levels.

---

## Overview

Ground-level ozone is a significant air pollutant that poses health risks, particularly to individuals with respiratory issues. This study focuses on understanding the hourly evolution of ozone levels at five stations in Lisbon and provides predictions using SARIMA models.

### Key Features
- **Data Source**: Hourly ozone concentration measurements from five stations in Lisbon during 2021.
- **Objective**: 
  1. Analyze seasonal trends and variability in ozone levels.
  2. Develop SARIMA models for time series forecasting.
  3. Provide insights into environmental and public health policies.
- **Methodology**:
  - Data imputation for missing values.
  - Box-Cox transformation for variance stabilization.
  - Model selection and validation using AIC, BIC, MAE, and RMSE.

---

## Data and Preprocessing

### Dataset Description
The dataset contains 8,760 hourly observations for each station:
1. **Alfragide**
2. **Beato**
3. **Entrecampos**
4. **Olivais**
5. **Reboleira**

### Preprocessing Steps
1. **Missing Data Imputation**:
   - Linear interpolation for small gaps.
   - Seasonal decomposition imputation (`seadec`) for large gaps.
2. **Transformation**:
   - Box-Cox transformation to stabilize variance and ensure normality.
3. **Exploratory Data Analysis**:
   - Histograms, boxplots, and summary statistics for each station.

---

## Time Series Modeling

### SARIMA Model
A SARIMA model captures both seasonal and non-seasonal components in a time series. The general form is:

\[
SARIMA(p, d, q)(P, D, Q)_{S}
\]

Where:
- \( p, d, q \): Regular autoregressive, differencing, and moving average orders.
- \( P, D, Q \): Seasonal autoregressive, differencing, and moving average orders.
- \( S \): Seasonal period (24 hours in this study).

### Model Selection and Validation
- Models tested for each station varied in \( p, q, P, Q \) to identify the best fit.
- **Evaluation Metrics**:
  - **AIC**: Akaike Information Criterion.
  - **BIC**: Bayesian Information Criterion.
  - **MAE**: Mean Absolute Error.
  - **RMSE**: Root Mean Squared Error.
- Residual diagnostics using ACF, PACF, and Ljung-Box tests.

### Best Models
- **Alfragide**: SARIMA(1, 0, 1)(1, 1, 1)\(_{24}\)
- **Beato**: SARIMA(2, 0, 2)(1, 1, 1)\(_{24}\)
- **Entrecampos**: SARIMA(2, 0, 2)(0, 1, 1)\(_{24}\)
- **Olivais**: SARIMA(2, 0, 2)(1, 1, 1)\(_{24}\)
- **Reboleira**: SARIMA(1, 0, 2)(1, 1, 1)\(_{24}\)

---

## Forecasting

### Prediction for January 1, 2022
Each station's forecast for the first five hours of January 2022 included 95% confidence intervals. While the predictions align with historical trends, some confidence intervals include unrealistic negative values, indicating areas for model refinement.

---

## Results and Discussion

### Key Findings
1. Seasonal patterns observed with peaks in ozone levels until May 2021.
2. SARIMA models with seasonal differencing (D=1) performed best.
3. Forecasting highlighted limitations, with some negative lower bounds in confidence intervals.

### Challenges
- High computational cost of SARIMA models with seasonal terms.
- Inconsistent confidence intervals for forecasted values.

---

## Technologies
- **Programming Language**: R
- **Libraries**:
  - `forecast` (SARIMA modelling)
  - `ggplot2` (Visualization)
  - `imputeTS` (Imputation)

---

## Usage

1. **Clone the repository**:
   ```bash
   git clone https://github.com/anibalpires4/Ozone-Level-Time-Series-Analysis.git
