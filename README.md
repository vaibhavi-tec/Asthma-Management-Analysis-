# Asthma Management Analysis  
## Data-Driven Insights into Asthma Risk Factors & Outcomes

---

## Overview
This repository presents a comprehensive analysis of asthma using a synthetic dataset of 10,000 patient records. The project focuses on identifying key demographic, lifestyle, environmental, and clinical factors that influence asthma severity, emergency room (ER) utilization, and patient risk levels. The goal is to support data-driven clinical decision-making and inform public health strategies related to asthma management.

---

## Dataset
- **File:** `synthetic_asthma_dataset.csv`  
- **Records:** 10,000 patients  
- **Variables:** 17 clinically meaningful features including:
  - **Demographics:** Age, Gender, BMI  
  - **Lifestyle:** Smoking status, Physical activity  
  - **Environmental exposure:** Air pollution level  
  - **Clinical markers:** FeNO levels, Peak expiratory flow  
  - **Healthcare outcomes:** ER visits, Asthma control level  
  - Medication adherence and comorbidities  

The dataset is synthetic but designed to closely reflect real-world asthma research data.

---

## Analysis & Methods
The analysis is implemented in **R** (`Asthma full code.R`) and includes:
- Exploratory Data Analysis (EDA) and visualizations  
- Multiple linear and Poisson regression for ER visit modeling  
- Logistic and LASSO regression for asthma prediction  
- Ordinal and multinomial logistic regression for asthma severity  
- Interaction analysis (Medication Adherence × Air Pollution)  
- Nonparametric methods (Kruskal–Wallis, Wilcoxon, Spearman correlation, Runs test)  

ER visits are also used as a proxy for high healthcare utilization where direct readmission data is unavailable.

---

## Files in This Repository
- `Asthma full code.R` – Complete R analysis script  
- `synthetic_asthma_dataset.csv` – Dataset used for analysis  
- `Final Report - Asthma Management Analysis.pdf` – Detailed project report  
- `Asthma Group Project Presentation.pptx` – Summary presentation of findings  

---

## Key Outcomes
- Identification of factors associated with poor asthma control  
- Insights into predictors of repeated ER utilization  
- Evaluation of joint effects of medication adherence and air pollution  
- Robust validation using nonparametric statistical methods  

---

## Tools & Libraries
R, dplyr, ggplot2, glmnet, pROC, MASS, nnet, randtests

---

## Disclaimer
This project uses **synthetic data** for academic and analytical purposes only and does not contain real patient information.
