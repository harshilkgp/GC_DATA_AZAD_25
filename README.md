# General Championship - Data Analytics'25 - IIT Kharagpur

## 🏆 Award: Second Runner-Up  
### 🏁 Organized by: **Technology Students' Gymkhana IIT Kharagpur**  
### 📈 Problem Statement by: **AnalytixLabs**

---

## 🚀 Overview

This repository presents a comprehensive solution for **optimizing marketing investments and strategy** for **ElectroMart**, a consumer electronics retail firm in Ontario, Canada. Developed as part of the **General Championship Data Analytics 2025 Hackathon** at IIT Kharagpur, our solution integrates advanced statistical modeling, impact analysis, and optimization frameworks to address core business objectives.

---

## 🗂️ Repository Structure

```bash
├── solution/                                   # Folder containing main solution files
│   ├── Exploratory_Data_Analysis.ipynb         # EDA, SHAP-based Impact Analysis
│   ├── Risk_Analysis.ipynb                     # Herfindahl Index, Volatility, Risk Scores
│   ├── Supply_Chain_KPIs.ipynb                 # KPI's
│   ├── Market_Mix_Modelling.R                  # Meta's Robyn MMM Implementation
│   ├── Market_Mix_Optimization_Framework.ipynb # Novel Optimization Model (MISOCP)
├── Problem Statement.pdf                       # Probelm Statement
├── Data.txt                                    # Contains links to raw and processed datasets
├── Report.pdf                                  # Detailed methodology, results & business strategy
```

---

## 🛠️ Tech Stack

- **Languages**: Python, R  
- **Libraries**: Pandas, Numpy, Scikit, Scipy, Pingouin, Seaborn, SHAP, Catboost, XGBoost, GurobiPy, Robyn  
- **Tools**: Jupyter Notebook, Git
- **APIs**: Meteostat (Weather Integration)

---

## 📎 Data Sources 

- refer `data.txt` *for data download links.*

---

## 📌 Deliverbales Required to be tackled in the Problem Statement

- What drove revenue (Performance Driver Analysis)?
- How much did each marketing lever contribute (Marketing ROI/Impact Analysis)?
- How to allocate next year’s budget optimally?
- Which product categories should be targeted?
- Which marketing channels work best and why?

---

## 🧠 Detailed Info about our approach

NOTE:- Refer `Report.pdf` for more comprehensive idea of our approach.

### 1. 📊 EDA & Impact Analysis
- GMV trend, discount-revenue patterns, order fluctuations  
- Weather data integration via Meteostat API  
- SHAP + CatBoost for causal Impact Analysis

### 2. ⚠️ Risk Analysis
- Channel dependency & volatility using Herfindahl-Hirschman Index  
- Risk-stratified budget bounding (Low to Very High Risk channels)

### 3. 📈 Marketing Mix Modelling for budget allocation (Benchmark for our novel approach)
- Used Meta’s **Robyn** (SOTA MMM tool) in R  
- Daily spend interpolation, spline smoothing, ridge regression, saturation effects

### 4. 🧮 Our Novel Optimization Framework for optimal budget allocation, finding target product category and indicating best marketing channel
- Custom-built Mixed-Integer SOCP model using **Gurobi**  
- Multi-dimensional constraints (budget limits, risk factors, spend shifts) decided based on EDA and Risk Analysis 

---

## 👥 Contributors

This solution was developed with dedication, collaboration, and pa