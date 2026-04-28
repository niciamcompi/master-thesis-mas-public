# Anomaly Detection in Pension Funds & Global Risks

A machine learning project for detecting financial anomalies in pension fund mandates, combining macroeconomic indicators, financial data, and sentiment analysis.

---

## 📌 Overview

This project was developed as part of a **MAS Data Science programme** and focuses on applying machine learning techniques to **financial risk management**.

The goal is to identify anomalies in pension fund mandates and understand how they relate to global economic and geopolitical events.

---

## 🎯 Objectives

* Detect financial anomalies in pension fund portfolios
* Develop a machine learning framework for anomaly detection
* Analyse the relationship between anomalies and global risks
* Improve risk management strategies using data-driven insights

---

## 🏦 Context: Swiss Pension System

The analysis is based on the Swiss three-pillar system:

* **AHV (1st pillar)** – state pension
* **BVG (2nd pillar)** – occupational pension
* **3rd pillar** – private pension

Key challenges:

* Low interest rates
* Market volatility
* Ageing population

---

## 📊 Data Sources

The project integrates multiple data sources:

### Macroeconomic Indicators

* GDP (SECO, BFS)
* Inflation & yield curves
* Unemployment statistics

### Financial Data

* Forex data (OANDA API)
* Gold prices
* Market indices

### Sentiment Data

* Global Risk Reports (WEF)
* Processed using **FinBERT**

---

## 🤖 Models & Methodology

The project combines different machine learning approaches:

### 1. Isolation Forest

* Unsupervised model
* Detects outliers by isolating anomalies

### 2. LSTM (Long Short-Term Memory)

* Time-series model
* Learns temporal financial patterns

### 3. Ensemble Model

* Combines Isolation Forest + LSTM
* Improves detection accuracy

---

## 📈 Results

* **Accuracy:** 97.3%
* **F1 Score:** 0.798

### Key Insights

* Financial anomalies align with major global events (2016–2024)
* Clear anomaly patterns across economic regimes:

| Period    | Events                | Impact                 |
| --------- | --------------------- | ---------------------- |
| 2016–2018 | Brexit, trade wars    | Forex volatility       |
| 2019–2021 | COVID-19 stimulus     | Artificial stability   |
| 2022–2024 | Inflation, rate hikes | High anomaly frequency |

---

## 📊 Case Studies

### Defensive Mandate (31710)

* High allocation to gold & CHF assets
* Strong resilience during crises
* Reduced drawdowns (4.3% vs 12.7%)

### Cyclical Mandate (25135)

* High exposure to energy & industrials
* Increased volatility during rate hikes
* Improved performance after reallocation

---

## 🌍 Link to Global Risks

The model findings align with global risk factors:

* Inflation shocks → anomaly spikes
* Geopolitical events → market disruptions
* Economic downturns → increased volatility
* Sentiment analysis improves predictive power

---

## 🧠 Key Takeaways

* AI-based anomaly detection enhances financial risk management
* Diversification reduces anomaly exposure
* Currency hedging is critical in volatile markets
* Sentiment analysis adds valuable predictive signals

---

## 🚀 Future Work

* Real-time macroeconomic data integration
* Improved feature engineering
* Multi-scale time-series modelling
* Adaptive portfolio rebalancing based on anomaly signals

---

## ⚙️ Technologies Used

* Python
* pandas, numpy
* scikit-learn
* TensorFlow / PyTorch (for LSTM)
* FinBERT (NLP)

---

## 👤 Author

Nicola Rothlin
MAS Data Science

