# CSP-571 Group 6
# Mushroom Classification Project

## Group Member
- Liwei He 

## Overview

This project predicts whether a simulated mushroom record is edible or
poisonous. It compares three models:

- Logistic regression
- Decision tree
- Random forest

## Dataset

The project uses the
[UCI Secondary Mushroom Dataset](https://archive.ics.uci.edu/dataset/848/secondary+mushroom+dataset).

## Repository Structure

```
CS-485/
├── data/                 Raw data and metadata
├── code/
│   ├── 01_eda.R          Data reading, cleaning, and EDA
│   └── 02_modeling.R     Model training and evaluation
├── report/
│   ├── figures/          Generated figures
│   ├── report.Rmd        Reproducible R Markdown report
│   └── report.html       Rendered analysis report
├── presentation/         Presentation materials
└── README.md
```

## Reproduce

The analysis can be run separately:

```r
source("code/01_eda.R")
source("code/02_modeling.R")
```

## Results

On the held-out test set, logistic regression achieved 86.55% accuracy, the
decision tree achieved 98.91%, and the random forest achieved 100%. The report
discusses why the perfect random forest result should be interpreted cautiously
for this simulated dataset.
