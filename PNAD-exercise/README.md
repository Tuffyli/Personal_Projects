# PNAD-exercise — Alimony Rights Expansion & Young Women’s Education (Brazil)

This repo reproduces and extends [**Rangel (2006)**](https://www.jstor.org/stable/3876398) on Brazil’s expansion of **alimony rights** for women in stable unions.  
I focus on **young women (15–24)** and study how eligibility for alimony relates to **years of education** after the policy change.

> **Note:** This is part of the project for the Social Policy Evaluation course.
---

## Overview

- **Research question:** Did the expansion of alimony rights improve schooling outcomes for young women?
- **Data:** PNAD microdata, years **1992, 1993, 1995** (filtered sample included; full PNAD not redistributed).
- **Design:**  
  1) Construct treatment/control groups from marital/union status;  
  2) **Propensity Score Matching (PSM)** to balance covariates;  
  3) **Weighted OLS** (no FE, UF×Year FE, and FE + controls).

---

## Treatment & Control

- **Treated:** Women aged 15–24 in **non-civil unions** (consensual or religious only), and who are **household head or spouse**.  
- **Control:** Similar women in **legally recognized (civil/civil+religious) unions**, same household roles.

Key covariates used in matching: **age, race/color, UF (state), household per-capita income, number of children (male/female), total children**.

---

## Citation

Rangel, M. A. (2006). *Alimony Rights and Intrahousehold Allocation of Resources: Evidence from Brazil.*  
**The Economic Journal**, 116(513), 627–658. https://doi.org/10.1111/j.1468-0297.2006.01086.x

---
