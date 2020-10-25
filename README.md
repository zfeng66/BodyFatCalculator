# STAT628-Module2: Body Fat Calculator (Group 1)

## Group Members
Mengqi Li (<mli653@wisc.edu>)  
Xiangyu Wang (<xwang2439@wisc.edu>)  
Yiqun Xiao (<yxiao85@wisc.edu>)  
Zijun Feng (<zfeng66@wisc.edu>)

## Project Introduction
Body fat is an important indicator of physical health, but it is not easy to measure directly and conveniently. In this project, our goal is to come up with a simple, robust, and accurate "rule-of-thumb" to estimate percentage of body fat using clinically available measurements. The statistical model is based on a real data set of 252 men with measurements of their percentage of body fat and various body circumference measurements. 

## Model

We tried to build up a regression model to predict body fat percentage. We did model selection and get the best model with 3 predictors (weight, abdomen circumference and wrist circumference). We also constructed a [body fat calculator](https://zijunfeng.shinyapps.io/body-fat-calculator/) on Shiny App server based on our model.

## Repository Contents:
-	a `data` folder containing the raw and cleaned data sets:
    1. `data/BodyFat.csv` is the raw data set.
    2. `data/BodyFat_clean.csv` is the cleaned data set.
-	a `code` folder containing all the code for analysis (Please set the working directory to the **main** folder `STAT628Module2/` before running code):
    1. `code/data_clean.R` is for data cleaning. It reads `data/BodyFat_clean.csv` and writes `data/BodyFat_clean.csv`.
    2. `code/BodyFat_clean.csv` is for model selection, statistical analysis and model diagnostics.
    3. `code/body-fat-calculator/` folder contains code for body fat calculator on Shiny App.
-	an `image` folder containing any figures/images/tables produced in analysis. The discriptions of figures in this folder can be found in correspoinding code comments.
- a two-page executive **summary** file `BodyFat_summary.pdf`.

## Body Fat Calculator (*Shiny App*):
Link: <https://zijunfeng.shinyapps.io/body-fat-calculator/>

