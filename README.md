# Data analysis Health and lifestyle dataset
A portfolio showcasing R data analysis projects, including data preparation, exploratory analysis, contingency table analysis, principal component analysis, correspondence analysis, and hierarchical and non-hierarchical cluster analysis.

**Health-and-lifestyle-dataset** : (https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset/code)
## Table of Contents
1. [Data Preparation](#data-preparation)
2. [Exploratory Data Analysis](#exploratory-data-analysis)
3. [Contingency Table Analysis and Measures of Association](#contingency-table-analysis-and-measures-of-association)
4. [Principal Component Analysis](#principal-component-analysis)
5. [Correspondence Analysis](#correspondence-analysis)
6. [Hierarchical and Non-Hierarchical Cluster Analysis](#hierarchical-and-non-hierarchical-cluster-analysis)
7. [Appendix](#appendix)

## 1. Data Preparation
The Sleep Health and Lifestyle Dataset contains 400 rows and 13 columns, covering a wide range of variables related to sleep and daily habits. Key features include comprehensive sleep metrics, lifestyle factors, cardiovascular health, and sleep disorder analysis.

### Included Variables:
- **Person ID:** Identifier for each individual.
- **Gender:** Gender of the person (Male/Female).
- **Age:** Age of the person in years.
- **Occupation:** Person's occupation or profession.
- **Sleep Duration (hours):** Number of hours the person sleeps per day.
- **Quality of Sleep (scale: 1-10):** Subjective rating of sleep quality on a scale of 1 to 10.
- **Physical Activity Level (minutes/day):** Number of minutes the person engages in physical activity daily.
- **Stress Level (scale: 1-10):** Subjective rating of the person's stress level on a scale of 1 to 10.
- **BMI Category:** Person's BMI category (Underweight, Normal, Overweight).
- **Blood Pressure (systolic/diastolic):** Person's blood pressure measurement, indicated as systolic pressure over diastolic pressure.
- **Heart Rate (bpm):** Person's resting heart rate in beats per minute.
- **Daily Steps:** Number of steps taken by the person per day.
- **Sleep Disorder:** Presence or absence of a sleep disorder in the person (None, Insomnia, Sleep Apnea).

### Data Recoding:
The variables were recoded to the correct format:
- **Nominal Categorical Variables:** Gender, Occupation, BMI Category, and Sleep Disorder.
- **Ordinal Variables:** Quality of Sleep and Stress Level.
- **Discrete Quantitative Variables:** Age and Daily Steps.
- **Continuous Quantitative Variables:** Sleep Duration, Physical Activity Level, Blood Pressure, and Heart Rate. The "Blood Pressure" variable was split into two separate columns: Systolic and Diastolic.

### Handling Missing Values:
The original dataset was checked for the presence of NA and infinite values. No NA or infinite values were found in the original dataset, so random NA value imputation was performed. A random seed was set for reproducibility, and 3% of the data was randomly assigned NA values. A total of 154 NA values were added to the dataset, representing 2.94% of the total data.
Next, the randomly introduced NA values were imputed using the K-nearest neighbors (KNN) imputation method with k=7.

### Variable Comparison:
An analysis was conducted to compare the original variables with those imputed by KNN. KNN imputation did not significantly alter the variables, with most variables remaining unaffected. The only variable that saw a slight change was "Occupation," where some minor adjustments were observed in the imputed values.

## 2. Exploratory Data Analysis
In this section, we conduct a descriptive study of 8 variables, selecting 2 of each type.

## 3. Contingency Table Analysis and Measures of Association
This section focuses on contingency tables, summarizing the relationship between different variables. It also studie the relationship between variables, specifically focusing on independence.

## 4. Principal Component Analysis
Principal Componenet Analysis is a technique used  for reducing the dimensionality of data, aiming to explain the most information with the fewest variables. In this study, quantitative variables such as age, daily steps, sleep duration, and physical activity level are analyzed. This analysis reveals two principal components representing an active lifestyle and the influence of age and sleep.

## 5. Correspondence Analysis
The correspondence analysis is a multivariate statistical technique applied to categorical data. Analysis of the percentage of explained variance suggests that 9 components are necessary to capture 78.34% of the total variance. In conclusion, 9 compenets are need to explain 4 variables, which is not as useful, this is due to the fact that there are a lot of categories in each variable. 

## 6. Hierarchical and Non-Hierarchical Cluster Analysis
Cluster analysis, a multivariate statistical technique for classifying individuals, includes two types: hierarchical and non-hierarchical. In the non-hierarchical approach, the K-means algorithm is used to group individuals based on various variables like gender, age, and sleep quality. After determining the optimal number of clusters using the elbow method, K-means analysis is conducted, and results are tabulated, revealing relationships between variables such as stress level, occupation, and age. On the other hand, hierarchical analysis, conducted using methods like complete, average, and single linkage, provides a visual representation of cluster structure hierarchically, facilitating the interpretation of data grouping.
