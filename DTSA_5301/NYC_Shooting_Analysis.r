---
title: "NYC Shooting Data Analysis DTSA_5301"
author: "R. Dadmun"
date: "2024-01-22"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Introduction

This is a breakdown of every shooting incident that occurred in NYC from 2006-2023. This data is manually extracted every quarter and reviewed by the Office of Management Analysis and Planning before being posted on the NYPD website. Each record represents a shooting incident in NYC and includes information about the event, the location and time of occurrence. Please refer to [NYPD Shooting Incident Data (Historic) - CKAN](https://catalog.data.gov/dataset/nypd-shooting-incident-data-historic) for moreinformation about this dataset.

## Purpose

The purpose of this document is to show that I can create an Rmarkdown document to highlight my data analysis process, and provide a guide to reproduce the analysis I performed for DTSA 5301.

### Import Libraries

We will be using the tidyverse package and the NYPD Shooting Incident Data (Historic) from the link below:

The following code will install the tidyverse, lubridate, magrittr and dplyr packages, and the second line will include it as a library within the Rmarkdown file:

```{r libraries, message=FALSE, warning=FALSE}
#install.packages("tidyverse")
library(tidyverse)
library(lubridate)
```

Next, we would like to ensure we have a connection point to our ddataset. This dataset was pulled from <https://catalog.data.gov> as a CSV. We utilize the "read.csv" command to ensure our dataset is imported into R.

```{r import_NYC_Shooting_Data}
NYC_Shootings <- read.csv("https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD", header = TRUE)
head(NYC_Shootings)
```

## Cleaning the data

After the data has been loaded in, we want to take the time to clean the data. We have removed 9 fields from the data in order to slim the dataset down. 

Removed Fields:
 **PRECINCT**,
 **JURISDICTION_CODE**,
 **LOCATION_DESC**,
 **PERP_AGE_GROUP**,
 **PERP_SEX**,
 **PERP_RACE**,
 **X_COORD_CD**, 
 **Y_COORD_CD**,
 **Lon_Lat**. 

```{r Delete Unused Fields}
NYC_Shootings_2 <- NYC_Shootings %>%      select(INCIDENT_KEY, 
                   OCCUR_DATE,
                   OCCUR_TIME,
                   BORO, 
                   STATISTICAL_MURDER_FLAG,
                   VIC_AGE_GROUP,
                   VIC_SEX,
                   VIC_RACE,
                   Latitude,
                   Longitude)

# Return the column name along with the missing values
lapply(NYC_Shootings_2, function(x) sum(is.na(x)))
```

Key observations on data type conversion are:

* **INCIDENT_KEY** should be treated as a string.
* **BORO** should be treated as a factor.
* **VIC_AGE_GROUP** should be treated as a factor.
* **VIC_SEX** should be treated as a factor.
* **VIC_RACE** should be treated as a factor.

We also want to ensure that all the entries for Unknown or NA values are standardized, so we will ensure all cases of inputs like this now read "Unknown"

```{r Data Tidy and Transformations}
NYC_Shootings_2 <- NYC_Shootings_2 %>%
  replace_na(list(PERP_AGE_GROUP = "Unknown", PERP_SEX = "Unknown", PERP_RACE = "Unknown"))

# Remove extreme values in data
NYC_Shootings_2$VIC_SEX   = recode(NYC_Shootings_2$VIC_SEX, U = "Unknown")
NYC_Shootings_2$VIC_RACE   = recode(NYC_Shootings_2$VIC_RACE, UNKNOWN = "Unknown")
NYC_Shootings_2$INCIDENT_KEY = as.character(NYC_Shootings_2$INCIDENT_KEY)
NYC_Shootings_2$BORO = as.factor(NYC_Shootings_2$BORO)
NYC_Shootings_2$VIC_AGE_GROUP = as.factor(NYC_Shootings_2$VIC_AGE_GROUP)
NYC_Shootings_2$VIC_SEX = as.factor(NYC_Shootings_2$VIC_SEX)
NYC_Shootings_2$VIC_RACE = as.factor(NYC_Shootings_2$VIC_RACE)

# Return summary statistics
summary(NYC_Shootings_2)
```

## Research Questions:

1. Which part of New York has the most number of incidents? Of those incidents, how many are murder cases? 

2. Which groups of people were most likely to be victims of a shooting in NYC?

## Visualizing and Analyzing the Data

#1. Which part of New York has the most number of incidents? Of those incidents, how many are murder cases? 

```{r Q1, echo=FALSE}
Boro_Graph <- ggplot(NYC_Shootings_2, aes(x = BORO)) +
  geom_bar() +
  labs(title = "Boroughs of New York City",
       x = "Boroughs of New York City",
       y = "Count of Incidents") +
  theme_classic()

table(NYC_Shootings_2$BORO, NYC_Shootings_2$STATISTICAL_MURDER_FLAG)
table(NYC_Shootings_2$BORO)
```

Based on the Graph, we can see that Brooklyn is 1st in the number of incidents, with 10,932 incidents, 2,122 of which were flagged as murders. The Bronx is significantly lower than this, with 7,935 incidents, of which 1542 were flagged as murders. Queens then follows, with 4,094 incidents. Both Manhattan and Staten Island both have a significantly smaller number of incidents than Brooklyn, with Staten Island having both their number of incidents and the flags for murder numbering in the triple digits. 


#2. Which groups of people were most likely to be victims of a shooting in NYC?

```{r}
table(NYC_Shootings_2$VIC_AGE_GROUP)

table(NYC_Shootings_2$VIC_SEX)

table(NYC_Shootings_2$VIC_AGE_GROUP,NYC_Shootings_2$VIC_SEX)
```

Our analysis on Age Groups shows that those aged 25-44 are the most likely to be a victim of a shooting in NYC with 12,279 incidents, followed closely by the 18-24 age group which has 10,085 incidents. The other age ranges had a maximum of no more than 2900 incidents, coming nowhere close to the median age ranges. 

Next, we created a table to show the sum of different victim genders. Victims were overwhelmingly Male, with 24,683 incidents against them. This is almost 10x the number of incidents against females, which only number 2,615.

Finally, we create a table to split the data along both of these axes. Within this table, we can see that the group with the highest rate of incidents are Males, aged 22-44 with a count of 11,374. This matches the outcomes we found from our initial 2 analyses. The second highest rate of incidents occurred with Males aged 18-24, with a count of 9,287.


## Identifing Bias

This topic can easily spur discrimination and bias amongst researchers. Common knowledge would push that incidents of violence are more likely to women over men, and more likely to occur after dark than during the daytime. These are examples of bias that must be validated with hard data in order to be stated as a takeaway - in fact they must also be statistically significant. In our analysis, we discovered that there were significantly more incidents vs Males than Females. 

Racial biases can also be influenced when viewing the data, as any race with higher incident rates may be viewed as "more dangerous". This is a strong implicit bias, as it does not take the surrounding population demographics into account.If a specific demographic makes up the majority of the population within a specific area, than of course the rates of violent incidents perputrated by that demographic will be elevated as compared to other, less present demographics.
