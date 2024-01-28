---
title: "Rmarkdown Practice - COVID Data"
author: "R. Dadmun"
date: "2024-01-22"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Holy Hell Batman I hope this works

First we ensure our libraries are loaded
```{r libraries}
library(tidyverse)
library(dplyr)
library(lubridate)
```

Next we initialize the data by logging the links to the data as strings in its own dataset titled URLs

```{r Initialize}
 url_in <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

 file_names <- c("time_series_covid19_confirmed_globa.csv", 
"time_series_covid19_deaths_global.csv",
"time_series_covid19_confirmed_US.csv",
"time_series_covid19_deaths_US.csv")
 
urls <- str_c(url_in,file_names)
```

Next we load them into specific datasets for R to read.

```{Load datasets}
  global_cases <- read_csv(urls[1])
  global_deaths <- read.csv(urls[2])
  US_Cases <- read.csv(urls[3])
  US_deaths <- read.csv(urls[4])
```

Then we clean the data

```{r Clean Data}
global_cases <- global_cases %>%
      pivot_longer(cols = -c('Province/State',
                             'Country/Region', 
                             Lat,
                             Long),
                   names_to = "date",
                   values_to = "cases") %>%
      select(-c(Lat,Long))

global_deaths <- global_deaths %>%
      pivot_longer(cols = -c('Province.State',
                             'Country.Region',
                             Lat,
                             Long),
                   names_to = "date",
                   values_to = "deaths") %>%
      select(-c(Lat,Long))

global_deaths <- global_deaths %>%
  rename(Province_State = 'Province.State',
        Country_Region = 'Country.Region')

global_cases <- global_cases %>%
    rename(Province_State = 'Province/State',
          Country_Region = 'Country/Region')

global <- global_cases %>%
    full_join(global_deaths) %>%
    mutate(date = mdy(date))

global <- global %>% filter(cases>0)
```

Now for US cases

```{r US Clean}
US_Cases %>% 
  pivot_longer(cols = -(UID:Combined_Key),
               names_to = "date",
               values_to = "cases") %>%
  select(Admin2:cases) %>%
  mutate(date = mdy(date)) %>%
  select(-c(Lat,Long_))

US_deaths <- US_deaths %>% 
  pivot_longer(cols = -(UID:Population),
               names_to = "date",
               values_to = "cases") %>%
  select(Admin2:cases) %>%
  mutate(date = mdy(date)) %>%
  select(-c(Lat,Long_))

US <- US_Cases %>%
  full_join(US_deaths)

```

## Looking through the data to plan our visualization

Group the data to plan for visualizations

``` {r Grouping_US_Data}
US_by_state <- US %>%
  group_by(Province_State, Country_Region) %>%
  summarize(cases = sum(cases), deaths = sum(deaths), Population = sum(Population))

```

## Creating a visualization

```{Visualization}
US_totals %>%
  filter(cases > 0) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(aes(color = "cases")) +
  geom_point(aes(color = "cases")) +
  geom_line(aes(y = deaths, color = "deaths")) +
  geom_point(aes(y = deaths, color = "deaths")) +
  scale_y_log10() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90)) +
  labs(titile = "COVID19 in US", y = NULL)

```