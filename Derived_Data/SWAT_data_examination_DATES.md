Review of Maine DEP EGAD Mussel Tissue Toxics Data Sampling Dates
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
9/10/2020

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder Reference](#establish-folder-reference)
      - [Copy Data](#copy-data)
      - [Remove duplicates](#remove-duplicates)
  - [Sampling Dates and Sites](#sampling-dates-and-sites)
      - [How Many Times has Each Site Been
        Sampled?](#how-many-times-has-each-site-been-sampled)
      - [Period of Data](#period-of-data)
      - [How Many Sampling Events Each
        Year?](#how-many-sampling-events-each-year)

<img
  src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
  style="position:absolute;top:10px;right:50px;" />

# Introduction

Maine’s Department of Environmental Protection (DEP) maintains a large
database of environmental data called “EGAD”. Citizens can request data
from the database through DEP staff.

CBEP requested data from DEP on levels of toxic contaminants in
shellfish tissue samples from Casco Bay. The result is a large (\>
100,000 line) excel spreadsheet containing data from about 40 sampling
dates from 20 locations, over a period of more than 15 years.

Unfortunately, the data delivery contains little metadata, so it takes
some effort to understand the data format and analyze it correctly.
Among other problems, we need to understand dates and locations of
samples, what analytes were used for different samples, etc.

In this notebook and accompanying notebooks, we take various slices
through the data to understand its structure.

# Load Libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(htmltools)  # used by knitr called here only to avoid startup text later in document
library(knitr)
```

# Load Data

## Establish Folder Reference

``` r
sibfldnm <- 'Original_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)
fn <- 'CascoBaySWATtissue_Bohlen.xlsx'
```

## Copy Data

This is a larger data file that takes some time to load. Getting the
column types right dramatically improves load speed. Much of the data is
qualitative, and can’t be handled in R.

``` r
SWAT_data <- read_excel(file.path(sibling, fn), 
    sheet = "Mussels Data", col_types = c("numeric", 
        "text", "text", "text", "text", "text", 
        "text", "text", "text", "text", "text", 
        "text", "date", "text", "text", 
        "text", "date", "text", "numeric", 
        "text", "text", "text", "text", 
        "text", "numeric", "numeric", "text", 
        "text", "text", "text", "text", 
        "text", "numeric", "text", 
        "text", "text", "text", "text", 
        "text", "text", "text"))

before <- nrow(SWAT_data)
```

## Remove duplicates

Many samples – nearly 20% – are members of a group of duplicates. We can
think of no valid reason why two records should be exact duplicates in
this setting, so we remove all duplicates using the unique() function.

``` r
SWAT_data <- unique(SWAT_data)
```

# Sampling Dates and Sites

``` r
dates_data <- SWAT_data %>%
  select(SAMPLE_DATE, `EGAD_SITE_NAME`) %>%
  group_by(`EGAD_SITE_NAME`, `SAMPLE_DATE`) %>%
  summarize(Code =  first(sub('.* - ','', `EGAD_SITE_NAME`)), 
            Site =  first(sub(' - .*','', `EGAD_SITE_NAME`)),
            Year =  first(as.numeric(format(SAMPLE_DATE,'%Y'))),
                        .groups = 'drop') %>%
  rename(Date = SAMPLE_DATE) %>%
  select(Code, Site, Year, Date) %>%
  arrange(Year, Date)
kable(dates_data)
```

| Code   | Site                               | Year | Date       |
| :----- | :--------------------------------- | ---: | :--------- |
| CBFROR | FORE RIVER OUTER                   | 2003 | 2003-10-01 |
| CBRYMT | ROYAL RIVER MOUTH                  | 2003 | 2003-10-01 |
| CBGDSW | SOUTHWEST END GREAT DIAMOND ISLAND | 2003 | 2003-10-01 |
| CBFROR | FORE RIVER OUTER                   | 2003 | 2003-10-16 |
| CBRYMT | ROYAL RIVER MOUTH                  | 2003 | 2003-11-04 |
| CBFROR | FORE RIVER OUTER                   | 2003 | 2003-11-17 |
| CBGDSW | SOUTHWEST END GREAT DIAMOND ISLAND | 2003 | 2003-11-17 |
| CBMCMC | MILL CREEK                         | 2006 | 2006-10-12 |
| CBGDCC | COCKTAIL COVE GREAT DIAMOND ISLAND | 2006 | 2006-10-17 |
| CBMCMC | MILL CREEK                         | 2006 | 2006-10-30 |
| CBBBBB | BACK BAY                           | 2006 | 2006-11-29 |
| CBANAN | FALMOUTH ANCHORAGE                 | 2007 | 2007-10-18 |
| CBHRHR | HARASEEKET RIVER                   | 2007 | 2007-10-18 |
| CBHRHR | HARASEEKET RIVER                   | 2007 | 2007-10-22 |
| CBJWPB | JEWEL ISLAND PUNCHBOWL             | 2007 | 2007-10-22 |
| CBEEEE | EAST END BEACH                     | 2007 | 2007-10-31 |
| CBSPSP | S PORTLAND SPRING POINT            | 2007 | 2007-10-31 |
| CBMBBH | BRUNSWICK MARE BROOK DRAINAGE      | 2007 | 2007-11-05 |
| CBFRMR | MIDDLE FORE RIVER                  | 2007 | 2007-11-05 |
| CBMBMB | MIDDLE BAY (OUTER)                 | 2008 | 2008-10-21 |
| CBPRMT | PRESUMPSCOT RIVER (MOUTH)          | 2008 | 2008-11-18 |
| CBFRIR | INNER FORE RIVER                   | 2009 | 2009-09-22 |
| CBQHQH | QUAHOG BAY                         | 2009 | 2009-09-24 |
| CBMBBR | MAQUOIT BAY                        | 2009 | 2009-09-28 |
| CBLNFT | LONG ISLAND                        | 2009 | 2009-10-27 |
| CBEEEE | EAST END BEACH                     | 2009 | 2009-11-10 |
| CBMCMC | MILL CREEK                         | 2009 | 2009-11-10 |
| CBSPSP | S PORTLAND SPRING POINT            | 2010 | 2010-10-14 |
| CBEEEE | EAST END BEACH                     | 2011 | 2011-10-03 |
| CBMCMC | MILL CREEK                         | 2011 | 2011-10-04 |
| CBSPSP | S PORTLAND SPRING POINT            | 2012 | 2012-09-25 |
| CBEEEE | EAST END BEACH                     | 2013 | 2013-10-09 |
| CBMCMC | MILL CREEK                         | 2014 | 2014-09-17 |
| CBHWNP | NAVY PIER                          | 2014 | 2014-09-19 |
| CBMBBH | BRUNSWICK MARE BROOK DRAINAGE      | 2014 | 2014-09-29 |
| CBSPSP | S PORTLAND SPRING POINT            | 2015 | 2015-09-21 |
| CBEEEE | EAST END BEACH                     | 2015 | 2015-09-22 |
| CBMBBH | BRUNSWICK MARE BROOK DRAINAGE      | 2016 | 2016-09-22 |
| CBEEEE | EAST END BEACH                     | 2017 | 2017-10-11 |
| CBMCMC | MILL CREEK                         | 2017 | 2017-10-12 |

That Suggests we have FORTY unique sampling SAMPLE\_DATEs and SITEs Note
that there are several times that we have multiple sites sampled in a
given date, and several times that the same site is sampled multiple
times in the same year.

## How Many Times has Each Site Been Sampled?

``` r
dates_data %>%
  group_by(Site, Year) %>%
  mutate(count = n())  %>%
  ungroup() %>%
  group_by(Site) %>%
  summarize(spread = range(Year)[[2]]-range(Year)[[1]] + 1,
            nsamps = n(),
            maxsamps = max(count),
            .groups = 'drop') %>%
  kable(col.names = c('Site',
                      'Period between First and Last Samples (Years)',
                      'Total Number of Sampling Events',
                      'Maximum Number of Samples in One Year'))
```

| Site                               | Period between First and Last Samples (Years) | Total Number of Sampling Events | Maximum Number of Samples in One Year |
| :--------------------------------- | --------------------------------------------: | ------------------------------: | ------------------------------------: |
| BACK BAY                           |                                             1 |                               1 |                                     1 |
| BRUNSWICK MARE BROOK DRAINAGE      |                                            10 |                               3 |                                     1 |
| COCKTAIL COVE GREAT DIAMOND ISLAND |                                             1 |                               1 |                                     1 |
| EAST END BEACH                     |                                            11 |                               6 |                                     1 |
| FALMOUTH ANCHORAGE                 |                                             1 |                               1 |                                     1 |
| FORE RIVER OUTER                   |                                             1 |                               3 |                                     3 |
| HARASEEKET RIVER                   |                                             1 |                               2 |                                     2 |
| INNER FORE RIVER                   |                                             1 |                               1 |                                     1 |
| JEWEL ISLAND PUNCHBOWL             |                                             1 |                               1 |                                     1 |
| LONG ISLAND                        |                                             1 |                               1 |                                     1 |
| MAQUOIT BAY                        |                                             1 |                               1 |                                     1 |
| MIDDLE BAY (OUTER)                 |                                             1 |                               1 |                                     1 |
| MIDDLE FORE RIVER                  |                                             1 |                               1 |                                     1 |
| MILL CREEK                         |                                            12 |                               6 |                                     2 |
| NAVY PIER                          |                                             1 |                               1 |                                     1 |
| PRESUMPSCOT RIVER (MOUTH)          |                                             1 |                               1 |                                     1 |
| QUAHOG BAY                         |                                             1 |                               1 |                                     1 |
| ROYAL RIVER MOUTH                  |                                             1 |                               2 |                                     2 |
| S PORTLAND SPRING POINT            |                                             9 |                               4 |                                     1 |
| SOUTHWEST END GREAT DIAMOND ISLAND |                                             1 |                               2 |                                     2 |

So, there are basically TWO sites which have been sampled fairly
regularly, and a handful of sites sampled more than twice. Trend
analysis may be possible looking at:

  - Mare Brook (three times over ten years)  
  - East End Beach (six times over eleven years)  
  - Mill Creek (Six Times over twelve years)  
  - Spring Point (four times over nine years)

Notice that the “Fore River Outer” site was sampled three times in one
year.

## Period of Data

``` r
range(dates_data$Year)
```

    ## [1] 2003 2017

## How Many Sampling Events Each Year?

``` r
dates_data %>%
  group_by(Year,Date) %>%
  summarize(Date = first(Date), .groups = 'drop_last') %>%
  summarize(count = n())
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 13 x 2
    ##     Year count
    ##    <dbl> <int>
    ##  1  2003     4
    ##  2  2006     4
    ##  3  2007     4
    ##  4  2008     2
    ##  5  2009     5
    ##  6  2010     1
    ##  7  2011     2
    ##  8  2012     1
    ##  9  2013     1
    ## 10  2014     3
    ## 11  2015     2
    ## 12  2016     1
    ## 13  2017     2
