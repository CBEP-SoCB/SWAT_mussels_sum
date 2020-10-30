Review of Maine DEP EGAD Mussel Tissue Toxics Data UNITS
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
9/10/2020

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder Reference](#establish-folder-reference)
      - [Copy Data](#copy-data)
      - [Remove duplicates](#remove-duplicates)
      - [Simplify Data and Add Unique Sample
        Codes](#simplify-data-and-add-unique-sample-codes)
  - [What Units are Used?](#what-units-are-used)
  - [Calculate Unit Equivalences](#calculate-unit-equivalences)
      - [Metric Prefixes](#metric-prefixes)
      - [Calculate Multipliers for
        Units](#calculate-multipliers-for-units)
  - [Express Data in Consistent
    Units](#express-data-in-consistent-units)

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

In this notebook we examine how units of concentration are expressed,
and develop a strategy to generate data in consistent units regardless
of how data was originally expressed.

# Load Libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(htmltools)  # used by knitr called here only to avoid startup text later in document
library(knitr)

library(CBEPgraphics)
load_cbep_fonts()
theme_set
```

    ## function (new) 
    ## {
    ##     old <- ggplot_global$theme_current
    ##     ggplot_global$theme_current <- new
    ##     invisible(old)
    ## }
    ## <bytecode: 0x000000001659c400>
    ## <environment: namespace:ggplot2>

``` r
library(LCensMeans)
```

# Load Data

## Establish Folder Reference

``` r
auntfldnm <- 'Original_Data'
parent   <- dirname(getwd())
grandparent <- dirname(parent)
aunt  <- file.path(grandparent,auntfldnm)
fn <- 'CascoBaySWATtissue_Bohlen.xlsx'
```

## Copy Data

This is a larger data file that takes some time to load. Getting the
column types right dramatically improves load speed. Much of the data is
qualitative, and can’t be handled in R.

``` r
SWAT_data <- read_excel(file.path(aunt, fn), 
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

## Simplify Data and Add Unique Sample Codes

This logic was developed in “SWAT\_data examination\_UNIQUE.Rmd”.

``` r
SWAT_simplified <- SWAT_data %>%
  # Eliminate uninformative identifiers
  select    (-`SAMPLE TYPE`, -`SAMPLE POINT TYPE`, -`SAMPLE LOCATION`,
             -`RESULT TYPE`, -`PARAMETER_QUALIFIER`, -`PARAMETER FILTERED`,
             -`SAMPLE FILTER`, -`DEPTH`, -`DEPTH UNITS`,
             -TREATMENT, -`METER_CALIBRATED`) %>%
  
  # Eliminate data we will not analyze
  select    (-SITE_DESCRIPTION, -ANALYSIS_DATE,
             -`QC TYPE`, -SAMPLED_BY, -`UNITS DESCRIPTION`,
             -`SAMPLE COMMENT`, -`LAB COMMENT`, -`VALIDATION COMMENT`) %>%
  
  # Create Site Code and Site Name
  mutate    (SiteCode =  sub('.* - ','', `EGAD_SITE_NAME`), 
             Site     =  sub(' - .*','', `EGAD_SITE_NAME`)) %>%
  select    (-EGAD_SITE_NAME) %>%
  
  # Create Year Time Stamp and (Draft 1) Unique Sample ID
  mutate    (Year  = as.numeric(format(SAMPLE_DATE, '%Y')),
             sample_id = gsub(" ", "_", SAMPLE_ID)) %>%
  group_by  (Year) %>%
  mutate    (tag = as.numeric(factor(SAMPLE_DATE))) %>%
  ungroup   ()  %>%
  mutate    (Code = paste(sample_id, Year, tag, sep = '_')) %>%
  select    (-sample_id, -tag) %>%
  select    (`SITE SEQ`, SiteCode, Site, Year, SAMPLE_DATE,
              SAMPLE_ID, Code, everything())
```

# What Units are Used?

We need to deal with data reported in multiple different units. Our
interest here focuses only on units of concentration.

``` r
knitr::kable(xtabs(~PARAMETER+`UNITS VALUE`, data  = SWAT_data))
```

|                                              |   % |  G | MG/KG | NG/G | NG/KG | PG/G | TRUE/FALSE | UG/G | UG/KG |
| :------------------------------------------- | --: | -: | ----: | ---: | ----: | ---: | ---------: | ---: | ----: |
| 1-METHYL NAPHTHALENE                         |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| 1-METHYLCHRYSENE                             |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 1-METHYLPHENANTHRENE                         |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| 1,2-DIMETHYLNAPHTHALENE                      |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 1,2,3,4,6,7,8-HPCDD                          |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 1,2,3,4,6,7,8-HPCDF                          |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 1,2,3,4,7,8-HXCDD                            |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 1,2,3,4,7,8-HXCDF                            |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 1,2,3,4,7,8,9-HEPTACHLORODIBENZOFURAN(HPCDF) |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 1,2,3,6,7,8-HXCDD                            |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 1,2,3,6,7,8-HXCDF                            |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 1,2,3,7,8-PECDD                              |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 1,2,3,7,8-PECDF                              |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 1,2,3,7,8,9-HXCDD                            |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 1,2,3,7,8,9-HXCDF                            |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 1,2,6-TRIMETHYLPHENANTHRENE                  |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 1,4,6,7-TETRAMETHYLNAPHTHALENE               |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 1,7-DIMETHYLFLUORENE                         |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 1,7-DIMETHYLPHENANTHRENE                     |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 1,8-DIMETHYLPHENANTHRENE                     |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 2’,3,4,4’,5-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    24 |  318 |          0 |    0 |     0 |
| 2-CHLOROBIPHENYL                             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2-METHYLANTHRACENE                           |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 2-METHYLFLUORENE                             |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 2-METHYLNAPHTHALENE                          |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| 2-METHYLPHENANTHRENE                         |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 2,2’-DICHLOROBIPHENYL                        |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,4’,5-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    24 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,4’,5,5’-OCTACHLOROBIPHENYL       |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,4’,5,5’,6-NONACHLOROBIPHENYL     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,4’,5,6’-OCTACHLOROBIPHENYL       |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,4’,5,6-OCTACHLOROBIPHENYL        |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,4’,5,6,6’-NONACHLOROBIPHENYL     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5’,6’-HEPTACHLOROBIPHENYL        |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5’,6-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5’,6,6’-OCTACHLOROBIPHENYL       |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5,5’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5,5’,6,6’-NONACHLOROBIPHENYL     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5,6’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,6’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,6-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,4,6,6’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,5-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |     7 |    0 |          0 |    0 |     0 |
| 2,2’,3,3’,5,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,5,5’,6-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,5,5’,6,6’-OCTACHLOROBIPHENYL       |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,5,6,6’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,6-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,3’,6,6’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4’,5,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4’,5,5’,6-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4’,5,6’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4’,5,6,6’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4’,6,6’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4,4’,5-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4,4’,5,5’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |     8 |    0 |          0 |    0 |     0 |
| 2,2’,3,4,4’,5,5’,6-OCTACHLOROBIPHENYL        |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4,4’,5,6’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4,4’,5,6-HEPTACHLOROBIPHENYL          |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4,4’,5,6,6’-OCTACHLOROBIPHENYL        |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4,4’,6,6’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4,5’,6-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4,5,5’-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4,5,6-HEXACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4,5,6,6’-HEPTACHLOROBIPHENYL          |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4,6’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,4,6,6’-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,5’,6-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |     7 |    0 |          0 |    0 |     0 |
| 2,2’,3,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,5,5’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,5,6’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,5,6,6’-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,6’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,3,6,6’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,4-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,4,4’,5-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |     7 |    0 |          0 |    0 |     0 |
| 2,2’,4,4’,5,6’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |     7 |    0 |          0 |    0 |     0 |
| 2,2’,4,4’,6,6’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,4,5’,6-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,4,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,4,6,6’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,5,5’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    23 |  318 |          0 |    0 |     0 |
| 2,2’,6-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,2’,6,6’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3’-DICHLOROBIPHENYL                        |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3’,4-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3’,4,4’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3’,4,4’,5-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    24 |  318 |          0 |    0 |     0 |
| 2,3’,4,4’,5,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    24 |  318 |          0 |    0 |     0 |
| 2,3’,4,5’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3’,4,5’,6-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3’,4,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3’,4,5,5’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3’,5’-TRICHLOROBIPHENYL                    |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3’,5’,6-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3’,5,5’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3’,6-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3-DICHLOROBIPHENYL                         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4’,5’-PENTACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4’,5’,6-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4’,5,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4,4’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    24 |  318 |          0 |    0 |     0 |
| 2,3,3’,4,4’,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |     8 |    0 |          0 |    0 |     0 |
| 2,3,3’,4,4’,5’,6-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4,4’,5-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |     8 |    0 |          0 |    0 |     0 |
| 2,3,3’,4,4’,5,5’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    24 |  318 |          0 |    0 |     0 |
| 2,3,3’,4,4’,5,5’,6-OCTACHLOROBIPHENYL        |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4,4’,5,6-HEPTACHLOROBIPHENYL          |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4,4’,6-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4,5’,6-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4,5-PENTACHLOROBIPHENYL               |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4,5,5’-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4,5,5’,6-HEPTACHLOROBIPHENYL          |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,4,5,6-HEXACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |     7 |    0 |          0 |    0 |     0 |
| 2,3,3’,4,6-PENTACHLOROBIPHENYL               |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,5’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,5,5’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,5,5’,6-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,3’,5,6-PENTACHLOROBIPHENYL               |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,4’-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,4’,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,4’,6-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,4,4’-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,4,4’,5-PENTACHLOROBIPHENYL               |   0 |  0 |     0 |    0 |    24 |  318 |          0 |    0 |     0 |
| 2,3,4,6,7,8-HXCDF                            |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 2,3,4,7,8-PECDF                              |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 2,3,5-TRICHLOROBIPHENYL                      |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,5-TRIMETHYLNAPHTHALENE                   |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| 2,3,6-TRICHLOROBIPHENYL                      |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,3,6-TRIMETHYLNAPHTHALENE                   |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 2,3,7,8-TCDD                                 |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 2,3,7,8-TETRACHLORODIBENZOFURAN              |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| 2,4’-DDD                                     |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| 2,4’-DDE                                     |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| 2,4’-DDT                                     |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| 2,4’-DICHLOROBIPHENYL                        |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,4’,5-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,4’,6-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,4-DICHLOROBIPHENYL                         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,4-DIMETHYLDIBENZOTHIOPHENE                 |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 2,5-DICHLOROBIPHENYL                         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,6-DICHLOROBIPHENYL                         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 2,6-DIMETHYLNAPHTHALENE                      |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| 2,6-DIMETHYLPHENANTHRENE                     |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 2/3-METHYLDIBENZOTHIOPHENES                  |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 3-CHLOROBIPHENYL                             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 3-METHYLFLUORANTHENE/BENZO\[A\]FLUORENE      |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 3-METHYLPHENANTHRENE                         |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 3,3’-DICHLOROBIPHENYL                        |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 3,3’,4-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 3,3’,4,4’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    24 |  318 |          0 |    0 |     0 |
| 3,3’,4,4’,5-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    24 |  318 |          0 |    0 |     0 |
| 3,3’,4,4’,5,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    24 |  318 |          0 |    0 |     0 |
| 3,3’,4,5’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 3,3’,4,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 3,3’,4,5,5’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 3,3’,5-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 3,3’,5,5’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 3,4’,5-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 3,4,4’-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 3,4,4’,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    24 |  318 |          0 |    0 |     0 |
| 3,4,5-TRICHLOROBIPHENYL                      |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 3,5-DICHLOROBIPHENYL                         |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 3,6-DIMETHYLPHENANTHRENE                     |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 4-CHLOROBIPHENYL                             |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 4,4’-DDD                                     |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| 4,4’-DDE                                     |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| 4,4’-DDT                                     |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| 4,4’-DICHLOROBIPHENYL                        |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| 5,9-DIMETHYLCHRYSENE                         |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 5/6-METHYLCHRYSENE                           |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 7-METHYLBENZO\[A\]PYRENE                     |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| 9/4-METHYLPHENANTHRENE                       |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| ACENAPHTHENE                                 |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| ACENAPHTHYLENE                               |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| ALACHLOR                                     |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| ALDRIN                                       |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| ALPHA-BHC                                    |   0 |  0 |     0 |  228 |    15 |    0 |          0 |    0 |     0 |
| ALPHA-CHLORDANE                              |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| ALUMINUM                                     |   0 |  0 |   148 |    0 |     8 |    0 |          0 |  118 |     0 |
| AMETRYN                                      |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| ANTHRACENE                                   |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| ARSENIC                                      |   0 |  0 |   130 |    0 |     0 |    0 |          0 |  118 |     0 |
| ATRAZINE                                     |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| AZINPHOS-METHYL                              |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| BENZO(A)ANTHRACENE                           |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| BENZO(A)PYRENE                               |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| BENZO(B)FLUORANTHENE                         |   0 |  0 |     0 |  102 |     8 |    0 |          0 |    0 |     0 |
| BENZO(E)PYRENE                               |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| BENZO(G,H,I)PERYLENE                         |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| BENZO(K)FLUORANTHENE                         |   0 |  0 |     0 |    0 |     8 |    0 |          0 |    0 |     0 |
| BENZO\[B,J,K\]FLUORANTHENES                  |   0 |  0 |     0 |  219 |     0 |    0 |          0 |    0 |     0 |
| BENZO\[J,K\]FLUORANTHENES                    |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| BENZOFLUORANTHENE                            |   0 |  0 |     0 |    0 |     8 |    0 |          0 |    0 |     0 |
| BETA-BHC                                     |   0 |  0 |     0 |  228 |    15 |    0 |          0 |    0 |     0 |
| BIPHENYL                                     |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| BUTRALIN                                     |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| BUTYLATE                                     |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| C1-ACENAPHTHENES                             |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C1-BENZO\[A\]ANTHRACENES/CHRYSENES           |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C1-BENZOFLUORANTHENES/BENZOPYRENES           |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C1-BIPHENYLS                                 |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C1-DIBENZOTHIOPHENES                         |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C1-FLUORANTHENES/PYRENES                     |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C1-FLUORENES                                 |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C1-NAPHTHALENES                              |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C1-PHENANTHRENES/ANTHRACENES                 |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C2-BENZO\[A\]ANTHRACENES/CHRYSENES           |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C2-BENZOFLUORANTHENES/BENZOPYRENES           |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C2-BIPHENYLS                                 |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C2-DIBENZOTHIOPHENES                         |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C2-FLUORANTHENES/PYRENES                     |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C2-FLUORENES                                 |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C2-NAPHTHALENES                              |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C2-PHENANTHRENES/ANTHRACENES                 |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C3-BENZO\[A\]ANTHRACENES/CHRYSENES           |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C3-DIBENZOTHIOPHENES                         |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C3-FLUORANTHENES/PYRENES                     |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C3-FLUORENES                                 |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C3-NAPHTHALENES                              |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C3-PHENANTHRENES/ANTHRACENES                 |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C4-BENZO\[A\]ANTHRACENES/CHRYSENES           |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C4-DIBENZOTHIOPHENES                         |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C4-FLUORANTHENES/PYRENES                     |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C4-NAPHTHALENES                              |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| C4-PHENANTHRENES/ANTHRACENES                 |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| CADMIUM                                      |   0 |  0 |   148 |    0 |     8 |    0 |          0 |  118 |     0 |
| CAPTAN                                       |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| CHLOROTHALONIL                               |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| CHLORPYRIFOS, O,O-DIMETHYL ANALOG            |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| CHLORPYRIPHOS                                |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| CHLORPYRIPHOS-OXON                           |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| CHROMIUM                                     |   0 |  0 |   148 |    0 |     8 |    0 |          0 |  118 |     0 |
| CHRYSENE                                     |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| CIS-NONACHLOR                                |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| COPPER                                       |   0 |  0 |   148 |    0 |     8 |    0 |          0 |  118 |     0 |
| CYANAZINE                                    |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| CYPERMETHRIN                                 |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| DCPA                                         |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| DECACHLOROBIPHENYL                           |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| DELTA-BHC                                    |   0 |  0 |     0 |  228 |     7 |    0 |          0 |    0 |     0 |
| DESETHYLATRAZINE                             |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| DIAZINON                                     |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| DIAZINON-OXON                                |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| DIBENZO(A,H)ANTHRACENE                       |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| DIBENZOTHIOPHENE                             |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| DIELDRIN                                     |   0 |  0 |     0 |  228 |    15 |    0 |          0 |    0 |     0 |
| DIMETHENAMID                                 |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| DIMETHOATE                                   |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| DISULFOTON                                   |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| DISULFOTON SULFONE                           |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| DX TOTAL TEQ (ND=0)                          |   0 |  0 |     0 |    0 |     0 |   12 |          0 |    0 |     0 |
| DX TOTAL TEQ (ND=1/2 DL)                     |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| DX TOTAL TEQ (ND=DL)                         |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| ENDOSULFAN I                                 |   0 |  0 |     0 |  228 |    15 |    0 |          0 |    0 |     0 |
| ENDOSULFAN II                                |   0 |  0 |     0 |  228 |    15 |    0 |          0 |    0 |     0 |
| ENDOSULFAN SULFATE                           |   0 |  0 |     0 |  228 |     7 |    0 |          0 |    0 |     0 |
| ENDRIN                                       |   0 |  0 |     0 |  228 |     7 |    0 |          0 |    0 |     0 |
| ENDRIN ALDEHYDE                              |   0 |  0 |     0 |  156 |     7 |    0 |          0 |    0 |     0 |
| ENDRIN KETONE                                |   0 |  0 |     0 |  228 |     7 |    0 |          0 |    0 |     0 |
| ETHALFLURALIN                                |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| ETHION                                       |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| FENITROTHION                                 |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| FLUFENACET                                   |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| FLUORANTHENE                                 |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| FLUORENE                                     |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| FLUTRIAFOL                                   |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| FONOFOS                                      |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| GAMMA-BHC (LINDANE)                          |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| GAMMA-CHLORDANE                              |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| HEPTACHLOR                                   |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| HEPTACHLOR EPOXIDE                           |   0 |  0 |     0 |  231 |    15 |    0 |          0 |    0 |     0 |
| HEXACHLOROBENZENE                            |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| INDENO(1,2,3-CD)PYRENE                       |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| IRON                                         |   0 |  0 |   148 |    0 |     8 |    0 |          0 |  118 |     0 |
| LEAD                                         |   0 |  0 |   148 |    0 |     8 |    0 |          0 |  118 |     0 |
| LINURON                                      |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| LIPIDS                                       | 230 |  0 |     0 |    0 |     0 |    0 |          0 |    0 |     0 |
| MALATHION                                    |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| MERCURY                                      |   0 |  0 |    22 |   18 |     9 |    0 |          0 |  118 |   108 |
| METHAMIDOPHOS                                |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| METHOPRENE                                   |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| METHOXYCHLOR                                 |   0 |  0 |     0 |  228 |     7 |    0 |          0 |    0 |     0 |
| METHYL PARATHION                             |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| METOLACHLOR                                  |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| METRIBUZIN                                   |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| MIREX                                        |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| MOISTURE                                     | 202 |  0 |     0 |    0 |     0 |    0 |          0 |    0 |     0 |
| Mytilus edulis                               |   0 |  0 |     0 |    0 |     0 |    0 |        166 |    0 |     0 |
| NAPHTHALENE                                  |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| NICKEL                                       |   0 |  0 |   148 |    0 |     8 |    0 |          0 |  118 |     0 |
| OCDD                                         |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| OCDF                                         |   0 |  0 |     0 |    0 |    16 |   12 |          0 |    0 |     0 |
| OCTACHLOROSTYRENE                            |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| OXYCHLORDANE                                 |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| PARATHION                                    |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| PCB-012/013                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-018/030                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-020/028                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-021/033                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-026/029                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-040/041/071                              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-044/047/065                              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-045/051                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-049/069                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-050/053                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-059/062/075                              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-061/070/074/076                          |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-083/099                                  |   0 |  0 |     0 |    0 |     9 |  318 |          0 |    0 |     0 |
| PCB-085/116/117                              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-086/087/097/108/119/125                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-088/091                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-090/101/113                              |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-093/095/098/100/102                      |   0 |  0 |     0 |    0 |     9 |  318 |          0 |    0 |     0 |
| PCB-093/098/100/102                          |   0 |  0 |     0 |    0 |     7 |    0 |          0 |    0 |     0 |
| PCB-107/124                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-110/115                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-128/166                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-129/138/160/163                          |   0 |  0 |     0 |    0 |     9 |  318 |          0 |    0 |     0 |
| PCB-129/138/163                              |   0 |  0 |     0 |    0 |     7 |    0 |          0 |    0 |     0 |
| PCB-134/143                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-135/151                                  |   0 |  0 |     0 |    0 |     7 |    0 |          0 |    0 |     0 |
| PCB-135/151/154                              |   0 |  0 |     0 |    0 |     9 |  318 |          0 |    0 |     0 |
| PCB-139/140                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-147/149                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-153/168                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-156/157                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-171/173                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-180/193                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-183/185                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-197/200                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB-198/199                                  |   0 |  0 |     0 |    0 |    16 |  318 |          0 |    0 |     0 |
| PCB TOTAL TEQ (ND=0)                         |   0 |  0 |     0 |    0 |     9 |  318 |          0 |    0 |     0 |
| PCB TOTAL TEQ (ND=1/2 DL)                    |   0 |  0 |     0 |    0 |     9 |  318 |          0 |    0 |     0 |
| PCB TOTAL TEQ (ND=DL)                        |   0 |  0 |     0 |    0 |     9 |  318 |          0 |    0 |     0 |
| PCBS                                         |   0 |  0 |     0 |    0 |     9 |  210 |          0 |    0 |     0 |
| PENDIMETHALIN                                |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| PENTACHLORONITROBENZENE                      |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROBUTANE SULFONATE                    |   0 |  0 |     0 |   80 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROBUTANOATE                           |   0 |  0 |     0 |   80 |     0 |    0 |          0 |    0 |     0 |
| PERFLUORODECANOATE                           |   0 |  0 |     0 |   80 |     0 |    0 |          0 |    0 |     0 |
| PERFLUORODODECANOATE                         |   0 |  0 |     0 |   80 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROHEPTANOATE                          |   0 |  0 |     0 |   80 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROHEXANE SULFONATE                    |   0 |  0 |     0 |   80 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROHEXANOATE                           |   0 |  0 |     0 |   80 |     0 |    0 |          0 |    0 |     0 |
| PERFLUORONONANOATE                           |   0 |  0 |     0 |   80 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROOCTANE SULFONAMIDE                  |   0 |  0 |     0 |   80 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROOCTANE SULFONATE                    |   0 |  0 |     0 |   80 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROOCTANOATE                           |   0 |  0 |     0 |   68 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROOCTANOIC ACID                       |   0 |  0 |     0 |   12 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROPENTANOATE                          |   0 |  0 |     0 |   80 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROUNDECANOATE                         |   0 |  0 |     0 |   80 |     0 |    0 |          0 |    0 |     0 |
| PERMETHRIN                                   |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| PERTHANE                                     |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| PERYLENE                                     |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| PHENANTHRENE                                 |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| PHORATE                                      |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| PHOSMET                                      |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| PIRIMIPHOS-METHYL                            |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| PYRENE                                       |   0 |  0 |     0 |  321 |    16 |    0 |          0 |    0 |     0 |
| RETENE                                       |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| SELENIUM                                     |   0 |  0 |   148 |    0 |     8 |    0 |          0 |  118 |     0 |
| SILVER                                       |   0 |  0 |   148 |    0 |     8 |    0 |          0 |  118 |     0 |
| SIMAZINE                                     |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| SOLIDS-TOTAL RESIDUE (TS)                    | 115 |  0 |     0 |    0 |     0 |    0 |          0 |    0 |     0 |
| TEBUCONAZOL                                  |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| TECNAZENE                                    |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| TERBUFOS                                     |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH-D                                  |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH-H                                  |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH-O                                  |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH19-D                                |   0 |  0 |     0 |  321 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH19-H                                |   0 |  0 |     0 |  321 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH19-O                                |   0 |  0 |     0 |  321 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH24-D                                |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH24-H                                |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH24-O                                |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH40-D                                |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH40-H                                |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH40-O                                |   0 |  0 |     0 |  210 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PCB-D                                  |   0 |  0 |     0 |    0 |     9 |  324 |          0 |    0 |     0 |
| TOTAL PCB-H                                  |   0 |  0 |     0 |    0 |     9 |  324 |          0 |    0 |     0 |
| TOTAL PCB-O                                  |   0 |  0 |     0 |    0 |     9 |  324 |          0 |    0 |     0 |
| TOTAL PESTICIDES21-D                         |   0 |  0 |     0 |  270 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PESTICIDES21-H                         |   0 |  0 |     0 |  270 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PESTICIDES21-O                         |   0 |  0 |     0 |  270 |     0 |    0 |          0 |    0 |     0 |
| TRANS-NONACHLOR                              |   0 |  0 |     0 |  228 |    23 |    0 |          0 |    0 |     0 |
| TRIALLATE                                    |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| TRIFLURALIN                                  |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| VELPAR                                       |   0 |  0 |     0 |   72 |     0 |    0 |          0 |    0 |     0 |
| WEIGHT                                       |   0 | 54 |     0 |    0 |     0 |    0 |          0 |    0 |     0 |
| ZINC                                         |   0 |  0 |   148 |    0 |     8 |    0 |          0 |  118 |     0 |

  - Lipids, moisture and solids are in percent  
  - Sample weight is in g  
  - metals are in mg/kg or ug/g (roughly, ppm)  
  - Organic contaminants are mostly in ng/g, ng/kg or pg/g

In other words, there is little consistency.

# Calculate Unit Equivalences

We explore automating assignment of multipliers just to minimize risk of
typographical or other errors.

``` r
(units <- unique(SWAT_simplified$`UNITS VALUE`))
```

    ## [1] "NG/G"       "MG/KG"      "TRUE/FALSE" "%"          "PG/G"      
    ## [6] "UG/KG"      "NG/KG"      "G"          "UG/G"

``` r
(units <- units [-c(3,4,8)])
```

    ## [1] "NG/G"  "MG/KG" "PG/G"  "UG/KG" "NG/KG" "UG/G"

## Metric Prefixes

``` r
prefixes <- "Prefix, Exponent
N,  -9
M,  -3
K,  +3
U,  -6
P, -12"
p <- as.tibble(read.csv(text = prefixes))
```

    ## Warning: `as.tibble()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
# The following works, but is mighty ugly.  there must be a better way....
p <- p %>% mutate(Multiplier = 10^Exponent) %>% arrange(Exponent)
p
```

    ## # A tibble: 5 x 3
    ##   Prefix Exponent Multiplier
    ##   <chr>     <int>      <dbl>
    ## 1 P           -12   1.00e-12
    ## 2 N            -9   1.00e- 9
    ## 3 U            -6   1.00e- 6
    ## 4 M            -3   1.00e- 3
    ## 5 K             3   1.00e+ 3

## Calculate Multipliers for Units

### Find the Numerator and Denominator

``` r
(num <-  sub("/.*", '', units))
```

    ## [1] "NG" "MG" "PG" "UG" "NG" "UG"

``` r
(den <-  sub("^.*/", '', units))
```

    ## [1] "G"  "KG" "G"  "KG" "KG" "G"

### Construct a Simple Function

That matches units in grams with related exponent.

``` r
find_exponent <- function (x){
  if (nchar(x)==1) return(0)
  pre <- substr(x,1,1)
  if (pre %in% p$Prefix) {
     return(p$Exponent[p$Prefix == pre])
  }
  else
    return(NA_integer_)
}
find_exponent("NG")
```

    ## [1] -9

### Use lapply to Calculate Net Exponent for Units

``` r
conversions <- tibble(units = units, num = num, den = den) %>%
  mutate( numexp = unlist(lapply(num, find_exponent)),
          denexp = unlist(lapply(den, find_exponent)),
          netexp = numexp-denexp) %>%
  arrange(netexp)
conversions
```

    ## # A tibble: 6 x 6
    ##   units num   den   numexp denexp netexp
    ##   <chr> <chr> <chr>  <int>  <dbl>  <dbl>
    ## 1 PG/G  PG    G        -12      0    -12
    ## 2 NG/KG NG    KG        -9      3    -12
    ## 3 NG/G  NG    G         -9      0     -9
    ## 4 UG/KG UG    KG        -6      3     -9
    ## 5 MG/KG MG    KG        -3      3     -6
    ## 6 UG/G  UG    G         -6      0     -6

# Express Data in Consistent Units

In other Analyses, we have used ug/g for metals and ng/g for organic
contaminants. So we will express concentrations in those units,
extracting the correct conversion factors from the conversions table.

``` r
uggexp <- -6
nggexp <- -9

test <- SWAT_simplified %>% 
  select(CONCENTRATION, `UNITS VALUE`) %>%
  mutate(theexp = conversions$netexp[match(`UNITS VALUE`, conversions$units)]) %>%
  mutate(uggconvexp = uggexp - theexp,
         nggconvexp = nggexp - theexp) %>%
  mutate(conc_ugg = CONCENTRATION * 10^uggconvexp,
         conc_ngg = CONCENTRATION * 10^nggconvexp,) %>%
  select (-theexp, -uggconvexp, -nggconvexp)
head(test,10)
```

    ## # A tibble: 10 x 4
    ##    CONCENTRATION `UNITS VALUE` conc_ugg conc_ngg
    ##            <dbl> <chr>            <dbl>    <dbl>
    ##  1         8.1   NG/G           8100     8.1    
    ##  2         0.238 NG/G            238     0.238  
    ##  3         1.51  NG/G           1510     1.51   
    ##  4        19.3   NG/G          19300    19.3    
    ##  5      1200.    MG/KG          1200.    1.20   
    ##  6         1     TRUE/FALSE       NA    NA      
    ##  7        16.6   %                NA    NA      
    ##  8         2.24  MG/KG             2.24  0.00224
    ##  9        13.5   MG/KG            13.5   0.0135 
    ## 10       182.    MG/KG           182.    0.182
