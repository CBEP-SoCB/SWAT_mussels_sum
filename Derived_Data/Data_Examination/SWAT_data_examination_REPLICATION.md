Review of Maine DEP EGAD Mussel Tissue Toxics Data: Replicates and
Duplicates
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
9/10/2020

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder Reference](#establish-folder-reference)
      - [Copy Data](#copy-data)
      - [Remove Duplicates](#remove-duplicates)
      - [Simplify the Data and Add Unique Sample
        Codes](#simplify-the-data-and-add-unique-sample-codes)
  - [Check for Duplicates](#check-for-duplicates)
      - [Why Does Duplication or Replication
        Occur?](#why-does-duplication-or-replication-occur)
      - [Single Remaining “Unexplained”
        Duplicate.](#single-remaining-unexplained-duplicate.)
  - [Remove Duplicate Samples](#remove-duplicate-samples)
      - [Test If That Worked](#test-if-that-worked)
  - [Replicates](#replicates)
      - [All Replicates](#all-replicates)
      - [Those That Are NOT Different Laboratory
        Samples](#those-that-are-not-different-laboratory-samples)
      - [The Remaining Non-duplicate Values Are All
        `WEIGHT`](#the-remaining-non-duplicate-values-are-all-weight)
      - [Can We Account for All
        Replicates?](#can-we-account-for-all-replicates)

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

In this notebook we look at sample replication and duplication.

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
library(htmltools)  # used by knitr; called only to avoid startup text later
library(knitr)
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

## Remove Duplicates

Many samples – nearly 20% – are members of a group of duplicates. We can
think of no valid reason why two records should be exact duplicates in
this setting, so we remove all duplicates using the unique() function.

``` r
SWAT_data <- unique(SWAT_data)
```

## Simplify the Data and Add Unique Sample Codes

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
  mutate    (SiteCode =  first(sub('.* - ','', `EGAD_SITE_NAME`)), 
             Site =  first(sub(' - .*','', `EGAD_SITE_NAME`))) %>%
  select(-EGAD_SITE_NAME) %>%
  
  # Create Year Time Stamp and (Draft 1) Unique Sample ID
  mutate    (Year  = as.numeric(format(SAMPLE_DATE, '%Y')),
             sample_id = gsub(" ", "_", SAMPLE_ID)) %>%
  group_by  (Year) %>%
  mutate    (tag = as.numeric(factor(SAMPLE_DATE))) %>%
  ungroup   ()  %>%
  mutate    (Code = paste(sample_id, Year, tag, sep = '_')) %>%
  select    (-sample_id, -tag) %>%
  select(`SITE SEQ`, SiteCode, Site, Year, SAMPLE_DATE,
         SAMPLE_ID, Code, everything())
SWAT_simplified
```

    ## # A tibble: 93,357 x 25
    ##    `SITE SEQ` SiteCode Site   Year SAMPLE_DATE         SAMPLE_ID Code 
    ##         <dbl> <chr>    <chr> <dbl> <dttm>              <chr>     <chr>
    ##  1      76091 CBJWPB   JEWE~  2007 2007-10-22 00:00:00 CBJWPB R~ CBJW~
    ##  2      76091 CBJWPB   JEWE~  2007 2007-10-22 00:00:00 CBJWPB R~ CBJW~
    ##  3      76091 CBJWPB   JEWE~  2007 2007-10-22 00:00:00 CBJWPB R~ CBJW~
    ##  4      76091 CBJWPB   JEWE~  2007 2007-10-22 00:00:00 CBJWPB R~ CBJW~
    ##  5      76091 CBJWPB   JEWE~  2007 2007-10-22 00:00:00 CBJWPB R~ CBJW~
    ##  6      76091 CBJWPB   JEWE~  2007 2007-10-22 00:00:00 CBJWPB R~ CBJW~
    ##  7      76091 CBJWPB   JEWE~  2007 2007-10-22 00:00:00 CBJWPB R~ CBJW~
    ##  8      76091 CBJWPB   JEWE~  2007 2007-10-22 00:00:00 CBJWPB R~ CBJW~
    ##  9      76091 CBJWPB   JEWE~  2007 2007-10-22 00:00:00 CBJWPB R~ CBJW~
    ## 10      76091 CBJWPB   JEWE~  2007 2007-10-22 00:00:00 CBJWPB R~ CBJW~
    ## # ... with 93,347 more rows, and 18 more variables:
    ## #   CURRENT_SAMPLE_POINT_NAME <chr>, `ANALYSIS LAB` <chr>, `SAMPLE COLLECTION
    ## #   METHOD` <chr>, ANALYSIS_LAB_SAMPLE_ID <chr>, `TEST METHOD` <chr>, `TEST
    ## #   METHOD DESCRIPTION` <chr>, PARAMETER <chr>, CONCENTRATION <dbl>, `UNITS
    ## #   VALUE` <chr>, `LAB QUALIFIER` <chr>, `VALIDATION QUALIFIER` <chr>,
    ## #   `QUALIFIER DESCRIPTION` <chr>, RL <dbl>, MDL <dbl>, `WEIGHT BASIS` <chr>,
    ## #   `PREP METHOD` <chr>, DILUTION_FACTOR <chr>, CAS_NO <chr>

Now that we have designations for single sampling events (locations /
dates / replicates) we can ask whether we get either duplicate data
entries or replicates.

# Check for Duplicates

``` r
SWAT_simplified %>%
  select(c(`SITE SEQ`, SAMPLE_DATE, SiteCode, CURRENT_SAMPLE_POINT_NAME,
           Code, PARAMETER, `WEIGHT BASIS`, CONCENTRATION)) %>%
  group_by(`SITE SEQ`, SAMPLE_DATE, Code, PARAMETER, `WEIGHT BASIS`) %>%
  summarize(nvals     = n(),
            .groups = 'drop') %>%
  filter(nvals > 1) %>%
  nrow()
```

    ## [1] 445

We have duplicate values for quite a few combinations of samples and
parameters. That is a surprise, since we pulled out complete replicates
already. (Some of these are data where values of CONCENTRATION are NA.)

## Why Does Duplication or Replication Occur?

### Identical Samples Sometimes Reported From Different Laboratories

``` r
SWAT_simplified %>% 
  filter(Code == 'CBBBBB_REP_1_2006_4', PARAMETER == "2,4'-DDD" |
           PARAMETER == "2,4'-DDE" | PARAMETER == "2,4'-DDT")
```

    ## # A tibble: 6 x 25
    ##   `SITE SEQ` SiteCode Site   Year SAMPLE_DATE         SAMPLE_ID Code 
    ##        <dbl> <chr>    <chr> <dbl> <dttm>              <chr>     <chr>
    ## 1      70672 CBJWPB   JEWE~  2006 2006-11-29 00:00:00 CBBBBB R~ CBBB~
    ## 2      70672 CBJWPB   JEWE~  2006 2006-11-29 00:00:00 CBBBBB R~ CBBB~
    ## 3      70672 CBJWPB   JEWE~  2006 2006-11-29 00:00:00 CBBBBB R~ CBBB~
    ## 4      70672 CBJWPB   JEWE~  2006 2006-11-29 00:00:00 CBBBBB R~ CBBB~
    ## 5      70672 CBJWPB   JEWE~  2006 2006-11-29 00:00:00 CBBBBB R~ CBBB~
    ## 6      70672 CBJWPB   JEWE~  2006 2006-11-29 00:00:00 CBBBBB R~ CBBB~
    ## # ... with 18 more variables: CURRENT_SAMPLE_POINT_NAME <chr>, `ANALYSIS
    ## #   LAB` <chr>, `SAMPLE COLLECTION METHOD` <chr>, ANALYSIS_LAB_SAMPLE_ID <chr>,
    ## #   `TEST METHOD` <chr>, `TEST METHOD DESCRIPTION` <chr>, PARAMETER <chr>,
    ## #   CONCENTRATION <dbl>, `UNITS VALUE` <chr>, `LAB QUALIFIER` <chr>,
    ## #   `VALIDATION QUALIFIER` <chr>, `QUALIFIER DESCRIPTION` <chr>, RL <dbl>,
    ## #   MDL <dbl>, `WEIGHT BASIS` <chr>, `PREP METHOD` <chr>,
    ## #   DILUTION_FACTOR <chr>, CAS_NO <chr>

So, duplication arose here because of designation to different analysis
labs. Some samples are allocated to “AXYS ANALYTIC SERVICES”, and some
to “PACE ANALYTICAL”, but data are otherwise identical, which is
unlikely. These are probably errors of some sort.

#### How Many of These Cases Are There?

The following selects distinct values WITH the analysis lab info
included. It then groups rows without the analysis lab information and
looks for groups with more than one value. What it should collect are
rows that differ only by ANALYSIS LAB.

``` r
tmp <- SWAT_simplified %>%
  select(Code, SiteCode, `SITE SEQ`, SAMPLE_DATE, PARAMETER,
         `ANALYSIS LAB`, `ANALYSIS_LAB_SAMPLE_ID`,
          CONCENTRATION, `WEIGHT BASIS`) %>%
  distinct() %>%
  group_by(`SITE SEQ`, SAMPLE_DATE,
           Code, PARAMETER, `WEIGHT BASIS`,
           CONCENTRATION) %>%
  summarize(nvals   = n(),
            .groups = 'drop') %>%
  filter(nvals > 1)
nrow(tmp)
```

    ## [1] 128

``` r
tmp
```

    ## # A tibble: 128 x 7
    ##    `SITE SEQ` SAMPLE_DATE         Code  PARAMETER `WEIGHT BASIS` CONCENTRATION
    ##         <dbl> <dttm>              <chr> <chr>     <chr>                  <dbl>
    ##  1      70672 2006-11-29 00:00:00 CBBB~ 2,4'-DDD  WET                    0.411
    ##  2      70672 2006-11-29 00:00:00 CBBB~ 2,4'-DDE  WET                    0.186
    ##  3      70672 2006-11-29 00:00:00 CBBB~ 2,4'-DDT  WET                    0.09 
    ##  4      70672 2006-11-29 00:00:00 CBBB~ 4,4'-DDD  WET                    1.51 
    ##  5      70672 2006-11-29 00:00:00 CBBB~ 4,4'-DDE  WET                    1.36 
    ##  6      70672 2006-11-29 00:00:00 CBBB~ 4,4'-DDT  WET                    0.292
    ##  7      70672 2006-11-29 00:00:00 CBBB~ ALDRIN    WET                   NA    
    ##  8      70672 2006-11-29 00:00:00 CBBB~ ALPHA-BHC WET                    0.172
    ##  9      70672 2006-11-29 00:00:00 CBBB~ ALPHA-CH~ WET                    0.162
    ## 10      70672 2006-11-29 00:00:00 CBBB~ BETA-BHC  WET                   NA    
    ## # ... with 118 more rows, and 1 more variable: nvals <int>

``` r
rm(tmp) 
```

### MOISTURE and LIPIDS Are Often Duplicates

Lets separate out by lab, and consider how many duplicates we STILL get

``` r
SWAT_simplified %>% select(Code, `SITE SEQ`, SAMPLE_DATE, PARAMETER,
                           `ANALYSIS LAB`, `ANALYSIS_LAB_SAMPLE_ID`,
                           CONCENTRATION, `WEIGHT BASIS`) %>%
  filter(`WEIGHT BASIS` == 'WET') %>%
  group_by(`SITE SEQ`, SAMPLE_DATE, Code, `ANALYSIS LAB`, PARAMETER) %>%
  summarise(n = n(),
            m = mean(CONCENTRATION, na.rm=TRUE),
            d = max(CONCENTRATION) - min(CONCENTRATION),
            .groups = 'drop') %>%
  filter(n>1 & d==0) %>%
  nrow()
```

    ## [1] 83

So, even separated by analysis lab, we see just over 80 duplicate
values. To figure out why, we need to look at the original data again.

``` r
SWAT_data %>%
  filter(`WEIGHT BASIS` == 'WET') %>%
  group_by(`SITE SEQ`, SAMPLE_DATE, SAMPLE_ID, PARAMETER, `ANALYSIS LAB` ) %>%
  mutate(n = n(),
         m = mean(CONCENTRATION, na.rm=TRUE),
         d = max(CONCENTRATION) - min(CONCENTRATION)) %>%
  filter(n>1 & d==0)
```

    ## # A tibble: 208 x 44
    ## # Groups:   SITE SEQ, SAMPLE_DATE, SAMPLE_ID, PARAMETER, ANALYSIS LAB [83]
    ##    `SITE SEQ` EGAD_SITE_NAME SITE_DESCRIPTION CURRENT_SAMPLE_~ `SAMPLE POINT T~
    ##         <dbl> <chr>          <chr>            <chr>            <chr>           
    ##  1      70678 MILL CREEK - ~ <NA>             REP 3            MARINE          
    ##  2      70678 MILL CREEK - ~ <NA>             REP 3            MARINE          
    ##  3      70678 MILL CREEK - ~ <NA>             REP 3            MARINE          
    ##  4      70678 MILL CREEK - ~ <NA>             REP 3            MARINE          
    ##  5      70678 MILL CREEK - ~ <NA>             REP 1 2011       MARINE          
    ##  6      70678 MILL CREEK - ~ <NA>             REP 1 2011       MARINE          
    ##  7      70678 MILL CREEK - ~ <NA>             REP 2 2011       MARINE          
    ##  8      70678 MILL CREEK - ~ <NA>             REP 3 2011       MARINE          
    ##  9      70678 MILL CREEK - ~ <NA>             REP 2 2011       MARINE          
    ## 10      70678 MILL CREEK - ~ <NA>             REP 4 2011       MARINE          
    ## # ... with 198 more rows, and 39 more variables: `ANALYSIS LAB` <chr>, `SAMPLE
    ## #   COLLECTION METHOD` <chr>, SAMPLE_ID <chr>, ANALYSIS_LAB_SAMPLE_ID <chr>,
    ## #   `QC TYPE` <chr>, `SAMPLE TYPE` <chr>, SAMPLED_BY <chr>, SAMPLE_DATE <dttm>,
    ## #   `SAMPLE LOCATION` <chr>, `TEST METHOD` <chr>, `TEST METHOD
    ## #   DESCRIPTION` <chr>, ANALYSIS_DATE <dttm>, PARAMETER <chr>,
    ## #   CONCENTRATION <dbl>, `UNITS VALUE` <chr>, `UNITS DESCRIPTION` <chr>, `LAB
    ## #   QUALIFIER` <chr>, `VALIDATION QUALIFIER` <chr>, `QUALIFIER
    ## #   DESCRIPTION` <chr>, RL <dbl>, MDL <dbl>, `WEIGHT BASIS` <chr>, `RESULT
    ## #   TYPE` <chr>, `PREP METHOD` <chr>, PARAMETER_QUALIFIER <chr>,
    ## #   DILUTION_FACTOR <chr>, `PARAMETER FILTERED` <chr>, `SAMPLE FILTER` <dbl>,
    ## #   DEPTH <chr>, `DEPTH UNITS` <chr>, `SAMPLE COMMENT` <chr>, `LAB
    ## #   COMMENT` <chr>, `VALIDATION COMMENT` <chr>, TREATMENT <chr>,
    ## #   METER_CALIBRATED <chr>, CAS_NO <chr>, n <int>, m <dbl>, d <dbl>

These are all either LIPID or MOISTURE values, expressed in percent.
They differ principally (only?) in the test method they are associated
with, and sometimes in a parameter qualifier. We believe that is because
these percent values are collected once per sample, but reported with
each analytic data series. We can eliminate this apparent parameter
duplication.

### Remove Lipid and Moisture Observations

THis code is just to count duplicates. To actually assemble data we
would want to drop all but the first observation in each group.

``` r
SWAT_simplified %>% select(Code, `SITE SEQ`, SAMPLE_DATE, PARAMETER,
                           `ANALYSIS LAB`, `ANALYSIS_LAB_SAMPLE_ID`,
                           CONCENTRATION, `WEIGHT BASIS`) %>%
  filter(`WEIGHT BASIS` == 'WET') %>%
  filter(PARAMETER != "LIPIDS" & PARAMETER != "MOISTURE") %>%
  group_by(`SITE SEQ`, SAMPLE_DATE, Code, `ANALYSIS LAB`, PARAMETER) %>%
  summarise(n = n(),
            m = mean(CONCENTRATION, na.rm=TRUE),
            d = max(CONCENTRATION) - min(CONCENTRATION),
            .groups = 'drop') %>%
  filter(n>1 & d==0)
```

    ## # A tibble: 1 x 8
    ##   `SITE SEQ` SAMPLE_DATE         Code  `ANALYSIS LAB` PARAMETER     n     m
    ##        <dbl> <dttm>              <chr> <chr>          <chr>     <int> <dbl>
    ## 1      70676 2003-11-17 00:00:00 CBGD~ PACE ANALYTIC~ MERCURY       2 0.027
    ## # ... with 1 more variable: d <dbl>

So there is only one “unexplained” duplicate. Lets check to see why it
occurred.

## Single Remaining “Unexplained” Duplicate.

``` r
SWAT_simplified %>% 
  filter(Code == 'CBGDSW_REP_2_2003_4', PARAMETER == "MERCURY")
```

    ## # A tibble: 2 x 25
    ##   `SITE SEQ` SiteCode Site   Year SAMPLE_DATE         SAMPLE_ID Code 
    ##        <dbl> <chr>    <chr> <dbl> <dttm>              <chr>     <chr>
    ## 1      70676 CBJWPB   JEWE~  2003 2003-11-17 00:00:00 CBGDSW R~ CBGD~
    ## 2      70676 CBJWPB   JEWE~  2003 2003-11-17 00:00:00 CBGDSW R~ CBGD~
    ## # ... with 18 more variables: CURRENT_SAMPLE_POINT_NAME <chr>, `ANALYSIS
    ## #   LAB` <chr>, `SAMPLE COLLECTION METHOD` <chr>, ANALYSIS_LAB_SAMPLE_ID <chr>,
    ## #   `TEST METHOD` <chr>, `TEST METHOD DESCRIPTION` <chr>, PARAMETER <chr>,
    ## #   CONCENTRATION <dbl>, `UNITS VALUE` <chr>, `LAB QUALIFIER` <chr>,
    ## #   `VALIDATION QUALIFIER` <chr>, `QUALIFIER DESCRIPTION` <chr>, RL <dbl>,
    ## #   MDL <dbl>, `WEIGHT BASIS` <chr>, `PREP METHOD` <chr>,
    ## #   DILUTION_FACTOR <chr>, CAS_NO <chr>

The two values are assigned to different analytic methods – also
unlikely to produce identical values, so likely an inadvertent
duplication.

# Remove Duplicate Samples

``` r
# Step 1:  Remove extra MERCURY Sample

SWAT_final <- SWAT_simplified %>%
  filter (! (Code == 'CBGDSW_REP_2_2003_4' &
               PARAMETER == "MERCURY" &
               `TEST METHOD` == "SW6020")) %>%

# Step 2:  Remove duplicate MOISTURE and LIPIDS values
# the logic here is that the "test" is unique for all rows
# EXCEPT those we might wand to remove, so we protect all
# other rows from being removed by "distinct".
  mutate(test = if_else(PARAMETER=='MOISTURE' | PARAMETER == 'LIPIDS',
                        0L , row_number())) %>%
  distinct(.keep_all = TRUE) %>%
  select  (-test) %>%
  
# Step 3:  Remove samples that differ only in the associated laboratory
  distinct(across(-`ANALYSIS LAB`), .keep_all = TRUE)
```

## Test If That Worked

``` r
SWAT_final %>% select(Code, `SITE SEQ`, SAMPLE_DATE, PARAMETER,
                           `ANALYSIS LAB`, `ANALYSIS_LAB_SAMPLE_ID`,
                           CONCENTRATION, `WEIGHT BASIS`) %>%
  filter(`WEIGHT BASIS` == 'WET') %>%
  filter(PARAMETER != "LIPIDS" & PARAMETER != "MOISTURE") %>%
  group_by(`SITE SEQ`, SAMPLE_DATE, Code, `ANALYSIS LAB`, PARAMETER) %>%
  summarise(n = n(),
            m = mean(CONCENTRATION, na.rm=TRUE),
            d = max(CONCENTRATION) - min(CONCENTRATION),
            .groups = 'drop') %>%
  filter(n>1 & d==0)
```

    ## # A tibble: 0 x 8
    ## # ... with 8 variables: `SITE SEQ` <dbl>, SAMPLE_DATE <dttm>, Code <chr>,
    ## #   `ANALYSIS LAB` <chr>, PARAMETER <chr>, n <int>, m <dbl>, d <dbl>

So, we apparently have no remaining quantitative duplicates\!

``` r
SWAT_final %>% nrow()
```

    ## [1] 93289

``` r
sum(duplicated(SWAT_final))
```

    ## [1] 0

# Replicates

## All Replicates

We do find laboratory replicates in the data. They are distinguished by
different `ANALYSIS_LAB_SAMPLE_ID`,

``` r
SWAT_final %>% select(Code, `SITE SEQ`, SAMPLE_DATE, PARAMETER,
                      `ANALYSIS LAB`, `ANALYSIS_LAB_SAMPLE_ID`,
                      `TEST METHOD`, CONCENTRATION, `WEIGHT BASIS`) %>%
  filter(`WEIGHT BASIS` == 'WET') %>%
  group_by(`SITE SEQ`, SAMPLE_DATE, Code, PARAMETER, `ANALYSIS LAB`) %>%
  summarise(n = n(),
            m = mean(CONCENTRATION, na.rm=TRUE),
            d = max(CONCENTRATION) - min(CONCENTRATION),
            .groups = 'drop') %>%
  filter(n>1 & d>0) %>%
  nrow()
```

    ## [1] 103

## Those That Are NOT Different Laboratory Samples

``` r
tmp <- SWAT_final %>% select(Code, `SITE SEQ`, SAMPLE_DATE, PARAMETER,
                      `ANALYSIS LAB`, `ANALYSIS_LAB_SAMPLE_ID`,
                      `TEST METHOD`, CONCENTRATION, `WEIGHT BASIS`) %>%
  filter(`WEIGHT BASIS` == 'WET') %>%
  group_by(`SITE SEQ`, SAMPLE_DATE, Code, PARAMETER,
           `ANALYSIS LAB`, `ANALYSIS_LAB_SAMPLE_ID`) %>%
  summarise(n = n(),
            m = mean(CONCENTRATION, na.rm=TRUE),
            d = max(CONCENTRATION) - min(CONCENTRATION),
            .groups = 'drop') %>%
  filter(n>1 & d>0)
nrow(tmp)
```

    ## [1] 8

## The Remaining Non-duplicate Values Are All `WEIGHT`

We define a replicate as a separate observation that occurs on data
collected at the same site and date. Replication can occur in the field
or in the laboratory. Replicates in the Field are usually identified in
the SAMPLE indicators. Laboratory duplicated are indicated by different
lab sample numbers.

We can search for entries where with all the sample and lab analysis ID
information included, we still get more than one sample. These are NOT
replicates, and need to be addressed.

``` r
tmp
```

    ## # A tibble: 8 x 9
    ##   `SITE SEQ` SAMPLE_DATE         Code  PARAMETER `ANALYSIS LAB` ANALYSIS_LAB_SA~
    ##        <dbl> <dttm>              <chr> <chr>     <chr>          <chr>           
    ## 1      70672 2006-11-29 00:00:00 CBBB~ WEIGHT    AXYS ANALYTIC~ BACK COVE, PORT~
    ## 2      70672 2006-11-29 00:00:00 CBBB~ WEIGHT    AXYS ANALYTIC~ BACK COVE, PORT~
    ## 3      70672 2006-11-29 00:00:00 CBBB~ WEIGHT    AXYS ANALYTIC~ BACK COVE, PORT~
    ## 4      70675 2006-10-17 00:00:00 CBGD~ WEIGHT    AXYS ANALYTIC~ COCKTAIL COVE, ~
    ## 5      70675 2006-10-17 00:00:00 CBGD~ WEIGHT    AXYS ANALYTIC~ COCKTAIL COVE, ~
    ## 6      70675 2006-10-17 00:00:00 CBGD~ WEIGHT    AXYS ANALYTIC~ COCKTAIL COVE, ~
    ## 7      70678 2006-10-30 00:00:00 CBMC~ WEIGHT    AXYS ANALYTIC~ MILL CREEK, FAL~
    ## 8      70678 2006-10-30 00:00:00 CBMC~ WEIGHT    AXYS ANALYTIC~ MILL CREEK, FAL~
    ## # ... with 3 more variables: n <int>, m <dbl>, d <dbl>

``` r
rm(tmp)
```

We have a small number of entries with multiple values even from single
labs and single lab sample IDs, but they are all WEIGHT.

We won’t need to deal with WEIGHT data at all, since we are working with
data already converted to dry weight or lipid weight basis.

It appears the WEIGHT in these cases is based on a subsample of bulk
tissue samples destined for different analytic methods. We see that in
this next block of code, which adds the laboratory method designation to
the grouping structure, and it eliminates remaining replication.

These values carry no information about field conditions, and thus are
of no interest to us. Since we will not analyze these values, we need
not remove them from the data.

## Can We Account for All Replicates?

``` r
SWAT_final %>% select(Code, `SITE SEQ`, SAMPLE_DATE, PARAMETER,
                      `ANALYSIS LAB`, `ANALYSIS_LAB_SAMPLE_ID`,
                      `TEST METHOD`, CONCENTRATION, `WEIGHT BASIS`) %>%
  filter(`WEIGHT BASIS` == 'WET') %>%
  group_by(`SITE SEQ`, SAMPLE_DATE, Code, PARAMETER, 
           `ANALYSIS LAB`,`ANALYSIS_LAB_SAMPLE_ID`,`TEST METHOD`) %>%
  summarise(n = n(),
            m = mean(CONCENTRATION, na.rm=TRUE),
            d = max(CONCENTRATION) - min(CONCENTRATION),
            .groups = 'drop') %>%
  filter(n>1 & d>0)
```

    ## # A tibble: 0 x 10
    ## # ... with 10 variables: `SITE SEQ` <dbl>, SAMPLE_DATE <dttm>, Code <chr>,
    ## #   PARAMETER <chr>, `ANALYSIS LAB` <chr>, ANALYSIS_LAB_SAMPLE_ID <chr>, `TEST
    ## #   METHOD` <chr>, n <int>, m <dbl>, d <dbl>
