Review of Maine DEP EGAD Mussel Tissue Toxics Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
9/10/2020

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder Reference](#establish-folder-reference)
      - [Copy Data](#copy-data)
      - [Remove duplicates](#remove-duplicates)
  - [Creating a Unique ID for Sampling
    Events](#creating-a-unique-id-for-sampling-events)
      - [What Timestep Provides Unique Sample
        Identifiers](#what-timestep-provides-unique-sample-identifiers)
      - [Demonstrate The Logic](#demonstrate-the-logic)
      - [Calculate Unique Sample Codes and Simplify the
        Data](#calculate-unique-sample-codes-and-simplify-the-data)

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

In this notebook we construct a code that uniquely identifies separate
sampling events (dates, sites, locations and field replicates).

# Load Libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts -------------------------------------------------------------------------- tidyverse_conflicts() --
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

# Creating a Unique ID for Sampling Events

Ideally, we want to be able to uniquely identify samples, so we know
when and where each sample was collected, and what data is associated
with a single sampling event. It appears that the most likely data field
to convey that information is the “SAMPLE\_ID” field. If that is
correct, it should correspond to unique combinations of some, or most
of:

  - SITE SEQ / EGAD\_SITE\_NAME
  - CURRENT\_SAMPLE\_POINT\_NAME
  - SAMPLE POINT TYPE
  - SAMPLE COLLECTION METHOD
  - SAMPLE TYPE
  - SAMPLE\_DATE

We can test that as follows:

``` r
SWAT_data %>%
  group_by(SAMPLE_ID) %>%
  summarize(nsites    = length(unique(`SITE SEQ`)),
            nsamppt   = length(unique(`CURRENT_SAMPLE_POINT_NAME`)),
            npttypes  = length(unique(`SAMPLE POINT TYPE`)),
            nmeth     = length(unique(`SAMPLE COLLECTION METHOD`)),
            nsamptype  = length(unique(`SAMPLE TYPE`)),
            ndates    = length(unique(`SAMPLE_DATE`)),
            .groups = 'drop') %>%
  summarize_at(vars(nsites:ndates), max)
```

    ## # A tibble: 1 x 6
    ##   nsites nsamppt npttypes nmeth nsamptype ndates
    ##    <int>   <int>    <int> <int>     <int>  <int>
    ## 1      1       1        1     2         2      3

From that we can see that SAMPLE\_ID is tied to unique

  - SITE SEQ / CURRENT\_SITE\_NAME / Code  
  - CURRENT\_SAMPLE\_POINT\_NAME  
  - SAMPLE POINT TYPE

But NOT to unique

  - SAMPLE COLLECTION METHOD  
  - SAMPLE TYPE  
  - SAMPLE\_DATE

It is possible that multiple sample types might be collected during a
single site visit, with each sent off to different laboratories (for
example). So the most troubling of these is the date. We next check if
the combination of SAMPLE\_ID and SAMPLE\_DATE provides unique
information.

## What Timestep Provides Unique Sample Identifiers

It would be nice to be able to make a unique sample ID code that does
not depend on the full date. We know we have multiple samples collected
at single sites in a few years, but I don’t know if they had the same
SAMPLE\_ID.

``` r
SWAT_data %>%
  group_by (SAMPLE_ID,SAMPLE_DATE) %>%
  summarize(SAMPLE_ID = first(SAMPLE_ID),
            SAMPLE_DATE = first(SAMPLE_DATE),
            year  = as.numeric(format(SAMPLE_DATE, '%Y')),
            month = as.numeric(format(SAMPLE_DATE, '%m')),
            day = as.numeric(format(SAMPLE_DATE, '%d')),
            .groups = 'drop') %>%
  # Count SAMPLE_ID on each day (they are unique)
  group_by (SAMPLE_ID, year, month, day) %>%
  mutate   (Ddates = sum(! is.na(SAMPLE_DATE))) %>%
  ungroup  () %>% 
  # Count SAMPLE_ID by month -- and keep the highest daily total
  group_by (SAMPLE_ID, year, month) %>%
  mutate   (maxDdates = max(Ddates),
            Mdates = sum(! is.na(SAMPLE_DATE))) %>%
  select(-Ddates) %>%
  ungroup() %>%
  # Count SAMPLE_ID by year -- and keep the highest daily and monthly totals
  group_by(SAMPLE_ID, year) %>%
  mutate(maxDdates = max(maxDdates),
         maxMdates = max(Mdates),
         Ydates = sum(! is.na(SAMPLE_DATE))) %>%
  select (-Mdates) %>%
  ungroup() %>%
  # Roll results up into a single row tibble
  summarize(maxDdates = max(maxDdates),
            maxMdates = max(maxMdates),
            maxYdates = max(Ydates),
            .groups = 'drop')
```

    ## # A tibble: 1 x 3
    ##   maxDdates maxMdates maxYdates
    ##       <int>     <int>     <int>
    ## 1         1         2         2

## Demonstrate The Logic

That shows that SAMPLE\_ID is not unique within years, or even within
years and months. To get to a unique combination, we must include the
day as well. On the other hand, we never have more than two sample dates
at any SAMPLE\_ID in a year. If we could figure out which is which, we
could use the year and a sequence number as a unique ID.

But it turns out, we can do better, and make the year and date codes
consistent across all samples collected in a given year. We never see
more than a few sample dates within a year (at all locations), so we can
code each sample date as a year plus a digit.

A slightly roundabout way to convert from dates to sequence of numbers
is to use as.numeric(factor(.)).

Here’s the logic, looking only as SAMPLE\_ID\_and DATES

``` r
tmp <- SWAT_data %>%
  group_by  (SAMPLE_ID,SAMPLE_DATE) %>%
  summarize (SAMPLE_ID = first(SAMPLE_ID),
             SAMPLE_DATE = first(SAMPLE_DATE),
             .groups = 'drop') %>%
  mutate    (Year  = as.numeric(format(SAMPLE_DATE, '%Y'))) %>%
  mutate    (sample_id = gsub(" ", "_", SAMPLE_ID)) %>%
  group_by  (Year) %>%
  mutate    (tag = as.numeric(factor(SAMPLE_DATE))) %>%
  ungroup   ()  %>%
  mutate    (code = paste(sample_id, Year, tag, sep = '_')) %>%
  select    (-sample_id) %>%
  arrange(Year, SAMPLE_ID)
tmp
```

    ## # A tibble: 157 x 5
    ##    SAMPLE_ID    SAMPLE_DATE          Year   tag code               
    ##    <chr>        <dttm>              <dbl> <dbl> <chr>              
    ##  1 CBFROR REP 1 2003-10-01 00:00:00  2003     1 CBFROR_REP_1_2003_1
    ##  2 CBFROR REP 1 2003-11-17 00:00:00  2003     4 CBFROR_REP_1_2003_4
    ##  3 CBFROR REP 2 2003-10-01 00:00:00  2003     1 CBFROR_REP_2_2003_1
    ##  4 CBFROR REP 2 2003-11-17 00:00:00  2003     4 CBFROR_REP_2_2003_4
    ##  5 CBFROR REP 3 2003-10-01 00:00:00  2003     1 CBFROR_REP_3_2003_1
    ##  6 CBFROR REP 3 2003-11-17 00:00:00  2003     4 CBFROR_REP_3_2003_4
    ##  7 CBFROR REP 4 2003-10-01 00:00:00  2003     1 CBFROR_REP_4_2003_1
    ##  8 CBFROR REP 4 2003-11-17 00:00:00  2003     4 CBFROR_REP_4_2003_4
    ##  9 CBGDSW REP 1 2003-10-01 00:00:00  2003     1 CBGDSW_REP_1_2003_1
    ## 10 CBGDSW REP 1 2003-11-17 00:00:00  2003     4 CBGDSW_REP_1_2003_4
    ## # ... with 147 more rows

So, other than really ugly unique sample identifiers, we’ve produced a
useful solution.

### A Quick Test

Confirm that we now have unique codes:

``` r
tmp %>% group_by(code) %>%
  summarize(n = length(unique(SAMPLE_ID,SAMPLE_DATE)),
            .groups = 'drop') %>%
  summarize(maxtimes = max(n))
```

    ## # A tibble: 1 x 1
    ##   maxtimes
    ##      <int>
    ## 1        1

## Calculate Unique Sample Codes and Simplify the Data

Now, we can add that logic (and our earlier lessons about uninformative
data codes) to the original data set.

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
  select(`SITE SEQ`, SiteCode, Site, Year, SAMPLE_DATE, SAMPLE_ID, Code, everything())
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

### Check Uniqueness

``` r
SWAT_simplified %>%
  select(c(SiteCode, CURRENT_SAMPLE_POINT_NAME,
           `SAMPLE COLLECTION METHOD`,
           SAMPLE_DATE, `TEST METHOD`, Code)) %>%
  group_by(Code) %>%
  summarize(nsites    = length(unique(`SiteCode`)),
            ndates    = length(unique(`SAMPLE_DATE`)),
            nsamppt   = length(unique(`CURRENT_SAMPLE_POINT_NAME`)),
            nmeth     = length(unique(`SAMPLE COLLECTION METHOD`)),
            .groups = 'drop')
```

    ## # A tibble: 157 x 5
    ##    Code                     nsites ndates nsamppt nmeth
    ##    <chr>                     <int>  <int>   <int> <int>
    ##  1 CBANAN_REP_1_2007_1           1      1       1     2
    ##  2 CBANAN_REP_2_2007_1           1      1       1     2
    ##  3 CBANAN_REP_3_2007_1           1      1       1     2
    ##  4 CBANAN_REP_4_2007_1           1      1       1     2
    ##  5 CBBBBB_REP_1_2006_4           1      1       1     1
    ##  6 CBBBBB_REP_2_2006_4           1      1       1     1
    ##  7 CBBBBB_REP_3_2006_4           1      1       1     1
    ##  8 CBEEEE_REP_1_2007_3           1      1       1     2
    ##  9 CBEEEE_REP_1_2009_5           1      1       1     2
    ## 10 CBEEEE_REP_1_2011_2011_1      1      1       1     2
    ## # ... with 147 more rows

So, that works – Each code has only one site, date and sample\_ID. Some
of those codes are annoyingly long, because they are constructed from
long subcomponents, but they are unique….
