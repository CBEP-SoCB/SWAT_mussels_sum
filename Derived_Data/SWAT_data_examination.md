Maine DEP EGAD Mussel Tissue Toxics Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
9/10/2020

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder Reference](#establish-folder-reference)
      - [Copy Data](#copy-data)
  - [Exploration of Data Codes](#exploration-of-data-codes)
      - [Uninformative Codes](#uninformative-codes)
      - [Possibly Informative Codes](#possibly-informative-codes)
  - [Sites](#sites)
      - [List of Sites](#list-of-sites)
      - [Match Sites with Geographic
        Locations](#match-sites-with-geographic-locations)
      - [Write CSV files](#write-csv-files)
  - [Sample Points](#sample-points)
      - [Examine Distances between Site and Sample
        Points](#examine-distances-between-site-and-sample-points)
      - [Map Visualization](#map-visualization)
  - [Sampling Dates](#sampling-dates)
      - [Dates and Sites Provide Unique Sampling Events, But Not Unique
        Samples](#dates-and-sites-provide-unique-sampling-events-but-not-unique-samples)
      - [Period of Data](#period-of-data)
      - [How Many Sampling Events Each
        Year?](#how-many-sampling-events-each-year)
      - [How Many Times has Each Site Been
        Sampled?](#how-many-times-has-each-site-been-sampled)
  - [Creating a Unique ID for Sampling
    Events](#creating-a-unique-id-for-sampling-events)
      - [What Timestep Provides Unique Sample
        Identifiers](#what-timestep-provides-unique-sample-identifiers)
      - [Demonstrate The Logic](#demonstrate-the-logic)
      - [Calculate Unique Sample Codes and Simplify the
        Data](#calculate-unique-sample-codes-and-simplify-the-data)
  - [Do We Get Replicate or Duplicate Parameter
    Values?](#do-we-get-replicate-or-duplicate-parameter-values)
      - [Why Does Duplication or Replication
        Occur?](#why-does-duplication-or-replication-occur)
      - [A Couple of Examples](#a-couple-of-examples)
      - [Identical Values Different
        Labs?](#identical-values-different-labs)
      - [Are ALL Replicates explained as Laboratory QA/QC
        Checks?](#are-all-replicates-explained-as-laboratory-qaqc-checks)
  - [Checking values based on Wet WEight, Dry Weight, and Lipid
    Weight](#checking-values-based-on-wet-weight-dry-weight-and-lipid-weight)
      - [Check that (value of LIP \<= DRY \<=
        WET)](#check-that-value-of-lip-dry-wet)
      - [Check that (value of LIP \<= DRY \<=
        WET)](#check-that-value-of-lip-dry-wet-1)
  - [PCB Nomenclature](#pcb-nomenclature)
      - [Examining CAS numbers](#examining-cas-numbers)
      - [When Are the PCB numerical codes
        used?](#when-are-the-pcb-numerical-codes-used)
  - [How to Group Parameters](#how-to-group-parameters)
      - [Export a Prelimnary list of
        Parameters](#export-a-prelimnary-list-of-parameters)
      - [Totals And Calculations](#totals-and-calculations)
      - [Read In a Hand Edited Classification of
        Parameters](#read-in-a-hand-edited-classification-of-parameters)
  - [How did DEP Calculate Totals?](#how-did-dep-calculate-totals)
  - [Units](#units)
  - [Real Sample Numbers?](#real-sample-numbers)

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

In this notebook we take various slices through the data to understand
its structure. It is a long notebook, because htere was a lot to
examine. However the functional core of hte notebook lies in creation of
cleaned data sets.

sample\_spatial.csv sites\_spatial.csv Parameter List.csv

While those data sets ARE produced in this notebook, WE plan to produce
a simpler version that skips all the data exploraiton.

# Load Libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
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
    ## <bytecode: 0x0000000019114a70>
    ## <environment: namespace:ggplot2>

``` r
library(LCensMeans)
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
```

# Exploration of Data Codes

## Uninformative Codes

### What is Sample\_ID

The SAMPLE\_ID itself appears to be a combination of other information,
including \* A site Code \* A current sample point name, and \*
Sometimes a YEAR \* Sometimes a method qualifier, like “DIOXIN”

That “DIOXIN” qualifier is unique to “MEPH” samples, which, based on the
similarity of the site codes used, appear to be 2003 Gulfwatch samples.
Data post 2000 are not available online through the Gulfwatch website.
In the EGAD data, they are associated with the “FORE RIVER OUTER”
location.

``` r
SWAT_data %>%
  filter(grepl('DIOXIN', SAMPLE_ID)) %>%
         select(SAMPLE_ID, EGAD_SITE_NAME) %>%
         unique() %>% arrange(SAMPLE_ID)
```

    ## # A tibble: 4 x 2
    ##   SAMPLE_ID            EGAD_SITE_NAME           
    ##   <chr>                <chr>                    
    ## 1 MEPH 1N101603 DIOXIN FORE RIVER OUTER - CBFROR
    ## 2 MEPH 2N101603 DIOXIN FORE RIVER OUTER - CBFROR
    ## 3 MEPH 3N101603 DIOXIN FORE RIVER OUTER - CBFROR
    ## 4 MEPH 4N101603 DIOXIN FORE RIVER OUTER - CBFROR

So, it looks like a unique sample identifier combines a LOT of info.

### What is `SAMPLE TYPE`

``` r
unique(SWAT_data$`SAMPLE TYPE`)
```

    ## [1] "WHOLE"                   "PHYSICAL CHARACTERISTIC"

``` r
as.data.frame(xtabs(~PARAMETER + `SAMPLE TYPE` , data = SWAT_data)) %>%
  filter(SAMPLE.TYPE ==     'PHYSICAL CHARACTERISTIC' & Freq > 0)
```

    ##        PARAMETER             SAMPLE.TYPE Freq
    ## 1 Mytilus edulis PHYSICAL CHARACTERISTIC  232

So the ONLY parameter ever included under ‘PHYSICAL CHARACTERISTIC’ is
the species of shellfish sampled. This variable conveys no independent
information, and so is of little use.

### What is `SAMPLE POINT TYPE`?

``` r
unique(SWAT_data$`SAMPLE POINT TYPE`)
```

    ## [1] "MARINE"

`SAMPLE POINT TYPE` contains only a single value, so in our context, it
is useless.

### What is `SAMPLE LOCATION`?

``` r
unique(SWAT_data$`SAMPLE LOCATION`)
```

    ## [1] "WADING"         "NOT APPLICABLE"

Again, no useful information here…

### What is `SAMPLE LOCATION`?

``` r
unique(SWAT_data$`SAMPLE LOCATION`)
```

    ## [1] "WADING"         "NOT APPLICABLE"

### What is `RESULT TYPE`?

``` r
unique(SWAT_data$`RESULT TYPE`)
```

    ## [1] "TARGET/REGULAR RESULT" "PHYSICAL MEASUREMENT"

``` r
SWAT_data %>% select(`RESULT TYPE`, PARAMETER) %>% filter(`RESULT TYPE`=="PHYSICAL MEASUREMENT")
```

    ## # A tibble: 228 x 2
    ##    `RESULT TYPE`        PARAMETER     
    ##    <chr>                <chr>         
    ##  1 PHYSICAL MEASUREMENT Mytilus edulis
    ##  2 PHYSICAL MEASUREMENT Mytilus edulis
    ##  3 PHYSICAL MEASUREMENT Mytilus edulis
    ##  4 PHYSICAL MEASUREMENT Mytilus edulis
    ##  5 PHYSICAL MEASUREMENT Mytilus edulis
    ##  6 PHYSICAL MEASUREMENT Mytilus edulis
    ##  7 PHYSICAL MEASUREMENT Mytilus edulis
    ##  8 PHYSICAL MEASUREMENT Mytilus edulis
    ##  9 PHYSICAL MEASUREMENT Mytilus edulis
    ## 10 PHYSICAL MEASUREMENT Mytilus edulis
    ## # ... with 218 more rows

So that is uninformative too. It mirrors SAMPLE TYPE.

### What is `PARAMETER_QUALIFIER`?

``` r
unique(SWAT_data$`PARAMETER_QUALIFIER`)
```

    ## [1] NA         "column 1" "column 2" "column"

### What are `PARAMETER FILTERED` and `SAMPLE FILTER`?

``` r
xtabs(~ `PARAMETER FILTERED` + `SAMPLE FILTER`, data = SWAT_data, addNA = TRUE)
```

    ##                   SAMPLE FILTER
    ## PARAMETER FILTERED   <NA>
    ##     NOT APPLICABLE  11266
    ##     UNFILTERED        286
    ##     <NA>           101332

SO both are uninformative.

### What are `DEPTH` and `DEPTH UNITS`?

``` r
xtabs( ~ DEPTH + `DEPTH UNITS`, data = SWAT_data, addNA = TRUE)
```

    ##       DEPTH UNITS
    ## DEPTH    <NA>
    ##   <NA> 112884

### What is `TREATMENT`?

``` r
unique(SWAT_data$`TREATMENT`)
```

    ## [1] "NOT APPLICABLE" NA

### What is `METER_CALIBRATED`?

``` r
unique(SWAT_data$`METER_CALIBRATED`)
```

    ## [1] NA

## Possibly Informative Codes

### What is `CURRENT_SAMPLE_POINT_NAME`?

``` r
as.tibble(xtabs(~SWAT_data$`CURRENT_SAMPLE_POINT_NAME`, addNA = TRUE)) %>%
  rename_at(c(1), ~'SPoint1')
```

    ## Warning: `as.tibble()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## # A tibble: 40 x 2
    ##    SPoint1        n
    ##    <chr>      <int>
    ##  1 1N            40
    ##  2 2N            40
    ##  3 3N            40
    ##  4 4N            39
    ##  5 REP 1      19402
    ##  6 REP 1 2010   873
    ##  7 REP 1 2011  1746
    ##  8 REP 1 2012   889
    ##  9 REP 1 2013   830
    ## 10 REP 1 2014  2452
    ## # ... with 30 more rows

These names appear meaningful. They are used consistently to distinguish
among samples, and are connected to the spatial data, but SAMPLE POINT
names are not unique, and can be duplicated between different sites or
within one Sites in different years. See the discussion of Sites and
spatial data, below.

### What is `PREP METHOD`?

``` r
unique(SWAT_data$`PREP METHOD`)
```

    ## [1] NA           "E1668A"     "MLA-007-E2" "MLA-007-E1" "NOAA1998M" 
    ## [6] "SW3051A"    "E1631E"     "METHOD"     "E1613B"

So, useful info, if I knew what the codes mean.

### What is `SAMPLE COLLECTION METHOD`?

``` r
unique(SWAT_data$`SAMPLE COLLECTION METHOD`)
```

    ## [1] "COMPOSITE SAMPLE" "HAND-PICKED"

#### Does `SAMPLE COLLECTION METHOD` Break Out by Parameter?

``` r
xtabs(~PARAMETER + `SAMPLE COLLECTION METHOD`, data = SWAT_data) %>%
  as.tibble() %>%
  pivot_wider(id_cols = PARAMETER,
              names_from = `SAMPLE COLLECTION METHOD`,
              values_from = n) %>%
  arrange(`COMPOSITE SAMPLE`, `PARAMETER`)
```

    ## # A tibble: 399 x 3
    ##    PARAMETER                            `COMPOSITE SAMPLE` `HAND-PICKED`
    ##    <chr>                                             <int>         <int>
    ##  1 2,2',3,3',5-PENTACHLOROBIPHENYL                       0            14
    ##  2 2,2',3,4,4',5,5'-HEPTACHLOROBIPHENYL                  0            12
    ##  3 2,2',3,5',6-PENTACHLOROBIPHENYL                       0            14
    ##  4 2,2',4,4',5-PENTACHLOROBIPHENYL                       0            14
    ##  5 2,2',4,4',5,6'-HEXACHLOROBIPHENYL                     0            14
    ##  6 2,3,3',4,4',5'-HEXACHLOROBIPHENYL                     0            12
    ##  7 2,3,3',4,4',5-HEXACHLOROBIPHENYL                      0            12
    ##  8 2,3,3',4,5,6-HEXACHLOROBIPHENYL                       0            14
    ##  9 BENZO(K)FLUORANTHENE                                  0            16
    ## 10 PCB-093/098/100/102                                   0            14
    ## # ... with 389 more rows

So, other than the fact that many of the pesticide data are derived only
from composite samples, and some PCBs (by chemical name) appear only
from ‘hand picked’ samples, it appears that these represent different
sampling strategies, or at least different labels for sample collection
strategies employed during sample collection.

Curiously, the species identification ‘Mytilus edulis’ is only included
in the ‘COMPOSITE SAMPLE’ category 7 times, MUCH less frequently than
some other parameters. That appears counter-intuitive. How can we have
more samples for some parameters than samples of mussels? something here
is inconsistent.

#### Does `SAMPLE COLLECTION METHOD` Break Out by Site?

``` r
xtabs(~EGAD_SITE_NAME + `SAMPLE COLLECTION METHOD`, data = SWAT_data) %>%
  as.tibble() %>%
  pivot_wider(id_cols = EGAD_SITE_NAME,
              names_from = `SAMPLE COLLECTION METHOD`,
              values_from = n)
```

    ## # A tibble: 20 x 3
    ##    EGAD_SITE_NAME                              `COMPOSITE SAMPLE` `HAND-PICKED`
    ##    <chr>                                                    <int>         <int>
    ##  1 BACK BAY - CBBBBB                                            0           778
    ##  2 BRUNSWICK MARE BROOK DRAINAGE - CBMBBH                    6134           120
    ##  3 COCKTAIL COVE GREAT DIAMOND ISLAND - CBGDCC                  0           757
    ##  4 EAST END BEACH - CBEEEE                                  26542           128
    ##  5 FALMOUTH ANCHORAGE - CBANAN                               2796             4
    ##  6 FORE RIVER OUTER - CBFROR                                    0          1771
    ##  7 HARASEEKET RIVER - CBHRHR                                 5592            16
    ##  8 INNER FORE RIVER - CBFRIR                                 4092             4
    ##  9 JEWEL ISLAND PUNCHBOWL - CBJWPB                           2796             4
    ## 10 LONG ISLAND - CBLNFT                                      4092             4
    ## 11 MAQUOIT BAY - CBMBBR                                      4096             4
    ## 12 MIDDLE BAY (OUTER) - CBMBMB                               2823             4
    ## 13 MIDDLE FORE RIVER - CBFRMR                                2988             4
    ## 14 MILL CREEK - CBMCMC                                      21404            38
    ## 15 NAVY PIER - CBHWNP                                        3320             4
    ## 16 PRESUMPSCOT RIVER (MOUTH)  - CBPRMT                       3209             4
    ## 17 QUAHOG BAY - CBQHQH                                       4101             4
    ## 18 ROYAL RIVER MOUTH - CBRYMT                                   0           298
    ## 19 S PORTLAND SPRING POINT - CBSPSP                          9400          3607
    ## 20 SOUTHWEST END GREAT DIAMOND ISLAND - CBGDSW                  0          1946

Almost. with a few exceptions, all or almost all data from each site is
flagged one way or the other.

#### Does `SAMPLE COLLECTION METHOD` Break Out by Date?

``` r
xtabs(~SAMPLE_DATE + `SAMPLE COLLECTION METHOD`, data = SWAT_data) %>%
  as.tibble() %>%
  pivot_wider(id_cols = SAMPLE_DATE,
              names_from = `SAMPLE COLLECTION METHOD`,
              values_from = n)
```

    ## # A tibble: 32 x 3
    ##    SAMPLE_DATE `COMPOSITE SAMPLE` `HAND-PICKED`
    ##    <chr>                    <int>         <int>
    ##  1 2003-10-01                   0           506
    ##  2 2003-10-16                   0           159
    ##  3 2003-11-04                   0           144
    ##  4 2003-11-17                   0          3206
    ##  5 2006-10-12                 174             9
    ##  6 2006-10-17                   0           757
    ##  7 2006-10-30                1989            12
    ##  8 2006-11-29                   0           778
    ##  9 2007-10-18                3092            12
    ## 10 2007-10-22                8092            12
    ## # ... with 22 more rows

Again, ALMOST all amples from each date are flagged one way or the
other.

#### When is “HAND PICKED” used?

But how can we figure out what these sample collection methods mean? We
know the Sample\_Collection\_Method for Gulfwatch was a composite of 20
mussels. Gulfwatch used the sample code “MEPH for a site in”Portland
Harbor". The related EGAD\_SITE\_NAME is ‘FORE RIVER OUTER - CBFROR’.

``` r
SWAT_data %>% filter(EGAD_SITE_NAME== 'FORE RIVER OUTER - CBFROR') %>%
  select(c(2, 13, 4, 7, 8)) %>%
  unique()
```

    ## # A tibble: 16 x 5
    ##    EGAD_SITE_NAME SAMPLE_DATE         CURRENT_SAMPLE_~ `SAMPLE COLLECT~
    ##    <chr>          <dttm>              <chr>            <chr>           
    ##  1 FORE RIVER OU~ 2003-10-16 00:00:00 2N               HAND-PICKED     
    ##  2 FORE RIVER OU~ 2003-10-16 00:00:00 4N               HAND-PICKED     
    ##  3 FORE RIVER OU~ 2003-10-16 00:00:00 1N               HAND-PICKED     
    ##  4 FORE RIVER OU~ 2003-10-16 00:00:00 1N               HAND-PICKED     
    ##  5 FORE RIVER OU~ 2003-10-16 00:00:00 2N               HAND-PICKED     
    ##  6 FORE RIVER OU~ 2003-10-16 00:00:00 3N               HAND-PICKED     
    ##  7 FORE RIVER OU~ 2003-10-16 00:00:00 3N               HAND-PICKED     
    ##  8 FORE RIVER OU~ 2003-11-17 00:00:00 REP 2            HAND-PICKED     
    ##  9 FORE RIVER OU~ 2003-11-17 00:00:00 REP 3            HAND-PICKED     
    ## 10 FORE RIVER OU~ 2003-10-16 00:00:00 4N               HAND-PICKED     
    ## 11 FORE RIVER OU~ 2003-11-17 00:00:00 REP 1            HAND-PICKED     
    ## 12 FORE RIVER OU~ 2003-10-01 00:00:00 REP 1            HAND-PICKED     
    ## 13 FORE RIVER OU~ 2003-10-01 00:00:00 REP 2            HAND-PICKED     
    ## 14 FORE RIVER OU~ 2003-11-17 00:00:00 REP 4            HAND-PICKED     
    ## 15 FORE RIVER OU~ 2003-10-01 00:00:00 REP 3            HAND-PICKED     
    ## 16 FORE RIVER OU~ 2003-10-01 00:00:00 REP 4            HAND-PICKED     
    ## # ... with 1 more variable: SAMPLE_ID <chr>

#### Conclusion – This is Confusing

This is confusing – it’s not clear what the different sample collection
codes mean. THey clearly segregate to some extent by sample events, but
it appears not entirely.

### What are `LAB QUALIFIER` and `VALIDATION QUALIFIER`?

``` r
SWAT_data %>% select(`LAB QUALIFIER`,`VALIDATION QUALIFIER`, `QUALIFIER DESCRIPTION`) %>% unique() %>% kable()
```

| LAB QUALIFIER | VALIDATION QUALIFIER | QUALIFIER DESCRIPTION                                                                                                                                                                                                                                                                                                            |
| :------------ | :------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| EMPC          | NA                   | PEAK DETECTED, BUT DID NOT MEET QUANTIFICATION CRITERIA, RESULT REPORTED REPRESENTS THE ESTIMATED MAXIMUM POSSIBLE CONCENTRATION                                                                                                                                                                                                 |
| B             | NA                   | COMPOUND IS FOUND IN THE ASSOCIATED METHOD BLANK (ORGANIC) OR THE REPORTED VALUE WAS LESS THAN THE REPORTING LIMIT BUT GREATER THAN OR EQUAL TO THE IDL. (INORGANIC)                                                                                                                                                             |
| NA            | NA                   | NA                                                                                                                                                                                                                                                                                                                               |
| U             | NA                   | NOT DETECTED ABOVE THE ASSOCIATED QUANTITATION LIMIT                                                                                                                                                                                                                                                                             |
| B/EMPC        | NA                   | COMPOUND IS FOUND IN THE ASSOCIATED METHOD BLANK (ORGANIC) OR THE REPORTED VALUE WAS LESS THAN THE REPORTING LIMIT BUT GREATER THAN OR EQUAL TO THE IDL. (INORGANIC) ACCOMODATES HISTORIC DATA. PEAK DETECTED, BUT DID NOT MEET QUANTIFICATION CRITERIA, RESULT REPORTED REPRESENTS THE ESTIMATED MAXIMUM POSSIBLE CONCENTRATION |
| BL            | NA                   | COMPOUND IS FOUND IN THE ASSOCIATED METHOD BLANK AS WELL AS INORGANIC SAMPLE.                                                                                                                                                                                                                                                    |
| T             | NA                   | ANALYTE RECALCULATED AGAINST ALTERNATE LABELED COMPOUND(S) OR INTERNAL STANDARD                                                                                                                                                                                                                                                  |
| NQ            | NA                   | NOT QUANTITATED                                                                                                                                                                                                                                                                                                                  |
| U             | U                    | NOT DETECTED ABOVE THE ASSOCIATED QUANTITATION LIMIT                                                                                                                                                                                                                                                                             |
| EMPC          | EMPC                 | PEAK DETECTED, BUT DID NOT MEET QUANTIFICATION CRITERIA, RESULT REPORTED REPRESENTS THE ESTIMATED MAXIMUM POSSIBLE CONCENTRATION                                                                                                                                                                                                 |
| B             | B                    | COMPOUND IS FOUND IN THE ASSOCIATED METHOD BLANK (ORGANIC) OR THE REPORTED VALUE WAS LESS THAN THE REPORTING LIMIT BUT GREATER THAN OR EQUAL TO THE IDL. (INORGANIC)                                                                                                                                                             |
| J             | J                    | ASSOCIATED VALUE IS ESTIMATED - MAY BE DUE TO FACTORS SUCH AS HOLDING TIME VIOLATIONS, BLANK CONTAMINATION, ETC.                                                                                                                                                                                                                 |
| B/EMPC        | B/EMPC               | COMPOUND IS FOUND IN THE ASSOCIATED METHOD BLANK (ORGANIC) OR THE REPORTED VALUE WAS LESS THAN THE REPORTING LIMIT BUT GREATER THAN OR EQUAL TO THE IDL. (INORGANIC) ACCOMODATES HISTORIC DATA. PEAK DETECTED, BUT DID NOT MEET QUANTIFICATION CRITERIA, RESULT REPORTED REPRESENTS THE ESTIMATED MAXIMUM POSSIBLE CONCENTRATION |
| UT            | NA                   | NOT DETECTED ABOVE THE ASSOCIATED QUANTITATION LIMIT; AND ANALYTE RECALCULATED AGAINST ALTERNATE LABELED COMPOUND(S) OR INTERNAL STANDARD                                                                                                                                                                                        |
| T/EMPC        | NA                   | ANALYTE RECALCULATED AGAINST ALTERNATE LABELED COMPOUND(S) OR INTERNAL STANDARD AND PEAK DETECTED, BUT DID NOT MEET QUANTIFICATION CRITERIA, RESULT REPORTED REPRESENTS THE ESTIMATED MAXIMUM POSSIBLE CONCENTRATION                                                                                                             |
| \*            | NA                   | QC RESULTS NOT WITHIN CONTROL LIMITS                                                                                                                                                                                                                                                                                             |
| E\*           | NA                   | REPORTED VALUE IS ESTIMATED DUE TO PRESENCE OF INTERFERENCE (INORGANIC) OR COMPOUND EXCEEDED UPPER LEVEL OF CALIBRATION RANGE (ORGANIC) AND QC RESULTS ARE NOT WITHIN CONTROL LIMITS                                                                                                                                             |
| J             | NA                   | ASSOCIATED VALUE IS ESTIMATED - MAY BE DUE TO FACTORS SUCH AS HOLDING TIME VIOLATIONS, BLANK CONTAMINATION, ETC.                                                                                                                                                                                                                 |
| N             | NA                   | PRESENCE OF AN ANALYTE IN WHICH A TENTATIVE ID CAN BE MADE AND VALUE REPRESENTS ITS APPROXIMATE CONCENTRATION                                                                                                                                                                                                                    |
| NC            | NA                   | NOT CONFIRMED                                                                                                                                                                                                                                                                                                                    |

So it looks like the second qualifier is a subset of the first.

``` r
xtabs(~`LAB QUALIFIER` + `VALIDATION QUALIFIER`, data = SWAT_data, addNA = TRUE)
```

    ##              VALIDATION QUALIFIER
    ## LAB QUALIFIER     B B/EMPC  EMPC     J     U  <NA>
    ##        *          0      0     0     0     0    16
    ##        B       3435      0     0     0     0 25508
    ##        B/EMPC     0    258     0     0     0  1546
    ##        BL         0      0     0     0     0   182
    ##        E*         0      0     0     0     0    18
    ##        EMPC       0      0   534     0     0  5637
    ##        J          0      0     0    34     0     8
    ##        N          0      0     0     0     0     2
    ##        NC         0      0     0     0     0     9
    ##        NQ         0      0     0     0     0   159
    ##        T          0      0     0     0     0   105
    ##        T/EMPC     0      0     0     0     0     6
    ##        U          0      0     0     0   859 22396
    ##        UT         0      0     0     0     0     6
    ##        <NA>       0      0     0     0     0 52166

Interestingly, relatively few of the LAb Qualifiers were pulled out in
the Validation Qualifiers. Why?

### What is `WEIGHT BASIS`?

``` r
unique(SWAT_data$`WEIGHT BASIS`)
```

    ## [1] "LIP" "WET" "DRY" NA

We’d like to see if these are present for all parameters

``` r
xtabs(~ PARAMETER + `WEIGHT BASIS`, data = SWAT_data, addNA = TRUE)
```

    ##                                               WEIGHT BASIS
    ## PARAMETER                                      DRY LIP WET <NA>
    ##   1-METHYL NAPHTHALENE                         126 126 154    0
    ##   1-METHYLCHRYSENE                              80  80  80    0
    ##   1-METHYLPHENANTHRENE                         126 126 154    0
    ##   1,2-DIMETHYLNAPHTHALENE                       80  80  80    0
    ##   1,2,3,4,6,7,8-HPCDD                            4   4  31    0
    ##   1,2,3,4,6,7,8-HPCDF                            4   4  31    0
    ##   1,2,3,4,7,8-HXCDD                              4   4  31    0
    ##   1,2,3,4,7,8-HXCDF                              4   4  31    0
    ##   1,2,3,4,7,8,9-HEPTACHLORODIBENZOFURAN(HPCDF)   4   4  31    0
    ##   1,2,3,6,7,8-HXCDD                              4   4  31    0
    ##   1,2,3,6,7,8-HXCDF                              4   4  31    0
    ##   1,2,3,7,8-PECDD                                4   4  31    0
    ##   1,2,3,7,8-PECDF                                4   4  31    0
    ##   1,2,3,7,8,9-HXCDD                              4   4  31    0
    ##   1,2,3,7,8,9-HXCDF                              4   4  31    0
    ##   1,2,6-TRIMETHYLPHENANTHRENE                   80  80  80    0
    ##   1,4,6,7-TETRAMETHYLNAPHTHALENE                80  80  80    0
    ##   1,7-DIMETHYLFLUORENE                          80  80  80    0
    ##   1,7-DIMETHYLPHENANTHRENE                      80  80  80    0
    ##   1,8-DIMETHYLPHENANTHRENE                      80  80  80    0
    ##   2',3,4,4',5-PENTACHLOROBIPHENYL              124 124 165    0
    ##   2-CHLOROBIPHENYL                             124 124 153    0
    ##   2-METHYLANTHRACENE                            80  80  80    0
    ##   2-METHYLFLUORENE                              80  80  80    0
    ##   2-METHYLNAPHTHALENE                          126 126 154    0
    ##   2-METHYLPHENANTHRENE                          80  80  80    0
    ##   2,2'-DICHLOROBIPHENYL                        124 124 153    0
    ##   2,2',3-TRICHLOROBIPHENYL                     124 124 153    0
    ##   2,2',3,3',4-PENTACHLOROBIPHENYL              124 124 153    0
    ##   2,2',3,3',4,4',5-HEPTACHLOROBIPHENYL         124 124 165    0
    ##   2,2',3,3',4,4',5,5'-OCTACHLOROBIPHENYL       124 124 153    0
    ##   2,2',3,3',4,4',5,5',6-NONACHLOROBIPHENYL     124 124 153    0
    ##   2,2',3,3',4,4',5,6'-OCTACHLOROBIPHENYL       124 124 153    0
    ##   2,2',3,3',4,4',5,6-OCTACHLOROBIPHENYL        124 124 153    0
    ##   2,2',3,3',4,4',5,6,6'-NONACHLOROBIPHENYL     124 124 153    0
    ##   2,2',3,3',4,5'-HEXACHLOROBIPHENYL            124 124 153    0
    ##   2,2',3,3',4,5',6'-HEPTACHLOROBIPHENYL        124 124 153    0
    ##   2,2',3,3',4,5',6-HEPTACHLOROBIPHENYL         124 124 153    0
    ##   2,2',3,3',4,5',6,6'-OCTACHLOROBIPHENYL       124 124 153    0
    ##   2,2',3,3',4,5,5'-HEPTACHLOROBIPHENYL         124 124 153    0
    ##   2,2',3,3',4,5,5',6,6'-NONACHLOROBIPHENYL     124 124 153    0
    ##   2,2',3,3',4,5,6'-HEPTACHLOROBIPHENYL         124 124 153    0
    ##   2,2',3,3',4,6'-HEXACHLOROBIPHENYL            124 124 153    0
    ##   2,2',3,3',4,6-HEXACHLOROBIPHENYL             124 124 153    0
    ##   2,2',3,3',4,6,6'-HEPTACHLOROBIPHENYL         124 124 153    0
    ##   2,2',3,3',5-PENTACHLOROBIPHENYL                0   0  14    0
    ##   2,2',3,3',5,5'-HEXACHLOROBIPHENYL            124 124 153    0
    ##   2,2',3,3',5,5',6-HEPTACHLOROBIPHENYL         124 124 153    0
    ##   2,2',3,3',5,5',6,6'-OCTACHLOROBIPHENYL       124 124 153    0
    ##   2,2',3,3',5,6,6'-HEPTACHLOROBIPHENYL         124 124 153    0
    ##   2,2',3,3',6-PENTACHLOROBIPHENYL              124 124 153    0
    ##   2,2',3,3',6,6'-HEXACHLOROBIPHENYL            124 124 153    0
    ##   2,2',3,4'-TETRACHLOROBIPHENYL                124 124 153    0
    ##   2,2',3,4',5,5'-HEXACHLOROBIPHENYL            124 124 153    0
    ##   2,2',3,4',5,5',6-HEPTACHLOROBIPHENYL         124 124 153    0
    ##   2,2',3,4',5,6'-HEXACHLOROBIPHENYL            124 124 153    0
    ##   2,2',3,4',5,6,6'-HEPTACHLOROBIPHENYL         124 124 153    0
    ##   2,2',3,4',6,6'-HEXACHLOROBIPHENYL            124 124 153    0
    ##   2,2',3,4,4',5-HEXACHLOROBIPHENYL             124 124 153    0
    ##   2,2',3,4,4',5,5'-HEPTACHLOROBIPHENYL           0   0  12    0
    ##   2,2',3,4,4',5,5',6-OCTACHLOROBIPHENYL        124 124 153    0
    ##   2,2',3,4,4',5,6'-HEPTACHLOROBIPHENYL         124 124 153    0
    ##   2,2',3,4,4',5,6-HEPTACHLOROBIPHENYL          124 124 153    0
    ##   2,2',3,4,4',5,6,6'-OCTACHLOROBIPHENYL        124 124 153    0
    ##   2,2',3,4,4',6,6'-HEPTACHLOROBIPHENYL         124 124 153    0
    ##   2,2',3,4,5',6-HEXACHLOROBIPHENYL             124 124 153    0
    ##   2,2',3,4,5,5'-HEXACHLOROBIPHENYL             124 124 153    0
    ##   2,2',3,4,5,6-HEXACHLOROBIPHENYL              124 124 153    0
    ##   2,2',3,4,5,6,6'-HEPTACHLOROBIPHENYL          124 124 153    0
    ##   2,2',3,4,6'-PENTACHLOROBIPHENYL              124 124 153    0
    ##   2,2',3,4,6,6'-HEXACHLOROBIPHENYL             124 124 153    0
    ##   2,2',3,5',6-PENTACHLOROBIPHENYL                0   0  14    0
    ##   2,2',3,5-TETRACHLOROBIPHENYL                 124 124 153    0
    ##   2,2',3,5,5'-PENTACHLOROBIPHENYL              124 124 153    0
    ##   2,2',3,5,6'-PENTACHLOROBIPHENYL              124 124 153    0
    ##   2,2',3,5,6,6'-HEXACHLOROBIPHENYL             124 124 153    0
    ##   2,2',3,6'-TETRACHLOROBIPHENYL                124 124 153    0
    ##   2,2',3,6,6'-PENTACHLOROBIPHENYL              124 124 153    0
    ##   2,2',4-TRICHLOROBIPHENYL                     124 124 153    0
    ##   2,2',4,4',5-PENTACHLOROBIPHENYL                0   0  14    0
    ##   2,2',4,4',5,6'-HEXACHLOROBIPHENYL              0   0  14    0
    ##   2,2',4,4',6,6'-HEXACHLOROBIPHENYL            124 124 153    0
    ##   2,2',4,5',6-PENTACHLOROBIPHENYL              124 124 153    0
    ##   2,2',4,5-TETRACHLOROBIPHENYL                 124 124 153    0
    ##   2,2',4,6,6'-PENTACHLOROBIPHENYL              124 124 153    0
    ##   2,2',5,5'-TETRACHLOROBIPHENYL                124 124 164    0
    ##   2,2',6-TRICHLOROBIPHENYL                     124 124 153    0
    ##   2,2',6,6'-TETRACHLOROBIPHENYL                124 124 153    0
    ##   2,3'-DICHLOROBIPHENYL                        124 124 153    0
    ##   2,3',4-TRICHLOROBIPHENYL                     124 124 153    0
    ##   2,3',4,4'-TETRACHLOROBIPHENYL                124 124 153    0
    ##   2,3',4,4',5-PENTACHLOROBIPHENYL              124 124 165    0
    ##   2,3',4,4',5,5'-HEXACHLOROBIPHENYL            124 124 165    0
    ##   2,3',4,5'-TETRACHLOROBIPHENYL                124 124 153    0
    ##   2,3',4,5',6-PENTACHLOROBIPHENYL              124 124 153    0
    ##   2,3',4,5-TETRACHLOROBIPHENYL                 124 124 153    0
    ##   2,3',4,5,5'-PENTACHLOROBIPHENYL              124 124 153    0
    ##   2,3',5'-TRICHLOROBIPHENYL                    124 124 153    0
    ##   2,3',5',6-TETRACHLOROBIPHENYL                124 124 153    0
    ##   2,3',5,5'-TETRACHLOROBIPHENYL                124 124 153    0
    ##   2,3',6-TRICHLOROBIPHENYL                     124 124 153    0
    ##   2,3-DICHLOROBIPHENYL                         124 124 153    0
    ##   2,3,3',4'-TETRACHLOROBIPHENYL                124 124 153    0
    ##   2,3,3',4',5'-PENTACHLOROBIPHENYL             124 124 153    0
    ##   2,3,3',4',5',6-HEXACHLOROBIPHENYL            124 124 153    0
    ##   2,3,3',4',5,5'-HEXACHLOROBIPHENYL            124 124 153    0
    ##   2,3,3',4-TETRACHLOROBIPHENYL                 124 124 153    0
    ##   2,3,3',4,4'-PENTACHLOROBIPHENYL              124 124 165    0
    ##   2,3,3',4,4',5'-HEXACHLOROBIPHENYL              0   0  12    0
    ##   2,3,3',4,4',5',6-HEPTACHLOROBIPHENYL         124 124 153    0
    ##   2,3,3',4,4',5-HEXACHLOROBIPHENYL               0   0  12    0
    ##   2,3,3',4,4',5,5'-HEPTACHLOROBIPHENYL         124 124 165    0
    ##   2,3,3',4,4',5,5',6-OCTACHLOROBIPHENYL        124 124 153    0
    ##   2,3,3',4,4',5,6-HEPTACHLOROBIPHENYL          124 124 153    0
    ##   2,3,3',4,4',6-HEXACHLOROBIPHENYL             124 124 153    0
    ##   2,3,3',4,5',6-HEXACHLOROBIPHENYL             124 124 153    0
    ##   2,3,3',4,5-PENTACHLOROBIPHENYL               124 124 153    0
    ##   2,3,3',4,5,5'-HEXACHLOROBIPHENYL             124 124 153    0
    ##   2,3,3',4,5,5',6-HEPTACHLOROBIPHENYL          124 124 153    0
    ##   2,3,3',4,5,6-HEXACHLOROBIPHENYL                0   0  14    0
    ##   2,3,3',4,6-PENTACHLOROBIPHENYL               124 124 153    0
    ##   2,3,3',5'-TETRACHLOROBIPHENYL                124 124 153    0
    ##   2,3,3',5-TETRACHLOROBIPHENYL                 124 124 153    0
    ##   2,3,3',5,5'-PENTACHLOROBIPHENYL              124 124 153    0
    ##   2,3,3',5,5',6-HEXACHLOROBIPHENYL             124 124 153    0
    ##   2,3,3',5,6-PENTACHLOROBIPHENYL               124 124 153    0
    ##   2,3,4'-TRICHLOROBIPHENYL                     124 124 153    0
    ##   2,3,4',5-TETRACHLOROBIPHENYL                 124 124 153    0
    ##   2,3,4',6-TETRACHLOROBIPHENYL                 124 124 153    0
    ##   2,3,4,4'-TETRACHLOROBIPHENYL                 124 124 153    0
    ##   2,3,4,4',5-PENTACHLOROBIPHENYL               124 124 165    0
    ##   2,3,4,6,7,8-HXCDF                              4   4  31    0
    ##   2,3,4,7,8-PECDF                                4   4  31    0
    ##   2,3,5-TRICHLOROBIPHENYL                      124 124 153    0
    ##   2,3,5-TRIMETHYLNAPHTHALENE                   126 126 154    0
    ##   2,3,6-TRICHLOROBIPHENYL                      124 124 153    0
    ##   2,3,6-TRIMETHYLNAPHTHALENE                    80  80  80    0
    ##   2,3,7,8-TCDD                                   4   4  31    0
    ##   2,3,7,8-TETRACHLORODIBENZOFURAN                4   4  31    0
    ##   2,4'-DDD                                      94  94 133    0
    ##   2,4'-DDE                                      94  94 133    0
    ##   2,4'-DDT                                      94  94 133    0
    ##   2,4'-DICHLOROBIPHENYL                        124 124 153    0
    ##   2,4',5-TRICHLOROBIPHENYL                     124 124 153    0
    ##   2,4',6-TRICHLOROBIPHENYL                     124 124 153    0
    ##   2,4-DICHLOROBIPHENYL                         124 124 153    0
    ##   2,4-DIMETHYLDIBENZOTHIOPHENE                  80  80  80    0
    ##   2,5-DICHLOROBIPHENYL                         124 124 153    0
    ##   2,6-DICHLOROBIPHENYL                         124 124 153    0
    ##   2,6-DIMETHYLNAPHTHALENE                      126 126 154    0
    ##   2,6-DIMETHYLPHENANTHRENE                      80  80  80    0
    ##   2/3-METHYLDIBENZOTHIOPHENES                   80  80  80    0
    ##   3-CHLOROBIPHENYL                             124 124 153    0
    ##   3-METHYLFLUORANTHENE/BENZO[A]FLUORENE         80  80  80    0
    ##   3-METHYLPHENANTHRENE                          80  80  80    0
    ##   3,3'-DICHLOROBIPHENYL                        124 124 153    0
    ##   3,3',4-TRICHLOROBIPHENYL                     124 124 153    0
    ##   3,3',4,4'-TETRACHLOROBIPHENYL                124 124 165    0
    ##   3,3',4,4',5-PENTACHLOROBIPHENYL              124 124 165    0
    ##   3,3',4,4',5,5'-HEXACHLOROBIPHENYL            124 124 165    0
    ##   3,3',4,5'-TETRACHLOROBIPHENYL                124 124 153    0
    ##   3,3',4,5-TETRACHLOROBIPHENYL                 124 124 153    0
    ##   3,3',4,5,5'-PENTACHLOROBIPHENYL              124 124 153    0
    ##   3,3',5-TRICHLOROBIPHENYL                     124 124 153    0
    ##   3,3',5,5'-TETRACHLOROBIPHENYL                124 124 153    0
    ##   3,4',5-TRICHLOROBIPHENYL                     124 124 153    0
    ##   3,4,4'-TRICHLOROBIPHENYL                     124 124 153    0
    ##   3,4,4',5-TETRACHLOROBIPHENYL                 124 124 165    0
    ##   3,4,5-TRICHLOROBIPHENYL                      124 124 153    0
    ##   3,5-DICHLOROBIPHENYL                         124 124 153    0
    ##   3,6-DIMETHYLPHENANTHRENE                      80  80  80    0
    ##   4-CHLOROBIPHENYL                             124 124 153    0
    ##   4,4'-DDD                                      94  94 133    0
    ##   4,4'-DDE                                      94  94 133    0
    ##   4,4'-DDT                                      94  94 133    0
    ##   4,4'-DICHLOROBIPHENYL                        124 124 153    0
    ##   5,9-DIMETHYLCHRYSENE                          80  80  80    0
    ##   5/6-METHYLCHRYSENE                            80  80  80    0
    ##   7-METHYLBENZO[A]PYRENE                        80  80  80    0
    ##   9/4-METHYLPHENANTHRENE                        80  80  80    0
    ##   ACENAPHTHENE                                 126 126 154    0
    ##   ACENAPHTHYLENE                               126 126 154    0
    ##   ALACHLOR                                      34  34  34    0
    ##   ALDRIN                                        94  94 133    0
    ##   ALPHA-BHC                                     94  94 117    0
    ##   ALPHA-CHLORDANE                               94  94 133    0
    ##   ALUMINUM                                     139  59 140    0
    ##   AMETRYN                                       34  34  34    0
    ##   ANTHRACENE                                   126 126 154    0
    ##   ARSENIC                                      124  44 124    0
    ##   ATRAZINE                                      34  34  34    0
    ##   AZINPHOS-METHYL                               34  34  34    0
    ##   BENZO(A)ANTHRACENE                           126 126 154    0
    ##   BENZO(A)PYRENE                               126 126 154    0
    ##   BENZO(B)FLUORANTHENE                          34  34  50    0
    ##   BENZO(E)PYRENE                               126 126 154    0
    ##   BENZO(G,H,I)PERYLENE                         126 126 154    0
    ##   BENZO(K)FLUORANTHENE                           0   0  16    0
    ##   BENZO[B,J,K]FLUORANTHENES                     92  92  92    0
    ##   BENZO[J,K]FLUORANTHENES                       34  34  34    0
    ##   BENZOFLUORANTHENE                              0   0  12    0
    ##   BETA-BHC                                      94  94 117    0
    ##   BIPHENYL                                     126 126 154    0
    ##   BUTRALIN                                      34  34  34    0
    ##   BUTYLATE                                      34  34  34    0
    ##   C1-ACENAPHTHENES                              80  80  80    0
    ##   C1-BENZO[A]ANTHRACENES/CHRYSENES              80  80  80    0
    ##   C1-BENZOFLUORANTHENES/BENZOPYRENES            80  80  80    0
    ##   C1-BIPHENYLS                                  80  80  80    0
    ##   C1-DIBENZOTHIOPHENES                          80  80  80    0
    ##   C1-FLUORANTHENES/PYRENES                      80  80  80    0
    ##   C1-FLUORENES                                  80  80  80    0
    ##   C1-NAPHTHALENES                               80  80  80    0
    ##   C1-PHENANTHRENES/ANTHRACENES                  80  80  80    0
    ##   C2-BENZO[A]ANTHRACENES/CHRYSENES              80  80  80    0
    ##   C2-BENZOFLUORANTHENES/BENZOPYRENES            80  80  80    0
    ##   C2-BIPHENYLS                                  80  80  80    0
    ##   C2-DIBENZOTHIOPHENES                          80  80  80    0
    ##   C2-FLUORANTHENES/PYRENES                      80  80  80    0
    ##   C2-FLUORENES                                  80  80  80    0
    ##   C2-NAPHTHALENES                               80  80  80    0
    ##   C2-PHENANTHRENES/ANTHRACENES                  80  80  80    0
    ##   C3-BENZO[A]ANTHRACENES/CHRYSENES              80  80  80    0
    ##   C3-DIBENZOTHIOPHENES                          80  80  80    0
    ##   C3-FLUORANTHENES/PYRENES                      80  80  80    0
    ##   C3-FLUORENES                                  80  80  80    0
    ##   C3-NAPHTHALENES                               80  80  80    0
    ##   C3-PHENANTHRENES/ANTHRACENES                  80  80  80    0
    ##   C4-BENZO[A]ANTHRACENES/CHRYSENES              80  80  80    0
    ##   C4-DIBENZOTHIOPHENES                          80  80  80    0
    ##   C4-FLUORANTHENES/PYRENES                      80  80  80    0
    ##   C4-NAPHTHALENES                               80  80  80    0
    ##   C4-PHENANTHRENES/ANTHRACENES                  80  80  80    0
    ##   CADMIUM                                      139  59 140    0
    ##   CAPTAN                                        34  34  34    0
    ##   CHLOROTHALONIL                                34  34  34    0
    ##   CHLORPYRIFOS, O,O-DIMETHYL ANALOG             34  34  34    0
    ##   CHLORPYRIPHOS                                 34  34  34    0
    ##   CHLORPYRIPHOS-OXON                            34  34  34    0
    ##   CHROMIUM                                     139  59 140    0
    ##   CHRYSENE                                     126 126 154    0
    ##   CIS-NONACHLOR                                 34  34  34    0
    ##   COPPER                                       139  59 140    0
    ##   CYANAZINE                                     34  34  34    0
    ##   CYPERMETHRIN                                  34  34  34    0
    ##   DCPA                                          34  34  34    0
    ##   DECACHLOROBIPHENYL                           124 124 153    0
    ##   DELTA-BHC                                     94  94 105    0
    ##   DESETHYLATRAZINE                              34  34  34    0
    ##   DIAZINON                                      34  34  34    0
    ##   DIAZINON-OXON                                 34  34  34    0
    ##   DIBENZO(A,H)ANTHRACENE                       126 126 154    0
    ##   DIBENZOTHIOPHENE                              80  80  80    0
    ##   DIELDRIN                                      94  94 121    0
    ##   DIMETHENAMID                                  34  34  34    0
    ##   DIMETHOATE                                    34  34  34    0
    ##   DISULFOTON                                    34  34  34    0
    ##   DISULFOTON SULFONE                            34  34  34    0
    ##   DX TOTAL TEQ (ND=0)                            4   4   4    0
    ##   DX TOTAL TEQ (ND=1/2 DL)                       4   4  31    0
    ##   DX TOTAL TEQ (ND=DL)                           4   4  31    0
    ##   ENDOSULFAN I                                  94  94 121    0
    ##   ENDOSULFAN II                                 94  94 121    0
    ##   ENDOSULFAN SULFATE                            94  94 105    0
    ##   ENDRIN                                        94  94 105    0
    ##   ENDRIN ALDEHYDE                               60  60  71    0
    ##   ENDRIN KETONE                                 94  94 105    0
    ##   ETHALFLURALIN                                 34  34  34    0
    ##   ETHION                                        34  34  34    0
    ##   FENITROTHION                                  34  34  34    0
    ##   FLUFENACET                                    34  34  34    0
    ##   FLUORANTHENE                                 126 126 154    0
    ##   FLUORENE                                     126 126 154    0
    ##   FLUTRIAFOL                                    34  34  34    0
    ##   FONOFOS                                       34  34  34    0
    ##   GAMMA-BHC (LINDANE)                           94  94 133    0
    ##   GAMMA-CHLORDANE                               94  94 133    0
    ##   HEPTACHLOR                                    94  94 133    0
    ##   HEPTACHLOR EPOXIDE                            95  95 122    0
    ##   HEXACHLOROBENZENE                             94  94 133    0
    ##   INDENO(1,2,3-CD)PYRENE                       126 126 154    0
    ##   IRON                                         139  59 140    0
    ##   LEAD                                         139  59 140    0
    ##   LINURON                                       34  34  34    0
    ##   LIPIDS                                         0 113 183    0
    ##   MALATHION                                     34  34  34    0
    ##   MERCURY                                      143  59 142    0
    ##   METHAMIDOPHOS                                 34  34  34    0
    ##   METHOPRENE                                    34  34  34    0
    ##   METHOXYCHLOR                                  94  94 105    0
    ##   METHYL PARATHION                              34  34  34    0
    ##   METOLACHLOR                                   34  34  34    0
    ##   METRIBUZIN                                    34  34  34    0
    ##   MIREX                                         94  94 133    0
    ##   MOISTURE                                      11   0 221    0
    ##   Mytilus edulis                                 0   0   0  232
    ##   NAPHTHALENE                                  126 126 154    0
    ##   NICKEL                                       139  59 140    0
    ##   OCDD                                           4   4  31    0
    ##   OCDF                                           4   4  31    0
    ##   OCTACHLOROSTYRENE                             34  34  34    0
    ##   OXYCHLORDANE                                  34  34  34    0
    ##   PARATHION                                     34  34  34    0
    ##   PCB-012/013                                  124 124 153    0
    ##   PCB-018/030                                  124 124 153    0
    ##   PCB-020/028                                  124 124 153    0
    ##   PCB-021/033                                  124 124 153    0
    ##   PCB-026/029                                  124 124 153    0
    ##   PCB-040/041/071                              124 124 153    0
    ##   PCB-044/047/065                              124 124 153    0
    ##   PCB-045/051                                  124 124 153    0
    ##   PCB-049/069                                  124 124 153    0
    ##   PCB-050/053                                  124 124 153    0
    ##   PCB-059/062/075                              124 124 153    0
    ##   PCB-061/070/074/076                          124 124 153    0
    ##   PCB-083/099                                  124 124 139    0
    ##   PCB-085/116/117                              124 124 153    0
    ##   PCB-086/087/097/108/119/125                  124 124 153    0
    ##   PCB-088/091                                  124 124 153    0
    ##   PCB-090/101/113                              124 124 153    0
    ##   PCB-093/095/098/100/102                      124 124 139    0
    ##   PCB-093/098/100/102                            0   0  14    0
    ##   PCB-107/124                                  124 124 153    0
    ##   PCB-110/115                                  124 124 153    0
    ##   PCB-128/166                                  124 124 153    0
    ##   PCB-129/138/160/163                          124 124 139    0
    ##   PCB-129/138/163                                0   0  14    0
    ##   PCB-134/143                                  124 124 153    0
    ##   PCB-135/151                                    0   0  14    0
    ##   PCB-135/151/154                              124 124 139    0
    ##   PCB-139/140                                  124 124 153    0
    ##   PCB-147/149                                  124 124 153    0
    ##   PCB-153/168                                  124 124 153    0
    ##   PCB-156/157                                  124 124 153    0
    ##   PCB-171/173                                  124 124 153    0
    ##   PCB-180/193                                  124 124 153    0
    ##   PCB-183/185                                  124 124 153    0
    ##   PCB-197/200                                  124 124 153    0
    ##   PCB-198/199                                  124 124 153    0
    ##   PCB TOTAL TEQ (ND=0)                         124 124 139    0
    ##   PCB TOTAL TEQ (ND=1/2 DL)                    124 124 139    0
    ##   PCB TOTAL TEQ (ND=DL)                        124 124 139    0
    ##   PCBS                                          80  80  95    0
    ##   PENDIMETHALIN                                 34  34  34    0
    ##   PENTACHLORONITROBENZENE                       34  34  34    0
    ##   PERFLUOROBUTANE SULFONATE                     32  28  32    0
    ##   PERFLUOROBUTANOATE                            32  28  32    0
    ##   PERFLUORODECANOATE                            32  28  32    0
    ##   PERFLUORODODECANOATE                          32  28  32    0
    ##   PERFLUOROHEPTANOATE                           32  28  32    0
    ##   PERFLUOROHEXANE SULFONATE                     32  28  32    0
    ##   PERFLUOROHEXANOATE                            32  28  32    0
    ##   PERFLUORONONANOATE                            32  28  32    0
    ##   PERFLUOROOCTANE SULFONAMIDE                   32  28  32    0
    ##   PERFLUOROOCTANE SULFONATE                     32  28  32    0
    ##   PERFLUOROOCTANOATE                            28  24  28    0
    ##   PERFLUOROOCTANOIC ACID                         4   4   4    0
    ##   PERFLUOROPENTANOATE                           32  28  32    0
    ##   PERFLUOROUNDECANOATE                          32  28  32    0
    ##   PERMETHRIN                                    34  34  34    0
    ##   PERTHANE                                      34  34  34    0
    ##   PERYLENE                                     126 126 154    0
    ##   PHENANTHRENE                                 126 126 154    0
    ##   PHORATE                                       34  34  34    0
    ##   PHOSMET                                       34  34  34    0
    ##   PIRIMIPHOS-METHYL                             34  34  34    0
    ##   PYRENE                                       126 126 154    0
    ##   RETENE                                        80  80  80    0
    ##   SELENIUM                                     139  59 140    0
    ##   SILVER                                       139  59 140    0
    ##   SIMAZINE                                      34  34  34    0
    ##   SOLIDS-TOTAL RESIDUE (TS)                     95   0  44    0
    ##   TEBUCONAZOL                                   34  34  34    0
    ##   TECNAZENE                                     34  34  34    0
    ##   TERBUFOS                                      34  34  34    0
    ##   TOTAL PAH-D                                   80  80  80    0
    ##   TOTAL PAH-H                                   80  80  80    0
    ##   TOTAL PAH-O                                   80  80  80    0
    ##   TOTAL PAH19-D                                126 126 126    0
    ##   TOTAL PAH19-H                                126 126 126    0
    ##   TOTAL PAH19-O                                126 126 126    0
    ##   TOTAL PAH24-D                                 80  80  80    0
    ##   TOTAL PAH24-H                                 80  80  80    0
    ##   TOTAL PAH24-O                                 80  80  80    0
    ##   TOTAL PAH40-D                                 80  80  80    0
    ##   TOTAL PAH40-H                                 80  80  80    0
    ##   TOTAL PAH40-O                                 80  80  80    0
    ##   TOTAL PCB-D                                  130 130 145    0
    ##   TOTAL PCB-H                                  130 130 145    0
    ##   TOTAL PCB-O                                  130 130 145    0
    ##   TOTAL PESTICIDES21-D                         112 112 112    0
    ##   TOTAL PESTICIDES21-H                         112 112 112    0
    ##   TOTAL PESTICIDES21-O                         112 112 112    0
    ##   TRANS-NONACHLOR                               94  94 133    0
    ##   TRIALLATE                                     34  34  34    0
    ##   TRIFLURALIN                                   34  34  34    0
    ##   VELPAR                                        34  34  34    0
    ##   WEIGHT                                         0   0  94    0
    ##   ZINC                                         139  59 140    0

### What is `DILUTION FACTOR`?

``` r
xtabs(~SWAT_data$`DILUTION_FACTOR`)
```

    ## SWAT_data$DILUTION_FACTOR
    ##     1    10     2    20     3     4     5    50    57    61    62    74    81 
    ## 98702   106   169    12    15   267   216    10     3     3     6     3     3 
    ##    83    85     9 
    ##     3     3    21

# Sites

## List of Sites

Note that we pull apart the site name into a site code and a longer
name.

``` r
sites <- SWAT_data %>%
  select(`SITE SEQ`, `EGAD_SITE_NAME`) %>%
  group_by(`SITE SEQ`, `EGAD_SITE_NAME`) %>%
  summarize(SiteCode =  first(sub('.* - ','', `EGAD_SITE_NAME`)), 
            Site =  first(sub(' - .*','', `EGAD_SITE_NAME`)),
                        .groups = 'drop') %>%
  select(-`EGAD_SITE_NAME`)
#write_csv(sites, 'cb_SWAT_sites_list.csv')
kable(sites)
```

| SITE SEQ | SiteCode | Site                               |
| -------: | :------- | :--------------------------------- |
|    70672 | CBBBBB   | BACK BAY                           |
|    70674 | CBFROR   | FORE RIVER OUTER                   |
|    70675 | CBGDCC   | COCKTAIL COVE GREAT DIAMOND ISLAND |
|    70676 | CBGDSW   | SOUTHWEST END GREAT DIAMOND ISLAND |
|    70677 | CBHWNP   | NAVY PIER                          |
|    70678 | CBMCMC   | MILL CREEK                         |
|    70679 | CBRYMT   | ROYAL RIVER MOUTH                  |
|    76071 | CBHRHR   | HARASEEKET RIVER                   |
|    76073 | CBANAN   | FALMOUTH ANCHORAGE                 |
|    76076 | CBMBBH   | BRUNSWICK MARE BROOK DRAINAGE      |
|    76079 | CBFRMR   | MIDDLE FORE RIVER                  |
|    76080 | CBEEEE   | EAST END BEACH                     |
|    76084 | CBSPSP   | S PORTLAND SPRING POINT            |
|    76091 | CBJWPB   | JEWEL ISLAND PUNCHBOWL             |
|    77495 | CBPRMT   | PRESUMPSCOT RIVER (MOUTH)          |
|    77497 | CBMBMB   | MIDDLE BAY (OUTER)                 |
|    82251 | CBMBBR   | MAQUOIT BAY                        |
|    82256 | CBFRIR   | INNER FORE RIVER                   |
|    82261 | CBQHQH   | QUAHOG BAY                         |
|    82266 | CBLNFT   | LONG ISLAND                        |

We can check that the results are unique:

``` r
any(duplicated(sites$`SITE SEQ`))
```

    ## [1] FALSE

``` r
any(duplicated(sites$SiteCode))
```

    ## [1] FALSE

``` r
any(duplicated(sites$Site))
```

    ## [1] FALSE

## Match Sites with Geographic Locations

In addition to the toxics data, we received a separate Excel file
containing geospatial data. That data includes repeat geographic data
for each site, apparently providing more information on separate sample
collection events. As far as we have been able to tell, however, there
is no consistent sample identifier between the geographic data and
toxics data as received.

Here we reduce the spatial data to data on each nominal sampling
location, and check that we have geographic data for each site. As is
often the case, we lack precise metadata on geographic coordinates. We
do know locations were collected in most cases with hand-held GPS units,
which suggests lat-long data are probably in WGS 1984.

We will need to return to the spatial data later to see whether sampling
events have slightly different nominal sampling locations from their
reported sites.

``` r
fn <- 'CascoBay_SWAT_Spatial.xlsx'
samples_spatial_data <- read_excel(file.path(sibling, fn))%>%
  mutate(`SITE UTM X` = as.numeric(`SITE UTM X`),
         `SITE UTM Y` = as.numeric(`SITE UTM Y`),
         `SITE LATITUDE` = as.numeric(`SITE LATITUDE`),
         `SITE LONGITUDE` = as.numeric(`SITE LONGITUDE`),
         `SAMPLE POINT UTM X` = as.numeric(`SAMPLE POINT UTM X`),
         `SAMPLE POINT UTM Y` = as.numeric(`SAMPLE POINT UTM Y`),
         `SAMPLE POINT LATITUDE` = as.numeric(`SAMPLE POINT LATITUDE`),
         `SAMPLE POINT LONGITUDE` = as.numeric(`SAMPLE POINT LONGITUDE`),
  )

sites_spatial_data <- samples_spatial_data  %>%
  rename(SITESEQ = `SITE SEQ`) %>%
    group_by(SITESEQ) %>%
    summarize(SITECODE  = first(sites$SiteCode[match(SITESEQ, sites$`SITE SEQ`)]),
              SITE  = first(sites$Site[match(SITESEQ, sites$`SITE SEQ`)]),
              UTM_E = first(`SITE UTM X`),
              UTM_N = first(`SITE UTM Y`),
              LAT   = first(`SITE LATITUDE`),
              LONG  = first(`SITE LONGITUDE`),
              .groups = 'drop') 
kable(sites_spatial_data)
```

| SITESEQ | SITECODE | SITE                               |   UTM\_E |  UTM\_N |      LAT |       LONG |
| ------: | :------- | :--------------------------------- | -------: | ------: | -------: | ---------: |
|   70615 | NA       | NA                                 | 411227.0 | 4851962 | 43.81540 | \-70.10383 |
|   70672 | CBBBBB   | BACK BAY                           | 398444.8 | 4836607 | 43.67553 | \-70.25983 |
|   70674 | CBFROR   | FORE RIVER OUTER                   | 398793.2 | 4833136 | 43.64433 | \-70.25486 |
|   70675 | CBGDCC   | COCKTAIL COVE GREAT DIAMOND ISLAND | 403913.1 | 4837479 | 43.68411 | \-70.19217 |
|   70676 | CBGDSW   | SOUTHWEST END GREAT DIAMOND ISLAND | 404062.5 | 4836811 | 43.67811 | \-70.19019 |
|   70677 | CBHWNP   | NAVY PIER                          | 418446.6 | 4848718 | 43.78703 | \-70.01358 |
|   70678 | CBMCMC   | MILL CREEK                         | 401566.9 | 4841472 | 43.71975 | \-70.22200 |
|   70679 | CBRYMT   | ROYAL RIVER MOUTH                  | 407753.4 | 4849928 | 43.79667 | \-70.14667 |
|   76071 | CBHRHR   | HARASEEKET RIVER                   | 411578.2 | 4852437 | 43.81972 | \-70.09955 |
|   76073 | CBANAN   | FALMOUTH ANCHORAGE                 | 402901.1 | 4842729 | 43.73124 | \-70.20567 |
|   76076 | CBMBBH   | BRUNSWICK MARE BROOK DRAINAGE      | 424162.1 | 4854574 | 43.84035 | \-69.94339 |
|   76079 | CBFRMR   | MIDDLE FORE RIVER                  | 398479.0 | 4832708 | 43.64044 | \-70.25867 |
|   76080 | CBEEEE   | EAST END BEACH                     | 400301.9 | 4835928 | 43.66967 | \-70.23668 |
|   76084 | CBSPSP   | S PORTLAND SPRING POINT            | 401120.8 | 4833799 | 43.65061 | \-70.22613 |
|   76091 | CBJWPB   | JEWEL ISLAND PUNCHBOWL             | 412240.3 | 4837567 | 43.68593 | \-70.08888 |
|   77495 | CBPRMT   | PRESUMPSCOT RIVER (MOUTH)          | 399498.1 | 4838470 | 43.69245 | \-70.24712 |
|   77497 | CBMBMB   | MIDDLE BAY (OUTER)                 | 415192.5 | 4846380 | 43.76562 | \-70.05365 |
|   82251 | CBMBBR   | MAQUOIT BAY                        | 414366.3 | 4851142 | 43.80838 | \-70.06467 |
|   82256 | CBFRIR   | INNER FORE RIVER                   | 395752.1 | 4833378 | 43.64609 | \-70.29260 |
|   82261 | CBQHQH   | QUAHOG BAY                         | 425327.4 | 4849547 | 43.79521 | \-69.92819 |
|   82266 | CBLNFT   | LONG ISLAND                        | 406422.4 | 4838411 | 43.69282 | \-70.16120 |
|   90135 | NA       | NA                                 | 421004.6 | 4843357 | 43.73904 | \-69.98101 |
|   90136 | NA       | NA                                 | 402858.7 | 4830891 | 43.62466 | \-70.20406 |
|   90137 | NA       | NA                                 | 422885.9 | 4842844 | 43.73462 | \-69.95757 |
|   90139 | NA       | NA                                 | 413231.7 | 4838984 | 43.69881 | \-70.07681 |
|   90140 | NA       | NA                                 | 406231.4 | 4840552 | 43.71207 | \-70.16394 |
|  116868 | NA       | NA                                 | 424135.3 | 4851783 | 43.81522 | \-69.94333 |
|  127173 | NA       | NA                                 | 399599.5 | 4839120 | 43.69831 | \-70.24598 |
|  127174 | NA       | NA                                 | 398952.8 | 4839366 | 43.70044 | \-70.25405 |

``` r
#rm(sites)
```

``` r
sum(! is.na(sites_spatial_data$SITE))
```

    ## [1] 20

So, they’re all there. Presumably the other sites have samples of other
shellfish (in the second tab of the source Excel File).

## Write CSV files

``` r
write_csv(sites_spatial_data, 'sites_spatial.csv')
```

ArcGIS was having trouble reading this file, so we try to simplify here.
Especially by simplifying column names (removing spaces) and removing
NAs. Without this step, NAs were forcing ArcGIS to interpret these data
columns as text.

``` r
samples_spatial_data %>%
  select ( -`SITE UTM X`, -`SITE UTM Y`, -`SITE LATITUDE`, -`SITE LONGITUDE`,
           - DATA_SOURCE, -LOCATION_ACCURACY, -LOCATION_METHOD) %>%
  rename_all(~gsub(' ', '', .)) %>%
  write_csv('samples_spatial.csv', na = '')
```

# Sample Points

Many Sites have more than one SAMPLE POINTs. The SAMPLE POINTs appear to
be actual sampling locations, each associated with a nominal site.
Unfortunately, recording of actual sampling locations appears
inconsistent with real (i.e., not identical) Sample Pt data not
available from earlier sample years, but typically collected more
frequently since.

## Examine Distances between Site and Sample Points

``` r
a <- samples_spatial_data %>%
  select(`SITE SEQ`, CURRENT_SITE_NAME, `SITE UTM X`, `SITE UTM Y`,
         `SAMPLE POINT NAME`, `SAMPLE POINT SEQ`,
         `SAMPLE POINT UTM X`,`SAMPLE POINT UTM Y`) %>%
  mutate(SiteCode =  sub('.* - ','', `CURRENT_SITE_NAME`)) %>%
  mutate(d = sqrt((`SAMPLE POINT UTM X` -`SITE UTM X`)^2 + 
                  (`SAMPLE POINT UTM Y` -`SITE UTM Y`)^2)) %>%
  group_by(`SiteCode`) %>%
  summarize(sitex    = first(`SITE UTM X`),
            sitey    = first(`SITE UTM Y`),
            maxx    = round(max(abs(`SAMPLE POINT UTM X` - `SITE UTM X`)),2),
            maxy    = round(max(abs(`SAMPLE POINT UTM Y` - `SITE UTM Y`)),2),
            maxd    = round(max(d), 2),
            n = n(),
            .groups = 'drop') %>%
  select(-sitex, -sitey)
kable(a, col.names = c('Site', 'Max X Distance', 'Max Y Distance', 'Maximum Distance From Site', 'N'))
```

| Site        | Max X Distance | Max Y Distance | Maximum Distance From Site |  N |
| :---------- | -------------: | -------------: | -------------------------: | -: |
| CBANAN      |          45.30 |          68.31 |                      81.96 |  4 |
| CBBBBB      |        1036.71 |         461.35 |                    1098.31 |  8 |
| CBEEEE      |             NA |             NA |                         NA | 20 |
| CBFRIR      |          81.26 |          48.45 |                      93.16 |  4 |
| CBFRMR      |         132.60 |         132.97 |                     186.26 |  4 |
| CBFROR      |           0.00 |           0.00 |                       0.00 |  8 |
| CBGDCC      |           0.00 |           0.00 |                       0.00 |  3 |
| CBGDSW      |           0.00 |           0.00 |                       0.00 |  4 |
| CBHASC      |          60.43 |         185.37 |                     194.97 |  4 |
| CBHRHR      |           6.58 |          16.11 |                      17.41 |  4 |
| CBHWNP      |         341.82 |         988.62 |                    1046.05 |  8 |
| CBJWPB      |           0.00 |           0.00 |                       0.00 |  4 |
| CBLNFT      |          35.75 |          35.01 |                      50.04 |  4 |
| CBMBBH      |             NA |             NA |                         NA | 25 |
| CBMBBR      |         478.90 |          44.72 |                     480.51 |  4 |
| CBMBMB      |             NA |             NA |                         NA |  4 |
| CBMCMC      |             NA |             NA |                         NA | 15 |
| CBPRES      |         127.73 |         990.10 |                     998.30 |  5 |
| CBPRMT      |             NA |             NA |                         NA | 16 |
| CBPRWS      |         273.21 |         480.22 |                     552.50 |  5 |
| CBQHQH      |          17.36 |          73.47 |                      75.24 |  4 |
| CBRYMT      |           0.00 |           0.00 |                       0.00 |  4 |
| CBSPSP      |         147.70 |          61.64 |                     147.71 | 16 |
| ME06-0017   |           0.00 |           0.00 |                       0.00 |  1 |
| NCCA10-1016 |           0.00 |           0.00 |                       0.00 |  1 |
| NCCA10-1017 |           0.00 |           0.00 |                       0.00 |  1 |
| NCCA10-1018 |           0.00 |           0.00 |                       0.00 |  1 |
| NCCA10-1020 |           0.00 |           0.00 |                       0.00 |  1 |
| NCCA10-1021 |           0.00 |           0.00 |                       0.00 |  1 |

## Map Visualization

Several Sites show (unreasonable?) large differences between Sample
Points and nominal Site locations. To make sense of this, we mapped
Sites and Sample Points in Arc GIS. \! [Map of SWAT Sampling Locations
and Nominal Sites](SWAT_Toxics_Locations.jpg) The map shows clearly that
sites are general areas, not the exact locations where samples were
collected. IN most cases, the distances are trivial on a map of this
scale, but not always.

  - **CBBBBB – Back Bay**: Do not appear to be typos, but exact
    locations do not appear to have been collected in all years. Samples
    appearto have ben collected from various locations along the Back
    Bay Shoreline.  
    identical for all samples. Locations in Samples in 2015 all fairly
    consistent, but differ slightly.  
  - **CBHWNP – Harpswell Navy Pier**: Multiple samples from each of two
    years, were given identical GPS coordinates. The two locations are
    some distance apart.  
  - **CBMBBR – Brunswick Mare Brook**: States positions from 2015 only
    accurate to 1000m. but they look better. Sampling Points are spread
    out along the shoreline and the nominal Site location is not all
    that close to most of them, but is located bayward of them.
  - **CBPRES – Presumpscot River East** – Sample Points along shore,
    East shore.  
  - **CBPRWS – Presumpscot River West** – Sample Points along shore,
    West shore.  
  - **CBPRM\* – Presumpscot River Mouth** – Sample Points along shore,
    BOTH shores.

# Sampling Dates

## Dates and Sites Provide Unique Sampling Events, But Not Unique Samples

From a practical point of view, it’s likely we have unique sampling
events – possibly including multiple locations or multiple replicates
within locations – on each sampling date.

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

That Suggests we have FORTY unique sampling dates and locations Note
that there are several times that we have multiple sites sampled in a
given date, and several times that the same site is sampled multiple
times in the same year.

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
\* Mare Brook (three times over ten years)  
\* East End Beach (six times over eleven years)  
\* Mill Creek (Six Times over twelve years)  
\* Spring Point (four times over nine years)

Notice that the Fore River Outer site was sampled three times in one
year.

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

From that we can see that SAMPLE\_ID is tied to unique \* SITE SEQ /
CURRENT\_SITE\_NAME / Code \* CURRENT\_SAMPLE\_POINT\_NAME \* SAMPLE
POINT TYPE

But NOT to unique \* SAMPLE COLLECTION METHOD \* SAMPLE TYPE \*
SAMPLE\_DATE

It is possible that multiple sample types might be collected during a
single site visit, with each sent off to different laboratories (for
example). So the most troubling of these is the date. We check if the
combination of SAMPLE\_ID and SAMPLE\_DATE provides unique information.

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

It turns out SAMPLE\_ID is not unique within years, or even within years
and months. To get to a unique combination, we must include the day as
well. On the other hand, we never have more than two sample dates at any
SAMPLE\_ID in a year. If we could figure out which is which, we could
use the year and a sequence number as a unique ID.

But it turns out, we can do better, and make the year and date codes
consistent across all samples collected in a given year. We never see
more than a few sample dates within a year (at all locations), so we can
code each sample date as a year plus a digit between one and three.

A slightly roundabout way to convert from dates to sequence numbers is
to use as.numeric(factor(.)).

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

So, other than really ugly unique sample idnetifiers, we’ve got a useful
solution.

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
  mutate    (code = paste(sample_id, Year, tag, sep = '_')) %>%
  select    (-sample_id, -tag) %>%
  select(`SITE SEQ`, SiteCode, Site, Year, SAMPLE_DATE, SAMPLE_ID, code, everything())
SWAT_simplified
```

    ## # A tibble: 112,884 x 25
    ##    `SITE SEQ` SiteCode Site   Year SAMPLE_DATE         SAMPLE_ID code 
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
    ## # ... with 112,874 more rows, and 18 more variables:
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
           SAMPLE_DATE, `TEST METHOD`, code)) %>%
  group_by(code) %>%
  summarize(nsites    = length(unique(`SiteCode`)),
            ndates    = length(unique(`SAMPLE_DATE`)),
            nsamppt   = length(unique(`CURRENT_SAMPLE_POINT_NAME`)),
            nmeth     = length(unique(`SAMPLE COLLECTION METHOD`)),
            .groups = 'drop') %>%
  kable()
```

| code                            | nsites | ndates | nsamppt | nmeth |
| :------------------------------ | -----: | -----: | ------: | ----: |
| CBANAN\_REP\_1\_2007\_1         |      1 |      1 |       1 |     2 |
| CBANAN\_REP\_2\_2007\_1         |      1 |      1 |       1 |     2 |
| CBANAN\_REP\_3\_2007\_1         |      1 |      1 |       1 |     2 |
| CBANAN\_REP\_4\_2007\_1         |      1 |      1 |       1 |     2 |
| CBBBBB\_REP\_1\_2006\_4         |      1 |      1 |       1 |     1 |
| CBBBBB\_REP\_2\_2006\_4         |      1 |      1 |       1 |     1 |
| CBBBBB\_REP\_3\_2006\_4         |      1 |      1 |       1 |     1 |
| CBEEEE\_REP\_1\_2007\_3         |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_1\_2009\_5         |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_1\_2011\_2011\_1   |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_1\_2013\_2013\_1   |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_1\_2015\_2015\_2   |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_1\_2017\_2017\_1   |      1 |      1 |       1 |     1 |
| CBEEEE\_REP\_2\_2007\_3         |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_2\_2009\_5         |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_2\_2011\_2011\_1   |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_2\_2013\_2013\_1   |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_2\_2015\_2015\_2   |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_2\_2017\_2017\_1   |      1 |      1 |       1 |     1 |
| CBEEEE\_REP\_3\_2007\_3         |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_3\_2009\_5         |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_3\_2011\_2011\_1   |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_3\_2013\_2013\_1   |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_3\_2015\_2015\_2   |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_3\_2017\_2017\_1   |      1 |      1 |       1 |     1 |
| CBEEEE\_REP\_4\_2007\_3         |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_4\_2009\_5         |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_4\_2011\_2011\_1   |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_4\_2013\_2013\_1   |      1 |      1 |       1 |     2 |
| CBEEEE\_REP\_4\_2015\_2015\_2   |      1 |      1 |       1 |     2 |
| CBFRIR\_REP\_1\_2009\_1         |      1 |      1 |       1 |     2 |
| CBFRIR\_REP\_2\_2009\_1         |      1 |      1 |       1 |     2 |
| CBFRIR\_REP\_3\_2009\_1         |      1 |      1 |       1 |     2 |
| CBFRIR\_REP\_4\_2009\_1         |      1 |      1 |       1 |     2 |
| CBFRMR\_REP\_1\_2007\_4         |      1 |      1 |       1 |     2 |
| CBFRMR\_REP\_2\_2007\_4         |      1 |      1 |       1 |     2 |
| CBFRMR\_REP\_3\_2007\_4         |      1 |      1 |       1 |     2 |
| CBFRMR\_REP\_4\_2007\_4         |      1 |      1 |       1 |     2 |
| CBFROR\_REP\_1\_2003\_1         |      1 |      1 |       1 |     1 |
| CBFROR\_REP\_1\_2003\_4         |      1 |      1 |       1 |     1 |
| CBFROR\_REP\_2\_2003\_1         |      1 |      1 |       1 |     1 |
| CBFROR\_REP\_2\_2003\_4         |      1 |      1 |       1 |     1 |
| CBFROR\_REP\_3\_2003\_1         |      1 |      1 |       1 |     1 |
| CBFROR\_REP\_3\_2003\_4         |      1 |      1 |       1 |     1 |
| CBFROR\_REP\_4\_2003\_1         |      1 |      1 |       1 |     1 |
| CBFROR\_REP\_4\_2003\_4         |      1 |      1 |       1 |     1 |
| CBGDCC\_REP\_1\_2006\_2         |      1 |      1 |       1 |     1 |
| CBGDCC\_REP\_2\_2006\_2         |      1 |      1 |       1 |     1 |
| CBGDCC\_REP\_3\_2006\_2         |      1 |      1 |       1 |     1 |
| CBGDSW\_REP\_1\_2003\_1         |      1 |      1 |       1 |     1 |
| CBGDSW\_REP\_1\_2003\_4         |      1 |      1 |       1 |     1 |
| CBGDSW\_REP\_2\_2003\_1         |      1 |      1 |       1 |     1 |
| CBGDSW\_REP\_2\_2003\_4         |      1 |      1 |       1 |     1 |
| CBGDSW\_REP\_3\_2003\_1         |      1 |      1 |       1 |     1 |
| CBGDSW\_REP\_3\_2003\_4         |      1 |      1 |       1 |     1 |
| CBGDSW\_REP\_4\_2003\_1         |      1 |      1 |       1 |     1 |
| CBGDSW\_REP\_4\_2003\_4         |      1 |      1 |       1 |     1 |
| CBHRHR\_REP\_1\_2007\_1         |      1 |      1 |       1 |     2 |
| CBHRHR\_REP\_1\_2007\_2         |      1 |      1 |       1 |     2 |
| CBHRHR\_REP\_2\_2007\_1         |      1 |      1 |       1 |     2 |
| CBHRHR\_REP\_2\_2007\_2         |      1 |      1 |       1 |     2 |
| CBHRHR\_REP\_3\_2007\_1         |      1 |      1 |       1 |     2 |
| CBHRHR\_REP\_3\_2007\_2         |      1 |      1 |       1 |     2 |
| CBHRHR\_REP\_4\_2007\_1         |      1 |      1 |       1 |     2 |
| CBHRHR\_REP\_4\_2007\_2         |      1 |      1 |       1 |     2 |
| CBHWNP\_REP\_1\_2014\_2014\_2   |      1 |      1 |       1 |     2 |
| CBHWNP\_REP\_2\_2014\_2014\_2   |      1 |      1 |       1 |     2 |
| CBHWNP\_REP\_3\_2014\_2014\_2   |      1 |      1 |       1 |     2 |
| CBHWNP\_REP\_4\_2014\_2014\_2   |      1 |      1 |       1 |     2 |
| CBJWPB\_REP\_1\_2007\_2         |      1 |      1 |       1 |     2 |
| CBJWPB\_REP\_2\_2007\_2         |      1 |      1 |       1 |     2 |
| CBJWPB\_REP\_3\_2007\_2         |      1 |      1 |       1 |     2 |
| CBJWPB\_REP\_4\_2007\_2         |      1 |      1 |       1 |     2 |
| CBLNFT\_REP\_1\_2009\_4         |      1 |      1 |       1 |     2 |
| CBLNFT\_REP\_2\_2009\_4         |      1 |      1 |       1 |     2 |
| CBLNFT\_REP\_3\_2009\_4         |      1 |      1 |       1 |     2 |
| CBLNFT\_REP\_4\_2009\_4         |      1 |      1 |       1 |     2 |
| CBMBBH\_REP\_1\_2007\_4         |      1 |      1 |       1 |     2 |
| CBMBBH\_REP\_1\_2014\_2014\_3   |      1 |      1 |       1 |     2 |
| CBMBBH\_REP\_1\_2016\_2016\_1   |      1 |      1 |       1 |     1 |
| CBMBBH\_REP\_2\_2007\_4         |      1 |      1 |       1 |     2 |
| CBMBBH\_REP\_2\_2014\_2014\_3   |      1 |      1 |       1 |     2 |
| CBMBBH\_REP\_2\_2016\_2016\_1   |      1 |      1 |       1 |     1 |
| CBMBBH\_REP\_3\_2007\_4         |      1 |      1 |       1 |     2 |
| CBMBBH\_REP\_3\_2014\_2014\_3   |      1 |      1 |       1 |     2 |
| CBMBBH\_REP\_3\_2016\_2016\_1   |      1 |      1 |       1 |     1 |
| CBMBBH\_REP\_4\_2007\_4         |      1 |      1 |       1 |     2 |
| CBMBBH\_REP\_4\_2014\_2014\_3   |      1 |      1 |       1 |     2 |
| CBMBBH\_REP\_4\_2016\_2016\_1   |      1 |      1 |       1 |     1 |
| CBMBBR\_REP\_1\_2009\_3         |      1 |      1 |       1 |     2 |
| CBMBBR\_REP\_2\_2009\_3         |      1 |      1 |       1 |     2 |
| CBMBBR\_REP\_3\_2009\_3         |      1 |      1 |       1 |     2 |
| CBMBBR\_REP\_4\_2009\_3         |      1 |      1 |       1 |     2 |
| CBMBMB\_REP\_1\_2008\_1         |      1 |      1 |       1 |     2 |
| CBMBMB\_REP\_2\_2008\_1         |      1 |      1 |       1 |     2 |
| CBMBMB\_REP\_3\_2008\_1         |      1 |      1 |       1 |     2 |
| CBMBMB\_REP\_4\_2008\_1         |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_1\_2006\_1         |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_1\_2006\_3         |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_1\_2009\_5         |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_1\_2011\_2011\_2   |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_1\_2014\_2014\_1   |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_1\_2017\_2017\_2   |      1 |      1 |       1 |     1 |
| CBMCMC\_REP\_2\_2006\_1         |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_2\_2006\_3         |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_2\_2009\_5         |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_2\_2011\_2011\_2   |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_2\_2014\_2014\_1   |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_2\_2017\_2017\_2   |      1 |      1 |       1 |     1 |
| CBMCMC\_REP\_3\_2006\_1         |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_3\_2006\_3         |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_3\_2009\_5         |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_3\_2011\_2011\_2   |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_3\_2014\_2014\_1   |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_3\_2017\_2017\_2   |      1 |      1 |       1 |     1 |
| CBMCMC\_REP\_4\_2009\_5         |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_4\_2011\_2011\_2   |      1 |      1 |       1 |     2 |
| CBMCMC\_REP\_4\_2017\_2017\_2   |      1 |      1 |       1 |     1 |
| CBPRMT\_REP\_1\_2008\_2         |      1 |      1 |       1 |     2 |
| CBPRMT\_REP\_2\_2008\_2         |      1 |      1 |       1 |     2 |
| CBPRMT\_REP\_3\_2008\_2         |      1 |      1 |       1 |     2 |
| CBPRMT\_REP\_4\_2008\_2         |      1 |      1 |       1 |     2 |
| CBQHQH\_REP\_1\_2009\_2         |      1 |      1 |       1 |     2 |
| CBQHQH\_REP\_2\_2009\_2         |      1 |      1 |       1 |     2 |
| CBQHQH\_REP\_3\_2009\_2         |      1 |      1 |       1 |     2 |
| CBQHQH\_REP\_4\_2009\_2         |      1 |      1 |       1 |     2 |
| CBRYMT\_REP\_1\_2003\_1         |      1 |      1 |       1 |     1 |
| CBRYMT\_REP\_1\_2003\_3         |      1 |      1 |       1 |     1 |
| CBRYMT\_REP\_2\_2003\_1         |      1 |      1 |       1 |     1 |
| CBRYMT\_REP\_2\_2003\_3         |      1 |      1 |       1 |     1 |
| CBRYMT\_REP\_3\_2003\_1         |      1 |      1 |       1 |     1 |
| CBRYMT\_REP\_3\_2003\_3         |      1 |      1 |       1 |     1 |
| CBRYMT\_REP\_4\_2003\_1         |      1 |      1 |       1 |     1 |
| CBSPSP\_REP\_1\_2007\_3         |      1 |      1 |       1 |     2 |
| CBSPSP\_REP\_1\_2010\_2010\_1   |      1 |      1 |       1 |     1 |
| CBSPSP\_REP\_1\_2012\_2012\_1   |      1 |      1 |       1 |     2 |
| CBSPSP\_REP\_1\_2015\_2015\_1   |      1 |      1 |       1 |     2 |
| CBSPSP\_REP\_2\_2007\_3         |      1 |      1 |       1 |     2 |
| CBSPSP\_REP\_2\_2010\_2010\_1   |      1 |      1 |       1 |     1 |
| CBSPSP\_REP\_2\_2012\_2012\_1   |      1 |      1 |       1 |     2 |
| CBSPSP\_REP\_2\_2015\_2015\_1   |      1 |      1 |       1 |     2 |
| CBSPSP\_REP\_3\_2007\_3         |      1 |      1 |       1 |     2 |
| CBSPSP\_REP\_3\_2010\_2010\_1   |      1 |      1 |       1 |     1 |
| CBSPSP\_REP\_3\_2012\_2012\_1   |      1 |      1 |       1 |     2 |
| CBSPSP\_REP\_3\_2015\_2015\_1   |      1 |      1 |       1 |     2 |
| CBSPSP\_REP\_4\_2007\_3         |      1 |      1 |       1 |     2 |
| CBSPSP\_REP\_4\_2010\_2010\_1   |      1 |      1 |       1 |     1 |
| CBSPSP\_REP\_4\_2012\_2012\_1   |      1 |      1 |       1 |     2 |
| CBSPSP\_REP\_4\_2015\_2015\_1   |      1 |      1 |       1 |     2 |
| MEPH\_1N101603\_2003\_2         |      1 |      1 |       1 |     1 |
| MEPH\_1N101603\_DIOXIN\_2003\_2 |      1 |      1 |       1 |     1 |
| MEPH\_2N101603\_2003\_2         |      1 |      1 |       1 |     1 |
| MEPH\_2N101603\_DIOXIN\_2003\_2 |      1 |      1 |       1 |     1 |
| MEPH\_3N101603\_2003\_2         |      1 |      1 |       1 |     1 |
| MEPH\_3N101603\_DIOXIN\_2003\_2 |      1 |      1 |       1 |     1 |
| MEPH\_4N101603\_2003\_2         |      1 |      1 |       1 |     1 |
| MEPH\_4N101603\_DIOXIN\_2003\_2 |      1 |      1 |       1 |     1 |

So, that works – Each code has only one site, date and sample\_ID. Some
of those codes are annoyingly long, because they are constructed from
long subcomponents, but they are unique….

# Do We Get Replicate or Duplicate Parameter Values?

Now that we have designations fdor single sampling events ( locations /
dates / replicates) we can ask whether we get either duplicate data
entries or replicates. \#\# Check for Duplicates

``` r
tmp <- SWAT_simplified %>%
  select(c(`SITE SEQ`, SAMPLE_DATE, SiteCode, CURRENT_SAMPLE_POINT_NAME,
           code, PARAMETER, `WEIGHT BASIS`, CONCENTRATION)) %>%
  group_by(`SITE SEQ`, SAMPLE_DATE, code, PARAMETER, `WEIGHT BASIS`) %>%
  summarize(nvals     = sum(!is.na(CONCENTRATION)),
            .groups = 'drop') %>%
  filter(nvals > 1)
  nrow(tmp[,1])
```

    ## [1] 11619

Wow\! We have duplicate values for ALMOST ALL combinations of samples
and parameters. That was a surprise.

``` r
head(tmp)
```

    ## # A tibble: 6 x 6
    ##   `SITE SEQ` SAMPLE_DATE         code             PARAMETER `WEIGHT BASIS` nvals
    ##        <dbl> <dttm>              <chr>            <chr>     <chr>          <int>
    ## 1      70672 2006-11-29 00:00:00 CBBBBB_REP_1_20~ 2,4'-DDD  WET                2
    ## 2      70672 2006-11-29 00:00:00 CBBBBB_REP_1_20~ 2,4'-DDE  WET                2
    ## 3      70672 2006-11-29 00:00:00 CBBBBB_REP_1_20~ 2,4'-DDT  WET                2
    ## 4      70672 2006-11-29 00:00:00 CBBBBB_REP_1_20~ 4,4'-DDD  WET                2
    ## 5      70672 2006-11-29 00:00:00 CBBBBB_REP_1_20~ 4,4'-DDE  WET                2
    ## 6      70672 2006-11-29 00:00:00 CBBBBB_REP_1_20~ 4,4'-DDT  WET                2

### Why Does Duplication or Replication Occur?

### A Couple of Examples

Lets look at a few of the apparent duplicates and see if we can figure
out where the duplication occurs.

``` r
SWAT_simplified %>% 
  filter(code == 'CBBBBB_REP_1_2006_4', PARAMETER == "2,4'-DDD" |
           PARAMETER == "2,4'-DDE" | PARAMETER == "2,4'-DDT")
```

    ## # A tibble: 6 x 25
    ##   `SITE SEQ` SiteCode Site   Year SAMPLE_DATE         SAMPLE_ID code 
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
to “PACE ANALYTICAL”, but reported concentrations are identical, which
is unlikely for different laboratories.

## Identical Values Different Labs?

``` r
tmp <- SWAT_simplified %>% select(code, `SITE SEQ`, SAMPLE_DATE, PARAMETER,
                           `ANALYSIS LAB`, `ANALYSIS_LAB_SAMPLE_ID`, CONCENTRATION,
                           `WEIGHT BASIS`) %>%
  filter(`WEIGHT BASIS` == 'WET') %>%
  group_by(`SITE SEQ`, SAMPLE_DATE, code, `ANALYSIS LAB`, PARAMETER) %>%
  summarise(n = n(),
            m = mean(CONCENTRATION, na.rm=TRUE),
            d = max(CONCENTRATION) - min(CONCENTRATION),
            .groups = 'drop') %>%
  filter(n>1 & d==0)
tmp %>% nrow()
```

    ## [1] 4655

``` r
tmp %>% pull(n) %>% range()
```

    ## [1] 2 6

So, even separated by analysis lab, we see several thousand duplicates
values. To figure out why, we need to look at individual cases in the
original data.

``` r
tmp <- SWAT_data %>%
  filter(`WEIGHT BASIS` == 'WET') %>%
  group_by(`SITE SEQ`, SAMPLE_DATE, SAMPLE_ID, PARAMETER, `ANALYSIS LAB` ) %>%
  mutate(n = n(),
         m = mean(CONCENTRATION, na.rm=TRUE),
         d = max(CONCENTRATION) - min(CONCENTRATION)) %>%
  filter(n>1 & d==0)
tmp %>% nrow()
```

    ## [1] 10673

``` r
tmp %>%  pull(n) %>% range()
```

    ## [1] 2 6

``` r
head(tmp,6)
```

    ## # A tibble: 6 x 44
    ## # Groups:   SITE SEQ, SAMPLE_DATE, SAMPLE_ID, PARAMETER, ANALYSIS LAB [2]
    ##   `SITE SEQ` EGAD_SITE_NAME SITE_DESCRIPTION CURRENT_SAMPLE_~ `SAMPLE POINT T~
    ##        <dbl> <chr>          <chr>            <chr>            <chr>           
    ## 1      70678 MILL CREEK - ~ <NA>             REP 1            MARINE          
    ## 2      70678 MILL CREEK - ~ <NA>             REP 1            MARINE          
    ## 3      70678 MILL CREEK - ~ <NA>             REP 1            MARINE          
    ## 4      70678 MILL CREEK - ~ <NA>             REP 1            MARINE          
    ## 5      70678 MILL CREEK - ~ <NA>             REP 1            MARINE          
    ## 6      70678 MILL CREEK - ~ <NA>             REP 1            MARINE          
    ## # ... with 39 more variables: `ANALYSIS LAB` <chr>, `SAMPLE COLLECTION
    ## #   METHOD` <chr>, SAMPLE_ID <chr>, ANALYSIS_LAB_SAMPLE_ID <chr>, `QC
    ## #   TYPE` <chr>, `SAMPLE TYPE` <chr>, SAMPLED_BY <chr>, SAMPLE_DATE <dttm>,
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

Note that the first three are identical. The second three are NEARLY
identical, but have a different CAS number,a dn the chemical name differ
only by the absence of an apostrophe after the 6. By looking on PubChem,
that tiny difference in nomenclature implies the sixth chlorine is
attached to a different benzene ring.

But we still have three unexplained duplicates of each of those values.

Could we find a way to test that multiple values on idividual samples
match?

## Are ALL Replicates explained as Laboratory QA/QC Checks?

WE define a replicate as a different observation. While it is possible
for two observations to come up as identical, there is some risk here of
overlooking real Laboratory duplicates are coded by
`ANALYSIS_LAB_SAMPLE_ID`. We can search for entries where with all the
sample and lab analysis ID information included, we still get more than
one sample

``` r
SWAT_simplified %>% select(code, `SITE SEQ`, SAMPLE_DATE, PARAMETER,
                           `ANALYSIS LAB`, `ANALYSIS_LAB_SAMPLE_ID`, CONCENTRATION,
                           `WEIGHT BASIS`) %>%
  filter(`WEIGHT BASIS` == 'WET') %>%
  group_by(`SITE SEQ`, SAMPLE_DATE, code, PARAMETER, `ANALYSIS LAB`,`ANALYSIS_LAB_SAMPLE_ID`) %>%
  summarise(n = n(),
            m = mean(CONCENTRATION, na.rm=TRUE),
            d = max(CONCENTRATION) - min(CONCENTRATION),
            .groups = 'drop') %>%
  filter(n>1 & d>0)
```

    ## # A tibble: 8 x 9
    ##   `SITE SEQ` SAMPLE_DATE         code  PARAMETER `ANALYSIS LAB` ANALYSIS_LAB_SA~
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

So, here we have DO have entries with multiple samples even from single
labs and single lab sample IDs, but they are all WEIGHTS. We don’t need
to deal with WEIGHTS directly coming from different lab analyses - -
that is, they are laboratory duplicates.

``` r
SWAT_data %>% 
  filter(`SITE SEQ` == 70672, as.Date(SAMPLE_DATE) == as.Date('2006-11-29'),
        SAMPLE_ID == 'CBBBBB REP 1',
        PARAMETER == "WEIGHT")
```

    ## # A tibble: 3 x 41
    ##   `SITE SEQ` EGAD_SITE_NAME SITE_DESCRIPTION CURRENT_SAMPLE_~ `SAMPLE POINT T~
    ##        <dbl> <chr>          <chr>            <chr>            <chr>           
    ## 1      70672 BACK BAY - CB~ <NA>             REP 1            MARINE          
    ## 2      70672 BACK BAY - CB~ <NA>             REP 1            MARINE          
    ## 3      70672 BACK BAY - CB~ <NA>             REP 1            MARINE          
    ## # ... with 36 more variables: `ANALYSIS LAB` <chr>, `SAMPLE COLLECTION
    ## #   METHOD` <chr>, SAMPLE_ID <chr>, ANALYSIS_LAB_SAMPLE_ID <chr>, `QC
    ## #   TYPE` <chr>, `SAMPLE TYPE` <chr>, SAMPLED_BY <chr>, SAMPLE_DATE <dttm>,
    ## #   `SAMPLE LOCATION` <chr>, `TEST METHOD` <chr>, `TEST METHOD
    ## #   DESCRIPTION` <chr>, ANALYSIS_DATE <dttm>, PARAMETER <chr>,
    ## #   CONCENTRATION <dbl>, `UNITS VALUE` <chr>, `UNITS DESCRIPTION` <chr>, `LAB
    ## #   QUALIFIER` <chr>, `VALIDATION QUALIFIER` <chr>, `QUALIFIER
    ## #   DESCRIPTION` <chr>, RL <dbl>, MDL <dbl>, `WEIGHT BASIS` <chr>, `RESULT
    ## #   TYPE` <chr>, `PREP METHOD` <chr>, PARAMETER_QUALIFIER <chr>,
    ## #   DILUTION_FACTOR <chr>, `PARAMETER FILTERED` <chr>, `SAMPLE FILTER` <dbl>,
    ## #   DEPTH <chr>, `DEPTH UNITS` <chr>, `SAMPLE COMMENT` <chr>, `LAB
    ## #   COMMENT` <chr>, `VALIDATION COMMENT` <chr>, TREATMENT <chr>,
    ## #   METER_CALIBRATED <chr>, CAS_NO <chr>

Weight Duplicates (and probably LIPID and MOISTURE Duplicates) are for
samples headed to different methods. We do not need to pay attention to
them if I am working with data already converted to dry weight or lipid
weight basis.

``` r
SWAT_data %>% 
  filter(`SITE SEQ` == 70679, as.Date(SAMPLE_DATE) == as.Date('2003-11-04'),
        SAMPLE_ID == 'CBRYMT REP 1',
        PARAMETER == "2',3,4,4',5-PENTACHLOROBIPHENYL")
```

    ## # A tibble: 4 x 41
    ##   `SITE SEQ` EGAD_SITE_NAME SITE_DESCRIPTION CURRENT_SAMPLE_~ `SAMPLE POINT T~
    ##        <dbl> <chr>          <chr>            <chr>            <chr>           
    ## 1      70679 ROYAL RIVER M~ <NA>             REP 1            MARINE          
    ## 2      70679 ROYAL RIVER M~ <NA>             REP 1            MARINE          
    ## 3      70679 ROYAL RIVER M~ <NA>             REP 1            MARINE          
    ## 4      70679 ROYAL RIVER M~ <NA>             REP 1            MARINE          
    ## # ... with 36 more variables: `ANALYSIS LAB` <chr>, `SAMPLE COLLECTION
    ## #   METHOD` <chr>, SAMPLE_ID <chr>, ANALYSIS_LAB_SAMPLE_ID <chr>, `QC
    ## #   TYPE` <chr>, `SAMPLE TYPE` <chr>, SAMPLED_BY <chr>, SAMPLE_DATE <dttm>,
    ## #   `SAMPLE LOCATION` <chr>, `TEST METHOD` <chr>, `TEST METHOD
    ## #   DESCRIPTION` <chr>, ANALYSIS_DATE <dttm>, PARAMETER <chr>,
    ## #   CONCENTRATION <dbl>, `UNITS VALUE` <chr>, `UNITS DESCRIPTION` <chr>, `LAB
    ## #   QUALIFIER` <chr>, `VALIDATION QUALIFIER` <chr>, `QUALIFIER
    ## #   DESCRIPTION` <chr>, RL <dbl>, MDL <dbl>, `WEIGHT BASIS` <chr>, `RESULT
    ## #   TYPE` <chr>, `PREP METHOD` <chr>, PARAMETER_QUALIFIER <chr>,
    ## #   DILUTION_FACTOR <chr>, `PARAMETER FILTERED` <chr>, `SAMPLE FILTER` <dbl>,
    ## #   DEPTH <chr>, `DEPTH UNITS` <chr>, `SAMPLE COMMENT` <chr>, `LAB
    ## #   COMMENT` <chr>, `VALIDATION COMMENT` <chr>, TREATMENT <chr>,
    ## #   METER_CALIBRATED <chr>, CAS_NO <chr>

Here we see two IDENTICAL values from each of two different laboratory
sample IDs. Again, this feels unlikely.

# Checking values based on Wet WEight, Dry Weight, and Lipid Weight

## Check that (value of LIP \<= DRY \<= WET)

``` r
SWAT_simplified %>%
  select(code, PARAMETER, `WEIGHT BASIS`, CONCENTRATION) %>%
  group_by(PARAMETER) %>%
  summarize(nlip = sum(`WEIGHT BASIS` == 'LIP', na.rm= TRUE),
            ndry = sum(`WEIGHT BASIS` == 'DRY', na.rm= TRUE),
            nwet = sum(`WEIGHT BASIS` == 'WET', na.rm= TRUE),
            problem = ! (nlip<= ndry & ndry <= nwet),
            .groups = 'drop') %>%
    filter(problem)
```

    ## # A tibble: 3 x 5
    ##   PARAMETER                  nlip  ndry  nwet problem
    ##   <chr>                     <int> <int> <int> <lgl>  
    ## 1 LIPIDS                      113     0   183 TRUE   
    ## 2 MERCURY                      59   143   142 TRUE   
    ## 3 SOLIDS-TOTAL RESIDUE (TS)     0    95    44 TRUE

Two of those appear to be labeling problems, since the parameters apply
to whole samples (e.g., percentage of lipids). It’s not clear what is
going on with Mercury.

## Check that (value of LIP \<= DRY \<= WET)

``` r
SWAT_simplified %>%
  select(code, PARAMETER, `WEIGHT BASIS`, CONCENTRATION) %>%
  group_by(code, PARAMETER) %>%
  summarize(nlip = sum(`WEIGHT BASIS` == 'LIP', na.rm= TRUE),
            ndry = sum(`WEIGHT BASIS` == 'DRY', na.rm= TRUE),
            nwet = sum(`WEIGHT BASIS` == 'WET', na.rm= TRUE),
            vlip = mean(CONCENTRATION[`WEIGHT BASIS` == 'LIP'], na.rm= TRUE),
            vdry = mean(CONCENTRATION[`WEIGHT BASIS` == 'DRY'], na.rm= TRUE),
            vwet = mean(CONCENTRATION[`WEIGHT BASIS` == 'WET'], na.rm= TRUE),
            problem = ! (vlip >= vdry & vdry >= vwet),
            .groups = 'drop') %>%
      filter(problem)
```

    ## # A tibble: 145 x 9
    ##    code       PARAMETER             nlip  ndry  nwet   vlip   vdry  vwet problem
    ##    <chr>      <chr>                <int> <int> <int>  <dbl>  <dbl> <dbl> <lgl>  
    ##  1 CBEEEE_RE~ 2',3,4,4',5-PENTACH~     2     2     2  61.1   88.2   8.55 TRUE   
    ##  2 CBEEEE_RE~ 2-CHLOROBIPHENYL         2     2     2   2.5    3.61  0.35 TRUE   
    ##  3 CBEEEE_RE~ 2,2'-DICHLOROBIPHEN~     2     2     2   8.14  11.7   1.14 TRUE   
    ##  4 CBEEEE_RE~ 2,2',3-TRICHLOROBIP~     2     2     2  26.4   38.1   3.7  TRUE   
    ##  5 CBEEEE_RE~ 2,2',3,3',4-PENTACH~     2     2     2 319    460    44.7  TRUE   
    ##  6 CBEEEE_RE~ 2,2',3,3',4,4',5-HE~     2     2     2 356    514    49.9  TRUE   
    ##  7 CBEEEE_RE~ 2,2',3,3',4,4',5,5'~     2     2     2 111    160    15.5  TRUE   
    ##  8 CBEEEE_RE~ 2,2',3,3',4,4',5,5'~     2     2     2  22.7   32.8   3.18 TRUE   
    ##  9 CBEEEE_RE~ 2,2',3,3',4,4',5,6'~     2     2     2  17.1   24.7   2.39 TRUE   
    ## 10 CBEEEE_RE~ 2,2',3,3',4,4',5,6-~     2     2     2  14.1   20.4   1.98 TRUE   
    ## # ... with 135 more rows

The last six problems are all Moisture values

These are all PCB values, all from one site, and all with an apparent
TWO sample values for each parameters.all PCBs wherethe apparent sample
size is two.

# PCB Nomenclature

Many of the PCBs chemical names and the PCB numerical designations turn
up in the same number of composite samples, which suggests they are
reported on the same samples . I wonder if these represent duplicate
data.

## Examining CAS numbers

Lets see how many times we have duplicate values with the same CAS
number.

``` r
SWAT_data %>% 
 # filter(`WEIGHT BASIS` == 'LIP') %>%
  group_by(CAS_NO) %>%
  summarize(nnames = length(unique(PARAMETER)),
            name = first (PARAMETER),
            .groups = 'drop') %>%
  arrange(name)
```

    ## # A tibble: 399 x 3
    ##    CAS_NO   nnames name                                        
    ##    <chr>     <int> <chr>                                       
    ##  1 90120         1 1-METHYL NAPHTHALENE                        
    ##  2 3351288       1 1-METHYLCHRYSENE                            
    ##  3 832699        1 1-METHYLPHENANTHRENE                        
    ##  4 573988        1 1,2-DIMETHYLNAPHTHALENE                     
    ##  5 35822469      1 1,2,3,4,6,7,8-HPCDD                         
    ##  6 67562394      1 1,2,3,4,6,7,8-HPCDF                         
    ##  7 39227286      1 1,2,3,4,7,8-HXCDD                           
    ##  8 70648269      1 1,2,3,4,7,8-HXCDF                           
    ##  9 55673897      1 1,2,3,4,7,8,9-HEPTACHLORODIBENZOFURAN(HPCDF)
    ## 10 57653857      1 1,2,3,6,7,8-HXCDD                           
    ## # ... with 389 more rows

That suggests we have no duplicates.

Looking closely, all the PCBs listed by PCB number are in fact mixtures
of PCBs. So technically, they are NOT individual PCBs, and should not
overlap with PCBs by CAS NUmber. The question is, how are they used? And
especially, how are the total PCBs calculated?

Lets pull one sample and look at that.

## When Are the PCB numerical codes used?

# How to Group Parameters

One of the challenges is that we want to report totals of various groups
of parameters, but the data contains no flag for major types of
parameters. We probably need to make a lookup table to be consistent.

The closest thing to an indicator of the parameters in use is the `TEST
METHOD` field, which provides a method code. It is possible that we
could generate a pretty good starting point for a lookup code based on
that, or it may be quicker to produce one by hand.

``` r
methods = SWAT_data %>% select(`TEST METHOD`) %>% arrange(`TEST METHOD`) %>% unique()
kable(methods)
```

| TEST METHOD        |
| :----------------- |
| CALCULATED         |
| CALCULATED WHO2005 |
| E160.3             |
| E1613              |
| E1613B             |
| E1631              |
| E1631E             |
| E1638              |
| E1638M/E200.8M     |
| E1668A             |
| E245.6M            |
| E270.3M            |
| FIELD              |
| MLA-007-E1         |
| MLA-007-E2         |
| MLA-035            |
| MLA-043            |
| NOAA1998M          |
| SM2540G            |
| SM2540GM           |
| SOP\_MSLC003       |
| SW6010BM/E200.7M   |
| SW6020             |
| SW6020A            |
| SW7471A            |
| SW7473M            |
| SW8081             |
| SW8081A            |
| SW8270C\_SIM       |
| SW8270CM           |

## Export a Prelimnary list of Parameters

We anticipate amending the list with parameter lists, largely by hand in
Excel

``` r
parm_list <- SWAT_data %>% select(PARAMETER) %>% arrange(PARAMETER) %>% unique()
kable(parm_list)
```

| PARAMETER                                    |
| :------------------------------------------- |
| 1-METHYL NAPHTHALENE                         |
| 1-METHYLCHRYSENE                             |
| 1-METHYLPHENANTHRENE                         |
| 1,2-DIMETHYLNAPHTHALENE                      |
| 1,2,3,4,6,7,8-HPCDD                          |
| 1,2,3,4,6,7,8-HPCDF                          |
| 1,2,3,4,7,8-HXCDD                            |
| 1,2,3,4,7,8-HXCDF                            |
| 1,2,3,4,7,8,9-HEPTACHLORODIBENZOFURAN(HPCDF) |
| 1,2,3,6,7,8-HXCDD                            |
| 1,2,3,6,7,8-HXCDF                            |
| 1,2,3,7,8-PECDD                              |
| 1,2,3,7,8-PECDF                              |
| 1,2,3,7,8,9-HXCDD                            |
| 1,2,3,7,8,9-HXCDF                            |
| 1,2,6-TRIMETHYLPHENANTHRENE                  |
| 1,4,6,7-TETRAMETHYLNAPHTHALENE               |
| 1,7-DIMETHYLFLUORENE                         |
| 1,7-DIMETHYLPHENANTHRENE                     |
| 1,8-DIMETHYLPHENANTHRENE                     |
| 2’,3,4,4’,5-PENTACHLOROBIPHENYL              |
| 2-CHLOROBIPHENYL                             |
| 2-METHYLANTHRACENE                           |
| 2-METHYLFLUORENE                             |
| 2-METHYLNAPHTHALENE                          |
| 2-METHYLPHENANTHRENE                         |
| 2,2’-DICHLOROBIPHENYL                        |
| 2,2’,3-TRICHLOROBIPHENYL                     |
| 2,2’,3,3’,4-PENTACHLOROBIPHENYL              |
| 2,2’,3,3’,4,4’,5-HEPTACHLOROBIPHENYL         |
| 2,2’,3,3’,4,4’,5,5’-OCTACHLOROBIPHENYL       |
| 2,2’,3,3’,4,4’,5,5’,6-NONACHLOROBIPHENYL     |
| 2,2’,3,3’,4,4’,5,6’-OCTACHLOROBIPHENYL       |
| 2,2’,3,3’,4,4’,5,6-OCTACHLOROBIPHENYL        |
| 2,2’,3,3’,4,4’,5,6,6’-NONACHLOROBIPHENYL     |
| 2,2’,3,3’,4,5’-HEXACHLOROBIPHENYL            |
| 2,2’,3,3’,4,5’,6’-HEPTACHLOROBIPHENYL        |
| 2,2’,3,3’,4,5’,6-HEPTACHLOROBIPHENYL         |
| 2,2’,3,3’,4,5’,6,6’-OCTACHLOROBIPHENYL       |
| 2,2’,3,3’,4,5,5’-HEPTACHLOROBIPHENYL         |
| 2,2’,3,3’,4,5,5’,6,6’-NONACHLOROBIPHENYL     |
| 2,2’,3,3’,4,5,6’-HEPTACHLOROBIPHENYL         |
| 2,2’,3,3’,4,6’-HEXACHLOROBIPHENYL            |
| 2,2’,3,3’,4,6-HEXACHLOROBIPHENYL             |
| 2,2’,3,3’,4,6,6’-HEPTACHLOROBIPHENYL         |
| 2,2’,3,3’,5-PENTACHLOROBIPHENYL              |
| 2,2’,3,3’,5,5’-HEXACHLOROBIPHENYL            |
| 2,2’,3,3’,5,5’,6-HEPTACHLOROBIPHENYL         |
| 2,2’,3,3’,5,5’,6,6’-OCTACHLOROBIPHENYL       |
| 2,2’,3,3’,5,6,6’-HEPTACHLOROBIPHENYL         |
| 2,2’,3,3’,6-PENTACHLOROBIPHENYL              |
| 2,2’,3,3’,6,6’-HEXACHLOROBIPHENYL            |
| 2,2’,3,4’-TETRACHLOROBIPHENYL                |
| 2,2’,3,4’,5,5’-HEXACHLOROBIPHENYL            |
| 2,2’,3,4’,5,5’,6-HEPTACHLOROBIPHENYL         |
| 2,2’,3,4’,5,6’-HEXACHLOROBIPHENYL            |
| 2,2’,3,4’,5,6,6’-HEPTACHLOROBIPHENYL         |
| 2,2’,3,4’,6,6’-HEXACHLOROBIPHENYL            |
| 2,2’,3,4,4’,5-HEXACHLOROBIPHENYL             |
| 2,2’,3,4,4’,5,5’-HEPTACHLOROBIPHENYL         |
| 2,2’,3,4,4’,5,5’,6-OCTACHLOROBIPHENYL        |
| 2,2’,3,4,4’,5,6’-HEPTACHLOROBIPHENYL         |
| 2,2’,3,4,4’,5,6-HEPTACHLOROBIPHENYL          |
| 2,2’,3,4,4’,5,6,6’-OCTACHLOROBIPHENYL        |
| 2,2’,3,4,4’,6,6’-HEPTACHLOROBIPHENYL         |
| 2,2’,3,4,5’,6-HEXACHLOROBIPHENYL             |
| 2,2’,3,4,5,5’-HEXACHLOROBIPHENYL             |
| 2,2’,3,4,5,6-HEXACHLOROBIPHENYL              |
| 2,2’,3,4,5,6,6’-HEPTACHLOROBIPHENYL          |
| 2,2’,3,4,6’-PENTACHLOROBIPHENYL              |
| 2,2’,3,4,6,6’-HEXACHLOROBIPHENYL             |
| 2,2’,3,5’,6-PENTACHLOROBIPHENYL              |
| 2,2’,3,5-TETRACHLOROBIPHENYL                 |
| 2,2’,3,5,5’-PENTACHLOROBIPHENYL              |
| 2,2’,3,5,6’-PENTACHLOROBIPHENYL              |
| 2,2’,3,5,6,6’-HEXACHLOROBIPHENYL             |
| 2,2’,3,6’-TETRACHLOROBIPHENYL                |
| 2,2’,3,6,6’-PENTACHLOROBIPHENYL              |
| 2,2’,4-TRICHLOROBIPHENYL                     |
| 2,2’,4,4’,5-PENTACHLOROBIPHENYL              |
| 2,2’,4,4’,5,6’-HEXACHLOROBIPHENYL            |
| 2,2’,4,4’,6,6’-HEXACHLOROBIPHENYL            |
| 2,2’,4,5’,6-PENTACHLOROBIPHENYL              |
| 2,2’,4,5-TETRACHLOROBIPHENYL                 |
| 2,2’,4,6,6’-PENTACHLOROBIPHENYL              |
| 2,2’,5,5’-TETRACHLOROBIPHENYL                |
| 2,2’,6-TRICHLOROBIPHENYL                     |
| 2,2’,6,6’-TETRACHLOROBIPHENYL                |
| 2,3’-DICHLOROBIPHENYL                        |
| 2,3’,4-TRICHLOROBIPHENYL                     |
| 2,3’,4,4’-TETRACHLOROBIPHENYL                |
| 2,3’,4,4’,5-PENTACHLOROBIPHENYL              |
| 2,3’,4,4’,5,5’-HEXACHLOROBIPHENYL            |
| 2,3’,4,5’-TETRACHLOROBIPHENYL                |
| 2,3’,4,5’,6-PENTACHLOROBIPHENYL              |
| 2,3’,4,5-TETRACHLOROBIPHENYL                 |
| 2,3’,4,5,5’-PENTACHLOROBIPHENYL              |
| 2,3’,5’-TRICHLOROBIPHENYL                    |
| 2,3’,5’,6-TETRACHLOROBIPHENYL                |
| 2,3’,5,5’-TETRACHLOROBIPHENYL                |
| 2,3’,6-TRICHLOROBIPHENYL                     |
| 2,3-DICHLOROBIPHENYL                         |
| 2,3,3’,4’-TETRACHLOROBIPHENYL                |
| 2,3,3’,4’,5’-PENTACHLOROBIPHENYL             |
| 2,3,3’,4’,5’,6-HEXACHLOROBIPHENYL            |
| 2,3,3’,4’,5,5’-HEXACHLOROBIPHENYL            |
| 2,3,3’,4-TETRACHLOROBIPHENYL                 |
| 2,3,3’,4,4’-PENTACHLOROBIPHENYL              |
| 2,3,3’,4,4’,5’-HEXACHLOROBIPHENYL            |
| 2,3,3’,4,4’,5’,6-HEPTACHLOROBIPHENYL         |
| 2,3,3’,4,4’,5-HEXACHLOROBIPHENYL             |
| 2,3,3’,4,4’,5,5’-HEPTACHLOROBIPHENYL         |
| 2,3,3’,4,4’,5,5’,6-OCTACHLOROBIPHENYL        |
| 2,3,3’,4,4’,5,6-HEPTACHLOROBIPHENYL          |
| 2,3,3’,4,4’,6-HEXACHLOROBIPHENYL             |
| 2,3,3’,4,5’,6-HEXACHLOROBIPHENYL             |
| 2,3,3’,4,5-PENTACHLOROBIPHENYL               |
| 2,3,3’,4,5,5’-HEXACHLOROBIPHENYL             |
| 2,3,3’,4,5,5’,6-HEPTACHLOROBIPHENYL          |
| 2,3,3’,4,5,6-HEXACHLOROBIPHENYL              |
| 2,3,3’,4,6-PENTACHLOROBIPHENYL               |
| 2,3,3’,5’-TETRACHLOROBIPHENYL                |
| 2,3,3’,5-TETRACHLOROBIPHENYL                 |
| 2,3,3’,5,5’-PENTACHLOROBIPHENYL              |
| 2,3,3’,5,5’,6-HEXACHLOROBIPHENYL             |
| 2,3,3’,5,6-PENTACHLOROBIPHENYL               |
| 2,3,4’-TRICHLOROBIPHENYL                     |
| 2,3,4’,5-TETRACHLOROBIPHENYL                 |
| 2,3,4’,6-TETRACHLOROBIPHENYL                 |
| 2,3,4,4’-TETRACHLOROBIPHENYL                 |
| 2,3,4,4’,5-PENTACHLOROBIPHENYL               |
| 2,3,4,6,7,8-HXCDF                            |
| 2,3,4,7,8-PECDF                              |
| 2,3,5-TRICHLOROBIPHENYL                      |
| 2,3,5-TRIMETHYLNAPHTHALENE                   |
| 2,3,6-TRICHLOROBIPHENYL                      |
| 2,3,6-TRIMETHYLNAPHTHALENE                   |
| 2,3,7,8-TCDD                                 |
| 2,3,7,8-TETRACHLORODIBENZOFURAN              |
| 2,4’-DDD                                     |
| 2,4’-DDE                                     |
| 2,4’-DDT                                     |
| 2,4’-DICHLOROBIPHENYL                        |
| 2,4’,5-TRICHLOROBIPHENYL                     |
| 2,4’,6-TRICHLOROBIPHENYL                     |
| 2,4-DICHLOROBIPHENYL                         |
| 2,4-DIMETHYLDIBENZOTHIOPHENE                 |
| 2,5-DICHLOROBIPHENYL                         |
| 2,6-DICHLOROBIPHENYL                         |
| 2,6-DIMETHYLNAPHTHALENE                      |
| 2,6-DIMETHYLPHENANTHRENE                     |
| 2/3-METHYLDIBENZOTHIOPHENES                  |
| 3-CHLOROBIPHENYL                             |
| 3-METHYLFLUORANTHENE/BENZO\[A\]FLUORENE      |
| 3-METHYLPHENANTHRENE                         |
| 3,3’-DICHLOROBIPHENYL                        |
| 3,3’,4-TRICHLOROBIPHENYL                     |
| 3,3’,4,4’-TETRACHLOROBIPHENYL                |
| 3,3’,4,4’,5-PENTACHLOROBIPHENYL              |
| 3,3’,4,4’,5,5’-HEXACHLOROBIPHENYL            |
| 3,3’,4,5’-TETRACHLOROBIPHENYL                |
| 3,3’,4,5-TETRACHLOROBIPHENYL                 |
| 3,3’,4,5,5’-PENTACHLOROBIPHENYL              |
| 3,3’,5-TRICHLOROBIPHENYL                     |
| 3,3’,5,5’-TETRACHLOROBIPHENYL                |
| 3,4’,5-TRICHLOROBIPHENYL                     |
| 3,4,4’-TRICHLOROBIPHENYL                     |
| 3,4,4’,5-TETRACHLOROBIPHENYL                 |
| 3,4,5-TRICHLOROBIPHENYL                      |
| 3,5-DICHLOROBIPHENYL                         |
| 3,6-DIMETHYLPHENANTHRENE                     |
| 4-CHLOROBIPHENYL                             |
| 4,4’-DDD                                     |
| 4,4’-DDE                                     |
| 4,4’-DDT                                     |
| 4,4’-DICHLOROBIPHENYL                        |
| 5,9-DIMETHYLCHRYSENE                         |
| 5/6-METHYLCHRYSENE                           |
| 7-METHYLBENZO\[A\]PYRENE                     |
| 9/4-METHYLPHENANTHRENE                       |
| ACENAPHTHENE                                 |
| ACENAPHTHYLENE                               |
| ALACHLOR                                     |
| ALDRIN                                       |
| ALPHA-BHC                                    |
| ALPHA-CHLORDANE                              |
| ALUMINUM                                     |
| AMETRYN                                      |
| ANTHRACENE                                   |
| ARSENIC                                      |
| ATRAZINE                                     |
| AZINPHOS-METHYL                              |
| BENZO(A)ANTHRACENE                           |
| BENZO(A)PYRENE                               |
| BENZO(B)FLUORANTHENE                         |
| BENZO(E)PYRENE                               |
| BENZO(G,H,I)PERYLENE                         |
| BENZO(K)FLUORANTHENE                         |
| BENZO\[B,J,K\]FLUORANTHENES                  |
| BENZO\[J,K\]FLUORANTHENES                    |
| BENZOFLUORANTHENE                            |
| BETA-BHC                                     |
| BIPHENYL                                     |
| BUTRALIN                                     |
| BUTYLATE                                     |
| C1-ACENAPHTHENES                             |
| C1-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| C1-BENZOFLUORANTHENES/BENZOPYRENES           |
| C1-BIPHENYLS                                 |
| C1-DIBENZOTHIOPHENES                         |
| C1-FLUORANTHENES/PYRENES                     |
| C1-FLUORENES                                 |
| C1-NAPHTHALENES                              |
| C1-PHENANTHRENES/ANTHRACENES                 |
| C2-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| C2-BENZOFLUORANTHENES/BENZOPYRENES           |
| C2-BIPHENYLS                                 |
| C2-DIBENZOTHIOPHENES                         |
| C2-FLUORANTHENES/PYRENES                     |
| C2-FLUORENES                                 |
| C2-NAPHTHALENES                              |
| C2-PHENANTHRENES/ANTHRACENES                 |
| C3-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| C3-DIBENZOTHIOPHENES                         |
| C3-FLUORANTHENES/PYRENES                     |
| C3-FLUORENES                                 |
| C3-NAPHTHALENES                              |
| C3-PHENANTHRENES/ANTHRACENES                 |
| C4-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| C4-DIBENZOTHIOPHENES                         |
| C4-FLUORANTHENES/PYRENES                     |
| C4-NAPHTHALENES                              |
| C4-PHENANTHRENES/ANTHRACENES                 |
| CADMIUM                                      |
| CAPTAN                                       |
| CHLOROTHALONIL                               |
| CHLORPYRIFOS, O,O-DIMETHYL ANALOG            |
| CHLORPYRIPHOS                                |
| CHLORPYRIPHOS-OXON                           |
| CHROMIUM                                     |
| CHRYSENE                                     |
| CIS-NONACHLOR                                |
| COPPER                                       |
| CYANAZINE                                    |
| CYPERMETHRIN                                 |
| DCPA                                         |
| DECACHLOROBIPHENYL                           |
| DELTA-BHC                                    |
| DESETHYLATRAZINE                             |
| DIAZINON                                     |
| DIAZINON-OXON                                |
| DIBENZO(A,H)ANTHRACENE                       |
| DIBENZOTHIOPHENE                             |
| DIELDRIN                                     |
| DIMETHENAMID                                 |
| DIMETHOATE                                   |
| DISULFOTON                                   |
| DISULFOTON SULFONE                           |
| DX TOTAL TEQ (ND=0)                          |
| DX TOTAL TEQ (ND=1/2 DL)                     |
| DX TOTAL TEQ (ND=DL)                         |
| ENDOSULFAN I                                 |
| ENDOSULFAN II                                |
| ENDOSULFAN SULFATE                           |
| ENDRIN                                       |
| ENDRIN ALDEHYDE                              |
| ENDRIN KETONE                                |
| ETHALFLURALIN                                |
| ETHION                                       |
| FENITROTHION                                 |
| FLUFENACET                                   |
| FLUORANTHENE                                 |
| FLUORENE                                     |
| FLUTRIAFOL                                   |
| FONOFOS                                      |
| GAMMA-BHC (LINDANE)                          |
| GAMMA-CHLORDANE                              |
| HEPTACHLOR                                   |
| HEPTACHLOR EPOXIDE                           |
| HEXACHLOROBENZENE                            |
| INDENO(1,2,3-CD)PYRENE                       |
| IRON                                         |
| LEAD                                         |
| LINURON                                      |
| LIPIDS                                       |
| MALATHION                                    |
| MERCURY                                      |
| METHAMIDOPHOS                                |
| METHOPRENE                                   |
| METHOXYCHLOR                                 |
| METHYL PARATHION                             |
| METOLACHLOR                                  |
| METRIBUZIN                                   |
| MIREX                                        |
| MOISTURE                                     |
| Mytilus edulis                               |
| NAPHTHALENE                                  |
| NICKEL                                       |
| OCDD                                         |
| OCDF                                         |
| OCTACHLOROSTYRENE                            |
| OXYCHLORDANE                                 |
| PARATHION                                    |
| PCB-012/013                                  |
| PCB-018/030                                  |
| PCB-020/028                                  |
| PCB-021/033                                  |
| PCB-026/029                                  |
| PCB-040/041/071                              |
| PCB-044/047/065                              |
| PCB-045/051                                  |
| PCB-049/069                                  |
| PCB-050/053                                  |
| PCB-059/062/075                              |
| PCB-061/070/074/076                          |
| PCB-083/099                                  |
| PCB-085/116/117                              |
| PCB-086/087/097/108/119/125                  |
| PCB-088/091                                  |
| PCB-090/101/113                              |
| PCB-093/095/098/100/102                      |
| PCB-093/098/100/102                          |
| PCB-107/124                                  |
| PCB-110/115                                  |
| PCB-128/166                                  |
| PCB-129/138/160/163                          |
| PCB-129/138/163                              |
| PCB-134/143                                  |
| PCB-135/151                                  |
| PCB-135/151/154                              |
| PCB-139/140                                  |
| PCB-147/149                                  |
| PCB-153/168                                  |
| PCB-156/157                                  |
| PCB-171/173                                  |
| PCB-180/193                                  |
| PCB-183/185                                  |
| PCB-197/200                                  |
| PCB-198/199                                  |
| PCB TOTAL TEQ (ND=0)                         |
| PCB TOTAL TEQ (ND=1/2 DL)                    |
| PCB TOTAL TEQ (ND=DL)                        |
| PCBS                                         |
| PENDIMETHALIN                                |
| PENTACHLORONITROBENZENE                      |
| PERFLUOROBUTANE SULFONATE                    |
| PERFLUOROBUTANOATE                           |
| PERFLUORODECANOATE                           |
| PERFLUORODODECANOATE                         |
| PERFLUOROHEPTANOATE                          |
| PERFLUOROHEXANE SULFONATE                    |
| PERFLUOROHEXANOATE                           |
| PERFLUORONONANOATE                           |
| PERFLUOROOCTANE SULFONAMIDE                  |
| PERFLUOROOCTANE SULFONATE                    |
| PERFLUOROOCTANOATE                           |
| PERFLUOROOCTANOIC ACID                       |
| PERFLUOROPENTANOATE                          |
| PERFLUOROUNDECANOATE                         |
| PERMETHRIN                                   |
| PERTHANE                                     |
| PERYLENE                                     |
| PHENANTHRENE                                 |
| PHORATE                                      |
| PHOSMET                                      |
| PIRIMIPHOS-METHYL                            |
| PYRENE                                       |
| RETENE                                       |
| SELENIUM                                     |
| SILVER                                       |
| SIMAZINE                                     |
| SOLIDS-TOTAL RESIDUE (TS)                    |
| TEBUCONAZOL                                  |
| TECNAZENE                                    |
| TERBUFOS                                     |
| TOTAL PAH-D                                  |
| TOTAL PAH-H                                  |
| TOTAL PAH-O                                  |
| TOTAL PAH19-D                                |
| TOTAL PAH19-H                                |
| TOTAL PAH19-O                                |
| TOTAL PAH24-D                                |
| TOTAL PAH24-H                                |
| TOTAL PAH24-O                                |
| TOTAL PAH40-D                                |
| TOTAL PAH40-H                                |
| TOTAL PAH40-O                                |
| TOTAL PCB-D                                  |
| TOTAL PCB-H                                  |
| TOTAL PCB-O                                  |
| TOTAL PESTICIDES21-D                         |
| TOTAL PESTICIDES21-H                         |
| TOTAL PESTICIDES21-O                         |
| TRANS-NONACHLOR                              |
| TRIALLATE                                    |
| TRIFLURALIN                                  |
| VELPAR                                       |
| WEIGHT                                       |
| ZINC                                         |

``` r
write_csv(parm_list, 'Parameter List.csv')
```

``` r
methods_data <- SWAT_data %>%
  select(PARAMETER, `TEST METHOD`) %>%
  group_by(`TEST METHOD`, PARAMETER) %>%
  summarize(Test      =  first(`TEST METHOD`),
                        .groups = 'drop') %>%
  rename(Parameter = PARAMETER)
kable(methods_data)
```

| TEST METHOD        | Parameter                                    | Test               |
| :----------------- | :------------------------------------------- | :----------------- |
| CALCULATED         | C1-ACENAPHTHENES                             | CALCULATED         |
| CALCULATED         | C1-BENZO\[A\]ANTHRACENES/CHRYSENES           | CALCULATED         |
| CALCULATED         | C1-BENZOFLUORANTHENES/BENZOPYRENES           | CALCULATED         |
| CALCULATED         | C1-BIPHENYLS                                 | CALCULATED         |
| CALCULATED         | C1-DIBENZOTHIOPHENES                         | CALCULATED         |
| CALCULATED         | C1-FLUORANTHENES/PYRENES                     | CALCULATED         |
| CALCULATED         | C1-FLUORENES                                 | CALCULATED         |
| CALCULATED         | C1-NAPHTHALENES                              | CALCULATED         |
| CALCULATED         | C1-PHENANTHRENES/ANTHRACENES                 | CALCULATED         |
| CALCULATED         | C2-BENZO\[A\]ANTHRACENES/CHRYSENES           | CALCULATED         |
| CALCULATED         | C2-BENZOFLUORANTHENES/BENZOPYRENES           | CALCULATED         |
| CALCULATED         | C2-BIPHENYLS                                 | CALCULATED         |
| CALCULATED         | C2-DIBENZOTHIOPHENES                         | CALCULATED         |
| CALCULATED         | C2-FLUORANTHENES/PYRENES                     | CALCULATED         |
| CALCULATED         | C2-FLUORENES                                 | CALCULATED         |
| CALCULATED         | C2-NAPHTHALENES                              | CALCULATED         |
| CALCULATED         | C2-PHENANTHRENES/ANTHRACENES                 | CALCULATED         |
| CALCULATED         | C3-BENZO\[A\]ANTHRACENES/CHRYSENES           | CALCULATED         |
| CALCULATED         | C3-DIBENZOTHIOPHENES                         | CALCULATED         |
| CALCULATED         | C3-FLUORANTHENES/PYRENES                     | CALCULATED         |
| CALCULATED         | C3-FLUORENES                                 | CALCULATED         |
| CALCULATED         | C3-NAPHTHALENES                              | CALCULATED         |
| CALCULATED         | C3-PHENANTHRENES/ANTHRACENES                 | CALCULATED         |
| CALCULATED         | C4-BENZO\[A\]ANTHRACENES/CHRYSENES           | CALCULATED         |
| CALCULATED         | C4-DIBENZOTHIOPHENES                         | CALCULATED         |
| CALCULATED         | C4-FLUORANTHENES/PYRENES                     | CALCULATED         |
| CALCULATED         | C4-NAPHTHALENES                              | CALCULATED         |
| CALCULATED         | C4-PHENANTHRENES/ANTHRACENES                 | CALCULATED         |
| CALCULATED         | PCB TOTAL TEQ (ND=0)                         | CALCULATED         |
| CALCULATED         | PCB TOTAL TEQ (ND=1/2 DL)                    | CALCULATED         |
| CALCULATED         | PCB TOTAL TEQ (ND=DL)                        | CALCULATED         |
| CALCULATED         | PCBS                                         | CALCULATED         |
| CALCULATED WHO2005 | C1-ACENAPHTHENES                             | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-BENZO\[A\]ANTHRACENES/CHRYSENES           | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-BENZOFLUORANTHENES/BENZOPYRENES           | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-BIPHENYLS                                 | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-DIBENZOTHIOPHENES                         | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-FLUORANTHENES/PYRENES                     | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-FLUORENES                                 | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-NAPHTHALENES                              | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-PHENANTHRENES/ANTHRACENES                 | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-BENZO\[A\]ANTHRACENES/CHRYSENES           | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-BENZOFLUORANTHENES/BENZOPYRENES           | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-BIPHENYLS                                 | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-DIBENZOTHIOPHENES                         | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-FLUORANTHENES/PYRENES                     | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-FLUORENES                                 | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-NAPHTHALENES                              | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-PHENANTHRENES/ANTHRACENES                 | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C3-BENZO\[A\]ANTHRACENES/CHRYSENES           | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C3-DIBENZOTHIOPHENES                         | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C3-FLUORANTHENES/PYRENES                     | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C3-FLUORENES                                 | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C3-NAPHTHALENES                              | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C3-PHENANTHRENES/ANTHRACENES                 | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C4-BENZO\[A\]ANTHRACENES/CHRYSENES           | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C4-DIBENZOTHIOPHENES                         | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C4-FLUORANTHENES/PYRENES                     | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C4-NAPHTHALENES                              | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C4-PHENANTHRENES/ANTHRACENES                 | CALCULATED WHO2005 |
| CALCULATED WHO2005 | DX TOTAL TEQ (ND=0)                          | CALCULATED WHO2005 |
| CALCULATED WHO2005 | DX TOTAL TEQ (ND=1/2 DL)                     | CALCULATED WHO2005 |
| CALCULATED WHO2005 | DX TOTAL TEQ (ND=DL)                         | CALCULATED WHO2005 |
| CALCULATED WHO2005 | PCB TOTAL TEQ (ND=0)                         | CALCULATED WHO2005 |
| CALCULATED WHO2005 | PCB TOTAL TEQ (ND=1/2 DL)                    | CALCULATED WHO2005 |
| CALCULATED WHO2005 | PCB TOTAL TEQ (ND=DL)                        | CALCULATED WHO2005 |
| CALCULATED WHO2005 | PCBS                                         | CALCULATED WHO2005 |
| E160.3             | SOLIDS-TOTAL RESIDUE (TS)                    | E160.3             |
| E1613              | 1,2,3,4,6,7,8-HPCDD                          | E1613              |
| E1613              | 1,2,3,4,6,7,8-HPCDF                          | E1613              |
| E1613              | 1,2,3,4,7,8-HXCDD                            | E1613              |
| E1613              | 1,2,3,4,7,8-HXCDF                            | E1613              |
| E1613              | 1,2,3,4,7,8,9-HEPTACHLORODIBENZOFURAN(HPCDF) | E1613              |
| E1613              | 1,2,3,6,7,8-HXCDD                            | E1613              |
| E1613              | 1,2,3,6,7,8-HXCDF                            | E1613              |
| E1613              | 1,2,3,7,8-PECDD                              | E1613              |
| E1613              | 1,2,3,7,8-PECDF                              | E1613              |
| E1613              | 1,2,3,7,8,9-HXCDD                            | E1613              |
| E1613              | 1,2,3,7,8,9-HXCDF                            | E1613              |
| E1613              | 2,3,4,6,7,8-HXCDF                            | E1613              |
| E1613              | 2,3,4,7,8-PECDF                              | E1613              |
| E1613              | 2,3,7,8-TCDD                                 | E1613              |
| E1613              | 2,3,7,8-TETRACHLORODIBENZOFURAN              | E1613              |
| E1613              | DX TOTAL TEQ (ND=1/2 DL)                     | E1613              |
| E1613              | DX TOTAL TEQ (ND=DL)                         | E1613              |
| E1613              | LIPIDS                                       | E1613              |
| E1613              | OCDD                                         | E1613              |
| E1613              | OCDF                                         | E1613              |
| E1613              | WEIGHT                                       | E1613              |
| E1613B             | 1,2,3,4,6,7,8-HPCDD                          | E1613B             |
| E1613B             | 1,2,3,4,6,7,8-HPCDF                          | E1613B             |
| E1613B             | 1,2,3,4,7,8-HXCDD                            | E1613B             |
| E1613B             | 1,2,3,4,7,8-HXCDF                            | E1613B             |
| E1613B             | 1,2,3,4,7,8,9-HEPTACHLORODIBENZOFURAN(HPCDF) | E1613B             |
| E1613B             | 1,2,3,6,7,8-HXCDD                            | E1613B             |
| E1613B             | 1,2,3,6,7,8-HXCDF                            | E1613B             |
| E1613B             | 1,2,3,7,8-PECDD                              | E1613B             |
| E1613B             | 1,2,3,7,8-PECDF                              | E1613B             |
| E1613B             | 1,2,3,7,8,9-HXCDD                            | E1613B             |
| E1613B             | 1,2,3,7,8,9-HXCDF                            | E1613B             |
| E1613B             | 2,3,4,6,7,8-HXCDF                            | E1613B             |
| E1613B             | 2,3,4,7,8-PECDF                              | E1613B             |
| E1613B             | 2,3,7,8-TCDD                                 | E1613B             |
| E1613B             | 2,3,7,8-TETRACHLORODIBENZOFURAN              | E1613B             |
| E1613B             | LIPIDS                                       | E1613B             |
| E1613B             | MOISTURE                                     | E1613B             |
| E1613B             | OCDD                                         | E1613B             |
| E1613B             | OCDF                                         | E1613B             |
| E1631              | MERCURY                                      | E1631              |
| E1631E             | MERCURY                                      | E1631E             |
| E1638              | ALUMINUM                                     | E1638              |
| E1638              | ARSENIC                                      | E1638              |
| E1638              | CADMIUM                                      | E1638              |
| E1638              | CHROMIUM                                     | E1638              |
| E1638              | COPPER                                       | E1638              |
| E1638              | IRON                                         | E1638              |
| E1638              | LEAD                                         | E1638              |
| E1638              | NICKEL                                       | E1638              |
| E1638              | SELENIUM                                     | E1638              |
| E1638              | SILVER                                       | E1638              |
| E1638              | ZINC                                         | E1638              |
| E1638M/E200.8M     | ARSENIC                                      | E1638M/E200.8M     |
| E1638M/E200.8M     | CADMIUM                                      | E1638M/E200.8M     |
| E1638M/E200.8M     | COPPER                                       | E1638M/E200.8M     |
| E1638M/E200.8M     | LEAD                                         | E1638M/E200.8M     |
| E1638M/E200.8M     | NICKEL                                       | E1638M/E200.8M     |
| E1638M/E200.8M     | SELENIUM                                     | E1638M/E200.8M     |
| E1638M/E200.8M     | SILVER                                       | E1638M/E200.8M     |
| E1668A             | 2’,3,4,4’,5-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2-CHLOROBIPHENYL                             | E1668A             |
| E1668A             | 2,2’-DICHLOROBIPHENYL                        | E1668A             |
| E1668A             | 2,2’,3-TRICHLOROBIPHENYL                     | E1668A             |
| E1668A             | 2,2’,3,3’,4-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,2’,3,3’,4,4’,5-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,2’,3,3’,4,4’,5,5’-OCTACHLOROBIPHENYL       | E1668A             |
| E1668A             | 2,2’,3,3’,4,4’,5,5’,6-NONACHLOROBIPHENYL     | E1668A             |
| E1668A             | 2,2’,3,3’,4,4’,5,6’-OCTACHLOROBIPHENYL       | E1668A             |
| E1668A             | 2,2’,3,3’,4,4’,5,6-OCTACHLOROBIPHENYL        | E1668A             |
| E1668A             | 2,2’,3,3’,4,4’,5,6,6’-NONACHLOROBIPHENYL     | E1668A             |
| E1668A             | 2,2’,3,3’,4,5’-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 2,2’,3,3’,4,5’,6’-HEPTACHLOROBIPHENYL        | E1668A             |
| E1668A             | 2,2’,3,3’,4,5’,6-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,2’,3,3’,4,5’,6,6’-OCTACHLOROBIPHENYL       | E1668A             |
| E1668A             | 2,2’,3,3’,4,5,5’-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,2’,3,3’,4,5,5’,6,6’-NONACHLOROBIPHENYL     | E1668A             |
| E1668A             | 2,2’,3,3’,4,5,6’-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,2’,3,3’,4,6’-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 2,2’,3,3’,4,6-HEXACHLOROBIPHENYL             | E1668A             |
| E1668A             | 2,2’,3,3’,4,6,6’-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,2’,3,3’,5-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,2’,3,3’,5,5’-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 2,2’,3,3’,5,5’,6-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,2’,3,3’,5,5’,6,6’-OCTACHLOROBIPHENYL       | E1668A             |
| E1668A             | 2,2’,3,3’,5,6,6’-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,2’,3,3’,6-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,2’,3,3’,6,6’-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 2,2’,3,4’-TETRACHLOROBIPHENYL                | E1668A             |
| E1668A             | 2,2’,3,4’,5,5’-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 2,2’,3,4’,5,5’,6-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,2’,3,4’,5,6’-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 2,2’,3,4’,5,6,6’-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,2’,3,4’,6,6’-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 2,2’,3,4,4’,5-HEXACHLOROBIPHENYL             | E1668A             |
| E1668A             | 2,2’,3,4,4’,5,5’-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,2’,3,4,4’,5,5’,6-OCTACHLOROBIPHENYL        | E1668A             |
| E1668A             | 2,2’,3,4,4’,5,6’-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,2’,3,4,4’,5,6-HEPTACHLOROBIPHENYL          | E1668A             |
| E1668A             | 2,2’,3,4,4’,5,6,6’-OCTACHLOROBIPHENYL        | E1668A             |
| E1668A             | 2,2’,3,4,4’,6,6’-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,2’,3,4,5’,6-HEXACHLOROBIPHENYL             | E1668A             |
| E1668A             | 2,2’,3,4,5,5’-HEXACHLOROBIPHENYL             | E1668A             |
| E1668A             | 2,2’,3,4,5,6-HEXACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,2’,3,4,5,6,6’-HEPTACHLOROBIPHENYL          | E1668A             |
| E1668A             | 2,2’,3,4,6’-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,2’,3,4,6,6’-HEXACHLOROBIPHENYL             | E1668A             |
| E1668A             | 2,2’,3,5’,6-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,2’,3,5-TETRACHLOROBIPHENYL                 | E1668A             |
| E1668A             | 2,2’,3,5,5’-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,2’,3,5,6’-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,2’,3,5,6,6’-HEXACHLOROBIPHENYL             | E1668A             |
| E1668A             | 2,2’,3,6’-TETRACHLOROBIPHENYL                | E1668A             |
| E1668A             | 2,2’,3,6,6’-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,2’,4-TRICHLOROBIPHENYL                     | E1668A             |
| E1668A             | 2,2’,4,4’,5-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,2’,4,4’,5,6’-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 2,2’,4,4’,6,6’-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 2,2’,4,5’,6-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,2’,4,5-TETRACHLOROBIPHENYL                 | E1668A             |
| E1668A             | 2,2’,4,6,6’-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,2’,5,5’-TETRACHLOROBIPHENYL                | E1668A             |
| E1668A             | 2,2’,6-TRICHLOROBIPHENYL                     | E1668A             |
| E1668A             | 2,2’,6,6’-TETRACHLOROBIPHENYL                | E1668A             |
| E1668A             | 2,3’-DICHLOROBIPHENYL                        | E1668A             |
| E1668A             | 2,3’,4-TRICHLOROBIPHENYL                     | E1668A             |
| E1668A             | 2,3’,4,4’-TETRACHLOROBIPHENYL                | E1668A             |
| E1668A             | 2,3’,4,4’,5-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,3’,4,4’,5,5’-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 2,3’,4,5’-TETRACHLOROBIPHENYL                | E1668A             |
| E1668A             | 2,3’,4,5’,6-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,3’,4,5-TETRACHLOROBIPHENYL                 | E1668A             |
| E1668A             | 2,3’,4,5,5’-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,3’,5’-TRICHLOROBIPHENYL                    | E1668A             |
| E1668A             | 2,3’,5’,6-TETRACHLOROBIPHENYL                | E1668A             |
| E1668A             | 2,3’,5,5’-TETRACHLOROBIPHENYL                | E1668A             |
| E1668A             | 2,3’,6-TRICHLOROBIPHENYL                     | E1668A             |
| E1668A             | 2,3-DICHLOROBIPHENYL                         | E1668A             |
| E1668A             | 2,3,3’,4’-TETRACHLOROBIPHENYL                | E1668A             |
| E1668A             | 2,3,3’,4’,5’-PENTACHLOROBIPHENYL             | E1668A             |
| E1668A             | 2,3,3’,4’,5’,6-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 2,3,3’,4’,5,5’-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 2,3,3’,4-TETRACHLOROBIPHENYL                 | E1668A             |
| E1668A             | 2,3,3’,4,4’-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,3,3’,4,4’,5’-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 2,3,3’,4,4’,5’,6-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,3,3’,4,4’,5-HEXACHLOROBIPHENYL             | E1668A             |
| E1668A             | 2,3,3’,4,4’,5,5’-HEPTACHLOROBIPHENYL         | E1668A             |
| E1668A             | 2,3,3’,4,4’,5,5’,6-OCTACHLOROBIPHENYL        | E1668A             |
| E1668A             | 2,3,3’,4,4’,5,6-HEPTACHLOROBIPHENYL          | E1668A             |
| E1668A             | 2,3,3’,4,4’,6-HEXACHLOROBIPHENYL             | E1668A             |
| E1668A             | 2,3,3’,4,5’,6-HEXACHLOROBIPHENYL             | E1668A             |
| E1668A             | 2,3,3’,4,5-PENTACHLOROBIPHENYL               | E1668A             |
| E1668A             | 2,3,3’,4,5,5’-HEXACHLOROBIPHENYL             | E1668A             |
| E1668A             | 2,3,3’,4,5,5’,6-HEPTACHLOROBIPHENYL          | E1668A             |
| E1668A             | 2,3,3’,4,5,6-HEXACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,3,3’,4,6-PENTACHLOROBIPHENYL               | E1668A             |
| E1668A             | 2,3,3’,5’-TETRACHLOROBIPHENYL                | E1668A             |
| E1668A             | 2,3,3’,5-TETRACHLOROBIPHENYL                 | E1668A             |
| E1668A             | 2,3,3’,5,5’-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 2,3,3’,5,5’,6-HEXACHLOROBIPHENYL             | E1668A             |
| E1668A             | 2,3,3’,5,6-PENTACHLOROBIPHENYL               | E1668A             |
| E1668A             | 2,3,4’-TRICHLOROBIPHENYL                     | E1668A             |
| E1668A             | 2,3,4’,5-TETRACHLOROBIPHENYL                 | E1668A             |
| E1668A             | 2,3,4’,6-TETRACHLOROBIPHENYL                 | E1668A             |
| E1668A             | 2,3,4,4’-TETRACHLOROBIPHENYL                 | E1668A             |
| E1668A             | 2,3,4,4’,5-PENTACHLOROBIPHENYL               | E1668A             |
| E1668A             | 2,3,5-TRICHLOROBIPHENYL                      | E1668A             |
| E1668A             | 2,3,6-TRICHLOROBIPHENYL                      | E1668A             |
| E1668A             | 2,4’-DICHLOROBIPHENYL                        | E1668A             |
| E1668A             | 2,4’,5-TRICHLOROBIPHENYL                     | E1668A             |
| E1668A             | 2,4’,6-TRICHLOROBIPHENYL                     | E1668A             |
| E1668A             | 2,4-DICHLOROBIPHENYL                         | E1668A             |
| E1668A             | 2,5-DICHLOROBIPHENYL                         | E1668A             |
| E1668A             | 2,6-DICHLOROBIPHENYL                         | E1668A             |
| E1668A             | 3-CHLOROBIPHENYL                             | E1668A             |
| E1668A             | 3,3’-DICHLOROBIPHENYL                        | E1668A             |
| E1668A             | 3,3’,4-TRICHLOROBIPHENYL                     | E1668A             |
| E1668A             | 3,3’,4,4’-TETRACHLOROBIPHENYL                | E1668A             |
| E1668A             | 3,3’,4,4’,5-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 3,3’,4,4’,5,5’-HEXACHLOROBIPHENYL            | E1668A             |
| E1668A             | 3,3’,4,5’-TETRACHLOROBIPHENYL                | E1668A             |
| E1668A             | 3,3’,4,5-TETRACHLOROBIPHENYL                 | E1668A             |
| E1668A             | 3,3’,4,5,5’-PENTACHLOROBIPHENYL              | E1668A             |
| E1668A             | 3,3’,5-TRICHLOROBIPHENYL                     | E1668A             |
| E1668A             | 3,3’,5,5’-TETRACHLOROBIPHENYL                | E1668A             |
| E1668A             | 3,4’,5-TRICHLOROBIPHENYL                     | E1668A             |
| E1668A             | 3,4,4’-TRICHLOROBIPHENYL                     | E1668A             |
| E1668A             | 3,4,4’,5-TETRACHLOROBIPHENYL                 | E1668A             |
| E1668A             | 3,4,5-TRICHLOROBIPHENYL                      | E1668A             |
| E1668A             | 3,5-DICHLOROBIPHENYL                         | E1668A             |
| E1668A             | 4-CHLOROBIPHENYL                             | E1668A             |
| E1668A             | 4,4’-DICHLOROBIPHENYL                        | E1668A             |
| E1668A             | DECACHLOROBIPHENYL                           | E1668A             |
| E1668A             | LIPIDS                                       | E1668A             |
| E1668A             | MOISTURE                                     | E1668A             |
| E1668A             | PCB-012/013                                  | E1668A             |
| E1668A             | PCB-018/030                                  | E1668A             |
| E1668A             | PCB-020/028                                  | E1668A             |
| E1668A             | PCB-021/033                                  | E1668A             |
| E1668A             | PCB-026/029                                  | E1668A             |
| E1668A             | PCB-040/041/071                              | E1668A             |
| E1668A             | PCB-044/047/065                              | E1668A             |
| E1668A             | PCB-045/051                                  | E1668A             |
| E1668A             | PCB-049/069                                  | E1668A             |
| E1668A             | PCB-050/053                                  | E1668A             |
| E1668A             | PCB-059/062/075                              | E1668A             |
| E1668A             | PCB-061/070/074/076                          | E1668A             |
| E1668A             | PCB-083/099                                  | E1668A             |
| E1668A             | PCB-085/116/117                              | E1668A             |
| E1668A             | PCB-086/087/097/108/119/125                  | E1668A             |
| E1668A             | PCB-088/091                                  | E1668A             |
| E1668A             | PCB-090/101/113                              | E1668A             |
| E1668A             | PCB-093/095/098/100/102                      | E1668A             |
| E1668A             | PCB-093/098/100/102                          | E1668A             |
| E1668A             | PCB-107/124                                  | E1668A             |
| E1668A             | PCB-110/115                                  | E1668A             |
| E1668A             | PCB-128/166                                  | E1668A             |
| E1668A             | PCB-129/138/160/163                          | E1668A             |
| E1668A             | PCB-129/138/163                              | E1668A             |
| E1668A             | PCB-134/143                                  | E1668A             |
| E1668A             | PCB-135/151                                  | E1668A             |
| E1668A             | PCB-135/151/154                              | E1668A             |
| E1668A             | PCB-139/140                                  | E1668A             |
| E1668A             | PCB-147/149                                  | E1668A             |
| E1668A             | PCB-153/168                                  | E1668A             |
| E1668A             | PCB-156/157                                  | E1668A             |
| E1668A             | PCB-171/173                                  | E1668A             |
| E1668A             | PCB-180/193                                  | E1668A             |
| E1668A             | PCB-183/185                                  | E1668A             |
| E1668A             | PCB-197/200                                  | E1668A             |
| E1668A             | PCB-198/199                                  | E1668A             |
| E1668A             | PCB TOTAL TEQ (ND=0)                         | E1668A             |
| E1668A             | PCB TOTAL TEQ (ND=1/2 DL)                    | E1668A             |
| E1668A             | PCB TOTAL TEQ (ND=DL)                        | E1668A             |
| E1668A             | PCBS                                         | E1668A             |
| E1668A             | TOTAL PCB-D                                  | E1668A             |
| E1668A             | TOTAL PCB-H                                  | E1668A             |
| E1668A             | TOTAL PCB-O                                  | E1668A             |
| E1668A             | WEIGHT                                       | E1668A             |
| E245.6M            | MERCURY                                      | E245.6M            |
| E270.3M            | SELENIUM                                     | E270.3M            |
| FIELD              | Mytilus edulis                               | FIELD              |
| MLA-007-E1         | 2,4’-DDD                                     | MLA-007-E1         |
| MLA-007-E1         | 2,4’-DDE                                     | MLA-007-E1         |
| MLA-007-E1         | 2,4’-DDT                                     | MLA-007-E1         |
| MLA-007-E1         | 4,4’-DDD                                     | MLA-007-E1         |
| MLA-007-E1         | 4,4’-DDE                                     | MLA-007-E1         |
| MLA-007-E1         | 4,4’-DDT                                     | MLA-007-E1         |
| MLA-007-E1         | ALDRIN                                       | MLA-007-E1         |
| MLA-007-E1         | ALPHA-BHC                                    | MLA-007-E1         |
| MLA-007-E1         | ALPHA-CHLORDANE                              | MLA-007-E1         |
| MLA-007-E1         | BETA-BHC                                     | MLA-007-E1         |
| MLA-007-E1         | GAMMA-BHC (LINDANE)                          | MLA-007-E1         |
| MLA-007-E1         | GAMMA-CHLORDANE                              | MLA-007-E1         |
| MLA-007-E1         | HEPTACHLOR                                   | MLA-007-E1         |
| MLA-007-E1         | HEXACHLOROBENZENE                            | MLA-007-E1         |
| MLA-007-E1         | LIPIDS                                       | MLA-007-E1         |
| MLA-007-E1         | MIREX                                        | MLA-007-E1         |
| MLA-007-E1         | MOISTURE                                     | MLA-007-E1         |
| MLA-007-E1         | TOTAL PESTICIDES21-D                         | MLA-007-E1         |
| MLA-007-E1         | TOTAL PESTICIDES21-H                         | MLA-007-E1         |
| MLA-007-E1         | TOTAL PESTICIDES21-O                         | MLA-007-E1         |
| MLA-007-E1         | TRANS-NONACHLOR                              | MLA-007-E1         |
| MLA-007-E2         | DELTA-BHC                                    | MLA-007-E2         |
| MLA-007-E2         | DIELDRIN                                     | MLA-007-E2         |
| MLA-007-E2         | ENDOSULFAN I                                 | MLA-007-E2         |
| MLA-007-E2         | ENDOSULFAN II                                | MLA-007-E2         |
| MLA-007-E2         | ENDOSULFAN SULFATE                           | MLA-007-E2         |
| MLA-007-E2         | ENDRIN                                       | MLA-007-E2         |
| MLA-007-E2         | ENDRIN ALDEHYDE                              | MLA-007-E2         |
| MLA-007-E2         | ENDRIN KETONE                                | MLA-007-E2         |
| MLA-007-E2         | HEPTACHLOR EPOXIDE                           | MLA-007-E2         |
| MLA-007-E2         | LIPIDS                                       | MLA-007-E2         |
| MLA-007-E2         | METHOXYCHLOR                                 | MLA-007-E2         |
| MLA-007-E2         | MOISTURE                                     | MLA-007-E2         |
| MLA-007-E2         | TOTAL PESTICIDES21-D                         | MLA-007-E2         |
| MLA-007-E2         | TOTAL PESTICIDES21-H                         | MLA-007-E2         |
| MLA-007-E2         | TOTAL PESTICIDES21-O                         | MLA-007-E2         |
| MLA-035            | 2,4’-DDD                                     | MLA-035            |
| MLA-035            | 2,4’-DDE                                     | MLA-035            |
| MLA-035            | 2,4’-DDT                                     | MLA-035            |
| MLA-035            | 4,4’-DDD                                     | MLA-035            |
| MLA-035            | 4,4’-DDE                                     | MLA-035            |
| MLA-035            | 4,4’-DDT                                     | MLA-035            |
| MLA-035            | ALACHLOR                                     | MLA-035            |
| MLA-035            | ALDRIN                                       | MLA-035            |
| MLA-035            | ALPHA-BHC                                    | MLA-035            |
| MLA-035            | ALPHA-CHLORDANE                              | MLA-035            |
| MLA-035            | AMETRYN                                      | MLA-035            |
| MLA-035            | ATRAZINE                                     | MLA-035            |
| MLA-035            | AZINPHOS-METHYL                              | MLA-035            |
| MLA-035            | BETA-BHC                                     | MLA-035            |
| MLA-035            | BUTRALIN                                     | MLA-035            |
| MLA-035            | BUTYLATE                                     | MLA-035            |
| MLA-035            | CAPTAN                                       | MLA-035            |
| MLA-035            | CHLOROTHALONIL                               | MLA-035            |
| MLA-035            | CHLORPYRIFOS, O,O-DIMETHYL ANALOG            | MLA-035            |
| MLA-035            | CHLORPYRIPHOS                                | MLA-035            |
| MLA-035            | CHLORPYRIPHOS-OXON                           | MLA-035            |
| MLA-035            | CIS-NONACHLOR                                | MLA-035            |
| MLA-035            | CYANAZINE                                    | MLA-035            |
| MLA-035            | CYPERMETHRIN                                 | MLA-035            |
| MLA-035            | DCPA                                         | MLA-035            |
| MLA-035            | DELTA-BHC                                    | MLA-035            |
| MLA-035            | DESETHYLATRAZINE                             | MLA-035            |
| MLA-035            | DIAZINON                                     | MLA-035            |
| MLA-035            | DIAZINON-OXON                                | MLA-035            |
| MLA-035            | DIELDRIN                                     | MLA-035            |
| MLA-035            | DIMETHENAMID                                 | MLA-035            |
| MLA-035            | DIMETHOATE                                   | MLA-035            |
| MLA-035            | DISULFOTON                                   | MLA-035            |
| MLA-035            | DISULFOTON SULFONE                           | MLA-035            |
| MLA-035            | ENDOSULFAN I                                 | MLA-035            |
| MLA-035            | ENDOSULFAN II                                | MLA-035            |
| MLA-035            | ENDOSULFAN SULFATE                           | MLA-035            |
| MLA-035            | ENDRIN                                       | MLA-035            |
| MLA-035            | ENDRIN KETONE                                | MLA-035            |
| MLA-035            | ETHALFLURALIN                                | MLA-035            |
| MLA-035            | ETHION                                       | MLA-035            |
| MLA-035            | FENITROTHION                                 | MLA-035            |
| MLA-035            | FLUFENACET                                   | MLA-035            |
| MLA-035            | FLUTRIAFOL                                   | MLA-035            |
| MLA-035            | FONOFOS                                      | MLA-035            |
| MLA-035            | GAMMA-BHC (LINDANE)                          | MLA-035            |
| MLA-035            | GAMMA-CHLORDANE                              | MLA-035            |
| MLA-035            | HEPTACHLOR                                   | MLA-035            |
| MLA-035            | HEPTACHLOR EPOXIDE                           | MLA-035            |
| MLA-035            | HEXACHLOROBENZENE                            | MLA-035            |
| MLA-035            | LINURON                                      | MLA-035            |
| MLA-035            | MALATHION                                    | MLA-035            |
| MLA-035            | METHAMIDOPHOS                                | MLA-035            |
| MLA-035            | METHOPRENE                                   | MLA-035            |
| MLA-035            | METHOXYCHLOR                                 | MLA-035            |
| MLA-035            | METHYL PARATHION                             | MLA-035            |
| MLA-035            | METOLACHLOR                                  | MLA-035            |
| MLA-035            | METRIBUZIN                                   | MLA-035            |
| MLA-035            | MIREX                                        | MLA-035            |
| MLA-035            | OCTACHLOROSTYRENE                            | MLA-035            |
| MLA-035            | OXYCHLORDANE                                 | MLA-035            |
| MLA-035            | PARATHION                                    | MLA-035            |
| MLA-035            | PENDIMETHALIN                                | MLA-035            |
| MLA-035            | PENTACHLORONITROBENZENE                      | MLA-035            |
| MLA-035            | PERMETHRIN                                   | MLA-035            |
| MLA-035            | PERTHANE                                     | MLA-035            |
| MLA-035            | PHORATE                                      | MLA-035            |
| MLA-035            | PHOSMET                                      | MLA-035            |
| MLA-035            | PIRIMIPHOS-METHYL                            | MLA-035            |
| MLA-035            | SIMAZINE                                     | MLA-035            |
| MLA-035            | TEBUCONAZOL                                  | MLA-035            |
| MLA-035            | TECNAZENE                                    | MLA-035            |
| MLA-035            | TERBUFOS                                     | MLA-035            |
| MLA-035            | TOTAL PESTICIDES21-D                         | MLA-035            |
| MLA-035            | TOTAL PESTICIDES21-H                         | MLA-035            |
| MLA-035            | TOTAL PESTICIDES21-O                         | MLA-035            |
| MLA-035            | TRANS-NONACHLOR                              | MLA-035            |
| MLA-035            | TRIALLATE                                    | MLA-035            |
| MLA-035            | TRIFLURALIN                                  | MLA-035            |
| MLA-035            | VELPAR                                       | MLA-035            |
| MLA-043            | LIPIDS                                       | MLA-043            |
| MLA-043            | MOISTURE                                     | MLA-043            |
| MLA-043            | Mytilus edulis                               | MLA-043            |
| MLA-043            | PERFLUOROBUTANE SULFONATE                    | MLA-043            |
| MLA-043            | PERFLUOROBUTANOATE                           | MLA-043            |
| MLA-043            | PERFLUORODECANOATE                           | MLA-043            |
| MLA-043            | PERFLUORODODECANOATE                         | MLA-043            |
| MLA-043            | PERFLUOROHEPTANOATE                          | MLA-043            |
| MLA-043            | PERFLUOROHEXANE SULFONATE                    | MLA-043            |
| MLA-043            | PERFLUOROHEXANOATE                           | MLA-043            |
| MLA-043            | PERFLUORONONANOATE                           | MLA-043            |
| MLA-043            | PERFLUOROOCTANE SULFONAMIDE                  | MLA-043            |
| MLA-043            | PERFLUOROOCTANE SULFONATE                    | MLA-043            |
| MLA-043            | PERFLUOROOCTANOATE                           | MLA-043            |
| MLA-043            | PERFLUOROOCTANOIC ACID                       | MLA-043            |
| MLA-043            | PERFLUOROPENTANOATE                          | MLA-043            |
| MLA-043            | PERFLUOROUNDECANOATE                         | MLA-043            |
| NOAA1998M          | SOLIDS-TOTAL RESIDUE (TS)                    | NOAA1998M          |
| SM2540G            | MOISTURE                                     | SM2540G            |
| SM2540G            | SOLIDS-TOTAL RESIDUE (TS)                    | SM2540G            |
| SM2540GM           | SOLIDS-TOTAL RESIDUE (TS)                    | SM2540GM           |
| SOP\_MSLC003       | SOLIDS-TOTAL RESIDUE (TS)                    | SOP\_MSLC003       |
| SW6010BM/E200.7M   | ALUMINUM                                     | SW6010BM/E200.7M   |
| SW6010BM/E200.7M   | CHROMIUM                                     | SW6010BM/E200.7M   |
| SW6010BM/E200.7M   | COPPER                                       | SW6010BM/E200.7M   |
| SW6010BM/E200.7M   | IRON                                         | SW6010BM/E200.7M   |
| SW6010BM/E200.7M   | NICKEL                                       | SW6010BM/E200.7M   |
| SW6010BM/E200.7M   | ZINC                                         | SW6010BM/E200.7M   |
| SW6020             | ALUMINUM                                     | SW6020             |
| SW6020             | CADMIUM                                      | SW6020             |
| SW6020             | CHROMIUM                                     | SW6020             |
| SW6020             | COPPER                                       | SW6020             |
| SW6020             | IRON                                         | SW6020             |
| SW6020             | LEAD                                         | SW6020             |
| SW6020             | MERCURY                                      | SW6020             |
| SW6020             | NICKEL                                       | SW6020             |
| SW6020             | SELENIUM                                     | SW6020             |
| SW6020             | SILVER                                       | SW6020             |
| SW6020             | ZINC                                         | SW6020             |
| SW6020A            | ALUMINUM                                     | SW6020A            |
| SW6020A            | ARSENIC                                      | SW6020A            |
| SW6020A            | CADMIUM                                      | SW6020A            |
| SW6020A            | CHROMIUM                                     | SW6020A            |
| SW6020A            | COPPER                                       | SW6020A            |
| SW6020A            | IRON                                         | SW6020A            |
| SW6020A            | LEAD                                         | SW6020A            |
| SW6020A            | NICKEL                                       | SW6020A            |
| SW6020A            | SELENIUM                                     | SW6020A            |
| SW6020A            | SILVER                                       | SW6020A            |
| SW6020A            | ZINC                                         | SW6020A            |
| SW7471A            | MERCURY                                      | SW7471A            |
| SW7473M            | MERCURY                                      | SW7473M            |
| SW8081             | ALPHA-CHLORDANE                              | SW8081             |
| SW8081             | DIELDRIN                                     | SW8081             |
| SW8081             | ENDOSULFAN I                                 | SW8081             |
| SW8081             | ENDOSULFAN II                                | SW8081             |
| SW8081             | GAMMA-BHC (LINDANE)                          | SW8081             |
| SW8081             | GAMMA-CHLORDANE                              | SW8081             |
| SW8081             | HEPTACHLOR                                   | SW8081             |
| SW8081             | HEPTACHLOR EPOXIDE                           | SW8081             |
| SW8081             | MIREX                                        | SW8081             |
| SW8081             | TRANS-NONACHLOR                              | SW8081             |
| SW8081A            | 2,4’-DDD                                     | SW8081A            |
| SW8081A            | 2,4’-DDE                                     | SW8081A            |
| SW8081A            | 2,4’-DDT                                     | SW8081A            |
| SW8081A            | 4,4’-DDD                                     | SW8081A            |
| SW8081A            | 4,4’-DDE                                     | SW8081A            |
| SW8081A            | 4,4’-DDT                                     | SW8081A            |
| SW8081A            | ALDRIN                                       | SW8081A            |
| SW8081A            | ALPHA-BHC                                    | SW8081A            |
| SW8081A            | ALPHA-CHLORDANE                              | SW8081A            |
| SW8081A            | BETA-BHC                                     | SW8081A            |
| SW8081A            | DELTA-BHC                                    | SW8081A            |
| SW8081A            | DIELDRIN                                     | SW8081A            |
| SW8081A            | ENDOSULFAN I                                 | SW8081A            |
| SW8081A            | ENDOSULFAN II                                | SW8081A            |
| SW8081A            | ENDOSULFAN SULFATE                           | SW8081A            |
| SW8081A            | ENDRIN                                       | SW8081A            |
| SW8081A            | ENDRIN ALDEHYDE                              | SW8081A            |
| SW8081A            | ENDRIN KETONE                                | SW8081A            |
| SW8081A            | GAMMA-BHC (LINDANE)                          | SW8081A            |
| SW8081A            | GAMMA-CHLORDANE                              | SW8081A            |
| SW8081A            | HEPTACHLOR                                   | SW8081A            |
| SW8081A            | HEPTACHLOR EPOXIDE                           | SW8081A            |
| SW8081A            | HEXACHLOROBENZENE                            | SW8081A            |
| SW8081A            | LIPIDS                                       | SW8081A            |
| SW8081A            | METHOXYCHLOR                                 | SW8081A            |
| SW8081A            | MIREX                                        | SW8081A            |
| SW8081A            | MOISTURE                                     | SW8081A            |
| SW8081A            | TRANS-NONACHLOR                              | SW8081A            |
| SW8081A            | WEIGHT                                       | SW8081A            |
| SW8270C\_SIM       | 1-METHYL NAPHTHALENE                         | SW8270C\_SIM       |
| SW8270C\_SIM       | 1-METHYLPHENANTHRENE                         | SW8270C\_SIM       |
| SW8270C\_SIM       | 2-METHYLNAPHTHALENE                          | SW8270C\_SIM       |
| SW8270C\_SIM       | 2,3,5-TRIMETHYLNAPHTHALENE                   | SW8270C\_SIM       |
| SW8270C\_SIM       | 2,4’-DDD                                     | SW8270C\_SIM       |
| SW8270C\_SIM       | 2,4’-DDE                                     | SW8270C\_SIM       |
| SW8270C\_SIM       | 2,4’-DDT                                     | SW8270C\_SIM       |
| SW8270C\_SIM       | 2,6-DIMETHYLNAPHTHALENE                      | SW8270C\_SIM       |
| SW8270C\_SIM       | 4,4’-DDD                                     | SW8270C\_SIM       |
| SW8270C\_SIM       | 4,4’-DDE                                     | SW8270C\_SIM       |
| SW8270C\_SIM       | 4,4’-DDT                                     | SW8270C\_SIM       |
| SW8270C\_SIM       | ACENAPHTHENE                                 | SW8270C\_SIM       |
| SW8270C\_SIM       | ACENAPHTHYLENE                               | SW8270C\_SIM       |
| SW8270C\_SIM       | ALDRIN                                       | SW8270C\_SIM       |
| SW8270C\_SIM       | ANTHRACENE                                   | SW8270C\_SIM       |
| SW8270C\_SIM       | BENZO(A)ANTHRACENE                           | SW8270C\_SIM       |
| SW8270C\_SIM       | BENZO(A)PYRENE                               | SW8270C\_SIM       |
| SW8270C\_SIM       | BENZO(B)FLUORANTHENE                         | SW8270C\_SIM       |
| SW8270C\_SIM       | BENZO(E)PYRENE                               | SW8270C\_SIM       |
| SW8270C\_SIM       | BENZO(G,H,I)PERYLENE                         | SW8270C\_SIM       |
| SW8270C\_SIM       | BENZO(K)FLUORANTHENE                         | SW8270C\_SIM       |
| SW8270C\_SIM       | BIPHENYL                                     | SW8270C\_SIM       |
| SW8270C\_SIM       | CHRYSENE                                     | SW8270C\_SIM       |
| SW8270C\_SIM       | DIBENZO(A,H)ANTHRACENE                       | SW8270C\_SIM       |
| SW8270C\_SIM       | FLUORANTHENE                                 | SW8270C\_SIM       |
| SW8270C\_SIM       | FLUORENE                                     | SW8270C\_SIM       |
| SW8270C\_SIM       | HEXACHLOROBENZENE                            | SW8270C\_SIM       |
| SW8270C\_SIM       | INDENO(1,2,3-CD)PYRENE                       | SW8270C\_SIM       |
| SW8270C\_SIM       | LIPIDS                                       | SW8270C\_SIM       |
| SW8270C\_SIM       | NAPHTHALENE                                  | SW8270C\_SIM       |
| SW8270C\_SIM       | PERYLENE                                     | SW8270C\_SIM       |
| SW8270C\_SIM       | PHENANTHRENE                                 | SW8270C\_SIM       |
| SW8270C\_SIM       | PYRENE                                       | SW8270C\_SIM       |
| SW8270CM           | 1-METHYL NAPHTHALENE                         | SW8270CM           |
| SW8270CM           | 1-METHYLCHRYSENE                             | SW8270CM           |
| SW8270CM           | 1-METHYLPHENANTHRENE                         | SW8270CM           |
| SW8270CM           | 1,2-DIMETHYLNAPHTHALENE                      | SW8270CM           |
| SW8270CM           | 1,2,6-TRIMETHYLPHENANTHRENE                  | SW8270CM           |
| SW8270CM           | 1,4,6,7-TETRAMETHYLNAPHTHALENE               | SW8270CM           |
| SW8270CM           | 1,7-DIMETHYLFLUORENE                         | SW8270CM           |
| SW8270CM           | 1,7-DIMETHYLPHENANTHRENE                     | SW8270CM           |
| SW8270CM           | 1,8-DIMETHYLPHENANTHRENE                     | SW8270CM           |
| SW8270CM           | 2-METHYLANTHRACENE                           | SW8270CM           |
| SW8270CM           | 2-METHYLFLUORENE                             | SW8270CM           |
| SW8270CM           | 2-METHYLNAPHTHALENE                          | SW8270CM           |
| SW8270CM           | 2-METHYLPHENANTHRENE                         | SW8270CM           |
| SW8270CM           | 2,3,5-TRIMETHYLNAPHTHALENE                   | SW8270CM           |
| SW8270CM           | 2,3,6-TRIMETHYLNAPHTHALENE                   | SW8270CM           |
| SW8270CM           | 2,4-DIMETHYLDIBENZOTHIOPHENE                 | SW8270CM           |
| SW8270CM           | 2,6-DIMETHYLNAPHTHALENE                      | SW8270CM           |
| SW8270CM           | 2,6-DIMETHYLPHENANTHRENE                     | SW8270CM           |
| SW8270CM           | 2/3-METHYLDIBENZOTHIOPHENES                  | SW8270CM           |
| SW8270CM           | 3-METHYLFLUORANTHENE/BENZO\[A\]FLUORENE      | SW8270CM           |
| SW8270CM           | 3-METHYLPHENANTHRENE                         | SW8270CM           |
| SW8270CM           | 3,6-DIMETHYLPHENANTHRENE                     | SW8270CM           |
| SW8270CM           | 5,9-DIMETHYLCHRYSENE                         | SW8270CM           |
| SW8270CM           | 5/6-METHYLCHRYSENE                           | SW8270CM           |
| SW8270CM           | 7-METHYLBENZO\[A\]PYRENE                     | SW8270CM           |
| SW8270CM           | 9/4-METHYLPHENANTHRENE                       | SW8270CM           |
| SW8270CM           | ACENAPHTHENE                                 | SW8270CM           |
| SW8270CM           | ACENAPHTHYLENE                               | SW8270CM           |
| SW8270CM           | ANTHRACENE                                   | SW8270CM           |
| SW8270CM           | BENZO(A)ANTHRACENE                           | SW8270CM           |
| SW8270CM           | BENZO(A)PYRENE                               | SW8270CM           |
| SW8270CM           | BENZO(B)FLUORANTHENE                         | SW8270CM           |
| SW8270CM           | BENZO(E)PYRENE                               | SW8270CM           |
| SW8270CM           | BENZO(G,H,I)PERYLENE                         | SW8270CM           |
| SW8270CM           | BENZO\[B,J,K\]FLUORANTHENES                  | SW8270CM           |
| SW8270CM           | BENZO\[J,K\]FLUORANTHENES                    | SW8270CM           |
| SW8270CM           | BENZOFLUORANTHENE                            | SW8270CM           |
| SW8270CM           | BIPHENYL                                     | SW8270CM           |
| SW8270CM           | C1-ACENAPHTHENES                             | SW8270CM           |
| SW8270CM           | C1-BENZO\[A\]ANTHRACENES/CHRYSENES           | SW8270CM           |
| SW8270CM           | C1-BENZOFLUORANTHENES/BENZOPYRENES           | SW8270CM           |
| SW8270CM           | C1-BIPHENYLS                                 | SW8270CM           |
| SW8270CM           | C1-DIBENZOTHIOPHENES                         | SW8270CM           |
| SW8270CM           | C1-FLUORANTHENES/PYRENES                     | SW8270CM           |
| SW8270CM           | C1-FLUORENES                                 | SW8270CM           |
| SW8270CM           | C1-NAPHTHALENES                              | SW8270CM           |
| SW8270CM           | C1-PHENANTHRENES/ANTHRACENES                 | SW8270CM           |
| SW8270CM           | C2-BENZO\[A\]ANTHRACENES/CHRYSENES           | SW8270CM           |
| SW8270CM           | C2-BENZOFLUORANTHENES/BENZOPYRENES           | SW8270CM           |
| SW8270CM           | C2-BIPHENYLS                                 | SW8270CM           |
| SW8270CM           | C2-DIBENZOTHIOPHENES                         | SW8270CM           |
| SW8270CM           | C2-FLUORANTHENES/PYRENES                     | SW8270CM           |
| SW8270CM           | C2-FLUORENES                                 | SW8270CM           |
| SW8270CM           | C2-NAPHTHALENES                              | SW8270CM           |
| SW8270CM           | C2-PHENANTHRENES/ANTHRACENES                 | SW8270CM           |
| SW8270CM           | C3-BENZO\[A\]ANTHRACENES/CHRYSENES           | SW8270CM           |
| SW8270CM           | C3-DIBENZOTHIOPHENES                         | SW8270CM           |
| SW8270CM           | C3-FLUORANTHENES/PYRENES                     | SW8270CM           |
| SW8270CM           | C3-FLUORENES                                 | SW8270CM           |
| SW8270CM           | C3-NAPHTHALENES                              | SW8270CM           |
| SW8270CM           | C3-PHENANTHRENES/ANTHRACENES                 | SW8270CM           |
| SW8270CM           | C4-BENZO\[A\]ANTHRACENES/CHRYSENES           | SW8270CM           |
| SW8270CM           | C4-DIBENZOTHIOPHENES                         | SW8270CM           |
| SW8270CM           | C4-FLUORANTHENES/PYRENES                     | SW8270CM           |
| SW8270CM           | C4-NAPHTHALENES                              | SW8270CM           |
| SW8270CM           | C4-PHENANTHRENES/ANTHRACENES                 | SW8270CM           |
| SW8270CM           | CHRYSENE                                     | SW8270CM           |
| SW8270CM           | DIBENZO(A,H)ANTHRACENE                       | SW8270CM           |
| SW8270CM           | DIBENZOTHIOPHENE                             | SW8270CM           |
| SW8270CM           | FLUORANTHENE                                 | SW8270CM           |
| SW8270CM           | FLUORENE                                     | SW8270CM           |
| SW8270CM           | INDENO(1,2,3-CD)PYRENE                       | SW8270CM           |
| SW8270CM           | LIPIDS                                       | SW8270CM           |
| SW8270CM           | MOISTURE                                     | SW8270CM           |
| SW8270CM           | NAPHTHALENE                                  | SW8270CM           |
| SW8270CM           | PERYLENE                                     | SW8270CM           |
| SW8270CM           | PHENANTHRENE                                 | SW8270CM           |
| SW8270CM           | PYRENE                                       | SW8270CM           |
| SW8270CM           | RETENE                                       | SW8270CM           |
| SW8270CM           | TOTAL PAH-D                                  | SW8270CM           |
| SW8270CM           | TOTAL PAH-H                                  | SW8270CM           |
| SW8270CM           | TOTAL PAH-O                                  | SW8270CM           |
| SW8270CM           | TOTAL PAH19-D                                | SW8270CM           |
| SW8270CM           | TOTAL PAH19-H                                | SW8270CM           |
| SW8270CM           | TOTAL PAH19-O                                | SW8270CM           |
| SW8270CM           | TOTAL PAH24-D                                | SW8270CM           |
| SW8270CM           | TOTAL PAH24-H                                | SW8270CM           |
| SW8270CM           | TOTAL PAH24-O                                | SW8270CM           |
| SW8270CM           | TOTAL PAH40-D                                | SW8270CM           |
| SW8270CM           | TOTAL PAH40-H                                | SW8270CM           |
| SW8270CM           | TOTAL PAH40-O                                | SW8270CM           |
| SW8270CM           | WEIGHT                                       | SW8270CM           |

## Totals And Calculations

``` r
totals_data <- SWAT_data %>%
  select(PARAMETER, `TEST METHOD`) %>%
  filter(grepl('TOTAL', PARAMETER, ignore.case = TRUE) |
           grepl('CALCULATED', `TEST METHOD`)) %>%
  group_by(`TEST METHOD`, PARAMETER) %>%
  summarize(Test      =  first(`TEST METHOD`),
                        .groups = 'drop') %>%
  rename(Parameter = PARAMETER)
kable(totals_data)
```

| TEST METHOD        | Parameter                          | Test               |
| :----------------- | :--------------------------------- | :----------------- |
| CALCULATED         | C1-ACENAPHTHENES                   | CALCULATED         |
| CALCULATED         | C1-BENZO\[A\]ANTHRACENES/CHRYSENES | CALCULATED         |
| CALCULATED         | C1-BENZOFLUORANTHENES/BENZOPYRENES | CALCULATED         |
| CALCULATED         | C1-BIPHENYLS                       | CALCULATED         |
| CALCULATED         | C1-DIBENZOTHIOPHENES               | CALCULATED         |
| CALCULATED         | C1-FLUORANTHENES/PYRENES           | CALCULATED         |
| CALCULATED         | C1-FLUORENES                       | CALCULATED         |
| CALCULATED         | C1-NAPHTHALENES                    | CALCULATED         |
| CALCULATED         | C1-PHENANTHRENES/ANTHRACENES       | CALCULATED         |
| CALCULATED         | C2-BENZO\[A\]ANTHRACENES/CHRYSENES | CALCULATED         |
| CALCULATED         | C2-BENZOFLUORANTHENES/BENZOPYRENES | CALCULATED         |
| CALCULATED         | C2-BIPHENYLS                       | CALCULATED         |
| CALCULATED         | C2-DIBENZOTHIOPHENES               | CALCULATED         |
| CALCULATED         | C2-FLUORANTHENES/PYRENES           | CALCULATED         |
| CALCULATED         | C2-FLUORENES                       | CALCULATED         |
| CALCULATED         | C2-NAPHTHALENES                    | CALCULATED         |
| CALCULATED         | C2-PHENANTHRENES/ANTHRACENES       | CALCULATED         |
| CALCULATED         | C3-BENZO\[A\]ANTHRACENES/CHRYSENES | CALCULATED         |
| CALCULATED         | C3-DIBENZOTHIOPHENES               | CALCULATED         |
| CALCULATED         | C3-FLUORANTHENES/PYRENES           | CALCULATED         |
| CALCULATED         | C3-FLUORENES                       | CALCULATED         |
| CALCULATED         | C3-NAPHTHALENES                    | CALCULATED         |
| CALCULATED         | C3-PHENANTHRENES/ANTHRACENES       | CALCULATED         |
| CALCULATED         | C4-BENZO\[A\]ANTHRACENES/CHRYSENES | CALCULATED         |
| CALCULATED         | C4-DIBENZOTHIOPHENES               | CALCULATED         |
| CALCULATED         | C4-FLUORANTHENES/PYRENES           | CALCULATED         |
| CALCULATED         | C4-NAPHTHALENES                    | CALCULATED         |
| CALCULATED         | C4-PHENANTHRENES/ANTHRACENES       | CALCULATED         |
| CALCULATED         | PCB TOTAL TEQ (ND=0)               | CALCULATED         |
| CALCULATED         | PCB TOTAL TEQ (ND=1/2 DL)          | CALCULATED         |
| CALCULATED         | PCB TOTAL TEQ (ND=DL)              | CALCULATED         |
| CALCULATED         | PCBS                               | CALCULATED         |
| CALCULATED WHO2005 | C1-ACENAPHTHENES                   | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-BENZO\[A\]ANTHRACENES/CHRYSENES | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-BENZOFLUORANTHENES/BENZOPYRENES | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-BIPHENYLS                       | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-DIBENZOTHIOPHENES               | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-FLUORANTHENES/PYRENES           | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-FLUORENES                       | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-NAPHTHALENES                    | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C1-PHENANTHRENES/ANTHRACENES       | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-BENZO\[A\]ANTHRACENES/CHRYSENES | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-BENZOFLUORANTHENES/BENZOPYRENES | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-BIPHENYLS                       | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-DIBENZOTHIOPHENES               | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-FLUORANTHENES/PYRENES           | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-FLUORENES                       | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-NAPHTHALENES                    | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C2-PHENANTHRENES/ANTHRACENES       | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C3-BENZO\[A\]ANTHRACENES/CHRYSENES | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C3-DIBENZOTHIOPHENES               | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C3-FLUORANTHENES/PYRENES           | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C3-FLUORENES                       | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C3-NAPHTHALENES                    | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C3-PHENANTHRENES/ANTHRACENES       | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C4-BENZO\[A\]ANTHRACENES/CHRYSENES | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C4-DIBENZOTHIOPHENES               | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C4-FLUORANTHENES/PYRENES           | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C4-NAPHTHALENES                    | CALCULATED WHO2005 |
| CALCULATED WHO2005 | C4-PHENANTHRENES/ANTHRACENES       | CALCULATED WHO2005 |
| CALCULATED WHO2005 | DX TOTAL TEQ (ND=0)                | CALCULATED WHO2005 |
| CALCULATED WHO2005 | DX TOTAL TEQ (ND=1/2 DL)           | CALCULATED WHO2005 |
| CALCULATED WHO2005 | DX TOTAL TEQ (ND=DL)               | CALCULATED WHO2005 |
| CALCULATED WHO2005 | PCB TOTAL TEQ (ND=0)               | CALCULATED WHO2005 |
| CALCULATED WHO2005 | PCB TOTAL TEQ (ND=1/2 DL)          | CALCULATED WHO2005 |
| CALCULATED WHO2005 | PCB TOTAL TEQ (ND=DL)              | CALCULATED WHO2005 |
| CALCULATED WHO2005 | PCBS                               | CALCULATED WHO2005 |
| E160.3             | SOLIDS-TOTAL RESIDUE (TS)          | E160.3             |
| E1613              | DX TOTAL TEQ (ND=1/2 DL)           | E1613              |
| E1613              | DX TOTAL TEQ (ND=DL)               | E1613              |
| E1668A             | PCB TOTAL TEQ (ND=0)               | E1668A             |
| E1668A             | PCB TOTAL TEQ (ND=1/2 DL)          | E1668A             |
| E1668A             | PCB TOTAL TEQ (ND=DL)              | E1668A             |
| E1668A             | TOTAL PCB-D                        | E1668A             |
| E1668A             | TOTAL PCB-H                        | E1668A             |
| E1668A             | TOTAL PCB-O                        | E1668A             |
| MLA-007-E1         | TOTAL PESTICIDES21-D               | MLA-007-E1         |
| MLA-007-E1         | TOTAL PESTICIDES21-H               | MLA-007-E1         |
| MLA-007-E1         | TOTAL PESTICIDES21-O               | MLA-007-E1         |
| MLA-007-E2         | TOTAL PESTICIDES21-D               | MLA-007-E2         |
| MLA-007-E2         | TOTAL PESTICIDES21-H               | MLA-007-E2         |
| MLA-007-E2         | TOTAL PESTICIDES21-O               | MLA-007-E2         |
| MLA-035            | TOTAL PESTICIDES21-D               | MLA-035            |
| MLA-035            | TOTAL PESTICIDES21-H               | MLA-035            |
| MLA-035            | TOTAL PESTICIDES21-O               | MLA-035            |
| NOAA1998M          | SOLIDS-TOTAL RESIDUE (TS)          | NOAA1998M          |
| SM2540G            | SOLIDS-TOTAL RESIDUE (TS)          | SM2540G            |
| SM2540GM           | SOLIDS-TOTAL RESIDUE (TS)          | SM2540GM           |
| SOP\_MSLC003       | SOLIDS-TOTAL RESIDUE (TS)          | SOP\_MSLC003       |
| SW8270CM           | TOTAL PAH-D                        | SW8270CM           |
| SW8270CM           | TOTAL PAH-H                        | SW8270CM           |
| SW8270CM           | TOTAL PAH-O                        | SW8270CM           |
| SW8270CM           | TOTAL PAH19-D                      | SW8270CM           |
| SW8270CM           | TOTAL PAH19-H                      | SW8270CM           |
| SW8270CM           | TOTAL PAH19-O                      | SW8270CM           |
| SW8270CM           | TOTAL PAH24-D                      | SW8270CM           |
| SW8270CM           | TOTAL PAH24-H                      | SW8270CM           |
| SW8270CM           | TOTAL PAH24-O                      | SW8270CM           |
| SW8270CM           | TOTAL PAH40-D                      | SW8270CM           |
| SW8270CM           | TOTAL PAH40-H                      | SW8270CM           |
| SW8270CM           | TOTAL PAH40-O                      | SW8270CM           |

## Read In a Hand Edited Classification of Parameters

We can then read in the resulting Excel File to provide groupings…

``` r
Parameter_List <- read_excel("Parameter List.xlsx", 
    sheet = "Parameter List") %>%
  mutate(Class = factor(Class)) %>%
  arrange(Class, PARAMETER)
Parameter_List
```

    ## # A tibble: 399 x 2
    ##    PARAMETER                                    Class 
    ##    <chr>                                        <fct> 
    ##  1 1,2,3,4,6,7,8-HPCDD                          Dioxin
    ##  2 1,2,3,4,6,7,8-HPCDF                          Dioxin
    ##  3 1,2,3,4,7,8-HXCDD                            Dioxin
    ##  4 1,2,3,4,7,8-HXCDF                            Dioxin
    ##  5 1,2,3,4,7,8,9-HEPTACHLORODIBENZOFURAN(HPCDF) Dioxin
    ##  6 1,2,3,6,7,8-HXCDD                            Dioxin
    ##  7 1,2,3,6,7,8-HXCDF                            Dioxin
    ##  8 1,2,3,7,8-PECDD                              Dioxin
    ##  9 1,2,3,7,8-PECDF                              Dioxin
    ## 10 1,2,3,7,8,9-HXCDD                            Dioxin
    ## # ... with 389 more rows

SWAT\_data %\>% \# filter(`WEIGHT BASIS` == ‘LIP’) %\>%
group\_by(CAS\_NO) %\>% summarize(nnames = length(unique(PARAMETER)),
name = first (PARAMETER), .groups = ‘drop’) %\>% arrange(name) %\>%
mutate(match(Parameter\_List))

# How did DEP Calculate Totals?

Many TOTAL parameters come in triplets – with a suffix of “-D’, or ‘-H’
or ‘-O’. It appears those are for totals calculated using different
assumptions about how to address non-detects, with -D stands
for”detection limit“, -H stands for”Half Detection Limit“, and -0
stands for”Zero".

Would be nice to confirm what parameters are included in each. so we can
apply maximum likelihood methods.

# Units

We need to deal with data reported in multiple different sets of units.
We will need conversion factors….

``` r
unique(SWAT_data$`UNITS VALUE`)
```

    ## [1] "NG/G"       "MG/KG"      "TRUE/FALSE" "%"          "PG/G"      
    ## [6] "UG/KG"      "NG/KG"      "G"          "UG/G"

So, conversion factors based on left of ‘/’ and right of ‘/’

Based on mg/g as basic unit

``` r
knitr::kable(xtabs(~PARAMETER+`UNITS VALUE`, data  = SWAT_data))
```

|                                              |   % |  G | MG/KG | NG/G | NG/KG | PG/G | TRUE/FALSE | UG/G | UG/KG |
| :------------------------------------------- | --: | -: | ----: | ---: | ----: | ---: | ---------: | ---: | ----: |
| 1-METHYL NAPHTHALENE                         |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| 1-METHYLCHRYSENE                             |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 1-METHYLPHENANTHRENE                         |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| 1,2-DIMETHYLNAPHTHALENE                      |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 1,2,3,4,6,7,8-HPCDD                          |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 1,2,3,4,6,7,8-HPCDF                          |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 1,2,3,4,7,8-HXCDD                            |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 1,2,3,4,7,8-HXCDF                            |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 1,2,3,4,7,8,9-HEPTACHLORODIBENZOFURAN(HPCDF) |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 1,2,3,6,7,8-HXCDD                            |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 1,2,3,6,7,8-HXCDF                            |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 1,2,3,7,8-PECDD                              |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 1,2,3,7,8-PECDF                              |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 1,2,3,7,8,9-HXCDD                            |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 1,2,3,7,8,9-HXCDF                            |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 1,2,6-TRIMETHYLPHENANTHRENE                  |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 1,4,6,7-TETRAMETHYLNAPHTHALENE               |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 1,7-DIMETHYLFLUORENE                         |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 1,7-DIMETHYLPHENANTHRENE                     |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 1,8-DIMETHYLPHENANTHRENE                     |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 2’,3,4,4’,5-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    41 |  372 |          0 |    0 |     0 |
| 2-CHLOROBIPHENYL                             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2-METHYLANTHRACENE                           |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 2-METHYLFLUORENE                             |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 2-METHYLNAPHTHALENE                          |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| 2-METHYLPHENANTHRENE                         |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 2,2’-DICHLOROBIPHENYL                        |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,4’,5-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    41 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,4’,5,5’-OCTACHLOROBIPHENYL       |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,4’,5,5’,6-NONACHLOROBIPHENYL     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,4’,5,6’-OCTACHLOROBIPHENYL       |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,4’,5,6-OCTACHLOROBIPHENYL        |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,4’,5,6,6’-NONACHLOROBIPHENYL     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5’,6’-HEPTACHLOROBIPHENYL        |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5’,6-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5’,6,6’-OCTACHLOROBIPHENYL       |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5,5’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5,5’,6,6’-NONACHLOROBIPHENYL     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,5,6’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,6’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,6-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,4,6,6’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,5-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    14 |    0 |          0 |    0 |     0 |
| 2,2’,3,3’,5,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,5,5’,6-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,5,5’,6,6’-OCTACHLOROBIPHENYL       |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,5,6,6’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,6-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,3’,6,6’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4’,5,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4’,5,5’,6-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4’,5,6’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4’,5,6,6’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4’,6,6’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4,4’,5-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4,4’,5,5’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    12 |    0 |          0 |    0 |     0 |
| 2,2’,3,4,4’,5,5’,6-OCTACHLOROBIPHENYL        |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4,4’,5,6’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4,4’,5,6-HEPTACHLOROBIPHENYL          |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4,4’,5,6,6’-OCTACHLOROBIPHENYL        |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4,4’,6,6’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4,5’,6-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4,5,5’-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4,5,6-HEXACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4,5,6,6’-HEPTACHLOROBIPHENYL          |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4,6’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,4,6,6’-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,5’,6-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    14 |    0 |          0 |    0 |     0 |
| 2,2’,3,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,5,5’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,5,6’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,5,6,6’-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,6’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,3,6,6’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,4-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,4,4’,5-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    14 |    0 |          0 |    0 |     0 |
| 2,2’,4,4’,5,6’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    14 |    0 |          0 |    0 |     0 |
| 2,2’,4,4’,6,6’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,4,5’,6-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,4,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,4,6,6’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,5,5’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    40 |  372 |          0 |    0 |     0 |
| 2,2’,6-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,2’,6,6’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3’-DICHLOROBIPHENYL                        |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3’,4-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3’,4,4’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3’,4,4’,5-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    41 |  372 |          0 |    0 |     0 |
| 2,3’,4,4’,5,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    41 |  372 |          0 |    0 |     0 |
| 2,3’,4,5’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3’,4,5’,6-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3’,4,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3’,4,5,5’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3’,5’-TRICHLOROBIPHENYL                    |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3’,5’,6-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3’,5,5’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3’,6-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3-DICHLOROBIPHENYL                         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4’,5’-PENTACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4’,5’,6-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4’,5,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4,4’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    41 |  372 |          0 |    0 |     0 |
| 2,3,3’,4,4’,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    12 |    0 |          0 |    0 |     0 |
| 2,3,3’,4,4’,5’,6-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4,4’,5-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    12 |    0 |          0 |    0 |     0 |
| 2,3,3’,4,4’,5,5’-HEPTACHLOROBIPHENYL         |   0 |  0 |     0 |    0 |    41 |  372 |          0 |    0 |     0 |
| 2,3,3’,4,4’,5,5’,6-OCTACHLOROBIPHENYL        |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4,4’,5,6-HEPTACHLOROBIPHENYL          |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4,4’,6-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4,5’,6-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4,5-PENTACHLOROBIPHENYL               |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4,5,5’-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4,5,5’,6-HEPTACHLOROBIPHENYL          |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,4,5,6-HEXACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    14 |    0 |          0 |    0 |     0 |
| 2,3,3’,4,6-PENTACHLOROBIPHENYL               |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,5’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,5,5’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,5,5’,6-HEXACHLOROBIPHENYL             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,3’,5,6-PENTACHLOROBIPHENYL               |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,4’-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,4’,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,4’,6-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,4,4’-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,4,4’,5-PENTACHLOROBIPHENYL               |   0 |  0 |     0 |    0 |    41 |  372 |          0 |    0 |     0 |
| 2,3,4,6,7,8-HXCDF                            |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 2,3,4,7,8-PECDF                              |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 2,3,5-TRICHLOROBIPHENYL                      |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,5-TRIMETHYLNAPHTHALENE                   |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| 2,3,6-TRICHLOROBIPHENYL                      |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,3,6-TRIMETHYLNAPHTHALENE                   |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 2,3,7,8-TCDD                                 |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 2,3,7,8-TETRACHLORODIBENZOFURAN              |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| 2,4’-DDD                                     |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| 2,4’-DDE                                     |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| 2,4’-DDT                                     |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| 2,4’-DICHLOROBIPHENYL                        |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,4’,5-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,4’,6-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,4-DICHLOROBIPHENYL                         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,4-DIMETHYLDIBENZOTHIOPHENE                 |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 2,5-DICHLOROBIPHENYL                         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,6-DICHLOROBIPHENYL                         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 2,6-DIMETHYLNAPHTHALENE                      |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| 2,6-DIMETHYLPHENANTHRENE                     |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 2/3-METHYLDIBENZOTHIOPHENES                  |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 3-CHLOROBIPHENYL                             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 3-METHYLFLUORANTHENE/BENZO\[A\]FLUORENE      |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 3-METHYLPHENANTHRENE                         |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 3,3’-DICHLOROBIPHENYL                        |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 3,3’,4-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 3,3’,4,4’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    41 |  372 |          0 |    0 |     0 |
| 3,3’,4,4’,5-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    41 |  372 |          0 |    0 |     0 |
| 3,3’,4,4’,5,5’-HEXACHLOROBIPHENYL            |   0 |  0 |     0 |    0 |    41 |  372 |          0 |    0 |     0 |
| 3,3’,4,5’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 3,3’,4,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 3,3’,4,5,5’-PENTACHLOROBIPHENYL              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 3,3’,5-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 3,3’,5,5’-TETRACHLOROBIPHENYL                |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 3,4’,5-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 3,4,4’-TRICHLOROBIPHENYL                     |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 3,4,4’,5-TETRACHLOROBIPHENYL                 |   0 |  0 |     0 |    0 |    41 |  372 |          0 |    0 |     0 |
| 3,4,5-TRICHLOROBIPHENYL                      |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 3,5-DICHLOROBIPHENYL                         |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 3,6-DIMETHYLPHENANTHRENE                     |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 4-CHLOROBIPHENYL                             |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 4,4’-DDD                                     |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| 4,4’-DDE                                     |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| 4,4’-DDT                                     |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| 4,4’-DICHLOROBIPHENYL                        |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| 5,9-DIMETHYLCHRYSENE                         |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 5/6-METHYLCHRYSENE                           |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 7-METHYLBENZO\[A\]PYRENE                     |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| 9/4-METHYLPHENANTHRENE                       |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| ACENAPHTHENE                                 |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| ACENAPHTHYLENE                               |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| ALACHLOR                                     |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| ALDRIN                                       |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| ALPHA-BHC                                    |   0 |  0 |     0 |  282 |    23 |    0 |          0 |    0 |     0 |
| ALPHA-CHLORDANE                              |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| ALUMINUM                                     |   0 |  0 |   184 |    0 |    16 |    0 |          0 |  138 |     0 |
| AMETRYN                                      |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| ANTHRACENE                                   |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| ARSENIC                                      |   0 |  0 |   154 |    0 |     0 |    0 |          0 |  138 |     0 |
| ATRAZINE                                     |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| AZINPHOS-METHYL                              |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| BENZO(A)ANTHRACENE                           |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| BENZO(A)PYRENE                               |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| BENZO(B)FLUORANTHENE                         |   0 |  0 |     0 |  102 |    16 |    0 |          0 |    0 |     0 |
| BENZO(E)PYRENE                               |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| BENZO(G,H,I)PERYLENE                         |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| BENZO(K)FLUORANTHENE                         |   0 |  0 |     0 |    0 |    16 |    0 |          0 |    0 |     0 |
| BENZO\[B,J,K\]FLUORANTHENES                  |   0 |  0 |     0 |  276 |     0 |    0 |          0 |    0 |     0 |
| BENZO\[J,K\]FLUORANTHENES                    |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| BENZOFLUORANTHENE                            |   0 |  0 |     0 |    0 |    12 |    0 |          0 |    0 |     0 |
| BETA-BHC                                     |   0 |  0 |     0 |  282 |    23 |    0 |          0 |    0 |     0 |
| BIPHENYL                                     |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| BUTRALIN                                     |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| BUTYLATE                                     |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| C1-ACENAPHTHENES                             |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C1-BENZO\[A\]ANTHRACENES/CHRYSENES           |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C1-BENZOFLUORANTHENES/BENZOPYRENES           |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C1-BIPHENYLS                                 |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C1-DIBENZOTHIOPHENES                         |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C1-FLUORANTHENES/PYRENES                     |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C1-FLUORENES                                 |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C1-NAPHTHALENES                              |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C1-PHENANTHRENES/ANTHRACENES                 |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C2-BENZO\[A\]ANTHRACENES/CHRYSENES           |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C2-BENZOFLUORANTHENES/BENZOPYRENES           |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C2-BIPHENYLS                                 |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C2-DIBENZOTHIOPHENES                         |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C2-FLUORANTHENES/PYRENES                     |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C2-FLUORENES                                 |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C2-NAPHTHALENES                              |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C2-PHENANTHRENES/ANTHRACENES                 |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C3-BENZO\[A\]ANTHRACENES/CHRYSENES           |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C3-DIBENZOTHIOPHENES                         |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C3-FLUORANTHENES/PYRENES                     |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C3-FLUORENES                                 |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C3-NAPHTHALENES                              |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C3-PHENANTHRENES/ANTHRACENES                 |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C4-BENZO\[A\]ANTHRACENES/CHRYSENES           |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C4-DIBENZOTHIOPHENES                         |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C4-FLUORANTHENES/PYRENES                     |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C4-NAPHTHALENES                              |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| C4-PHENANTHRENES/ANTHRACENES                 |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| CADMIUM                                      |   0 |  0 |   184 |    0 |    16 |    0 |          0 |  138 |     0 |
| CAPTAN                                       |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| CHLOROTHALONIL                               |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| CHLORPYRIFOS, O,O-DIMETHYL ANALOG            |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| CHLORPYRIPHOS                                |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| CHLORPYRIPHOS-OXON                           |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| CHROMIUM                                     |   0 |  0 |   184 |    0 |    16 |    0 |          0 |  138 |     0 |
| CHRYSENE                                     |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| CIS-NONACHLOR                                |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| COPPER                                       |   0 |  0 |   184 |    0 |    16 |    0 |          0 |  138 |     0 |
| CYANAZINE                                    |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| CYPERMETHRIN                                 |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| DCPA                                         |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| DECACHLOROBIPHENYL                           |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| DELTA-BHC                                    |   0 |  0 |     0 |  282 |    11 |    0 |          0 |    0 |     0 |
| DESETHYLATRAZINE                             |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| DIAZINON                                     |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| DIAZINON-OXON                                |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| DIBENZO(A,H)ANTHRACENE                       |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| DIBENZOTHIOPHENE                             |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| DIELDRIN                                     |   0 |  0 |     0 |  282 |    27 |    0 |          0 |    0 |     0 |
| DIMETHENAMID                                 |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| DIMETHOATE                                   |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| DISULFOTON                                   |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| DISULFOTON SULFONE                           |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| DX TOTAL TEQ (ND=0)                          |   0 |  0 |     0 |    0 |     0 |   12 |          0 |    0 |     0 |
| DX TOTAL TEQ (ND=1/2 DL)                     |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| DX TOTAL TEQ (ND=DL)                         |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| ENDOSULFAN I                                 |   0 |  0 |     0 |  282 |    27 |    0 |          0 |    0 |     0 |
| ENDOSULFAN II                                |   0 |  0 |     0 |  282 |    27 |    0 |          0 |    0 |     0 |
| ENDOSULFAN SULFATE                           |   0 |  0 |     0 |  282 |    11 |    0 |          0 |    0 |     0 |
| ENDRIN                                       |   0 |  0 |     0 |  282 |    11 |    0 |          0 |    0 |     0 |
| ENDRIN ALDEHYDE                              |   0 |  0 |     0 |  180 |    11 |    0 |          0 |    0 |     0 |
| ENDRIN KETONE                                |   0 |  0 |     0 |  282 |    11 |    0 |          0 |    0 |     0 |
| ETHALFLURALIN                                |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| ETHION                                       |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| FENITROTHION                                 |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| FLUFENACET                                   |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| FLUORANTHENE                                 |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| FLUORENE                                     |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| FLUTRIAFOL                                   |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| FONOFOS                                      |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| GAMMA-BHC (LINDANE)                          |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| GAMMA-CHLORDANE                              |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| HEPTACHLOR                                   |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| HEPTACHLOR EPOXIDE                           |   0 |  0 |     0 |  285 |    27 |    0 |          0 |    0 |     0 |
| HEXACHLOROBENZENE                            |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| INDENO(1,2,3-CD)PYRENE                       |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| IRON                                         |   0 |  0 |   184 |    0 |    16 |    0 |          0 |  138 |     0 |
| LEAD                                         |   0 |  0 |   184 |    0 |    16 |    0 |          0 |  138 |     0 |
| LINURON                                      |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| LIPIDS                                       | 296 |  0 |     0 |    0 |     0 |    0 |          0 |    0 |     0 |
| MALATHION                                    |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| MERCURY                                      |   0 |  0 |    22 |   30 |    18 |    0 |          0 |  142 |   132 |
| METHAMIDOPHOS                                |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| METHOPRENE                                   |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| METHOXYCHLOR                                 |   0 |  0 |     0 |  282 |    11 |    0 |          0 |    0 |     0 |
| METHYL PARATHION                             |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| METOLACHLOR                                  |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| METRIBUZIN                                   |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| MIREX                                        |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| MOISTURE                                     | 232 |  0 |     0 |    0 |     0 |    0 |          0 |    0 |     0 |
| Mytilus edulis                               |   0 |  0 |     0 |    0 |     0 |    0 |        232 |    0 |     0 |
| NAPHTHALENE                                  |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| NICKEL                                       |   0 |  0 |   184 |    0 |    16 |    0 |          0 |  138 |     0 |
| OCDD                                         |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| OCDF                                         |   0 |  0 |     0 |    0 |    27 |   12 |          0 |    0 |     0 |
| OCTACHLOROSTYRENE                            |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| OXYCHLORDANE                                 |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| PARATHION                                    |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| PCB-012/013                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-018/030                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-020/028                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-021/033                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-026/029                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-040/041/071                              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-044/047/065                              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-045/051                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-049/069                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-050/053                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-059/062/075                              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-061/070/074/076                          |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-083/099                                  |   0 |  0 |     0 |    0 |    15 |  372 |          0 |    0 |     0 |
| PCB-085/116/117                              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-086/087/097/108/119/125                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-088/091                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-090/101/113                              |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-093/095/098/100/102                      |   0 |  0 |     0 |    0 |    15 |  372 |          0 |    0 |     0 |
| PCB-093/098/100/102                          |   0 |  0 |     0 |    0 |    14 |    0 |          0 |    0 |     0 |
| PCB-107/124                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-110/115                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-128/166                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-129/138/160/163                          |   0 |  0 |     0 |    0 |    15 |  372 |          0 |    0 |     0 |
| PCB-129/138/163                              |   0 |  0 |     0 |    0 |    14 |    0 |          0 |    0 |     0 |
| PCB-134/143                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-135/151                                  |   0 |  0 |     0 |    0 |    14 |    0 |          0 |    0 |     0 |
| PCB-135/151/154                              |   0 |  0 |     0 |    0 |    15 |  372 |          0 |    0 |     0 |
| PCB-139/140                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-147/149                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-153/168                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-156/157                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-171/173                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-180/193                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-183/185                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-197/200                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB-198/199                                  |   0 |  0 |     0 |    0 |    29 |  372 |          0 |    0 |     0 |
| PCB TOTAL TEQ (ND=0)                         |   0 |  0 |     0 |    0 |    15 |  372 |          0 |    0 |     0 |
| PCB TOTAL TEQ (ND=1/2 DL)                    |   0 |  0 |     0 |    0 |    15 |  372 |          0 |    0 |     0 |
| PCB TOTAL TEQ (ND=DL)                        |   0 |  0 |     0 |    0 |    15 |  372 |          0 |    0 |     0 |
| PCBS                                         |   0 |  0 |     0 |    0 |    15 |  240 |          0 |    0 |     0 |
| PENDIMETHALIN                                |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| PENTACHLORONITROBENZENE                      |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROBUTANE SULFONATE                    |   0 |  0 |     0 |   92 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROBUTANOATE                           |   0 |  0 |     0 |   92 |     0 |    0 |          0 |    0 |     0 |
| PERFLUORODECANOATE                           |   0 |  0 |     0 |   92 |     0 |    0 |          0 |    0 |     0 |
| PERFLUORODODECANOATE                         |   0 |  0 |     0 |   92 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROHEPTANOATE                          |   0 |  0 |     0 |   92 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROHEXANE SULFONATE                    |   0 |  0 |     0 |   92 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROHEXANOATE                           |   0 |  0 |     0 |   92 |     0 |    0 |          0 |    0 |     0 |
| PERFLUORONONANOATE                           |   0 |  0 |     0 |   92 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROOCTANE SULFONAMIDE                  |   0 |  0 |     0 |   92 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROOCTANE SULFONATE                    |   0 |  0 |     0 |   92 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROOCTANOATE                           |   0 |  0 |     0 |   80 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROOCTANOIC ACID                       |   0 |  0 |     0 |   12 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROPENTANOATE                          |   0 |  0 |     0 |   92 |     0 |    0 |          0 |    0 |     0 |
| PERFLUOROUNDECANOATE                         |   0 |  0 |     0 |   92 |     0 |    0 |          0 |    0 |     0 |
| PERMETHRIN                                   |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| PERTHANE                                     |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| PERYLENE                                     |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| PHENANTHRENE                                 |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| PHORATE                                      |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| PHOSMET                                      |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| PIRIMIPHOS-METHYL                            |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| PYRENE                                       |   0 |  0 |     0 |  378 |    28 |    0 |          0 |    0 |     0 |
| RETENE                                       |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| SELENIUM                                     |   0 |  0 |   184 |    0 |    16 |    0 |          0 |  138 |     0 |
| SILVER                                       |   0 |  0 |   184 |    0 |    16 |    0 |          0 |  138 |     0 |
| SIMAZINE                                     |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| SOLIDS-TOTAL RESIDUE (TS)                    | 139 |  0 |     0 |    0 |     0 |    0 |          0 |    0 |     0 |
| TEBUCONAZOL                                  |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| TECNAZENE                                    |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| TERBUFOS                                     |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH-D                                  |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH-H                                  |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH-O                                  |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH19-D                                |   0 |  0 |     0 |  378 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH19-H                                |   0 |  0 |     0 |  378 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH19-O                                |   0 |  0 |     0 |  378 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH24-D                                |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH24-H                                |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH24-O                                |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH40-D                                |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH40-H                                |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PAH40-O                                |   0 |  0 |     0 |  240 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PCB-D                                  |   0 |  0 |     0 |    0 |    15 |  390 |          0 |    0 |     0 |
| TOTAL PCB-H                                  |   0 |  0 |     0 |    0 |    15 |  390 |          0 |    0 |     0 |
| TOTAL PCB-O                                  |   0 |  0 |     0 |    0 |    15 |  390 |          0 |    0 |     0 |
| TOTAL PESTICIDES21-D                         |   0 |  0 |     0 |  336 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PESTICIDES21-H                         |   0 |  0 |     0 |  336 |     0 |    0 |          0 |    0 |     0 |
| TOTAL PESTICIDES21-O                         |   0 |  0 |     0 |  336 |     0 |    0 |          0 |    0 |     0 |
| TRANS-NONACHLOR                              |   0 |  0 |     0 |  282 |    39 |    0 |          0 |    0 |     0 |
| TRIALLATE                                    |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| TRIFLURALIN                                  |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| VELPAR                                       |   0 |  0 |     0 |  102 |     0 |    0 |          0 |    0 |     0 |
| WEIGHT                                       |   0 | 94 |     0 |    0 |     0 |    0 |          0 |    0 |     0 |
| ZINC                                         |   0 |  0 |   184 |    0 |    16 |    0 |          0 |  138 |     0 |

So, lipids, moisture and solids are in percent metals are in mg/g Sample
weight is in g most organic contaminants are in ng/kg or pg/g Some
metals data in UG/G

So. lets put everything in ug/g

``` r
mytxt <- 'Abbrev, Multiplier
"G", NA
"MG/KG", 1
"NG/G",  0.001
"NG/KG", 0.000001
"PG/G",  1000000
"UG/KG", 1000
"UG/G",  1
"TRUE/FALSE", NA
"%", NA'

conversions <- read.csv(text = mytxt)
```

``` r
knitr::kable(xtabs(~ PARAMETER + `WEIGHT BASIS`, data = SWAT_data))
```

|                                              | DRY | LIP | WET |
| :------------------------------------------- | --: | --: | --: |
| 1-METHYL NAPHTHALENE                         | 126 | 126 | 154 |
| 1-METHYLCHRYSENE                             |  80 |  80 |  80 |
| 1-METHYLPHENANTHRENE                         | 126 | 126 | 154 |
| 1,2-DIMETHYLNAPHTHALENE                      |  80 |  80 |  80 |
| 1,2,3,4,6,7,8-HPCDD                          |   4 |   4 |  31 |
| 1,2,3,4,6,7,8-HPCDF                          |   4 |   4 |  31 |
| 1,2,3,4,7,8-HXCDD                            |   4 |   4 |  31 |
| 1,2,3,4,7,8-HXCDF                            |   4 |   4 |  31 |
| 1,2,3,4,7,8,9-HEPTACHLORODIBENZOFURAN(HPCDF) |   4 |   4 |  31 |
| 1,2,3,6,7,8-HXCDD                            |   4 |   4 |  31 |
| 1,2,3,6,7,8-HXCDF                            |   4 |   4 |  31 |
| 1,2,3,7,8-PECDD                              |   4 |   4 |  31 |
| 1,2,3,7,8-PECDF                              |   4 |   4 |  31 |
| 1,2,3,7,8,9-HXCDD                            |   4 |   4 |  31 |
| 1,2,3,7,8,9-HXCDF                            |   4 |   4 |  31 |
| 1,2,6-TRIMETHYLPHENANTHRENE                  |  80 |  80 |  80 |
| 1,4,6,7-TETRAMETHYLNAPHTHALENE               |  80 |  80 |  80 |
| 1,7-DIMETHYLFLUORENE                         |  80 |  80 |  80 |
| 1,7-DIMETHYLPHENANTHRENE                     |  80 |  80 |  80 |
| 1,8-DIMETHYLPHENANTHRENE                     |  80 |  80 |  80 |
| 2’,3,4,4’,5-PENTACHLOROBIPHENYL              | 124 | 124 | 165 |
| 2-CHLOROBIPHENYL                             | 124 | 124 | 153 |
| 2-METHYLANTHRACENE                           |  80 |  80 |  80 |
| 2-METHYLFLUORENE                             |  80 |  80 |  80 |
| 2-METHYLNAPHTHALENE                          | 126 | 126 | 154 |
| 2-METHYLPHENANTHRENE                         |  80 |  80 |  80 |
| 2,2’-DICHLOROBIPHENYL                        | 124 | 124 | 153 |
| 2,2’,3-TRICHLOROBIPHENYL                     | 124 | 124 | 153 |
| 2,2’,3,3’,4-PENTACHLOROBIPHENYL              | 124 | 124 | 153 |
| 2,2’,3,3’,4,4’,5-HEPTACHLOROBIPHENYL         | 124 | 124 | 165 |
| 2,2’,3,3’,4,4’,5,5’-OCTACHLOROBIPHENYL       | 124 | 124 | 153 |
| 2,2’,3,3’,4,4’,5,5’,6-NONACHLOROBIPHENYL     | 124 | 124 | 153 |
| 2,2’,3,3’,4,4’,5,6’-OCTACHLOROBIPHENYL       | 124 | 124 | 153 |
| 2,2’,3,3’,4,4’,5,6-OCTACHLOROBIPHENYL        | 124 | 124 | 153 |
| 2,2’,3,3’,4,4’,5,6,6’-NONACHLOROBIPHENYL     | 124 | 124 | 153 |
| 2,2’,3,3’,4,5’-HEXACHLOROBIPHENYL            | 124 | 124 | 153 |
| 2,2’,3,3’,4,5’,6’-HEPTACHLOROBIPHENYL        | 124 | 124 | 153 |
| 2,2’,3,3’,4,5’,6-HEPTACHLOROBIPHENYL         | 124 | 124 | 153 |
| 2,2’,3,3’,4,5’,6,6’-OCTACHLOROBIPHENYL       | 124 | 124 | 153 |
| 2,2’,3,3’,4,5,5’-HEPTACHLOROBIPHENYL         | 124 | 124 | 153 |
| 2,2’,3,3’,4,5,5’,6,6’-NONACHLOROBIPHENYL     | 124 | 124 | 153 |
| 2,2’,3,3’,4,5,6’-HEPTACHLOROBIPHENYL         | 124 | 124 | 153 |
| 2,2’,3,3’,4,6’-HEXACHLOROBIPHENYL            | 124 | 124 | 153 |
| 2,2’,3,3’,4,6-HEXACHLOROBIPHENYL             | 124 | 124 | 153 |
| 2,2’,3,3’,4,6,6’-HEPTACHLOROBIPHENYL         | 124 | 124 | 153 |
| 2,2’,3,3’,5-PENTACHLOROBIPHENYL              |   0 |   0 |  14 |
| 2,2’,3,3’,5,5’-HEXACHLOROBIPHENYL            | 124 | 124 | 153 |
| 2,2’,3,3’,5,5’,6-HEPTACHLOROBIPHENYL         | 124 | 124 | 153 |
| 2,2’,3,3’,5,5’,6,6’-OCTACHLOROBIPHENYL       | 124 | 124 | 153 |
| 2,2’,3,3’,5,6,6’-HEPTACHLOROBIPHENYL         | 124 | 124 | 153 |
| 2,2’,3,3’,6-PENTACHLOROBIPHENYL              | 124 | 124 | 153 |
| 2,2’,3,3’,6,6’-HEXACHLOROBIPHENYL            | 124 | 124 | 153 |
| 2,2’,3,4’-TETRACHLOROBIPHENYL                | 124 | 124 | 153 |
| 2,2’,3,4’,5,5’-HEXACHLOROBIPHENYL            | 124 | 124 | 153 |
| 2,2’,3,4’,5,5’,6-HEPTACHLOROBIPHENYL         | 124 | 124 | 153 |
| 2,2’,3,4’,5,6’-HEXACHLOROBIPHENYL            | 124 | 124 | 153 |
| 2,2’,3,4’,5,6,6’-HEPTACHLOROBIPHENYL         | 124 | 124 | 153 |
| 2,2’,3,4’,6,6’-HEXACHLOROBIPHENYL            | 124 | 124 | 153 |
| 2,2’,3,4,4’,5-HEXACHLOROBIPHENYL             | 124 | 124 | 153 |
| 2,2’,3,4,4’,5,5’-HEPTACHLOROBIPHENYL         |   0 |   0 |  12 |
| 2,2’,3,4,4’,5,5’,6-OCTACHLOROBIPHENYL        | 124 | 124 | 153 |
| 2,2’,3,4,4’,5,6’-HEPTACHLOROBIPHENYL         | 124 | 124 | 153 |
| 2,2’,3,4,4’,5,6-HEPTACHLOROBIPHENYL          | 124 | 124 | 153 |
| 2,2’,3,4,4’,5,6,6’-OCTACHLOROBIPHENYL        | 124 | 124 | 153 |
| 2,2’,3,4,4’,6,6’-HEPTACHLOROBIPHENYL         | 124 | 124 | 153 |
| 2,2’,3,4,5’,6-HEXACHLOROBIPHENYL             | 124 | 124 | 153 |
| 2,2’,3,4,5,5’-HEXACHLOROBIPHENYL             | 124 | 124 | 153 |
| 2,2’,3,4,5,6-HEXACHLOROBIPHENYL              | 124 | 124 | 153 |
| 2,2’,3,4,5,6,6’-HEPTACHLOROBIPHENYL          | 124 | 124 | 153 |
| 2,2’,3,4,6’-PENTACHLOROBIPHENYL              | 124 | 124 | 153 |
| 2,2’,3,4,6,6’-HEXACHLOROBIPHENYL             | 124 | 124 | 153 |
| 2,2’,3,5’,6-PENTACHLOROBIPHENYL              |   0 |   0 |  14 |
| 2,2’,3,5-TETRACHLOROBIPHENYL                 | 124 | 124 | 153 |
| 2,2’,3,5,5’-PENTACHLOROBIPHENYL              | 124 | 124 | 153 |
| 2,2’,3,5,6’-PENTACHLOROBIPHENYL              | 124 | 124 | 153 |
| 2,2’,3,5,6,6’-HEXACHLOROBIPHENYL             | 124 | 124 | 153 |
| 2,2’,3,6’-TETRACHLOROBIPHENYL                | 124 | 124 | 153 |
| 2,2’,3,6,6’-PENTACHLOROBIPHENYL              | 124 | 124 | 153 |
| 2,2’,4-TRICHLOROBIPHENYL                     | 124 | 124 | 153 |
| 2,2’,4,4’,5-PENTACHLOROBIPHENYL              |   0 |   0 |  14 |
| 2,2’,4,4’,5,6’-HEXACHLOROBIPHENYL            |   0 |   0 |  14 |
| 2,2’,4,4’,6,6’-HEXACHLOROBIPHENYL            | 124 | 124 | 153 |
| 2,2’,4,5’,6-PENTACHLOROBIPHENYL              | 124 | 124 | 153 |
| 2,2’,4,5-TETRACHLOROBIPHENYL                 | 124 | 124 | 153 |
| 2,2’,4,6,6’-PENTACHLOROBIPHENYL              | 124 | 124 | 153 |
| 2,2’,5,5’-TETRACHLOROBIPHENYL                | 124 | 124 | 164 |
| 2,2’,6-TRICHLOROBIPHENYL                     | 124 | 124 | 153 |
| 2,2’,6,6’-TETRACHLOROBIPHENYL                | 124 | 124 | 153 |
| 2,3’-DICHLOROBIPHENYL                        | 124 | 124 | 153 |
| 2,3’,4-TRICHLOROBIPHENYL                     | 124 | 124 | 153 |
| 2,3’,4,4’-TETRACHLOROBIPHENYL                | 124 | 124 | 153 |
| 2,3’,4,4’,5-PENTACHLOROBIPHENYL              | 124 | 124 | 165 |
| 2,3’,4,4’,5,5’-HEXACHLOROBIPHENYL            | 124 | 124 | 165 |
| 2,3’,4,5’-TETRACHLOROBIPHENYL                | 124 | 124 | 153 |
| 2,3’,4,5’,6-PENTACHLOROBIPHENYL              | 124 | 124 | 153 |
| 2,3’,4,5-TETRACHLOROBIPHENYL                 | 124 | 124 | 153 |
| 2,3’,4,5,5’-PENTACHLOROBIPHENYL              | 124 | 124 | 153 |
| 2,3’,5’-TRICHLOROBIPHENYL                    | 124 | 124 | 153 |
| 2,3’,5’,6-TETRACHLOROBIPHENYL                | 124 | 124 | 153 |
| 2,3’,5,5’-TETRACHLOROBIPHENYL                | 124 | 124 | 153 |
| 2,3’,6-TRICHLOROBIPHENYL                     | 124 | 124 | 153 |
| 2,3-DICHLOROBIPHENYL                         | 124 | 124 | 153 |
| 2,3,3’,4’-TETRACHLOROBIPHENYL                | 124 | 124 | 153 |
| 2,3,3’,4’,5’-PENTACHLOROBIPHENYL             | 124 | 124 | 153 |
| 2,3,3’,4’,5’,6-HEXACHLOROBIPHENYL            | 124 | 124 | 153 |
| 2,3,3’,4’,5,5’-HEXACHLOROBIPHENYL            | 124 | 124 | 153 |
| 2,3,3’,4-TETRACHLOROBIPHENYL                 | 124 | 124 | 153 |
| 2,3,3’,4,4’-PENTACHLOROBIPHENYL              | 124 | 124 | 165 |
| 2,3,3’,4,4’,5’-HEXACHLOROBIPHENYL            |   0 |   0 |  12 |
| 2,3,3’,4,4’,5’,6-HEPTACHLOROBIPHENYL         | 124 | 124 | 153 |
| 2,3,3’,4,4’,5-HEXACHLOROBIPHENYL             |   0 |   0 |  12 |
| 2,3,3’,4,4’,5,5’-HEPTACHLOROBIPHENYL         | 124 | 124 | 165 |
| 2,3,3’,4,4’,5,5’,6-OCTACHLOROBIPHENYL        | 124 | 124 | 153 |
| 2,3,3’,4,4’,5,6-HEPTACHLOROBIPHENYL          | 124 | 124 | 153 |
| 2,3,3’,4,4’,6-HEXACHLOROBIPHENYL             | 124 | 124 | 153 |
| 2,3,3’,4,5’,6-HEXACHLOROBIPHENYL             | 124 | 124 | 153 |
| 2,3,3’,4,5-PENTACHLOROBIPHENYL               | 124 | 124 | 153 |
| 2,3,3’,4,5,5’-HEXACHLOROBIPHENYL             | 124 | 124 | 153 |
| 2,3,3’,4,5,5’,6-HEPTACHLOROBIPHENYL          | 124 | 124 | 153 |
| 2,3,3’,4,5,6-HEXACHLOROBIPHENYL              |   0 |   0 |  14 |
| 2,3,3’,4,6-PENTACHLOROBIPHENYL               | 124 | 124 | 153 |
| 2,3,3’,5’-TETRACHLOROBIPHENYL                | 124 | 124 | 153 |
| 2,3,3’,5-TETRACHLOROBIPHENYL                 | 124 | 124 | 153 |
| 2,3,3’,5,5’-PENTACHLOROBIPHENYL              | 124 | 124 | 153 |
| 2,3,3’,5,5’,6-HEXACHLOROBIPHENYL             | 124 | 124 | 153 |
| 2,3,3’,5,6-PENTACHLOROBIPHENYL               | 124 | 124 | 153 |
| 2,3,4’-TRICHLOROBIPHENYL                     | 124 | 124 | 153 |
| 2,3,4’,5-TETRACHLOROBIPHENYL                 | 124 | 124 | 153 |
| 2,3,4’,6-TETRACHLOROBIPHENYL                 | 124 | 124 | 153 |
| 2,3,4,4’-TETRACHLOROBIPHENYL                 | 124 | 124 | 153 |
| 2,3,4,4’,5-PENTACHLOROBIPHENYL               | 124 | 124 | 165 |
| 2,3,4,6,7,8-HXCDF                            |   4 |   4 |  31 |
| 2,3,4,7,8-PECDF                              |   4 |   4 |  31 |
| 2,3,5-TRICHLOROBIPHENYL                      | 124 | 124 | 153 |
| 2,3,5-TRIMETHYLNAPHTHALENE                   | 126 | 126 | 154 |
| 2,3,6-TRICHLOROBIPHENYL                      | 124 | 124 | 153 |
| 2,3,6-TRIMETHYLNAPHTHALENE                   |  80 |  80 |  80 |
| 2,3,7,8-TCDD                                 |   4 |   4 |  31 |
| 2,3,7,8-TETRACHLORODIBENZOFURAN              |   4 |   4 |  31 |
| 2,4’-DDD                                     |  94 |  94 | 133 |
| 2,4’-DDE                                     |  94 |  94 | 133 |
| 2,4’-DDT                                     |  94 |  94 | 133 |
| 2,4’-DICHLOROBIPHENYL                        | 124 | 124 | 153 |
| 2,4’,5-TRICHLOROBIPHENYL                     | 124 | 124 | 153 |
| 2,4’,6-TRICHLOROBIPHENYL                     | 124 | 124 | 153 |
| 2,4-DICHLOROBIPHENYL                         | 124 | 124 | 153 |
| 2,4-DIMETHYLDIBENZOTHIOPHENE                 |  80 |  80 |  80 |
| 2,5-DICHLOROBIPHENYL                         | 124 | 124 | 153 |
| 2,6-DICHLOROBIPHENYL                         | 124 | 124 | 153 |
| 2,6-DIMETHYLNAPHTHALENE                      | 126 | 126 | 154 |
| 2,6-DIMETHYLPHENANTHRENE                     |  80 |  80 |  80 |
| 2/3-METHYLDIBENZOTHIOPHENES                  |  80 |  80 |  80 |
| 3-CHLOROBIPHENYL                             | 124 | 124 | 153 |
| 3-METHYLFLUORANTHENE/BENZO\[A\]FLUORENE      |  80 |  80 |  80 |
| 3-METHYLPHENANTHRENE                         |  80 |  80 |  80 |
| 3,3’-DICHLOROBIPHENYL                        | 124 | 124 | 153 |
| 3,3’,4-TRICHLOROBIPHENYL                     | 124 | 124 | 153 |
| 3,3’,4,4’-TETRACHLOROBIPHENYL                | 124 | 124 | 165 |
| 3,3’,4,4’,5-PENTACHLOROBIPHENYL              | 124 | 124 | 165 |
| 3,3’,4,4’,5,5’-HEXACHLOROBIPHENYL            | 124 | 124 | 165 |
| 3,3’,4,5’-TETRACHLOROBIPHENYL                | 124 | 124 | 153 |
| 3,3’,4,5-TETRACHLOROBIPHENYL                 | 124 | 124 | 153 |
| 3,3’,4,5,5’-PENTACHLOROBIPHENYL              | 124 | 124 | 153 |
| 3,3’,5-TRICHLOROBIPHENYL                     | 124 | 124 | 153 |
| 3,3’,5,5’-TETRACHLOROBIPHENYL                | 124 | 124 | 153 |
| 3,4’,5-TRICHLOROBIPHENYL                     | 124 | 124 | 153 |
| 3,4,4’-TRICHLOROBIPHENYL                     | 124 | 124 | 153 |
| 3,4,4’,5-TETRACHLOROBIPHENYL                 | 124 | 124 | 165 |
| 3,4,5-TRICHLOROBIPHENYL                      | 124 | 124 | 153 |
| 3,5-DICHLOROBIPHENYL                         | 124 | 124 | 153 |
| 3,6-DIMETHYLPHENANTHRENE                     |  80 |  80 |  80 |
| 4-CHLOROBIPHENYL                             | 124 | 124 | 153 |
| 4,4’-DDD                                     |  94 |  94 | 133 |
| 4,4’-DDE                                     |  94 |  94 | 133 |
| 4,4’-DDT                                     |  94 |  94 | 133 |
| 4,4’-DICHLOROBIPHENYL                        | 124 | 124 | 153 |
| 5,9-DIMETHYLCHRYSENE                         |  80 |  80 |  80 |
| 5/6-METHYLCHRYSENE                           |  80 |  80 |  80 |
| 7-METHYLBENZO\[A\]PYRENE                     |  80 |  80 |  80 |
| 9/4-METHYLPHENANTHRENE                       |  80 |  80 |  80 |
| ACENAPHTHENE                                 | 126 | 126 | 154 |
| ACENAPHTHYLENE                               | 126 | 126 | 154 |
| ALACHLOR                                     |  34 |  34 |  34 |
| ALDRIN                                       |  94 |  94 | 133 |
| ALPHA-BHC                                    |  94 |  94 | 117 |
| ALPHA-CHLORDANE                              |  94 |  94 | 133 |
| ALUMINUM                                     | 139 |  59 | 140 |
| AMETRYN                                      |  34 |  34 |  34 |
| ANTHRACENE                                   | 126 | 126 | 154 |
| ARSENIC                                      | 124 |  44 | 124 |
| ATRAZINE                                     |  34 |  34 |  34 |
| AZINPHOS-METHYL                              |  34 |  34 |  34 |
| BENZO(A)ANTHRACENE                           | 126 | 126 | 154 |
| BENZO(A)PYRENE                               | 126 | 126 | 154 |
| BENZO(B)FLUORANTHENE                         |  34 |  34 |  50 |
| BENZO(E)PYRENE                               | 126 | 126 | 154 |
| BENZO(G,H,I)PERYLENE                         | 126 | 126 | 154 |
| BENZO(K)FLUORANTHENE                         |   0 |   0 |  16 |
| BENZO\[B,J,K\]FLUORANTHENES                  |  92 |  92 |  92 |
| BENZO\[J,K\]FLUORANTHENES                    |  34 |  34 |  34 |
| BENZOFLUORANTHENE                            |   0 |   0 |  12 |
| BETA-BHC                                     |  94 |  94 | 117 |
| BIPHENYL                                     | 126 | 126 | 154 |
| BUTRALIN                                     |  34 |  34 |  34 |
| BUTYLATE                                     |  34 |  34 |  34 |
| C1-ACENAPHTHENES                             |  80 |  80 |  80 |
| C1-BENZO\[A\]ANTHRACENES/CHRYSENES           |  80 |  80 |  80 |
| C1-BENZOFLUORANTHENES/BENZOPYRENES           |  80 |  80 |  80 |
| C1-BIPHENYLS                                 |  80 |  80 |  80 |
| C1-DIBENZOTHIOPHENES                         |  80 |  80 |  80 |
| C1-FLUORANTHENES/PYRENES                     |  80 |  80 |  80 |
| C1-FLUORENES                                 |  80 |  80 |  80 |
| C1-NAPHTHALENES                              |  80 |  80 |  80 |
| C1-PHENANTHRENES/ANTHRACENES                 |  80 |  80 |  80 |
| C2-BENZO\[A\]ANTHRACENES/CHRYSENES           |  80 |  80 |  80 |
| C2-BENZOFLUORANTHENES/BENZOPYRENES           |  80 |  80 |  80 |
| C2-BIPHENYLS                                 |  80 |  80 |  80 |
| C2-DIBENZOTHIOPHENES                         |  80 |  80 |  80 |
| C2-FLUORANTHENES/PYRENES                     |  80 |  80 |  80 |
| C2-FLUORENES                                 |  80 |  80 |  80 |
| C2-NAPHTHALENES                              |  80 |  80 |  80 |
| C2-PHENANTHRENES/ANTHRACENES                 |  80 |  80 |  80 |
| C3-BENZO\[A\]ANTHRACENES/CHRYSENES           |  80 |  80 |  80 |
| C3-DIBENZOTHIOPHENES                         |  80 |  80 |  80 |
| C3-FLUORANTHENES/PYRENES                     |  80 |  80 |  80 |
| C3-FLUORENES                                 |  80 |  80 |  80 |
| C3-NAPHTHALENES                              |  80 |  80 |  80 |
| C3-PHENANTHRENES/ANTHRACENES                 |  80 |  80 |  80 |
| C4-BENZO\[A\]ANTHRACENES/CHRYSENES           |  80 |  80 |  80 |
| C4-DIBENZOTHIOPHENES                         |  80 |  80 |  80 |
| C4-FLUORANTHENES/PYRENES                     |  80 |  80 |  80 |
| C4-NAPHTHALENES                              |  80 |  80 |  80 |
| C4-PHENANTHRENES/ANTHRACENES                 |  80 |  80 |  80 |
| CADMIUM                                      | 139 |  59 | 140 |
| CAPTAN                                       |  34 |  34 |  34 |
| CHLOROTHALONIL                               |  34 |  34 |  34 |
| CHLORPYRIFOS, O,O-DIMETHYL ANALOG            |  34 |  34 |  34 |
| CHLORPYRIPHOS                                |  34 |  34 |  34 |
| CHLORPYRIPHOS-OXON                           |  34 |  34 |  34 |
| CHROMIUM                                     | 139 |  59 | 140 |
| CHRYSENE                                     | 126 | 126 | 154 |
| CIS-NONACHLOR                                |  34 |  34 |  34 |
| COPPER                                       | 139 |  59 | 140 |
| CYANAZINE                                    |  34 |  34 |  34 |
| CYPERMETHRIN                                 |  34 |  34 |  34 |
| DCPA                                         |  34 |  34 |  34 |
| DECACHLOROBIPHENYL                           | 124 | 124 | 153 |
| DELTA-BHC                                    |  94 |  94 | 105 |
| DESETHYLATRAZINE                             |  34 |  34 |  34 |
| DIAZINON                                     |  34 |  34 |  34 |
| DIAZINON-OXON                                |  34 |  34 |  34 |
| DIBENZO(A,H)ANTHRACENE                       | 126 | 126 | 154 |
| DIBENZOTHIOPHENE                             |  80 |  80 |  80 |
| DIELDRIN                                     |  94 |  94 | 121 |
| DIMETHENAMID                                 |  34 |  34 |  34 |
| DIMETHOATE                                   |  34 |  34 |  34 |
| DISULFOTON                                   |  34 |  34 |  34 |
| DISULFOTON SULFONE                           |  34 |  34 |  34 |
| DX TOTAL TEQ (ND=0)                          |   4 |   4 |   4 |
| DX TOTAL TEQ (ND=1/2 DL)                     |   4 |   4 |  31 |
| DX TOTAL TEQ (ND=DL)                         |   4 |   4 |  31 |
| ENDOSULFAN I                                 |  94 |  94 | 121 |
| ENDOSULFAN II                                |  94 |  94 | 121 |
| ENDOSULFAN SULFATE                           |  94 |  94 | 105 |
| ENDRIN                                       |  94 |  94 | 105 |
| ENDRIN ALDEHYDE                              |  60 |  60 |  71 |
| ENDRIN KETONE                                |  94 |  94 | 105 |
| ETHALFLURALIN                                |  34 |  34 |  34 |
| ETHION                                       |  34 |  34 |  34 |
| FENITROTHION                                 |  34 |  34 |  34 |
| FLUFENACET                                   |  34 |  34 |  34 |
| FLUORANTHENE                                 | 126 | 126 | 154 |
| FLUORENE                                     | 126 | 126 | 154 |
| FLUTRIAFOL                                   |  34 |  34 |  34 |
| FONOFOS                                      |  34 |  34 |  34 |
| GAMMA-BHC (LINDANE)                          |  94 |  94 | 133 |
| GAMMA-CHLORDANE                              |  94 |  94 | 133 |
| HEPTACHLOR                                   |  94 |  94 | 133 |
| HEPTACHLOR EPOXIDE                           |  95 |  95 | 122 |
| HEXACHLOROBENZENE                            |  94 |  94 | 133 |
| INDENO(1,2,3-CD)PYRENE                       | 126 | 126 | 154 |
| IRON                                         | 139 |  59 | 140 |
| LEAD                                         | 139 |  59 | 140 |
| LINURON                                      |  34 |  34 |  34 |
| LIPIDS                                       |   0 | 113 | 183 |
| MALATHION                                    |  34 |  34 |  34 |
| MERCURY                                      | 143 |  59 | 142 |
| METHAMIDOPHOS                                |  34 |  34 |  34 |
| METHOPRENE                                   |  34 |  34 |  34 |
| METHOXYCHLOR                                 |  94 |  94 | 105 |
| METHYL PARATHION                             |  34 |  34 |  34 |
| METOLACHLOR                                  |  34 |  34 |  34 |
| METRIBUZIN                                   |  34 |  34 |  34 |
| MIREX                                        |  94 |  94 | 133 |
| MOISTURE                                     |  11 |   0 | 221 |
| NAPHTHALENE                                  | 126 | 126 | 154 |
| NICKEL                                       | 139 |  59 | 140 |
| OCDD                                         |   4 |   4 |  31 |
| OCDF                                         |   4 |   4 |  31 |
| OCTACHLOROSTYRENE                            |  34 |  34 |  34 |
| OXYCHLORDANE                                 |  34 |  34 |  34 |
| PARATHION                                    |  34 |  34 |  34 |
| PCB-012/013                                  | 124 | 124 | 153 |
| PCB-018/030                                  | 124 | 124 | 153 |
| PCB-020/028                                  | 124 | 124 | 153 |
| PCB-021/033                                  | 124 | 124 | 153 |
| PCB-026/029                                  | 124 | 124 | 153 |
| PCB-040/041/071                              | 124 | 124 | 153 |
| PCB-044/047/065                              | 124 | 124 | 153 |
| PCB-045/051                                  | 124 | 124 | 153 |
| PCB-049/069                                  | 124 | 124 | 153 |
| PCB-050/053                                  | 124 | 124 | 153 |
| PCB-059/062/075                              | 124 | 124 | 153 |
| PCB-061/070/074/076                          | 124 | 124 | 153 |
| PCB-083/099                                  | 124 | 124 | 139 |
| PCB-085/116/117                              | 124 | 124 | 153 |
| PCB-086/087/097/108/119/125                  | 124 | 124 | 153 |
| PCB-088/091                                  | 124 | 124 | 153 |
| PCB-090/101/113                              | 124 | 124 | 153 |
| PCB-093/095/098/100/102                      | 124 | 124 | 139 |
| PCB-093/098/100/102                          |   0 |   0 |  14 |
| PCB-107/124                                  | 124 | 124 | 153 |
| PCB-110/115                                  | 124 | 124 | 153 |
| PCB-128/166                                  | 124 | 124 | 153 |
| PCB-129/138/160/163                          | 124 | 124 | 139 |
| PCB-129/138/163                              |   0 |   0 |  14 |
| PCB-134/143                                  | 124 | 124 | 153 |
| PCB-135/151                                  |   0 |   0 |  14 |
| PCB-135/151/154                              | 124 | 124 | 139 |
| PCB-139/140                                  | 124 | 124 | 153 |
| PCB-147/149                                  | 124 | 124 | 153 |
| PCB-153/168                                  | 124 | 124 | 153 |
| PCB-156/157                                  | 124 | 124 | 153 |
| PCB-171/173                                  | 124 | 124 | 153 |
| PCB-180/193                                  | 124 | 124 | 153 |
| PCB-183/185                                  | 124 | 124 | 153 |
| PCB-197/200                                  | 124 | 124 | 153 |
| PCB-198/199                                  | 124 | 124 | 153 |
| PCB TOTAL TEQ (ND=0)                         | 124 | 124 | 139 |
| PCB TOTAL TEQ (ND=1/2 DL)                    | 124 | 124 | 139 |
| PCB TOTAL TEQ (ND=DL)                        | 124 | 124 | 139 |
| PCBS                                         |  80 |  80 |  95 |
| PENDIMETHALIN                                |  34 |  34 |  34 |
| PENTACHLORONITROBENZENE                      |  34 |  34 |  34 |
| PERFLUOROBUTANE SULFONATE                    |  32 |  28 |  32 |
| PERFLUOROBUTANOATE                           |  32 |  28 |  32 |
| PERFLUORODECANOATE                           |  32 |  28 |  32 |
| PERFLUORODODECANOATE                         |  32 |  28 |  32 |
| PERFLUOROHEPTANOATE                          |  32 |  28 |  32 |
| PERFLUOROHEXANE SULFONATE                    |  32 |  28 |  32 |
| PERFLUOROHEXANOATE                           |  32 |  28 |  32 |
| PERFLUORONONANOATE                           |  32 |  28 |  32 |
| PERFLUOROOCTANE SULFONAMIDE                  |  32 |  28 |  32 |
| PERFLUOROOCTANE SULFONATE                    |  32 |  28 |  32 |
| PERFLUOROOCTANOATE                           |  28 |  24 |  28 |
| PERFLUOROOCTANOIC ACID                       |   4 |   4 |   4 |
| PERFLUOROPENTANOATE                          |  32 |  28 |  32 |
| PERFLUOROUNDECANOATE                         |  32 |  28 |  32 |
| PERMETHRIN                                   |  34 |  34 |  34 |
| PERTHANE                                     |  34 |  34 |  34 |
| PERYLENE                                     | 126 | 126 | 154 |
| PHENANTHRENE                                 | 126 | 126 | 154 |
| PHORATE                                      |  34 |  34 |  34 |
| PHOSMET                                      |  34 |  34 |  34 |
| PIRIMIPHOS-METHYL                            |  34 |  34 |  34 |
| PYRENE                                       | 126 | 126 | 154 |
| RETENE                                       |  80 |  80 |  80 |
| SELENIUM                                     | 139 |  59 | 140 |
| SILVER                                       | 139 |  59 | 140 |
| SIMAZINE                                     |  34 |  34 |  34 |
| SOLIDS-TOTAL RESIDUE (TS)                    |  95 |   0 |  44 |
| TEBUCONAZOL                                  |  34 |  34 |  34 |
| TECNAZENE                                    |  34 |  34 |  34 |
| TERBUFOS                                     |  34 |  34 |  34 |
| TOTAL PAH-D                                  |  80 |  80 |  80 |
| TOTAL PAH-H                                  |  80 |  80 |  80 |
| TOTAL PAH-O                                  |  80 |  80 |  80 |
| TOTAL PAH19-D                                | 126 | 126 | 126 |
| TOTAL PAH19-H                                | 126 | 126 | 126 |
| TOTAL PAH19-O                                | 126 | 126 | 126 |
| TOTAL PAH24-D                                |  80 |  80 |  80 |
| TOTAL PAH24-H                                |  80 |  80 |  80 |
| TOTAL PAH24-O                                |  80 |  80 |  80 |
| TOTAL PAH40-D                                |  80 |  80 |  80 |
| TOTAL PAH40-H                                |  80 |  80 |  80 |
| TOTAL PAH40-O                                |  80 |  80 |  80 |
| TOTAL PCB-D                                  | 130 | 130 | 145 |
| TOTAL PCB-H                                  | 130 | 130 | 145 |
| TOTAL PCB-O                                  | 130 | 130 | 145 |
| TOTAL PESTICIDES21-D                         | 112 | 112 | 112 |
| TOTAL PESTICIDES21-H                         | 112 | 112 | 112 |
| TOTAL PESTICIDES21-O                         | 112 | 112 | 112 |
| TRANS-NONACHLOR                              |  94 |  94 | 133 |
| TRIALLATE                                    |  34 |  34 |  34 |
| TRIFLURALIN                                  |  34 |  34 |  34 |
| VELPAR                                       |  34 |  34 |  34 |
| WEIGHT                                       |   0 |   0 |  94 |
| ZINC                                         | 139 |  59 | 140 |

# Real Sample Numbers?

Question; I KNOW We don’t have 372 separate samples, so how did we come
up with 372 entries under “COMPOSITE SAMPLE” for many PAHs?
