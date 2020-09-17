Review of Maine DEP EGAD Mussel Tissue Toxics Data Codes
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
9/10/2020

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder Reference](#establish-folder-reference)
      - [Copy Data](#copy-data)
      - [Remove duplicates](#remove-duplicates)
      - [How many duplicates?](#how-many-duplicates)
      - [Remove all Complete Duplicate Data
        Rows](#remove-all-complete-duplicate-data-rows)
  - [Exploration of Data Codes](#exploration-of-data-codes)
      - [Uninformative Codes](#uninformative-codes)
      - [Possibly Informative Codes](#possibly-informative-codes)

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

In this Notebook we review the structure of the data to determine which
data columns contain useful information, and derive rules for working
with the data.

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

## How many duplicates?

Let’s generate a data subset including all complete duplicates, and
evaluate its size.

``` r
dups <- SWAT_data[duplicated(SWAT_data),]
cat(round(nrow(dups)/nrow(SWAT_data) * 100,1),
    " percent of observations are duplicates.")
```

    ## 17.3  percent of observations are duplicates.

``` r
rm(dups)
```

## Remove all Complete Duplicate Data Rows

``` r
SWAT_data <- unique(SWAT_data)
```

``` r
(after <- nrow(SWAT_data))
```

    ## [1] 93357

``` r
cat('We retained ', round(after/before,3)*100,
    ' percent of rows in the original data.')
```

    ## We retained  82.7  percent of rows in the original data.

# Exploration of Data Codes

## Uninformative Codes

### What is `Sample_ID`

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

So, SAMPLE\_ID combines a LOT of info that is mostly also contained in
other data. Unfortunately, as we will see below, it is NOT a unique
sample identifier.

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
    ## 1 Mytilus edulis PHYSICAL CHARACTERISTIC  166

So the ONLY parameter ever included under ‘PHYSICAL CHARACTERISTIC’ is
the species of shellfish sampled. Thus “SAMPLE TYPES” conveys no
independent information, and so is of little use.

### What is `SAMPLE POINT TYPE`?

``` r
unique(SWAT_data$`SAMPLE POINT TYPE`)
```

    ## [1] "MARINE"

`SAMPLE POINT TYPE` contains only a single value so it is useless.

### What is `SAMPLE LOCATION`?

``` r
unique(SWAT_data$`SAMPLE LOCATION`)
```

    ## [1] "WADING"         "NOT APPLICABLE"

Again, no useful information here.

### What is `RESULT TYPE`?

``` r
unique(SWAT_data$`RESULT TYPE`)
```

    ## [1] "TARGET/REGULAR RESULT" "PHYSICAL MEASUREMENT"

``` r
SWAT_data %>%
  select(`RESULT TYPE`, PARAMETER) %>%
  filter(`RESULT TYPE`=="PHYSICAL MEASUREMENT") %>%
  select (-`RESULT TYPE`) %>%
  unique()
```

    ## # A tibble: 1 x 1
    ##   PARAMETER     
    ##   <chr>         
    ## 1 Mytilus edulis

So that is uninformative too. It mirrors SAMPLE TYPE. The only item ever
flagged as a “PHYSICAL MEASUREMENT” is is the species of shellfish.

### What is `PARAMETER_QUALIFIER`?

``` r
unique(SWAT_data$`PARAMETER_QUALIFIER`)
```

    ## [1] NA         "column 1" "column 2" "column"

This does not appear to be informative, but we learn later that there
are some places where this column is the ONLY difference between what
are otherwise apparently identical data rows.

### What are `PARAMETER FILTERED` and `SAMPLE FILTER`?

``` r
xtabs(~ `PARAMETER FILTERED` + `SAMPLE FILTER`, data = SWAT_data, addNA = TRUE)
```

    ##                   SAMPLE FILTER
    ## PARAMETER FILTERED  <NA>
    ##     NOT APPLICABLE 11266
    ##     UNFILTERED       286
    ##     <NA>           81805

SO both are uninformative. Of course, tissue samples are not filtered.

### What are `DEPTH` and `DEPTH UNITS`?

``` r
xtabs( ~ DEPTH + `DEPTH UNITS`, data = SWAT_data, addNA = TRUE)
```

    ##       DEPTH UNITS
    ## DEPTH   <NA>
    ##   <NA> 93357

Samples were picked up from the shore, so depth is irrelevant.

### What is `TREATMENT`?

``` r
unique(SWAT_data$`TREATMENT`)
```

    ## [1] "NOT APPLICABLE" NA

These are observational data.

### What is `METER_CALIBRATED`?

``` r
unique(SWAT_data$`METER_CALIBRATED`)
```

    ## [1] NA

Samples were collected, and the data provided to us does not include any
*in situ* ancillary data like temperature or dissolved oxygen, so no
data was collected with instruments that require calibration in the
field.

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
    ##  5 REP 1      13908
    ##  6 REP 1 2010   873
    ##  7 REP 1 2011  1746
    ##  8 REP 1 2012   880
    ##  9 REP 1 2013   830
    ## 10 REP 1 2014  2452
    ## # ... with 30 more rows

These names are meaningful, but difficult to use. They are used
consistently to distinguish among samples, and are connected to the
spatial data, but SAMPLE POINT names are not unique, and can be
duplicated between different sites or within one Sites in different
years. See the discussion of Sites and spatial data, below.

### What is `PREP METHOD`?

``` r
unique(SWAT_data$`PREP METHOD`)
```

    ## [1] NA           "E1668A"     "MLA-007-E2" "MLA-007-E1" "NOAA1998M" 
    ## [6] "SW3051A"    "E1631E"     "METHOD"     "E1613B"

So,potentially useful info, if I knew what the codes mean. For now, we
largely ignore these details as not relevant for our interpretation of
results at this time. But we may need to evaluate that assumption later.

### What is `SAMPLE COLLECTION METHOD`?

``` r
unique(SWAT_data$`SAMPLE COLLECTION METHOD`)
```

    ## [1] "COMPOSITE SAMPLE" "HAND-PICKED"

Although this suggests this Code is valuable, it’s not obvious what it
means from internal data alone, or whether it has been applied fully
consistently. We know from other information that some studies (e.g.,
Gulfwatch) explicitly uses *composite samples* from twenty *hand picked*
mussels. So both terms appear to apply. We dig in a little deeper.

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
    ##  1 2,2',3,3',5-PENTACHLOROBIPHENYL                       0             7
    ##  2 2,2',3,4,4',5,5'-HEPTACHLOROBIPHENYL                  0             8
    ##  3 2,2',3,5',6-PENTACHLOROBIPHENYL                       0             7
    ##  4 2,2',4,4',5-PENTACHLOROBIPHENYL                       0             7
    ##  5 2,2',4,4',5,6'-HEXACHLOROBIPHENYL                     0             7
    ##  6 2,3,3',4,4',5'-HEXACHLOROBIPHENYL                     0             8
    ##  7 2,3,3',4,4',5-HEXACHLOROBIPHENYL                      0             8
    ##  8 2,3,3',4,5,6-HEXACHLOROBIPHENYL                       0             7
    ##  9 BENZO(K)FLUORANTHENE                                  0             8
    ## 10 PCB-093/098/100/102                                   0             7
    ## # ... with 389 more rows

So, other than the fact that many of the pesticide data are derived only
from composite samples, and some PCBs (by chemical name) appear only
from ‘hand picked’ samples, it appears that these represent different
sampling strategies, or at least different labels for sample collection
strategies employed during sample collection.

Curiously, the species identification ‘*Mytilus edulis*’ is only
included in the ‘COMPOSITE SAMPLE’ category 7 times, MUCH less
frequently than some other parameters. That appears counter-intuitive.
How can we have more samples for parameters than samples of mussels?

This points not only to problems with how this Code was assigned, but
also to a couple of things will be explored find later:  
1\. Some samples are reported three times, on different weight bases, so
we get each parameter three times for each physical sample.  
2\. There are some laboratory duplicates, again, representing physical
samples multiple times.  
3\. Some samples are reported – apparently duplicated – as coming from
different laboratories or different methods, but with no other
differences.

Still, it appears this Code is inconsistent.

#### Does `SAMPLE COLLECTION METHOD` Break Out by Site?

``` r
xtabs(~EGAD_SITE_NAME + `SAMPLE COLLECTION METHOD`, data = SWAT_data) %>%
  as.tibble() %>%
  pivot_wider(id_cols = EGAD_SITE_NAME,
              names_from = `SAMPLE COLLECTION METHOD`,
              values_from = n) %>%
  arrange(`COMPOSITE SAMPLE`)
```

    ## # A tibble: 20 x 3
    ##    EGAD_SITE_NAME                              `COMPOSITE SAMPLE` `HAND-PICKED`
    ##    <chr>                                                    <int>         <int>
    ##  1 BACK BAY - CBBBBB                                            0           778
    ##  2 COCKTAIL COVE GREAT DIAMOND ISLAND - CBGDCC                  0           757
    ##  3 FORE RIVER OUTER - CBFROR                                    0           965
    ##  4 ROYAL RIVER MOUTH - CBRYMT                                   0           160
    ##  5 SOUTHWEST END GREAT DIAMOND ISLAND - CBGDSW                  0           973
    ##  6 FALMOUTH ANCHORAGE - CBANAN                               2796             4
    ##  7 HARASEEKET RIVER - CBHRHR                                 2796             8
    ##  8 JEWEL ISLAND PUNCHBOWL - CBJWPB                           2796             4
    ##  9 MIDDLE BAY (OUTER) - CBMBMB                               2823             4
    ## 10 MIDDLE FORE RIVER - CBFRMR                                2988             4
    ## 11 PRESUMPSCOT RIVER (MOUTH)  - CBPRMT                       3209             4
    ## 12 NAVY PIER - CBHWNP                                        3320             4
    ## 13 INNER FORE RIVER - CBFRIR                                 4092             4
    ## 14 LONG ISLAND - CBLNFT                                      4092             4
    ## 15 MAQUOIT BAY - CBMBBR                                      4092             4
    ## 16 QUAHOG BAY - CBQHQH                                       4101             4
    ## 17 BRUNSWICK MARE BROOK DRAINAGE - CBMBBH                    6134           120
    ## 18 S PORTLAND SPRING POINT - CBSPSP                          9364          3607
    ## 19 MILL CREEK - CBMCMC                                      13824            18
    ## 20 EAST END BEACH - CBEEEE                                  19384           120

So this ALMOST lines up. With a few exceptions, all or almost all data
from each site is flagged one way or the other.

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
    ##  1 2003-10-01                   0           264
    ##  2 2003-10-16                   0           159
    ##  3 2003-11-04                   0            72
    ##  4 2003-11-17                   0          1603
    ##  5 2006-10-12                  58             3
    ##  6 2006-10-17                   0           757
    ##  7 2006-10-30                 663             4
    ##  8 2006-11-29                   0           778
    ##  9 2007-10-18                2944             8
    ## 10 2007-10-22                5444             8
    ## # ... with 22 more rows

Again, ALMOST all samples from each date are flagged one way or the
other. This suggests there may be some inconsistency with how this Code
has been implied.

#### When is “HAND PICKED” used?

But how can we figure out what these sample collection methods mean? We
know the Sample\_Collection\_Method for Gulfwatch was a composite of 20
mussels. Gulfwatch used the sample code “MEPH” for a site in “Portland
Harbor”. The related EGAD\_SITE\_NAME is ‘FORE RIVER OUTER - CBFROR’.

``` r
SWAT_data %>% filter(EGAD_SITE_NAME== 'FORE RIVER OUTER - CBFROR') %>%
  select(c(2, 13, 7, 4, 8)) %>%
  unique()
```

    ## # A tibble: 16 x 5
    ##    EGAD_SITE_NAME SAMPLE_DATE         `SAMPLE COLLECT~ CURRENT_SAMPLE_~
    ##    <chr>          <dttm>              <chr>            <chr>           
    ##  1 FORE RIVER OU~ 2003-10-16 00:00:00 HAND-PICKED      2N              
    ##  2 FORE RIVER OU~ 2003-10-16 00:00:00 HAND-PICKED      4N              
    ##  3 FORE RIVER OU~ 2003-10-16 00:00:00 HAND-PICKED      1N              
    ##  4 FORE RIVER OU~ 2003-10-16 00:00:00 HAND-PICKED      1N              
    ##  5 FORE RIVER OU~ 2003-10-16 00:00:00 HAND-PICKED      2N              
    ##  6 FORE RIVER OU~ 2003-10-16 00:00:00 HAND-PICKED      3N              
    ##  7 FORE RIVER OU~ 2003-10-16 00:00:00 HAND-PICKED      3N              
    ##  8 FORE RIVER OU~ 2003-11-17 00:00:00 HAND-PICKED      REP 2           
    ##  9 FORE RIVER OU~ 2003-11-17 00:00:00 HAND-PICKED      REP 3           
    ## 10 FORE RIVER OU~ 2003-10-16 00:00:00 HAND-PICKED      4N              
    ## 11 FORE RIVER OU~ 2003-11-17 00:00:00 HAND-PICKED      REP 1           
    ## 12 FORE RIVER OU~ 2003-10-01 00:00:00 HAND-PICKED      REP 1           
    ## 13 FORE RIVER OU~ 2003-10-01 00:00:00 HAND-PICKED      REP 2           
    ## 14 FORE RIVER OU~ 2003-11-17 00:00:00 HAND-PICKED      REP 4           
    ## 15 FORE RIVER OU~ 2003-10-01 00:00:00 HAND-PICKED      REP 3           
    ## 16 FORE RIVER OU~ 2003-10-01 00:00:00 HAND-PICKED      REP 4           
    ## # ... with 1 more variable: SAMPLE_ID <chr>

So this site, which follows the Gulfwatch methods is flagged
consistently as “HAND-PICKED”.

#### Conclusion – This is Confusing

This is confusing – it’s not clear what the different sample collection
codes mean. They clearly segregate to some extent by sample events, but
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
    ##        *          0      0     0     0     0     8
    ##        B       3435      0     0     0     0 21986
    ##        B/EMPC     0    258     0     0     0  1329
    ##        BL         0      0     0     0     0   130
    ##        E*         0      0     0     0     0     9
    ##        EMPC       0      0   534     0     0  4583
    ##        J          0      0     0    34     0     4
    ##        N          0      0     0     0     0     1
    ##        NC         0      0     0     0     0     9
    ##        NQ         0      0     0     0     0    99
    ##        T          0      0     0     0     0    75
    ##        T/EMPC     0      0     0     0     0     6
    ##        U          0      0     0     0   859 17342
    ##        UT         0      0     0     0     0     6
    ##        <NA>       0      0     0     0     0 42650

Interestingly, relatively few of the Lab Qualifiers were pulled out in
the Validation Qualifiers. Why?

``` r
SWAT_data %>% 
  select(`LAB QUALIFIER`, `VALIDATION QUALIFIER`, CONCENTRATION, RL, MDL) %>%
  group_by(`LAB QUALIFIER`, `VALIDATION QUALIFIER`) %>%
  summarize(has_data = any(! is.na(CONCENTRATION)),
            all_data = all(! is.na(CONCENTRATION)),
            has_RL   = any(! is.na(RL)),
            all_RL   = all(! is.na(RL)),
            has_MDL  = any(! is.na(MDL)),
            all_MDL  = all(! is.na(MDL))
            )
```

    ## `summarise()` regrouping output by 'LAB QUALIFIER' (override with `.groups` argument)

    ## # A tibble: 20 x 8
    ## # Groups:   LAB QUALIFIER [15]
    ##    `LAB QUALIFIER` `VALIDATION QUA~ has_data all_data has_RL all_RL has_MDL
    ##    <chr>           <chr>            <lgl>    <lgl>    <lgl>  <lgl>  <lgl>  
    ##  1 *               <NA>             TRUE     TRUE     FALSE  FALSE  FALSE  
    ##  2 B               B                TRUE     TRUE     TRUE   TRUE   TRUE   
    ##  3 B               <NA>             TRUE     TRUE     TRUE   FALSE  TRUE   
    ##  4 B/EMPC          B/EMPC           TRUE     TRUE     TRUE   TRUE   TRUE   
    ##  5 B/EMPC          <NA>             TRUE     TRUE     TRUE   FALSE  TRUE   
    ##  6 BL              <NA>             TRUE     TRUE     TRUE   TRUE   TRUE   
    ##  7 E*              <NA>             TRUE     TRUE     FALSE  FALSE  FALSE  
    ##  8 EMPC            EMPC             TRUE     TRUE     TRUE   TRUE   TRUE   
    ##  9 EMPC            <NA>             TRUE     TRUE     TRUE   FALSE  TRUE   
    ## 10 J               J                TRUE     TRUE     TRUE   TRUE   TRUE   
    ## 11 J               <NA>             TRUE     TRUE     FALSE  FALSE  FALSE  
    ## 12 N               <NA>             TRUE     TRUE     FALSE  FALSE  FALSE  
    ## 13 NC              <NA>             TRUE     TRUE     TRUE   TRUE   TRUE   
    ## 14 NQ              <NA>             FALSE    FALSE    FALSE  FALSE  FALSE  
    ## 15 T               <NA>             TRUE     TRUE     TRUE   TRUE   TRUE   
    ## 16 T/EMPC          <NA>             TRUE     TRUE     TRUE   TRUE   TRUE   
    ## 17 U               U                FALSE    FALSE    TRUE   TRUE   TRUE   
    ## 18 U               <NA>             FALSE    FALSE    TRUE   TRUE   TRUE   
    ## 19 UT              <NA>             FALSE    FALSE    TRUE   TRUE   TRUE   
    ## 20 <NA>            <NA>             TRUE     TRUE     TRUE   FALSE  TRUE   
    ## # ... with 1 more variable: all_MDL <lgl>

We generally know how to handle the `VALIDATION QUALIFIERS`

  - U codes we treat as right censored.  
  - J codes are estimated values. We usually include those values in
    analyses, but they may have higher error. We do not explicitly model
    that. Also in these data they do not have associated detection
    limits.  
  - B codes have different meanings for organic and inorganic compounds,
    but generally they include values and and detection limits.
  - EMPC and B/EMPC are Estimated Maximum Possible Concentrations. We
    typically will handle these simply as observations, but we might
    want to treat them as right censored instead, especially if they are
    abundant.

### What is `WEIGHT BASIS`?

``` r
unique(SWAT_data$`WEIGHT BASIS`)
```

    ## [1] "LIP" "WET" "DRY" NA

This is explored in detail in a later notebook.

### What is `DILUTION FACTOR`?

``` r
kable(xtabs(~as.numeric(SWAT_data$`DILUTION_FACTOR`)),
      col.names =c("Dilution", "Frequency"))
```

| Dilution | Frequency |
| :------- | --------: |
| 1        |     83431 |
| 2        |       160 |
| 3        |        12 |
| 4        |       267 |
| 5        |       204 |
| 9        |        21 |
| 10       |       106 |
| 20       |        12 |
| 50       |        10 |
| 57       |         3 |
| 61       |         3 |
| 62       |         6 |
| 74       |         3 |
| 81       |         3 |
| 83       |         3 |
| 85       |         3 |

While this is good to know, it is not clear how we use it in our
analyses, as it is our understanding that the CONCENTRATION values are
already adjusted for dilution.
