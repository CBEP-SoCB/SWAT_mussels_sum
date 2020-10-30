Review of Maine DEP EGAD Mussel Tissue Toxics Parameters
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
9/18/2020

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder Reference](#establish-folder-reference)
      - [Copy Data](#copy-data)
      - [Remove duplicates](#remove-duplicates)
      - [Simplify Data and Add Unique Sample
        Codes](#simplify-data-and-add-unique-sample-codes)
      - [Add Class to the Working Simplified
        Data](#add-class-to-the-working-simplified-data)
  - [PCB Nomenclature](#pcb-nomenclature)
      - [Examining CAS numbers](#examining-cas-numbers)
  - [When Does Each Total Exist?](#when-does-each-total-exist)

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

Unfortunately, the data delivery contains limited metadata, so it takes
some effort to understand the data format and analyze it correctly.

In this notebook we review the parameters identified in the data and
assign them to groups to facilitate analysis. We also check on a number
of technical matters dealing with how various totals were calculated.
These include: \* Figuring out which compounds were included in various
totals \* Determining how NOn-detects were included in totals

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
library(htmltools)  # used by knitr called here only to avoid startup text later
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

## Add Class to the Working Simplified Data

We can then read in the resulting Excel File to provide groupings…

``` r
Parameter_List <- read_excel(file.path(parent,"Parameter List.xlsx"), 
    sheet = "Parameter List") %>%
  mutate(Class = factor(Class)) %>%
  arrange(Class, PARAMETER)

SWAT_simplified <- SWAT_simplified %>% 
  mutate(Class = Parameter_List$Class[match(PARAMETER, Parameter_List$PARAMETER)])
```

# PCB Nomenclature

Many of the PCBs chemical names and the PCB numerical designations turn
up in the same number of composite samples, which suggests they are
reported on the same samples. I wonder if these represent duplicate
data.

## Examining CAS numbers

Lets see how many times we have duplicate values with the same CAS
number.

``` r
SWAT_simplified %>% 
 # filter(`WEIGHT BASIS` == 'LIP') %>%
  group_by(CAS_NO) %>%
  filter(grepl('PCB', Class)) %>%
  summarize(nnames = length(unique(PARAMETER)),
            name = first (PARAMETER),
            .groups = 'drop') %>%
  arrange(name)
```

    ## # A tibble: 178 x 3
    ##    CAS_NO   nnames name                                    
    ##    <chr>     <int> <chr>                                   
    ##  1 65510443      1 2',3,4,4',5-PENTACHLOROBIPHENYL         
    ##  2 2051607       1 2-CHLOROBIPHENYL                        
    ##  3 13029088      1 2,2'-DICHLOROBIPHENYL                   
    ##  4 38444789      1 2,2',3-TRICHLOROBIPHENYL                
    ##  5 52663624      1 2,2',3,3',4-PENTACHLOROBIPHENYL         
    ##  6 35065306      1 2,2',3,3',4,4',5-HEPTACHLOROBIPHENYL    
    ##  7 35694087      1 2,2',3,3',4,4',5,5'-OCTACHLOROBIPHENYL  
    ##  8 40186729      1 2,2',3,3',4,4',5,5',6-NONACHLOROBIPHENYL
    ##  9 42740501      1 2,2',3,3',4,4',5,6'-OCTACHLOROBIPHENYL  
    ## 10 52663782      1 2,2',3,3',4,4',5,6-OCTACHLOROBIPHENYL   
    ## # ... with 168 more rows

So we have no duplicates.

All the PCBs listed by PCB number are in fact mixtures of PCBs. So
technically, they are NOT individual PCBs. The question is, how are they
used? And especially, how are the total PCBs calculated?

WE zero in on a few parameters that either emphasize PCB names
(mixtures) or chemical nomenclature (individual compounds).

``` r
SWAT_simplified %>% 
  select(Code, `TEST METHOD`, Year, ANALYSIS_LAB_SAMPLE_ID, `WEIGHT BASIS`,
         PARAMETER, CONCENTRATION, Class) %>%
  filter (`WEIGHT BASIS` == 'WET') %>%
  filter(grepl('PCB', Class)) %>%
  pivot_wider(names_from = PARAMETER, values_from = CONCENTRATION) %>%
  select(-ANALYSIS_LAB_SAMPLE_ID, -Class, -Code) %>%
  select(sort(current_vars())) %>%
  select(`TEST METHOD`, Year, everything()) %>%
  select(1,2, 135:145) %>%
  arrange(Year)
```

    ## Warning: `current_vars()` is deprecated as of dplyr 0.8.4.
    ## Please use `tidyselect::peek_vars()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## # A tibble: 471 x 13
    ##    `TEST METHOD`  Year `4-CHLOROBIPHEN~ `4,4'-DICHLOROB~ DECACHLOROBIPHE~
    ##    <chr>         <dbl>            <dbl>            <dbl>            <dbl>
    ##  1 E1668A         2003               NA               NA               NA
    ##  2 E1668A         2003               NA               NA               NA
    ##  3 E1668A         2003               NA               NA               NA
    ##  4 E1668A         2003               NA               NA               NA
    ##  5 E1613          2003               NA               NA               NA
    ##  6 E1613          2003               NA               NA               NA
    ##  7 E1668A         2003               NA               NA               NA
    ##  8 E1668A         2003               NA               NA               NA
    ##  9 E1613          2003               NA               NA               NA
    ## 10 E1668A         2003               NA               NA               NA
    ## # ... with 461 more rows, and 8 more variables: `PCB-012/013` <dbl>,
    ## #   `PCB-018/030` <dbl>, `PCB-020/028` <dbl>, `PCB-021/033` <dbl>,
    ## #   `PCB-026/029` <dbl>, `PCB-040/041/071` <dbl>, `PCB-044/047/065` <dbl>,
    ## #   `PCB-045/051` <dbl>

It does, generally appear that samples EITHER provide data on individual
PCBs or on co-eluted PCBs. But interestingly, almost all samples
indicate to the same `TEST METHOD`, E1613, or E1613A. There is no
obvious pattern with respect to when the sample was collected.

We are unlikely to be able to resolve this discrepancy “after the fact”.
We next turn to looking at the PCB totals reported in the ata

# When Does Each Total Exist?

We look narrowly at WET weights and totals that treat non-detects as
zero. We make the assumption for now that the lessons learned from that
subset apply to the related data.

``` r
SWAT_simplified %>%
  select(Code, ANALYSIS_LAB_SAMPLE_ID, `TEST METHOD`, `WEIGHT BASIS`,
         PARAMETER, CONCENTRATION) %>%
  filter (`WEIGHT BASIS` == 'WET') %>%
  filter(grepl("PCB", PARAMETER)) %>%
  filter(grepl("TOTAL", PARAMETER) |
           grepl('TEQ', PARAMETER) |
           grepl('CALCULATED', `TEST METHOD`)) %>%
  pivot_wider(names_from = PARAMETER, values_from = CONCENTRATION)
```

    ## # A tibble: 195 x 11
    ##    Code  ANALYSIS_LAB_SA~ `TEST METHOD` `WEIGHT BASIS` `PCB TOTAL TEQ ~
    ##    <chr> <chr>            <chr>         <chr>                     <dbl>
    ##  1 CBJW~ L10702-41        E1668A        WET                      0.077 
    ##  2 CBJW~ L10702-42        E1668A        WET                      0.073 
    ##  3 CBJW~ L10702-43        E1668A        WET                      0.093 
    ##  4 CBJW~ L10702-44        E1668A        WET                      0.074 
    ##  5 CBAN~ L10702-1         E1668A        WET                      0.0171
    ##  6 CBAN~ L10702-2         E1668A        WET                      0.213 
    ##  7 CBAN~ L10702-3 (A)     E1668A        WET                      0.25  
    ##  8 CBAN~ L10702-4         E1668A        WET                      0.2   
    ##  9 CBMC~ MILL CREEK, FAL~ E1668A        WET                      0.275 
    ## 10 CBMC~ L13939-9         CALCULATED W~ WET                      0.226 
    ## # ... with 185 more rows, and 6 more variables: `PCB TOTAL TEQ (ND=1/2
    ## #   DL)` <dbl>, `PCB TOTAL TEQ (ND=DL)` <dbl>, `TOTAL PCB-H` <dbl>, `TOTAL
    ## #   PCB-O` <dbl>, `TOTAL PCB-D` <dbl>, PCBS <dbl>

We appear to have THREE different types of totals:

1.  PCB TOTAL TEQ  
2.  TOTAL PCB  
3.  PCBs

<!-- end list -->

  - Groups 1 and 2 co-occur sometimes, but not always.
  - Group 3 appears to always occur with Group 1, and never with Group
    2.
  - Groups 2 and 3 appear disjoint.

None of these make it obvious which PCBs were included in the sum to
calculate “totals”.
