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
  - [How to Group Parameters?](#how-to-group-parameters)
      - [Develop a Preliminary list of
        Parameters](#develop-a-preliminary-list-of-parameters)
      - [Read Excel File with Classification of
        Parameters](#read-excel-file-with-classification-of-parameters)
      - [Add Class to the Working Simplified
        Data](#add-class-to-the-working-simplified-data)
      - [How are contaminant classes distributed among sampling
        events?](#how-are-contaminant-classes-distributed-among-sampling-events)
  - [Totals And Calculations](#totals-and-calculations)
      - [List of Total And Calculated
        Values](#list-of-total-and-calculated-values)
      - [Alternate Versions of Totals?](#alternate-versions-of-totals)
  - [Check if Data is Consistent with Our
    Interpretation](#check-if-data-is-consistent-with-our-interpretation)
  - [How Were Totals Calculated?](#how-were-totals-calculated)

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

# How to Group Parameters?

One of the challenges is that we want to report totals of various groups
of parameters, but the data contains no flag for major groups of
parameters.

The closest thing to an indicator of the groupings of parameters in use
is the `TEST METHOD` field. We generate a pretty good starting point for
a lookup table to assign parameters to groups based on the `TEST
METHOD`.

We then export that list to Excel, and edit it by hand to assign
parameters to groups.

## Develop a Preliminary list of Parameters

``` r
SWAT_simplified %>%
  select(PARAMETER, `TEST METHOD`) %>%
  group_by(`TEST METHOD`, PARAMETER) %>%
  summarize(Test      =  first(`TEST METHOD`),
                        .groups = 'drop') %>%
  rename(Parameter = PARAMETER) %>%
  select(-Test) %>%
  kable()
```

| TEST METHOD        | Parameter                                    |
| :----------------- | :------------------------------------------- |
| CALCULATED         | C1-ACENAPHTHENES                             |
| CALCULATED         | C1-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| CALCULATED         | C1-BENZOFLUORANTHENES/BENZOPYRENES           |
| CALCULATED         | C1-BIPHENYLS                                 |
| CALCULATED         | C1-DIBENZOTHIOPHENES                         |
| CALCULATED         | C1-FLUORANTHENES/PYRENES                     |
| CALCULATED         | C1-FLUORENES                                 |
| CALCULATED         | C1-NAPHTHALENES                              |
| CALCULATED         | C1-PHENANTHRENES/ANTHRACENES                 |
| CALCULATED         | C2-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| CALCULATED         | C2-BENZOFLUORANTHENES/BENZOPYRENES           |
| CALCULATED         | C2-BIPHENYLS                                 |
| CALCULATED         | C2-DIBENZOTHIOPHENES                         |
| CALCULATED         | C2-FLUORANTHENES/PYRENES                     |
| CALCULATED         | C2-FLUORENES                                 |
| CALCULATED         | C2-NAPHTHALENES                              |
| CALCULATED         | C2-PHENANTHRENES/ANTHRACENES                 |
| CALCULATED         | C3-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| CALCULATED         | C3-DIBENZOTHIOPHENES                         |
| CALCULATED         | C3-FLUORANTHENES/PYRENES                     |
| CALCULATED         | C3-FLUORENES                                 |
| CALCULATED         | C3-NAPHTHALENES                              |
| CALCULATED         | C3-PHENANTHRENES/ANTHRACENES                 |
| CALCULATED         | C4-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| CALCULATED         | C4-DIBENZOTHIOPHENES                         |
| CALCULATED         | C4-FLUORANTHENES/PYRENES                     |
| CALCULATED         | C4-NAPHTHALENES                              |
| CALCULATED         | C4-PHENANTHRENES/ANTHRACENES                 |
| CALCULATED         | PCB TOTAL TEQ (ND=0)                         |
| CALCULATED         | PCB TOTAL TEQ (ND=1/2 DL)                    |
| CALCULATED         | PCB TOTAL TEQ (ND=DL)                        |
| CALCULATED         | PCBS                                         |
| CALCULATED WHO2005 | C1-ACENAPHTHENES                             |
| CALCULATED WHO2005 | C1-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| CALCULATED WHO2005 | C1-BENZOFLUORANTHENES/BENZOPYRENES           |
| CALCULATED WHO2005 | C1-BIPHENYLS                                 |
| CALCULATED WHO2005 | C1-DIBENZOTHIOPHENES                         |
| CALCULATED WHO2005 | C1-FLUORANTHENES/PYRENES                     |
| CALCULATED WHO2005 | C1-FLUORENES                                 |
| CALCULATED WHO2005 | C1-NAPHTHALENES                              |
| CALCULATED WHO2005 | C1-PHENANTHRENES/ANTHRACENES                 |
| CALCULATED WHO2005 | C2-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| CALCULATED WHO2005 | C2-BENZOFLUORANTHENES/BENZOPYRENES           |
| CALCULATED WHO2005 | C2-BIPHENYLS                                 |
| CALCULATED WHO2005 | C2-DIBENZOTHIOPHENES                         |
| CALCULATED WHO2005 | C2-FLUORANTHENES/PYRENES                     |
| CALCULATED WHO2005 | C2-FLUORENES                                 |
| CALCULATED WHO2005 | C2-NAPHTHALENES                              |
| CALCULATED WHO2005 | C2-PHENANTHRENES/ANTHRACENES                 |
| CALCULATED WHO2005 | C3-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| CALCULATED WHO2005 | C3-DIBENZOTHIOPHENES                         |
| CALCULATED WHO2005 | C3-FLUORANTHENES/PYRENES                     |
| CALCULATED WHO2005 | C3-FLUORENES                                 |
| CALCULATED WHO2005 | C3-NAPHTHALENES                              |
| CALCULATED WHO2005 | C3-PHENANTHRENES/ANTHRACENES                 |
| CALCULATED WHO2005 | C4-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| CALCULATED WHO2005 | C4-DIBENZOTHIOPHENES                         |
| CALCULATED WHO2005 | C4-FLUORANTHENES/PYRENES                     |
| CALCULATED WHO2005 | C4-NAPHTHALENES                              |
| CALCULATED WHO2005 | C4-PHENANTHRENES/ANTHRACENES                 |
| CALCULATED WHO2005 | DX TOTAL TEQ (ND=0)                          |
| CALCULATED WHO2005 | DX TOTAL TEQ (ND=1/2 DL)                     |
| CALCULATED WHO2005 | DX TOTAL TEQ (ND=DL)                         |
| CALCULATED WHO2005 | PCB TOTAL TEQ (ND=0)                         |
| CALCULATED WHO2005 | PCB TOTAL TEQ (ND=1/2 DL)                    |
| CALCULATED WHO2005 | PCB TOTAL TEQ (ND=DL)                        |
| CALCULATED WHO2005 | PCBS                                         |
| E160.3             | SOLIDS-TOTAL RESIDUE (TS)                    |
| E1613              | 1,2,3,4,6,7,8-HPCDD                          |
| E1613              | 1,2,3,4,6,7,8-HPCDF                          |
| E1613              | 1,2,3,4,7,8-HXCDD                            |
| E1613              | 1,2,3,4,7,8-HXCDF                            |
| E1613              | 1,2,3,4,7,8,9-HEPTACHLORODIBENZOFURAN(HPCDF) |
| E1613              | 1,2,3,6,7,8-HXCDD                            |
| E1613              | 1,2,3,6,7,8-HXCDF                            |
| E1613              | 1,2,3,7,8-PECDD                              |
| E1613              | 1,2,3,7,8-PECDF                              |
| E1613              | 1,2,3,7,8,9-HXCDD                            |
| E1613              | 1,2,3,7,8,9-HXCDF                            |
| E1613              | 2,3,4,6,7,8-HXCDF                            |
| E1613              | 2,3,4,7,8-PECDF                              |
| E1613              | 2,3,7,8-TCDD                                 |
| E1613              | 2,3,7,8-TETRACHLORODIBENZOFURAN              |
| E1613              | DX TOTAL TEQ (ND=1/2 DL)                     |
| E1613              | DX TOTAL TEQ (ND=DL)                         |
| E1613              | LIPIDS                                       |
| E1613              | OCDD                                         |
| E1613              | OCDF                                         |
| E1613              | WEIGHT                                       |
| E1613B             | 1,2,3,4,6,7,8-HPCDD                          |
| E1613B             | 1,2,3,4,6,7,8-HPCDF                          |
| E1613B             | 1,2,3,4,7,8-HXCDD                            |
| E1613B             | 1,2,3,4,7,8-HXCDF                            |
| E1613B             | 1,2,3,4,7,8,9-HEPTACHLORODIBENZOFURAN(HPCDF) |
| E1613B             | 1,2,3,6,7,8-HXCDD                            |
| E1613B             | 1,2,3,6,7,8-HXCDF                            |
| E1613B             | 1,2,3,7,8-PECDD                              |
| E1613B             | 1,2,3,7,8-PECDF                              |
| E1613B             | 1,2,3,7,8,9-HXCDD                            |
| E1613B             | 1,2,3,7,8,9-HXCDF                            |
| E1613B             | 2,3,4,6,7,8-HXCDF                            |
| E1613B             | 2,3,4,7,8-PECDF                              |
| E1613B             | 2,3,7,8-TCDD                                 |
| E1613B             | 2,3,7,8-TETRACHLORODIBENZOFURAN              |
| E1613B             | LIPIDS                                       |
| E1613B             | MOISTURE                                     |
| E1613B             | OCDD                                         |
| E1613B             | OCDF                                         |
| E1631              | MERCURY                                      |
| E1631E             | MERCURY                                      |
| E1638              | ALUMINUM                                     |
| E1638              | ARSENIC                                      |
| E1638              | CADMIUM                                      |
| E1638              | CHROMIUM                                     |
| E1638              | COPPER                                       |
| E1638              | IRON                                         |
| E1638              | LEAD                                         |
| E1638              | NICKEL                                       |
| E1638              | SELENIUM                                     |
| E1638              | SILVER                                       |
| E1638              | ZINC                                         |
| E1638M/E200.8M     | ARSENIC                                      |
| E1638M/E200.8M     | CADMIUM                                      |
| E1638M/E200.8M     | COPPER                                       |
| E1638M/E200.8M     | LEAD                                         |
| E1638M/E200.8M     | NICKEL                                       |
| E1638M/E200.8M     | SELENIUM                                     |
| E1638M/E200.8M     | SILVER                                       |
| E1668A             | 2’,3,4,4’,5-PENTACHLOROBIPHENYL              |
| E1668A             | 2-CHLOROBIPHENYL                             |
| E1668A             | 2,2’-DICHLOROBIPHENYL                        |
| E1668A             | 2,2’,3-TRICHLOROBIPHENYL                     |
| E1668A             | 2,2’,3,3’,4-PENTACHLOROBIPHENYL              |
| E1668A             | 2,2’,3,3’,4,4’,5-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,2’,3,3’,4,4’,5,5’-OCTACHLOROBIPHENYL       |
| E1668A             | 2,2’,3,3’,4,4’,5,5’,6-NONACHLOROBIPHENYL     |
| E1668A             | 2,2’,3,3’,4,4’,5,6’-OCTACHLOROBIPHENYL       |
| E1668A             | 2,2’,3,3’,4,4’,5,6-OCTACHLOROBIPHENYL        |
| E1668A             | 2,2’,3,3’,4,4’,5,6,6’-NONACHLOROBIPHENYL     |
| E1668A             | 2,2’,3,3’,4,5’-HEXACHLOROBIPHENYL            |
| E1668A             | 2,2’,3,3’,4,5’,6’-HEPTACHLOROBIPHENYL        |
| E1668A             | 2,2’,3,3’,4,5’,6-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,2’,3,3’,4,5’,6,6’-OCTACHLOROBIPHENYL       |
| E1668A             | 2,2’,3,3’,4,5,5’-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,2’,3,3’,4,5,5’,6,6’-NONACHLOROBIPHENYL     |
| E1668A             | 2,2’,3,3’,4,5,6’-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,2’,3,3’,4,6’-HEXACHLOROBIPHENYL            |
| E1668A             | 2,2’,3,3’,4,6-HEXACHLOROBIPHENYL             |
| E1668A             | 2,2’,3,3’,4,6,6’-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,2’,3,3’,5-PENTACHLOROBIPHENYL              |
| E1668A             | 2,2’,3,3’,5,5’-HEXACHLOROBIPHENYL            |
| E1668A             | 2,2’,3,3’,5,5’,6-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,2’,3,3’,5,5’,6,6’-OCTACHLOROBIPHENYL       |
| E1668A             | 2,2’,3,3’,5,6,6’-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,2’,3,3’,6-PENTACHLOROBIPHENYL              |
| E1668A             | 2,2’,3,3’,6,6’-HEXACHLOROBIPHENYL            |
| E1668A             | 2,2’,3,4’-TETRACHLOROBIPHENYL                |
| E1668A             | 2,2’,3,4’,5,5’-HEXACHLOROBIPHENYL            |
| E1668A             | 2,2’,3,4’,5,5’,6-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,2’,3,4’,5,6’-HEXACHLOROBIPHENYL            |
| E1668A             | 2,2’,3,4’,5,6,6’-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,2’,3,4’,6,6’-HEXACHLOROBIPHENYL            |
| E1668A             | 2,2’,3,4,4’,5-HEXACHLOROBIPHENYL             |
| E1668A             | 2,2’,3,4,4’,5,5’-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,2’,3,4,4’,5,5’,6-OCTACHLOROBIPHENYL        |
| E1668A             | 2,2’,3,4,4’,5,6’-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,2’,3,4,4’,5,6-HEPTACHLOROBIPHENYL          |
| E1668A             | 2,2’,3,4,4’,5,6,6’-OCTACHLOROBIPHENYL        |
| E1668A             | 2,2’,3,4,4’,6,6’-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,2’,3,4,5’,6-HEXACHLOROBIPHENYL             |
| E1668A             | 2,2’,3,4,5,5’-HEXACHLOROBIPHENYL             |
| E1668A             | 2,2’,3,4,5,6-HEXACHLOROBIPHENYL              |
| E1668A             | 2,2’,3,4,5,6,6’-HEPTACHLOROBIPHENYL          |
| E1668A             | 2,2’,3,4,6’-PENTACHLOROBIPHENYL              |
| E1668A             | 2,2’,3,4,6,6’-HEXACHLOROBIPHENYL             |
| E1668A             | 2,2’,3,5’,6-PENTACHLOROBIPHENYL              |
| E1668A             | 2,2’,3,5-TETRACHLOROBIPHENYL                 |
| E1668A             | 2,2’,3,5,5’-PENTACHLOROBIPHENYL              |
| E1668A             | 2,2’,3,5,6’-PENTACHLOROBIPHENYL              |
| E1668A             | 2,2’,3,5,6,6’-HEXACHLOROBIPHENYL             |
| E1668A             | 2,2’,3,6’-TETRACHLOROBIPHENYL                |
| E1668A             | 2,2’,3,6,6’-PENTACHLOROBIPHENYL              |
| E1668A             | 2,2’,4-TRICHLOROBIPHENYL                     |
| E1668A             | 2,2’,4,4’,5-PENTACHLOROBIPHENYL              |
| E1668A             | 2,2’,4,4’,5,6’-HEXACHLOROBIPHENYL            |
| E1668A             | 2,2’,4,4’,6,6’-HEXACHLOROBIPHENYL            |
| E1668A             | 2,2’,4,5’,6-PENTACHLOROBIPHENYL              |
| E1668A             | 2,2’,4,5-TETRACHLOROBIPHENYL                 |
| E1668A             | 2,2’,4,6,6’-PENTACHLOROBIPHENYL              |
| E1668A             | 2,2’,5,5’-TETRACHLOROBIPHENYL                |
| E1668A             | 2,2’,6-TRICHLOROBIPHENYL                     |
| E1668A             | 2,2’,6,6’-TETRACHLOROBIPHENYL                |
| E1668A             | 2,3’-DICHLOROBIPHENYL                        |
| E1668A             | 2,3’,4-TRICHLOROBIPHENYL                     |
| E1668A             | 2,3’,4,4’-TETRACHLOROBIPHENYL                |
| E1668A             | 2,3’,4,4’,5-PENTACHLOROBIPHENYL              |
| E1668A             | 2,3’,4,4’,5,5’-HEXACHLOROBIPHENYL            |
| E1668A             | 2,3’,4,5’-TETRACHLOROBIPHENYL                |
| E1668A             | 2,3’,4,5’,6-PENTACHLOROBIPHENYL              |
| E1668A             | 2,3’,4,5-TETRACHLOROBIPHENYL                 |
| E1668A             | 2,3’,4,5,5’-PENTACHLOROBIPHENYL              |
| E1668A             | 2,3’,5’-TRICHLOROBIPHENYL                    |
| E1668A             | 2,3’,5’,6-TETRACHLOROBIPHENYL                |
| E1668A             | 2,3’,5,5’-TETRACHLOROBIPHENYL                |
| E1668A             | 2,3’,6-TRICHLOROBIPHENYL                     |
| E1668A             | 2,3-DICHLOROBIPHENYL                         |
| E1668A             | 2,3,3’,4’-TETRACHLOROBIPHENYL                |
| E1668A             | 2,3,3’,4’,5’-PENTACHLOROBIPHENYL             |
| E1668A             | 2,3,3’,4’,5’,6-HEXACHLOROBIPHENYL            |
| E1668A             | 2,3,3’,4’,5,5’-HEXACHLOROBIPHENYL            |
| E1668A             | 2,3,3’,4-TETRACHLOROBIPHENYL                 |
| E1668A             | 2,3,3’,4,4’-PENTACHLOROBIPHENYL              |
| E1668A             | 2,3,3’,4,4’,5’-HEXACHLOROBIPHENYL            |
| E1668A             | 2,3,3’,4,4’,5’,6-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,3,3’,4,4’,5-HEXACHLOROBIPHENYL             |
| E1668A             | 2,3,3’,4,4’,5,5’-HEPTACHLOROBIPHENYL         |
| E1668A             | 2,3,3’,4,4’,5,5’,6-OCTACHLOROBIPHENYL        |
| E1668A             | 2,3,3’,4,4’,5,6-HEPTACHLOROBIPHENYL          |
| E1668A             | 2,3,3’,4,4’,6-HEXACHLOROBIPHENYL             |
| E1668A             | 2,3,3’,4,5’,6-HEXACHLOROBIPHENYL             |
| E1668A             | 2,3,3’,4,5-PENTACHLOROBIPHENYL               |
| E1668A             | 2,3,3’,4,5,5’-HEXACHLOROBIPHENYL             |
| E1668A             | 2,3,3’,4,5,5’,6-HEPTACHLOROBIPHENYL          |
| E1668A             | 2,3,3’,4,5,6-HEXACHLOROBIPHENYL              |
| E1668A             | 2,3,3’,4,6-PENTACHLOROBIPHENYL               |
| E1668A             | 2,3,3’,5’-TETRACHLOROBIPHENYL                |
| E1668A             | 2,3,3’,5-TETRACHLOROBIPHENYL                 |
| E1668A             | 2,3,3’,5,5’-PENTACHLOROBIPHENYL              |
| E1668A             | 2,3,3’,5,5’,6-HEXACHLOROBIPHENYL             |
| E1668A             | 2,3,3’,5,6-PENTACHLOROBIPHENYL               |
| E1668A             | 2,3,4’-TRICHLOROBIPHENYL                     |
| E1668A             | 2,3,4’,5-TETRACHLOROBIPHENYL                 |
| E1668A             | 2,3,4’,6-TETRACHLOROBIPHENYL                 |
| E1668A             | 2,3,4,4’-TETRACHLOROBIPHENYL                 |
| E1668A             | 2,3,4,4’,5-PENTACHLOROBIPHENYL               |
| E1668A             | 2,3,5-TRICHLOROBIPHENYL                      |
| E1668A             | 2,3,6-TRICHLOROBIPHENYL                      |
| E1668A             | 2,4’-DICHLOROBIPHENYL                        |
| E1668A             | 2,4’,5-TRICHLOROBIPHENYL                     |
| E1668A             | 2,4’,6-TRICHLOROBIPHENYL                     |
| E1668A             | 2,4-DICHLOROBIPHENYL                         |
| E1668A             | 2,5-DICHLOROBIPHENYL                         |
| E1668A             | 2,6-DICHLOROBIPHENYL                         |
| E1668A             | 3-CHLOROBIPHENYL                             |
| E1668A             | 3,3’-DICHLOROBIPHENYL                        |
| E1668A             | 3,3’,4-TRICHLOROBIPHENYL                     |
| E1668A             | 3,3’,4,4’-TETRACHLOROBIPHENYL                |
| E1668A             | 3,3’,4,4’,5-PENTACHLOROBIPHENYL              |
| E1668A             | 3,3’,4,4’,5,5’-HEXACHLOROBIPHENYL            |
| E1668A             | 3,3’,4,5’-TETRACHLOROBIPHENYL                |
| E1668A             | 3,3’,4,5-TETRACHLOROBIPHENYL                 |
| E1668A             | 3,3’,4,5,5’-PENTACHLOROBIPHENYL              |
| E1668A             | 3,3’,5-TRICHLOROBIPHENYL                     |
| E1668A             | 3,3’,5,5’-TETRACHLOROBIPHENYL                |
| E1668A             | 3,4’,5-TRICHLOROBIPHENYL                     |
| E1668A             | 3,4,4’-TRICHLOROBIPHENYL                     |
| E1668A             | 3,4,4’,5-TETRACHLOROBIPHENYL                 |
| E1668A             | 3,4,5-TRICHLOROBIPHENYL                      |
| E1668A             | 3,5-DICHLOROBIPHENYL                         |
| E1668A             | 4-CHLOROBIPHENYL                             |
| E1668A             | 4,4’-DICHLOROBIPHENYL                        |
| E1668A             | DECACHLOROBIPHENYL                           |
| E1668A             | LIPIDS                                       |
| E1668A             | MOISTURE                                     |
| E1668A             | PCB-012/013                                  |
| E1668A             | PCB-018/030                                  |
| E1668A             | PCB-020/028                                  |
| E1668A             | PCB-021/033                                  |
| E1668A             | PCB-026/029                                  |
| E1668A             | PCB-040/041/071                              |
| E1668A             | PCB-044/047/065                              |
| E1668A             | PCB-045/051                                  |
| E1668A             | PCB-049/069                                  |
| E1668A             | PCB-050/053                                  |
| E1668A             | PCB-059/062/075                              |
| E1668A             | PCB-061/070/074/076                          |
| E1668A             | PCB-083/099                                  |
| E1668A             | PCB-085/116/117                              |
| E1668A             | PCB-086/087/097/108/119/125                  |
| E1668A             | PCB-088/091                                  |
| E1668A             | PCB-090/101/113                              |
| E1668A             | PCB-093/095/098/100/102                      |
| E1668A             | PCB-093/098/100/102                          |
| E1668A             | PCB-107/124                                  |
| E1668A             | PCB-110/115                                  |
| E1668A             | PCB-128/166                                  |
| E1668A             | PCB-129/138/160/163                          |
| E1668A             | PCB-129/138/163                              |
| E1668A             | PCB-134/143                                  |
| E1668A             | PCB-135/151                                  |
| E1668A             | PCB-135/151/154                              |
| E1668A             | PCB-139/140                                  |
| E1668A             | PCB-147/149                                  |
| E1668A             | PCB-153/168                                  |
| E1668A             | PCB-156/157                                  |
| E1668A             | PCB-171/173                                  |
| E1668A             | PCB-180/193                                  |
| E1668A             | PCB-183/185                                  |
| E1668A             | PCB-197/200                                  |
| E1668A             | PCB-198/199                                  |
| E1668A             | PCB TOTAL TEQ (ND=0)                         |
| E1668A             | PCB TOTAL TEQ (ND=1/2 DL)                    |
| E1668A             | PCB TOTAL TEQ (ND=DL)                        |
| E1668A             | PCBS                                         |
| E1668A             | TOTAL PCB-D                                  |
| E1668A             | TOTAL PCB-H                                  |
| E1668A             | TOTAL PCB-O                                  |
| E1668A             | WEIGHT                                       |
| E245.6M            | MERCURY                                      |
| E270.3M            | SELENIUM                                     |
| FIELD              | Mytilus edulis                               |
| MLA-007-E1         | 2,4’-DDD                                     |
| MLA-007-E1         | 2,4’-DDE                                     |
| MLA-007-E1         | 2,4’-DDT                                     |
| MLA-007-E1         | 4,4’-DDD                                     |
| MLA-007-E1         | 4,4’-DDE                                     |
| MLA-007-E1         | 4,4’-DDT                                     |
| MLA-007-E1         | ALDRIN                                       |
| MLA-007-E1         | ALPHA-BHC                                    |
| MLA-007-E1         | ALPHA-CHLORDANE                              |
| MLA-007-E1         | BETA-BHC                                     |
| MLA-007-E1         | GAMMA-BHC (LINDANE)                          |
| MLA-007-E1         | GAMMA-CHLORDANE                              |
| MLA-007-E1         | HEPTACHLOR                                   |
| MLA-007-E1         | HEXACHLOROBENZENE                            |
| MLA-007-E1         | LIPIDS                                       |
| MLA-007-E1         | MIREX                                        |
| MLA-007-E1         | MOISTURE                                     |
| MLA-007-E1         | TOTAL PESTICIDES21-D                         |
| MLA-007-E1         | TOTAL PESTICIDES21-H                         |
| MLA-007-E1         | TOTAL PESTICIDES21-O                         |
| MLA-007-E1         | TRANS-NONACHLOR                              |
| MLA-007-E2         | DELTA-BHC                                    |
| MLA-007-E2         | DIELDRIN                                     |
| MLA-007-E2         | ENDOSULFAN I                                 |
| MLA-007-E2         | ENDOSULFAN II                                |
| MLA-007-E2         | ENDOSULFAN SULFATE                           |
| MLA-007-E2         | ENDRIN                                       |
| MLA-007-E2         | ENDRIN ALDEHYDE                              |
| MLA-007-E2         | ENDRIN KETONE                                |
| MLA-007-E2         | HEPTACHLOR EPOXIDE                           |
| MLA-007-E2         | LIPIDS                                       |
| MLA-007-E2         | METHOXYCHLOR                                 |
| MLA-007-E2         | MOISTURE                                     |
| MLA-007-E2         | TOTAL PESTICIDES21-D                         |
| MLA-007-E2         | TOTAL PESTICIDES21-H                         |
| MLA-007-E2         | TOTAL PESTICIDES21-O                         |
| MLA-035            | 2,4’-DDD                                     |
| MLA-035            | 2,4’-DDE                                     |
| MLA-035            | 2,4’-DDT                                     |
| MLA-035            | 4,4’-DDD                                     |
| MLA-035            | 4,4’-DDE                                     |
| MLA-035            | 4,4’-DDT                                     |
| MLA-035            | ALACHLOR                                     |
| MLA-035            | ALDRIN                                       |
| MLA-035            | ALPHA-BHC                                    |
| MLA-035            | ALPHA-CHLORDANE                              |
| MLA-035            | AMETRYN                                      |
| MLA-035            | ATRAZINE                                     |
| MLA-035            | AZINPHOS-METHYL                              |
| MLA-035            | BETA-BHC                                     |
| MLA-035            | BUTRALIN                                     |
| MLA-035            | BUTYLATE                                     |
| MLA-035            | CAPTAN                                       |
| MLA-035            | CHLOROTHALONIL                               |
| MLA-035            | CHLORPYRIFOS, O,O-DIMETHYL ANALOG            |
| MLA-035            | CHLORPYRIPHOS                                |
| MLA-035            | CHLORPYRIPHOS-OXON                           |
| MLA-035            | CIS-NONACHLOR                                |
| MLA-035            | CYANAZINE                                    |
| MLA-035            | CYPERMETHRIN                                 |
| MLA-035            | DCPA                                         |
| MLA-035            | DELTA-BHC                                    |
| MLA-035            | DESETHYLATRAZINE                             |
| MLA-035            | DIAZINON                                     |
| MLA-035            | DIAZINON-OXON                                |
| MLA-035            | DIELDRIN                                     |
| MLA-035            | DIMETHENAMID                                 |
| MLA-035            | DIMETHOATE                                   |
| MLA-035            | DISULFOTON                                   |
| MLA-035            | DISULFOTON SULFONE                           |
| MLA-035            | ENDOSULFAN I                                 |
| MLA-035            | ENDOSULFAN II                                |
| MLA-035            | ENDOSULFAN SULFATE                           |
| MLA-035            | ENDRIN                                       |
| MLA-035            | ENDRIN KETONE                                |
| MLA-035            | ETHALFLURALIN                                |
| MLA-035            | ETHION                                       |
| MLA-035            | FENITROTHION                                 |
| MLA-035            | FLUFENACET                                   |
| MLA-035            | FLUTRIAFOL                                   |
| MLA-035            | FONOFOS                                      |
| MLA-035            | GAMMA-BHC (LINDANE)                          |
| MLA-035            | GAMMA-CHLORDANE                              |
| MLA-035            | HEPTACHLOR                                   |
| MLA-035            | HEPTACHLOR EPOXIDE                           |
| MLA-035            | HEXACHLOROBENZENE                            |
| MLA-035            | LINURON                                      |
| MLA-035            | MALATHION                                    |
| MLA-035            | METHAMIDOPHOS                                |
| MLA-035            | METHOPRENE                                   |
| MLA-035            | METHOXYCHLOR                                 |
| MLA-035            | METHYL PARATHION                             |
| MLA-035            | METOLACHLOR                                  |
| MLA-035            | METRIBUZIN                                   |
| MLA-035            | MIREX                                        |
| MLA-035            | OCTACHLOROSTYRENE                            |
| MLA-035            | OXYCHLORDANE                                 |
| MLA-035            | PARATHION                                    |
| MLA-035            | PENDIMETHALIN                                |
| MLA-035            | PENTACHLORONITROBENZENE                      |
| MLA-035            | PERMETHRIN                                   |
| MLA-035            | PERTHANE                                     |
| MLA-035            | PHORATE                                      |
| MLA-035            | PHOSMET                                      |
| MLA-035            | PIRIMIPHOS-METHYL                            |
| MLA-035            | SIMAZINE                                     |
| MLA-035            | TEBUCONAZOL                                  |
| MLA-035            | TECNAZENE                                    |
| MLA-035            | TERBUFOS                                     |
| MLA-035            | TOTAL PESTICIDES21-D                         |
| MLA-035            | TOTAL PESTICIDES21-H                         |
| MLA-035            | TOTAL PESTICIDES21-O                         |
| MLA-035            | TRANS-NONACHLOR                              |
| MLA-035            | TRIALLATE                                    |
| MLA-035            | TRIFLURALIN                                  |
| MLA-035            | VELPAR                                       |
| MLA-043            | LIPIDS                                       |
| MLA-043            | MOISTURE                                     |
| MLA-043            | Mytilus edulis                               |
| MLA-043            | PERFLUOROBUTANE SULFONATE                    |
| MLA-043            | PERFLUOROBUTANOATE                           |
| MLA-043            | PERFLUORODECANOATE                           |
| MLA-043            | PERFLUORODODECANOATE                         |
| MLA-043            | PERFLUOROHEPTANOATE                          |
| MLA-043            | PERFLUOROHEXANE SULFONATE                    |
| MLA-043            | PERFLUOROHEXANOATE                           |
| MLA-043            | PERFLUORONONANOATE                           |
| MLA-043            | PERFLUOROOCTANE SULFONAMIDE                  |
| MLA-043            | PERFLUOROOCTANE SULFONATE                    |
| MLA-043            | PERFLUOROOCTANOATE                           |
| MLA-043            | PERFLUOROOCTANOIC ACID                       |
| MLA-043            | PERFLUOROPENTANOATE                          |
| MLA-043            | PERFLUOROUNDECANOATE                         |
| NOAA1998M          | SOLIDS-TOTAL RESIDUE (TS)                    |
| SM2540G            | MOISTURE                                     |
| SM2540G            | SOLIDS-TOTAL RESIDUE (TS)                    |
| SM2540GM           | SOLIDS-TOTAL RESIDUE (TS)                    |
| SOP\_MSLC003       | SOLIDS-TOTAL RESIDUE (TS)                    |
| SW6010BM/E200.7M   | ALUMINUM                                     |
| SW6010BM/E200.7M   | CHROMIUM                                     |
| SW6010BM/E200.7M   | COPPER                                       |
| SW6010BM/E200.7M   | IRON                                         |
| SW6010BM/E200.7M   | NICKEL                                       |
| SW6010BM/E200.7M   | ZINC                                         |
| SW6020             | ALUMINUM                                     |
| SW6020             | CADMIUM                                      |
| SW6020             | CHROMIUM                                     |
| SW6020             | COPPER                                       |
| SW6020             | IRON                                         |
| SW6020             | LEAD                                         |
| SW6020             | MERCURY                                      |
| SW6020             | NICKEL                                       |
| SW6020             | SELENIUM                                     |
| SW6020             | SILVER                                       |
| SW6020             | ZINC                                         |
| SW6020A            | ALUMINUM                                     |
| SW6020A            | ARSENIC                                      |
| SW6020A            | CADMIUM                                      |
| SW6020A            | CHROMIUM                                     |
| SW6020A            | COPPER                                       |
| SW6020A            | IRON                                         |
| SW6020A            | LEAD                                         |
| SW6020A            | NICKEL                                       |
| SW6020A            | SELENIUM                                     |
| SW6020A            | SILVER                                       |
| SW6020A            | ZINC                                         |
| SW7471A            | MERCURY                                      |
| SW7473M            | MERCURY                                      |
| SW8081             | ALPHA-CHLORDANE                              |
| SW8081             | DIELDRIN                                     |
| SW8081             | ENDOSULFAN I                                 |
| SW8081             | ENDOSULFAN II                                |
| SW8081             | GAMMA-BHC (LINDANE)                          |
| SW8081             | GAMMA-CHLORDANE                              |
| SW8081             | HEPTACHLOR                                   |
| SW8081             | HEPTACHLOR EPOXIDE                           |
| SW8081             | MIREX                                        |
| SW8081             | TRANS-NONACHLOR                              |
| SW8081A            | 2,4’-DDD                                     |
| SW8081A            | 2,4’-DDE                                     |
| SW8081A            | 2,4’-DDT                                     |
| SW8081A            | 4,4’-DDD                                     |
| SW8081A            | 4,4’-DDE                                     |
| SW8081A            | 4,4’-DDT                                     |
| SW8081A            | ALDRIN                                       |
| SW8081A            | ALPHA-BHC                                    |
| SW8081A            | ALPHA-CHLORDANE                              |
| SW8081A            | BETA-BHC                                     |
| SW8081A            | DELTA-BHC                                    |
| SW8081A            | DIELDRIN                                     |
| SW8081A            | ENDOSULFAN I                                 |
| SW8081A            | ENDOSULFAN II                                |
| SW8081A            | ENDOSULFAN SULFATE                           |
| SW8081A            | ENDRIN                                       |
| SW8081A            | ENDRIN ALDEHYDE                              |
| SW8081A            | ENDRIN KETONE                                |
| SW8081A            | GAMMA-BHC (LINDANE)                          |
| SW8081A            | GAMMA-CHLORDANE                              |
| SW8081A            | HEPTACHLOR                                   |
| SW8081A            | HEPTACHLOR EPOXIDE                           |
| SW8081A            | HEXACHLOROBENZENE                            |
| SW8081A            | LIPIDS                                       |
| SW8081A            | METHOXYCHLOR                                 |
| SW8081A            | MIREX                                        |
| SW8081A            | MOISTURE                                     |
| SW8081A            | TRANS-NONACHLOR                              |
| SW8081A            | WEIGHT                                       |
| SW8270C\_SIM       | 1-METHYL NAPHTHALENE                         |
| SW8270C\_SIM       | 1-METHYLPHENANTHRENE                         |
| SW8270C\_SIM       | 2-METHYLNAPHTHALENE                          |
| SW8270C\_SIM       | 2,3,5-TRIMETHYLNAPHTHALENE                   |
| SW8270C\_SIM       | 2,4’-DDD                                     |
| SW8270C\_SIM       | 2,4’-DDE                                     |
| SW8270C\_SIM       | 2,4’-DDT                                     |
| SW8270C\_SIM       | 2,6-DIMETHYLNAPHTHALENE                      |
| SW8270C\_SIM       | 4,4’-DDD                                     |
| SW8270C\_SIM       | 4,4’-DDE                                     |
| SW8270C\_SIM       | 4,4’-DDT                                     |
| SW8270C\_SIM       | ACENAPHTHENE                                 |
| SW8270C\_SIM       | ACENAPHTHYLENE                               |
| SW8270C\_SIM       | ALDRIN                                       |
| SW8270C\_SIM       | ANTHRACENE                                   |
| SW8270C\_SIM       | BENZO(A)ANTHRACENE                           |
| SW8270C\_SIM       | BENZO(A)PYRENE                               |
| SW8270C\_SIM       | BENZO(B)FLUORANTHENE                         |
| SW8270C\_SIM       | BENZO(E)PYRENE                               |
| SW8270C\_SIM       | BENZO(G,H,I)PERYLENE                         |
| SW8270C\_SIM       | BENZO(K)FLUORANTHENE                         |
| SW8270C\_SIM       | BIPHENYL                                     |
| SW8270C\_SIM       | CHRYSENE                                     |
| SW8270C\_SIM       | DIBENZO(A,H)ANTHRACENE                       |
| SW8270C\_SIM       | FLUORANTHENE                                 |
| SW8270C\_SIM       | FLUORENE                                     |
| SW8270C\_SIM       | HEXACHLOROBENZENE                            |
| SW8270C\_SIM       | INDENO(1,2,3-CD)PYRENE                       |
| SW8270C\_SIM       | LIPIDS                                       |
| SW8270C\_SIM       | NAPHTHALENE                                  |
| SW8270C\_SIM       | PERYLENE                                     |
| SW8270C\_SIM       | PHENANTHRENE                                 |
| SW8270C\_SIM       | PYRENE                                       |
| SW8270CM           | 1-METHYL NAPHTHALENE                         |
| SW8270CM           | 1-METHYLCHRYSENE                             |
| SW8270CM           | 1-METHYLPHENANTHRENE                         |
| SW8270CM           | 1,2-DIMETHYLNAPHTHALENE                      |
| SW8270CM           | 1,2,6-TRIMETHYLPHENANTHRENE                  |
| SW8270CM           | 1,4,6,7-TETRAMETHYLNAPHTHALENE               |
| SW8270CM           | 1,7-DIMETHYLFLUORENE                         |
| SW8270CM           | 1,7-DIMETHYLPHENANTHRENE                     |
| SW8270CM           | 1,8-DIMETHYLPHENANTHRENE                     |
| SW8270CM           | 2-METHYLANTHRACENE                           |
| SW8270CM           | 2-METHYLFLUORENE                             |
| SW8270CM           | 2-METHYLNAPHTHALENE                          |
| SW8270CM           | 2-METHYLPHENANTHRENE                         |
| SW8270CM           | 2,3,5-TRIMETHYLNAPHTHALENE                   |
| SW8270CM           | 2,3,6-TRIMETHYLNAPHTHALENE                   |
| SW8270CM           | 2,4-DIMETHYLDIBENZOTHIOPHENE                 |
| SW8270CM           | 2,6-DIMETHYLNAPHTHALENE                      |
| SW8270CM           | 2,6-DIMETHYLPHENANTHRENE                     |
| SW8270CM           | 2/3-METHYLDIBENZOTHIOPHENES                  |
| SW8270CM           | 3-METHYLFLUORANTHENE/BENZO\[A\]FLUORENE      |
| SW8270CM           | 3-METHYLPHENANTHRENE                         |
| SW8270CM           | 3,6-DIMETHYLPHENANTHRENE                     |
| SW8270CM           | 5,9-DIMETHYLCHRYSENE                         |
| SW8270CM           | 5/6-METHYLCHRYSENE                           |
| SW8270CM           | 7-METHYLBENZO\[A\]PYRENE                     |
| SW8270CM           | 9/4-METHYLPHENANTHRENE                       |
| SW8270CM           | ACENAPHTHENE                                 |
| SW8270CM           | ACENAPHTHYLENE                               |
| SW8270CM           | ANTHRACENE                                   |
| SW8270CM           | BENZO(A)ANTHRACENE                           |
| SW8270CM           | BENZO(A)PYRENE                               |
| SW8270CM           | BENZO(B)FLUORANTHENE                         |
| SW8270CM           | BENZO(E)PYRENE                               |
| SW8270CM           | BENZO(G,H,I)PERYLENE                         |
| SW8270CM           | BENZO\[B,J,K\]FLUORANTHENES                  |
| SW8270CM           | BENZO\[J,K\]FLUORANTHENES                    |
| SW8270CM           | BENZOFLUORANTHENE                            |
| SW8270CM           | BIPHENYL                                     |
| SW8270CM           | C1-ACENAPHTHENES                             |
| SW8270CM           | C1-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| SW8270CM           | C1-BENZOFLUORANTHENES/BENZOPYRENES           |
| SW8270CM           | C1-BIPHENYLS                                 |
| SW8270CM           | C1-DIBENZOTHIOPHENES                         |
| SW8270CM           | C1-FLUORANTHENES/PYRENES                     |
| SW8270CM           | C1-FLUORENES                                 |
| SW8270CM           | C1-NAPHTHALENES                              |
| SW8270CM           | C1-PHENANTHRENES/ANTHRACENES                 |
| SW8270CM           | C2-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| SW8270CM           | C2-BENZOFLUORANTHENES/BENZOPYRENES           |
| SW8270CM           | C2-BIPHENYLS                                 |
| SW8270CM           | C2-DIBENZOTHIOPHENES                         |
| SW8270CM           | C2-FLUORANTHENES/PYRENES                     |
| SW8270CM           | C2-FLUORENES                                 |
| SW8270CM           | C2-NAPHTHALENES                              |
| SW8270CM           | C2-PHENANTHRENES/ANTHRACENES                 |
| SW8270CM           | C3-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| SW8270CM           | C3-DIBENZOTHIOPHENES                         |
| SW8270CM           | C3-FLUORANTHENES/PYRENES                     |
| SW8270CM           | C3-FLUORENES                                 |
| SW8270CM           | C3-NAPHTHALENES                              |
| SW8270CM           | C3-PHENANTHRENES/ANTHRACENES                 |
| SW8270CM           | C4-BENZO\[A\]ANTHRACENES/CHRYSENES           |
| SW8270CM           | C4-DIBENZOTHIOPHENES                         |
| SW8270CM           | C4-FLUORANTHENES/PYRENES                     |
| SW8270CM           | C4-NAPHTHALENES                              |
| SW8270CM           | C4-PHENANTHRENES/ANTHRACENES                 |
| SW8270CM           | CHRYSENE                                     |
| SW8270CM           | DIBENZO(A,H)ANTHRACENE                       |
| SW8270CM           | DIBENZOTHIOPHENE                             |
| SW8270CM           | FLUORANTHENE                                 |
| SW8270CM           | FLUORENE                                     |
| SW8270CM           | INDENO(1,2,3-CD)PYRENE                       |
| SW8270CM           | LIPIDS                                       |
| SW8270CM           | MOISTURE                                     |
| SW8270CM           | NAPHTHALENE                                  |
| SW8270CM           | PERYLENE                                     |
| SW8270CM           | PHENANTHRENE                                 |
| SW8270CM           | PYRENE                                       |
| SW8270CM           | RETENE                                       |
| SW8270CM           | TOTAL PAH-D                                  |
| SW8270CM           | TOTAL PAH-H                                  |
| SW8270CM           | TOTAL PAH-O                                  |
| SW8270CM           | TOTAL PAH19-D                                |
| SW8270CM           | TOTAL PAH19-H                                |
| SW8270CM           | TOTAL PAH19-O                                |
| SW8270CM           | TOTAL PAH24-D                                |
| SW8270CM           | TOTAL PAH24-H                                |
| SW8270CM           | TOTAL PAH24-O                                |
| SW8270CM           | TOTAL PAH40-D                                |
| SW8270CM           | TOTAL PAH40-H                                |
| SW8270CM           | TOTAL PAH40-O                                |
| SW8270CM           | WEIGHT                                       |

## Read Excel File with Classification of Parameters

We can then read in the resulting Excel File to provide groupings…

``` r
Parameter_List <- read_excel(file.path(parent,"Parameter List.xlsx"), 
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

## Add Class to the Working Simplified Data

``` r
SWAT_simplified <- SWAT_simplified %>% 
  mutate(Class = Parameter_List$Class[match(PARAMETER, Parameter_List$PARAMETER)])
```

## How are contaminant classes distributed among sampling events?

``` r
SWAT_simplified %>% 
  group_by(SiteCode, SAMPLE_DATE, Class) %>%
  summarize(n = sum(! is.na(CONCENTRATION)),
            .groups = 'drop') %>%
  pivot_wider(names_from = Class, values_from = n, values_fill = 0) %>%
  arrange(PCB, PAH) %>%
  kable()
```

| SiteCode | SAMPLE\_DATE | Metal | PAH | PAH Calculated |  PCB | PCB Calculated | PCB Mixture | Pesticide | Pesticides Calculated | Physical | Species | PFC | Dioxin | Dioxin Calculated |
| :------- | :----------- | ----: | --: | -------------: | ---: | -------------: | ----------: | --------: | --------------------: | -------: | ------: | --: | -----: | ----------------: |
| CBHRHR   | 2007-10-18   |   132 |   0 |              0 |    0 |              0 |           0 |         0 |                     0 |        4 |       4 |   0 |      0 |                 0 |
| CBMBBH   | 2016-09-22   |     0 |   0 |              0 |    0 |              0 |           0 |         0 |                     0 |        4 |       4 |   4 |      0 |                 0 |
| CBMCMC   | 2006-10-12   |     0 |   0 |              0 |    0 |              0 |           0 |        23 |                     0 |        6 |       3 |   0 |      0 |                 0 |
| CBGDSW   | 2003-10-01   |     0 |   0 |              0 |    2 |              0 |           0 |         0 |                     0 |        8 |       4 |   0 |      9 |                 8 |
| CBRYMT   | 2003-10-01   |     0 |   0 |              0 |    3 |              0 |           0 |         0 |                     0 |        8 |       4 |   0 |      9 |                 8 |
| CBFROR   | 2003-10-01   |     0 |   0 |              0 |    4 |              0 |           0 |         0 |                     0 |        8 |       4 |   0 |     13 |                 8 |
| CBRYMT   | 2003-11-04   |     0 |   0 |              0 |   53 |              0 |           0 |         0 |                     0 |        8 |       4 |   0 |      0 |                 0 |
| CBFROR   | 2003-10-16   |     0 |   0 |              0 |   58 |              0 |           0 |         0 |                     0 |       16 |       8 |   0 |     17 |                 8 |
| CBFROR   | 2003-11-17   |    44 |  30 |              0 |  123 |              0 |          78 |        10 |                     0 |       10 |       7 |   0 |      0 |                 0 |
| CBGDSW   | 2003-11-17   |    45 |  12 |              0 |  149 |              0 |          96 |         8 |                     0 |       12 |       8 |   0 |      0 |                 0 |
| CBGDCC   | 2006-10-17   |    58 |  67 |              0 |  314 |             21 |          99 |        47 |                     0 |       21 |       3 |   0 |      0 |                 0 |
| CBBBBB   | 2006-11-29   |    64 |  69 |              0 |  316 |             21 |          99 |        71 |                     0 |       16 |       3 |   0 |      0 |                 0 |
| CBMCMC   | 2006-10-30   |    58 |  45 |              0 |  320 |             21 |          99 |        16 |                     0 |       21 |       4 |   0 |      0 |                 0 |
| CBEEEE   | 2017-10-11   |    72 | 423 |            342 |  810 |             63 |         288 |         0 |                     0 |        9 |       3 |   0 |      0 |                 0 |
| CBMCMC   | 2014-09-17   |    66 | 405 |            342 |  978 |             63 |         297 |         0 |                     0 |       18 |       3 |   0 |      0 |                 0 |
| CBMCMC   | 2017-10-12   |    96 | 561 |            456 | 1086 |             84 |         384 |         0 |                     0 |       20 |       4 |   0 |      0 |                 0 |
| CBSPSP   | 2010-10-14   |    96 | 552 |            480 | 1098 |             84 |         384 |       114 |                    36 |       20 |       4 |   0 |      0 |                 0 |
| CBANAN   | 2007-10-18   |   132 | 270 |             36 | 1155 |             72 |         384 |        96 |                    36 |       12 |       4 |   0 |      0 |                 0 |
| CBEEEE   | 2007-10-31   |   132 | 345 |             45 | 1164 |             72 |         384 |       120 |                    72 |       12 |       4 |  12 |      0 |                 0 |
| CBLNFT   | 2009-10-27   |    96 | 516 |            459 | 1182 |             84 |         390 |       231 |                    36 |       12 |       4 |   0 |      0 |                 0 |
| CBFRMR   | 2007-11-05   |   132 | 264 |             36 | 1191 |             72 |         384 |       126 |                    72 |       12 |       4 |  12 |      0 |                 0 |
| CBMBBR   | 2009-09-28   |    96 | 432 |            438 | 1203 |             84 |         387 |       195 |                    36 |       12 |       4 |   0 |      0 |                 0 |
| CBQHQH   | 2009-09-24   |    96 | 456 |            429 | 1209 |             93 |         396 |        93 |                    36 |       12 |       4 |   0 |      0 |                 0 |
| CBHWNP   | 2014-09-19   |    94 | 516 |            453 | 1212 |             84 |         396 |         0 |                     0 |       32 |       4 |  12 |      0 |                 0 |
| CBJWPB   | 2007-10-22   |   132 | 261 |             36 | 1218 |             72 |         387 |       105 |                    36 |       12 |       4 |   0 |      0 |                 0 |
| CBEEEE   | 2009-11-10   |    96 | 537 |            462 | 1236 |             84 |         390 |       225 |                    36 |       12 |       4 |   0 |      0 |                 0 |
| CBMBMB   | 2008-10-21   |   138 | 267 |             36 | 1239 |             72 |         393 |        42 |                    63 |       12 |       4 |   0 |      0 |                 0 |
| CBPRMT   | 2008-11-18   |   132 | 276 |             36 | 1248 |             72 |         396 |        57 |                    45 |       20 |       4 |  12 |    141 |                36 |
| CBEEEE   | 2013-10-09   |    96 | 564 |            474 | 1248 |             84 |         393 |         0 |                     0 |       28 |       4 |  12 |      0 |                 0 |
| CBSPSP   | 2007-10-31   |   132 | 276 |             36 | 1251 |             72 |         384 |       135 |                    36 |       12 |       4 |   0 |      0 |                 0 |
| CBHRHR   | 2007-10-22   |     0 | 276 |             36 | 1254 |             72 |         396 |       114 |                    36 |        8 |       4 |   0 |      0 |                 0 |
| CBFRIR   | 2009-09-22   |    96 | 552 |            480 | 1257 |             84 |         396 |       210 |                    36 |       12 |       4 |   0 |      0 |                 0 |
| CBMCMC   | 2011-10-04   |    96 | 531 |            468 | 1272 |             84 |         396 |       129 |                    36 |       20 |       4 |   0 |      0 |                 0 |
| CBSPSP   | 2015-09-21   |    96 | 561 |            456 | 1275 |             84 |         393 |         0 |                     0 |       20 |       4 |   0 |      0 |                 0 |
| CBEEEE   | 2011-10-03   |    96 | 552 |            468 | 1278 |             93 |         393 |       132 |                    36 |       20 |       4 |   0 |      0 |                 0 |
| CBMBBH   | 2007-11-05   |   132 | 276 |             36 | 1281 |             72 |         396 |        87 |                    54 |       12 |       4 |   0 |      0 |                 0 |
| CBMCMC   | 2009-11-10   |    96 | 468 |            456 | 1287 |             84 |         396 |       213 |                    36 |       12 |       4 |   0 |      0 |                 0 |
| CBEEEE   | 2015-09-22   |    96 | 561 |            468 | 1290 |             84 |         396 |         0 |                     0 |       20 |       4 |   0 |      0 |                 0 |
| CBMBBH   | 2014-09-29   |    94 | 519 |            456 | 1299 |             84 |         396 |         0 |                     0 |       32 |       4 |   9 |      0 |                 0 |
| CBSPSP   | 2012-09-25   |    96 | 558 |            480 | 1302 |             84 |         396 |       108 |                    36 |       36 |       4 |   0 |      0 |                 0 |

What that shows is that MOST samples include data from most parameters,
but:

1.  The CALCULATED variables are not consistently provided
2.  PCBs, and Dioxins and Furans, and PFCs are each more specialized,
    and less frequently sampled.

# Totals And Calculations

Many parameters are TOTALS or Calculated sums of related contaminants.
These are derived parameters which may be of special interest for
summaries for the State of Casco Bay report, but they are not primary
analytic values.

These pose challenges in interpreting secondary data such as from EGAD
because: 1. Totals are not provided for all samples, potentially biasing
results 2. Methods for calculating totals are not provided in the
available metadata 3. Totals must be based on specific assumptions for
handling non-detects.

## List of Total And Calculated Values

``` r
SWAT_simplified %>%
  select(PARAMETER, `TEST METHOD`) %>%
  filter(grepl('TOTAL', PARAMETER, ignore.case = TRUE) |
         grepl('CALCULATED', `TEST METHOD`)) %>%
  group_by(`TEST METHOD`, PARAMETER) %>%
  summarize(.groups = 'drop') %>%
  rename(Parameter = PARAMETER, Test = `TEST METHOD`) %>%
  kable()
```

| Test               | Parameter                          |
| :----------------- | :--------------------------------- |
| CALCULATED         | C1-ACENAPHTHENES                   |
| CALCULATED         | C1-BENZO\[A\]ANTHRACENES/CHRYSENES |
| CALCULATED         | C1-BENZOFLUORANTHENES/BENZOPYRENES |
| CALCULATED         | C1-BIPHENYLS                       |
| CALCULATED         | C1-DIBENZOTHIOPHENES               |
| CALCULATED         | C1-FLUORANTHENES/PYRENES           |
| CALCULATED         | C1-FLUORENES                       |
| CALCULATED         | C1-NAPHTHALENES                    |
| CALCULATED         | C1-PHENANTHRENES/ANTHRACENES       |
| CALCULATED         | C2-BENZO\[A\]ANTHRACENES/CHRYSENES |
| CALCULATED         | C2-BENZOFLUORANTHENES/BENZOPYRENES |
| CALCULATED         | C2-BIPHENYLS                       |
| CALCULATED         | C2-DIBENZOTHIOPHENES               |
| CALCULATED         | C2-FLUORANTHENES/PYRENES           |
| CALCULATED         | C2-FLUORENES                       |
| CALCULATED         | C2-NAPHTHALENES                    |
| CALCULATED         | C2-PHENANTHRENES/ANTHRACENES       |
| CALCULATED         | C3-BENZO\[A\]ANTHRACENES/CHRYSENES |
| CALCULATED         | C3-DIBENZOTHIOPHENES               |
| CALCULATED         | C3-FLUORANTHENES/PYRENES           |
| CALCULATED         | C3-FLUORENES                       |
| CALCULATED         | C3-NAPHTHALENES                    |
| CALCULATED         | C3-PHENANTHRENES/ANTHRACENES       |
| CALCULATED         | C4-BENZO\[A\]ANTHRACENES/CHRYSENES |
| CALCULATED         | C4-DIBENZOTHIOPHENES               |
| CALCULATED         | C4-FLUORANTHENES/PYRENES           |
| CALCULATED         | C4-NAPHTHALENES                    |
| CALCULATED         | C4-PHENANTHRENES/ANTHRACENES       |
| CALCULATED         | PCB TOTAL TEQ (ND=0)               |
| CALCULATED         | PCB TOTAL TEQ (ND=1/2 DL)          |
| CALCULATED         | PCB TOTAL TEQ (ND=DL)              |
| CALCULATED         | PCBS                               |
| CALCULATED WHO2005 | C1-ACENAPHTHENES                   |
| CALCULATED WHO2005 | C1-BENZO\[A\]ANTHRACENES/CHRYSENES |
| CALCULATED WHO2005 | C1-BENZOFLUORANTHENES/BENZOPYRENES |
| CALCULATED WHO2005 | C1-BIPHENYLS                       |
| CALCULATED WHO2005 | C1-DIBENZOTHIOPHENES               |
| CALCULATED WHO2005 | C1-FLUORANTHENES/PYRENES           |
| CALCULATED WHO2005 | C1-FLUORENES                       |
| CALCULATED WHO2005 | C1-NAPHTHALENES                    |
| CALCULATED WHO2005 | C1-PHENANTHRENES/ANTHRACENES       |
| CALCULATED WHO2005 | C2-BENZO\[A\]ANTHRACENES/CHRYSENES |
| CALCULATED WHO2005 | C2-BENZOFLUORANTHENES/BENZOPYRENES |
| CALCULATED WHO2005 | C2-BIPHENYLS                       |
| CALCULATED WHO2005 | C2-DIBENZOTHIOPHENES               |
| CALCULATED WHO2005 | C2-FLUORANTHENES/PYRENES           |
| CALCULATED WHO2005 | C2-FLUORENES                       |
| CALCULATED WHO2005 | C2-NAPHTHALENES                    |
| CALCULATED WHO2005 | C2-PHENANTHRENES/ANTHRACENES       |
| CALCULATED WHO2005 | C3-BENZO\[A\]ANTHRACENES/CHRYSENES |
| CALCULATED WHO2005 | C3-DIBENZOTHIOPHENES               |
| CALCULATED WHO2005 | C3-FLUORANTHENES/PYRENES           |
| CALCULATED WHO2005 | C3-FLUORENES                       |
| CALCULATED WHO2005 | C3-NAPHTHALENES                    |
| CALCULATED WHO2005 | C3-PHENANTHRENES/ANTHRACENES       |
| CALCULATED WHO2005 | C4-BENZO\[A\]ANTHRACENES/CHRYSENES |
| CALCULATED WHO2005 | C4-DIBENZOTHIOPHENES               |
| CALCULATED WHO2005 | C4-FLUORANTHENES/PYRENES           |
| CALCULATED WHO2005 | C4-NAPHTHALENES                    |
| CALCULATED WHO2005 | C4-PHENANTHRENES/ANTHRACENES       |
| CALCULATED WHO2005 | DX TOTAL TEQ (ND=0)                |
| CALCULATED WHO2005 | DX TOTAL TEQ (ND=1/2 DL)           |
| CALCULATED WHO2005 | DX TOTAL TEQ (ND=DL)               |
| CALCULATED WHO2005 | PCB TOTAL TEQ (ND=0)               |
| CALCULATED WHO2005 | PCB TOTAL TEQ (ND=1/2 DL)          |
| CALCULATED WHO2005 | PCB TOTAL TEQ (ND=DL)              |
| CALCULATED WHO2005 | PCBS                               |
| E160.3             | SOLIDS-TOTAL RESIDUE (TS)          |
| E1613              | DX TOTAL TEQ (ND=1/2 DL)           |
| E1613              | DX TOTAL TEQ (ND=DL)               |
| E1668A             | PCB TOTAL TEQ (ND=0)               |
| E1668A             | PCB TOTAL TEQ (ND=1/2 DL)          |
| E1668A             | PCB TOTAL TEQ (ND=DL)              |
| E1668A             | TOTAL PCB-D                        |
| E1668A             | TOTAL PCB-H                        |
| E1668A             | TOTAL PCB-O                        |
| MLA-007-E1         | TOTAL PESTICIDES21-D               |
| MLA-007-E1         | TOTAL PESTICIDES21-H               |
| MLA-007-E1         | TOTAL PESTICIDES21-O               |
| MLA-007-E2         | TOTAL PESTICIDES21-D               |
| MLA-007-E2         | TOTAL PESTICIDES21-H               |
| MLA-007-E2         | TOTAL PESTICIDES21-O               |
| MLA-035            | TOTAL PESTICIDES21-D               |
| MLA-035            | TOTAL PESTICIDES21-H               |
| MLA-035            | TOTAL PESTICIDES21-O               |
| NOAA1998M          | SOLIDS-TOTAL RESIDUE (TS)          |
| SM2540G            | SOLIDS-TOTAL RESIDUE (TS)          |
| SM2540GM           | SOLIDS-TOTAL RESIDUE (TS)          |
| SOP\_MSLC003       | SOLIDS-TOTAL RESIDUE (TS)          |
| SW8270CM           | TOTAL PAH-D                        |
| SW8270CM           | TOTAL PAH-H                        |
| SW8270CM           | TOTAL PAH-O                        |
| SW8270CM           | TOTAL PAH19-D                      |
| SW8270CM           | TOTAL PAH19-H                      |
| SW8270CM           | TOTAL PAH19-O                      |
| SW8270CM           | TOTAL PAH24-D                      |
| SW8270CM           | TOTAL PAH24-H                      |
| SW8270CM           | TOTAL PAH24-O                      |
| SW8270CM           | TOTAL PAH40-D                      |
| SW8270CM           | TOTAL PAH40-H                      |
| SW8270CM           | TOTAL PAH40-O                      |

## Alternate Versions of Totals?

Many TOTAL parameters come in triplets – with a suffix of “-D’, or ‘-H’
or ‘-O’. It appears those are for totals calculated using different
assumptions about how to address non-detects, specifically, totals
with”-D" stands for “detection limit”, “-H” stands for “Half Detection
Limit”, and “-O” stands for “Zero”.

# Check if Data is Consistent with Our Interpretation

If our interpretation is correct, any total with -D is greater than or
equal to related Totals with -H, which in turn will be greater than any
total with -O.

We can check that as follows:

``` r
SWAT_simplified %>%
  filter (`WEIGHT BASIS` == 'WET') %>%
  select(Code, ANALYSIS_LAB_SAMPLE_ID, PARAMETER, CONCENTRATION) %>%
  filter(grepl('TOTAL', PARAMETER, ignore.case = TRUE)) %>%
  filter(grepl('-[DHO]$', PARAMETER)) %>%
  mutate(suffix = substr(PARAMETER, nchar(PARAMETER), nchar(PARAMETER)),
         prefix = substr(PARAMETER, 1, nchar(PARAMETER)-2)) %>%
  pivot_wider(c(Code, prefix, ANALYSIS_LAB_SAMPLE_ID),
              names_from = suffix, values_from = CONCENTRATION) %>%
  mutate(trouble = ! (O <=H & H <= D)) %>%
  filter(trouble)
```

    ## # A tibble: 0 x 7
    ## # ... with 7 variables: Code <chr>, prefix <chr>, ANALYSIS_LAB_SAMPLE_ID <chr>,
    ## #   O <dbl>, H <dbl>, D <dbl>, trouble <lgl>

So, we appear to be correct.

Note than none of these uses statistical procedures for estimating
non-detect values. It would be nice to be able to use more modern
approaches, but that would require detailed understanding of how each of
the totals were calculated.

# How Were Totals Calculated?

We have limited metadata showing us how specific calculated and total
values were calculated. It appears some detail is available in papers
referenced in recent SWAT reports, especially LeBlanc et al., 2009.

> Leblanc, L.A., Krahforst, C.F., Aubé, J., Roach, S., Brun, G.,
> Harding, G., Hennigar, P., Page, D., Jones, S., Shaw, S., Stahlnecker,
> J., Schwartz, J., Taylor, D., Thorpe, B., & Wells, P. (2009).
> Eighteenth Year of the Gulf of Maine Environmental Monitoring Program.
> Gulf of Maine Council on the Marine Environment.

Tables in that report appear to provide recipies for calculating several
important data summaries.

Analysis of PAH total and PCB totals took a lot of time and effort, so
we have broken them off into separate notebooks: \*
“SWAT\_data\_examinatin\_PAH.Rmd” \*
"SWAT\_data\_examination\_PCB.Rmd:
