Review of Maine DEP EGAD Mussel Tissue Toxics PAHs
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
9/18/2020

  - [Introduction](#introduction)
  - [PAHs Totals](#pahs-totals)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder Reference](#establish-folder-reference)
      - [Copy Data](#copy-data)
      - [Remove duplicates](#remove-duplicates)
      - [Simplify Data and Add Unique Sample
        Codes](#simplify-data-and-add-unique-sample-codes)
      - [Add Class to the Working Data](#add-class-to-the-working-data)
  - [When Does Each Total Exist?](#when-does-each-total-exist)
  - [Are Total PAHs and Total PAH40
    equal?](#are-total-pahs-and-total-pah40-equal)
  - [Preparing to Calculate Diagnostic
    Totals](#preparing-to-calculate-diagnostic-totals)
      - [Load PAH Lists](#load-pah-lists)
      - [Create Helper Function](#create-helper-function)
  - [Calculating PAH 19](#calculating-pah-19)
  - [Warnings about Missing Parameters for
    PAH19](#warnings-about-missing-parameters-for-pah19)
  - [How are Benzoflouranthenes
    Reported?](#how-are-benzoflouranthenes-reported)
      - [Relatioship between Totals and
        benzoflouranthenes](#relatioship-between-totals-and-benzoflouranthenes)
  - [“C-” Totals](#c--totals)
  - [Diagnotic Ratios](#diagnotic-ratios)
  - [Next Steps](#next-steps)

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

In this notebook we review how PAH values were handled in the data and
try to recreate some calculations to check our understanding, and in
hopes that we could recreate some calculations using more modern
statistical methods for addressing non-detects.

# PAHs Totals

In the Leblanc et al. 2009 paper (Table 2), totals (for PAHs) were
described as follows:

| Parameter               | Definition from Leblanc et al 2009                             |
| ----------------------- | -------------------------------------------------------------- |
| PAH19                   | Sum of substituted PAHS                                        |
| —————–                  | ——————————————-                                                |
| PAH24                   | PAH19 plus “a few alkyl-substituted PAHs, as indicated”        |
| —————–                  | ——————————————-                                                |
| Total PAH               | Sum of 40 PAH compounds in the table.                          |
| —————–                  | ——————————————-                                                |
| PCB21                   | The Sum of 21 PCB Congeners, calculated to be consiustent with |
| NOAA Status and Trends. |                                                                |
| —————–                  | ——————————————-                                                |
| PEST21                  | Sum of chlorinated PEsticides and DDTs                         |

The exact PAH’s included in each of the totals are indicated in the
tables in LeBlanc et al. 2009. We added tabs to the Excel File
“Parameter List.xlsx” that include each of the parameters metnioned in
that table, as best as we can reconstruct them.

**The Tables in LeBlanc et al. 2009 contain what appear to be errors.
For example, there are 41 PAH compounds listed in Table 2, not 40 as
suggested by the nomenclature.**

A more thorough review of which PAHs were measured from many of these
same field samples is available in the MAine DEP SWAT program’s
2017-2018 report to the legislature:

> Maine Department of Environmental Protection (DEP) 2019. Surface Water
> Ambient Toxics Monitoring Program 2017-2018 Report to the Joint
> Standing Committee on Environment and Natural Resources, 129th
> Legislature, First Session. Available at:
> <http://www.maine.gov/dep/water/monitoring/toxics/swat/index.htm>

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

## Add Class to the Working Data

We can then read in the resulting Excel File to provide groupings…

``` r
Parameter_List <- read_excel(file.path(parent,"Parameter List.xlsx"), 
    sheet = "Parameter List") %>%
  mutate(Class = factor(Class)) %>%
  arrange(Class, PARAMETER)

SWAT_simplified <- SWAT_simplified %>% 
  mutate(Class = Parameter_List$Class[match(PARAMETER, Parameter_List$PARAMETER)])
```

# When Does Each Total Exist?

We look narrowly at WET weights and totals that treat non-detects as
zero. We make the assumption for now that the lessons learned from that
subset apply to the related data.

``` r
SWAT_simplified %>%
  select(Code, ANALYSIS_LAB_SAMPLE_ID, `WEIGHT BASIS`,
         PARAMETER, CONCENTRATION) %>%
  filter (`WEIGHT BASIS` == 'WET') %>%
  filter(grepl('-O$', PARAMETER)) %>%
  filter(grepl("TOTAL PAH/d/d", PARAMETER) |
           grepl("TOTAL PAH", PARAMETER)) %>%
  pivot_wider(c(Code, ANALYSIS_LAB_SAMPLE_ID),
              names_from = PARAMETER, values_from = CONCENTRATION) %>%
  mutate(HasTotal = !(is.na(`TOTAL PAH-O` )),
         Has40 = !(is.na( `TOTAL PAH40-O`)),
         Has24 = !(is.na( `TOTAL PAH24-O`)),
         Has19 = !(is.na( `TOTAL PAH19-O`))) %>%
  select(-`TOTAL PAH-O`, -`TOTAL PAH40-O`,
         -`TOTAL PAH24-O`, -`TOTAL PAH19-O`) %>%
  group_by(HasTotal) %>%
  summarize(With40 = sum(Has40),
            With24 = sum(Has24),
            With19 = sum(Has19),
            .groups = 'drop')
```

    ## # A tibble: 2 x 4
    ##   HasTotal With40 With24 With19
    ##   <lgl>     <int>  <int>  <int>
    ## 1 FALSE         0      0     37
    ## 2 TRUE         70     70     70

So measures of the totals are usually all found together. The 19 PAH
subset is sometimes available when the other totals are NOT available.

# Are Total PAHs and Total PAH40 equal?

``` r
SWAT_simplified %>%
  select(Code, ANALYSIS_LAB_SAMPLE_ID, `WEIGHT BASIS`,
         PARAMETER, CONCENTRATION) %>%
  filter (`WEIGHT BASIS` == 'WET') %>%
  filter(grepl('-O$', PARAMETER)) %>%
  filter(grepl("TOTAL PAH40", PARAMETER) |
           grepl("TOTAL PAH", PARAMETER)) %>%
  pivot_wider(c(Code, ANALYSIS_LAB_SAMPLE_ID),
              names_from = PARAMETER, values_from = CONCENTRATION) %>%
  mutate(same = `TOTAL PAH40-O` == `TOTAL PAH-O`) %>%
  group_by(same) %>%
  summarize(n = n(),
            totallarger = sum(`TOTAL PAH40-O` < `TOTAL PAH-O` ))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 3
    ##   same      n totallarger
    ##   <lgl> <int>       <int>
    ## 1 FALSE    70          70
    ## 2 NA       37          NA

The two values are NOT the same. When they both exist, TOTAL PAH is
always larger.

# Preparing to Calculate Diagnostic Totals

## Load PAH Lists

``` r
pah19list <- read_excel(file.path(parent,"Parameter List.xlsx"), 
                        sheet = "PAH19 List", col_names = 'Parameter') %>%
  pull('Parameter')
pah24list <- read_excel(file.path(parent,"Parameter List.xlsx"), 
                        sheet = "PAH24 List", col_names = 'Parameter') %>%
  pull('Parameter')
pah40list <- read_excel(file.path(parent,"Parameter List.xlsx"), 
                        sheet = "PAH40 List", col_names = 'Parameter') %>%
  pull('Parameter')
```

## Create Helper Function

We need a helper function to make facilitate calculating selective sums.

We want to traverse a pair of vectors so that we add the second value to
an accumulating sum if and only if the value in out first vector is in
our selection.

But we also want to highlight problems with the input data, such has
missing values. For a sum to be 100% legitimate, it needs to include all
values.

(Option: Could add parameter to allow user to select how this program
behaves if parameters are missing. Warn, Return NA, Raise Error.)

``` r
add_if_list <- function(parms, vals, sel) {
  stopifnot(length(parms) == length(vals))
  if (! all(sel %in% parms)) {
    miss <- sel[ ! (sel %in% parms)]
      warning('The following parameters are missing:\n')
      walk(miss, function(X) warning('\t', X, '\n'))
  }
  sum(vals[parms %in% sel], na.rm = TRUE)
}
```

Note that this function warns NOT if data for a particular parameter is
missing, but if there is no entry for that parameter. This allows us to
accommodate right censored values, where CONCENTRATION is NA, but there
is information on the detection limits (in another column).

# Calculating PAH 19

We can calculate PAH19 totals and compare results to the `TOTAL PAH19`
results provided for (most) samples by DEP.

``` r
tst <- SWAT_simplified %>%
  select(Code, ANALYSIS_LAB_SAMPLE_ID, `WEIGHT BASIS`,
         PARAMETER, CONCENTRATION, Class)%>%
  filter (`WEIGHT BASIS` == 'WET') %>%
  #filter(grepl('CBANAN_REP_[0-9]_2007_1', Code)) %>%
  filter(grepl('PAH', Class)) %>%
  group_by(Code, ANALYSIS_LAB_SAMPLE_ID) %>%
  summarize(PAH19 = add_if_list(PARAMETER, CONCENTRATION, pah19list),
            hasoriginal = any(PARAMETER == 'TOTAL PAH19-O'),
            original = sum(if_else(PARAMETER == 'TOTAL PAH19-O',
                                   CONCENTRATION,0)),
            difference = PAH19-original,
            .groups = 'drop') %>%
  mutate(original = if_else(hasoriginal, original, NA_real_),
         difference = if_else(hasoriginal, difference, NA_real_)) %>%
  select ( - hasoriginal)
```

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     DIBENZOTHIOPHENE

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(B)FLUORANTHENE

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

    ## Warning in add_if_list(PARAMETER, CONCENTRATION, pah19list): The following parameters are missing:

    ## Warning in .f(.x[[i]], ...):     BENZO[B,J,K]FLUORANTHENES

    ## Warning in .f(.x[[i]], ...):     BENZO(K)FLUORANTHENE

``` r
tst
```

    ## # A tibble: 123 x 5
    ##    Code                     ANALYSIS_LAB_SAMPLE_ID     PAH19 original difference
    ##    <chr>                    <chr>                      <dbl>    <dbl>      <dbl>
    ##  1 CBANAN_REP_1_2007_1      L10702-1                    27.1     26.8      0.258
    ##  2 CBANAN_REP_2_2007_1      L10702-2                    20.0     20.5     -0.538
    ##  3 CBANAN_REP_3_2007_1      L10702-3                    26.3     27.1     -0.779
    ##  4 CBANAN_REP_4_2007_1      L10702-4                    35.8     35.2      0.545
    ##  5 CBBBBB_REP_1_2006_4      BACK COVE, PORTLAND - REP~  31.5     NA       NA    
    ##  6 CBBBBB_REP_2_2006_4      BACK COVE, PORTLAND - REP~  32.1     NA       NA    
    ##  7 CBBBBB_REP_3_2006_4      BACK COVE, PORTLAND - REP~  33.7     NA       NA    
    ##  8 CBEEEE_REP_1_2007_3      L10702-17                   36.7     36.8     -0.105
    ##  9 CBEEEE_REP_1_2009_5      L13939-1                    42.2     44.4     -2.18 
    ## 10 CBEEEE_REP_1_2011_2011_1 L17179-5                    38.4     34.2      4.19 
    ## # ... with 113 more rows

``` r
mean(tst$difference, na.rm=TRUE)
```

    ## [1] -1.005807

``` r
sd(tst$difference, na.rm=TRUE)
```

    ## [1] 2.916763

``` r
max(tst$difference, na.rm=TRUE)
```

    ## [1] 4.34

``` r
min(tst$difference, na.rm=TRUE)
```

    ## [1] -14.178

So, our totals are not quite the same. They are, on average, slightly
lower, but not consistently so. Occasionally, differences are very
large. Not good….

Analysis of the other PAH totals (PAH24, PAH40) show similar differences
between original reported totals, and the totals we calculated. We are
unable to resolve those differences, which probably relate to the exact
compounds added for each sample.

# Warnings about Missing Parameters for PAH19

The warnings produced by our helper function help identify data
inconsistnecies. IN particular, they highlight differences among samples
in the way they handle the following parameters:  
\* BENZO(B)FLUORANTHENE \* BENZO(K)FLUORANTHENE \* DIBENZOTHIOPHENE

The thiophenes are not measured in all samples.

Handling of the Benzoflouranthenes differs because BENZO(B)FLUORANTHENE,
BENZO(j)FLUORANTHENE and BENZO(K)FLUORANTHENE are chemically quite
similar. They are often reported as a sum, under a variety of
conventions, including:

``` r
BenzoFs <- SWAT_simplified %>%
  select(PARAMETER, Class) %>%
  filter(Class == 'PAH') %>%
  filter(grepl('BENZO', PARAMETER)) %>%
  filter(grepl('FLUORANTHENE', PARAMETER)) %>%
  unique()  %>% pull(PARAMETER)
(BenzoFs <- BenzoFs[-2])   # remove a spurious match
```

    ## [1] "BENZO[B,J,K]FLUORANTHENES" "BENZOFLUORANTHENE"        
    ## [3] "BENZO(B)FLUORANTHENE"      "BENZO[J,K]FLUORANTHENES"  
    ## [5] "BENZO(K)FLUORANTHENE"

# How are Benzoflouranthenes Reported?

``` r
SWAT_simplified %>%
  select(Code, ANALYSIS_LAB_SAMPLE_ID, `WEIGHT BASIS`,
         `TEST METHOD`, PARAMETER, CONCENTRATION, Class)%>%
  filter (`WEIGHT BASIS` == 'WET') %>%
  filter(PARAMETER %in% BenzoFs) %>%
  pivot_wider(c(Code, ANALYSIS_LAB_SAMPLE_ID, `TEST METHOD`),
                names_from = PARAMETER, values_from = CONCENTRATION) %>%
  rename_all(~ c('Code', 'Samp', 'Method', 'BJK', 'None', 'B', 'JK', 'K'))
```

    ## # A tibble: 123 x 8
    ##    Code            Samp                  Method     BJK   None     B    JK     K
    ##    <chr>           <chr>                 <chr>    <dbl>  <dbl> <dbl> <dbl> <dbl>
    ##  1 CBJWPB_REP_1_2~ L10702-41 L           SW8270~  0.257 NA        NA    NA    NA
    ##  2 CBJWPB_REP_2_2~ L10702-42 L           SW8270~  0.274 NA        NA    NA    NA
    ##  3 CBJWPB_REP_3_2~ L10702-43 LI          SW8270~  0.31  NA        NA    NA    NA
    ##  4 CBJWPB_REP_4_2~ L10702-44 L           SW8270~  0.286 NA        NA    NA    NA
    ##  5 CBANAN_REP_1_2~ L10702-1              SW8270~  2.53  NA        NA    NA    NA
    ##  6 CBANAN_REP_2_2~ L10702-2              SW8270~  1.87  NA        NA    NA    NA
    ##  7 CBANAN_REP_3_2~ L10702-3              SW8270~  2.18  NA        NA    NA    NA
    ##  8 CBANAN_REP_4_2~ L10702-4              SW8270~  3.36  NA        NA    NA    NA
    ##  9 CBMCMC_REP_1_2~ L13939-9              SW8270~  1.61  NA        NA    NA    NA
    ## 10 CBMCMC_REP_2_2~ MILL CREEK, FALMOUTH~ SW8270~ NA      0.914    NA    NA    NA
    ## # ... with 113 more rows

The data falls into FOUR categories:

1.  ONLY BENZO\[B,J,K\]FLUORANTHENES reported  
2.  ONLY BENZOFLUORANTHENE reported (2006 data) These samples report the
    same `TEST METHOD` as the ones reporting
    BENZO\[B,J,K\]FLUORANTHENES, so we suspect the terms are synonyms,
    but can not be certain.
3.  BENZO\[J,K\]FLUORANTHENES and BENZO(B)FLUORANTHENE reported
    separately. These samples also report the same `TEST METHOD`. Sum of
    the two should be similar to the first two categories, except that
    differences may arise due to treatment of non-detects. Such
    differences should be small.
4.  Although BENZO(K)FLOURANTHENE was reported above detection limits
    only once, examination of that sample and its field replicates show
    that for those samples, both BENZO(B)FLOURANTHENE and
    BENZO(K)FLOURANTHENE were reported, but almost always below
    detection limits. These samples use a slightly different `TEST
    METHOD`. It is not clear whether the sum of these two values is
    comparable to teh prior three methods or not.

All benzoflouranthenes can be summed, without double counting, but
totals may not be directly comparable, especially for Group (4). In
particular, PAH19 totals (as described in Leblanc et al. 2009) nominally
include BENZO(B)FLUORANTHENE and BENZO(K)FLUORANTHENE, but not
BENZO(J)FLUORANTHENE.

We include all of these near-synonyms in our list of parameters to sum
to produce PAH19, PAH24, and PAH40 totals. This appears appropriate
since different samples do not “mix and match” how results were
reported.

**Done.**

## Relatioship between Totals and benzoflouranthenes

Lets see what totals are reported with each pattern.

``` r
SWAT_simplified %>%
  select(Code, ANALYSIS_LAB_SAMPLE_ID, `WEIGHT BASIS`,
         PARAMETER, CONCENTRATION, Class)%>%
  filter (`WEIGHT BASIS` == 'WET') %>%
  filter(PARAMETER %in% BenzoFs | grepl('TOTAL*.PAH', PARAMETER)) %>%
  filter(! (grepl('-H', PARAMETER) | grepl('-D', PARAMETER))) %>%
  pivot_wider(c(Code, ANALYSIS_LAB_SAMPLE_ID),
                names_from = PARAMETER, values_from = CONCENTRATION) %>%
  mutate(across(3:11, function(X) as.integer(X>0))) %>%
  select(-`BENZO(B)FLUORANTHENE`, -BENZOFLUORANTHENE) %>%
  group_by(`BENZO[B,J,K]FLUORANTHENES`, `BENZO[J,K]FLUORANTHENES`, `BENZO(K)FLUORANTHENE`) %>%
  summarize(across(3:6, sum, na.rm=TRUE), .groups = 'keep')
```

    ## # A tibble: 4 x 7
    ## # Groups:   BENZO[B,J,K]FLUORANTHENES, BENZO[J,K]FLUORANTHENES,
    ## #   BENZO(K)FLUORANTHENE [4]
    ##   `BENZO[B,J,K]FL~ `BENZO[J,K]FLUO~ `BENZO(K)FLUORA~ `TOTAL PAH19-O`
    ##              <int>            <int>            <int>           <int>
    ## 1                1               NA               NA              73
    ## 2               NA                1               NA              34
    ## 3               NA               NA                1               0
    ## 4               NA               NA               NA               0
    ## # ... with 3 more variables: `TOTAL PAH-O` <int>, `TOTAL PAH24-O` <int>, `TOTAL
    ## #   PAH40-O` <int>

  - BENZO\[BJK\]FLOURANTHENES are reported as TOTAL PAH19 ALWAYS(?)
    other TOTAL PAH values sometimes.  
  - BENZO\[J,K\]FLUORANTHENES and BENZO\[B\]FLUORANTHENES are reported
    with TOTALs  
  - BENZOFLOURANTHENE was NEVER reported with TOTALs  
  - BENZO(K)FLOURANTHENE was NEVER reported with TOTALs

# “C-” Totals

Other Totals and Calculated values follow standard practice. These
include all the values prefaced by “C1-”, “C2-”, “C3-”, and “C4-” except
the diagnostic ratios treated next.

The number following the C represents the number of alkyl carbons added
to a basic PAH structure. Many of these categories originated in the
uncertainty of separating chemically similar compounds using certain
(often older) analytic methods. More recently, with methods better able
to differentiate among these compounds, the totals may be calculated, as
apparently they are here.

``` r
SWAT_simplified %>%
  select(Code, `TEST METHOD`, ANALYSIS_LAB_SAMPLE_ID, `WEIGHT BASIS`,
         PARAMETER, CONCENTRATION, Class)%>%
  filter (`WEIGHT BASIS` == 'WET') %>%
  filter(grepl('C[1-9]-', PARAMETER)) %>%    # C functions
  filter(! grepl('/', PARAMETER)) %>%       # Not c ratios
  pivot_wider(names_from = Class, values_from = CONCENTRATION) %>%
  select( -ANALYSIS_LAB_SAMPLE_ID, -`WEIGHT BASIS`)
```

    ## # A tibble: 980 x 4
    ##    Code                `TEST METHOD`      PARAMETER            `PAH Calculated`
    ##    <chr>               <chr>              <chr>                           <dbl>
    ##  1 CBMCMC_REP_1_2009_5 CALCULATED WHO2005 C4-DIBENZOTHIOPHENES            0.87 
    ##  2 CBMCMC_REP_1_2009_5 CALCULATED WHO2005 C3-DIBENZOTHIOPHENES            1.83 
    ##  3 CBMCMC_REP_1_2009_5 CALCULATED WHO2005 C2-DIBENZOTHIOPHENES            2.49 
    ##  4 CBMCMC_REP_1_2009_5 CALCULATED WHO2005 C1-DIBENZOTHIOPHENES           NA    
    ##  5 CBMCMC_REP_1_2009_5 CALCULATED WHO2005 C1-NAPHTHALENES                 1.23 
    ##  6 CBMCMC_REP_1_2009_5 CALCULATED WHO2005 C2-BIPHENYLS                    1.29 
    ##  7 CBMCMC_REP_1_2009_5 CALCULATED WHO2005 C1-BIPHENYLS                    0.255
    ##  8 CBMCMC_REP_1_2009_5 CALCULATED WHO2005 C3-FLUORENES                   14.4  
    ##  9 CBMCMC_REP_1_2009_5 CALCULATED WHO2005 C2-NAPHTHALENES                 3.37 
    ## 10 CBMCMC_REP_1_2009_5 CALCULATED WHO2005 C2-FLUORENES                    7.58 
    ## # ... with 970 more rows

We have not yet been able to determine for sure which individual PAHs
are summed to produce each of these calaulated totals.

# Diagnotic Ratios

Some of these PARAMETERS are diagnostic RATIOS, not totals. These ratios
are useful for determining the likely sources of the PAH mixtures. Such
“fingerprinting” is not a priority for the 2020 State of the Bay
report.

Here is a list of the diagnostic ratios reported:

``` r
SWAT_simplified %>%
  select(Code, `TEST METHOD`, ANALYSIS_LAB_SAMPLE_ID, `WEIGHT BASIS`,
         PARAMETER, CONCENTRATION, Class)%>%
  filter (`WEIGHT BASIS` == 'WET') %>%
  filter(grepl('C[1-9]-.*?/', PARAMETER)) %>%
  group_by(PARAMETER) %>%
  summarize (Name = first( PARAMETER), .groups = 'drop') %>%
  pull(Name)
```

    ##  [1] "C1-BENZO[A]ANTHRACENES/CHRYSENES"   "C1-BENZOFLUORANTHENES/BENZOPYRENES"
    ##  [3] "C1-FLUORANTHENES/PYRENES"           "C1-PHENANTHRENES/ANTHRACENES"      
    ##  [5] "C2-BENZO[A]ANTHRACENES/CHRYSENES"   "C2-BENZOFLUORANTHENES/BENZOPYRENES"
    ##  [7] "C2-FLUORANTHENES/PYRENES"           "C2-PHENANTHRENES/ANTHRACENES"      
    ##  [9] "C3-BENZO[A]ANTHRACENES/CHRYSENES"   "C3-FLUORANTHENES/PYRENES"          
    ## [11] "C3-PHENANTHRENES/ANTHRACENES"       "C4-BENZO[A]ANTHRACENES/CHRYSENES"  
    ## [13] "C4-FLUORANTHENES/PYRENES"           "C4-PHENANTHRENES/ANTHRACENES"

# Next Steps

In the absence of certainty about how totals were calculated, we are not
in a position to recalculate totals either for basic QA/QC checks or to
use more modern statistical methods for handling non-detects. We are
forced to rely on the diagnostic totals calculated by DEP.
