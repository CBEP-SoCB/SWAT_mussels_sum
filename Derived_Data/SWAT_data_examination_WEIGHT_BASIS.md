Review of Maine DEP EGAD Mussel Tissue Toxics Data Weight Basis
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
  - [Check That Number of LIP \<= DRY \<=
    WET](#check-that-number-of-lip-dry-wet)
  - [Check That Value of LIP \<= DRY \<=
    WET](#check-that-value-of-lip-dry-wet)
  - [Hand Calculate Values for
    Comparison](#hand-calculate-values-for-comparison)
  - [Identify / Remove Suspect Values](#identify-remove-suspect-values)
      - [The Suspect Values](#the-suspect-values)
      - [A Single Logical Test Value](#a-single-logical-test-value)

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

In this notebook, we conduct a basic data quality aSSUrance check,
looking to determine whether values expressed on a wet weight, dry
weight, and lipid weight basis make sense.

  - DRY and LIPID based values are derived from wet weight values, and
    so should be both no more numerous.  
  - Lipid weight is a fraction of dry weight, concentrations expressed
    on a lipid weight basis should always be greater than values
    expressed on a dry weight basis, which should likewise exceed values
    expressed on a wet weight basis.

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
library(htmltools)  # used by knitr called here  to avoid startup text later
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
    ## <bytecode: 0x00000000165a30b0>
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

# Check That Number of LIP \<= DRY \<= WET

``` r
SWAT_simplified %>%
  select(Code, PARAMETER, `WEIGHT BASIS`, CONCENTRATION) %>%
  group_by(PARAMETER) %>%
  summarize(nlip = sum(`WEIGHT BASIS` == 'LIP', na.rm= TRUE),
            ndry = sum(`WEIGHT BASIS` == 'DRY', na.rm= TRUE),
            nwet = sum(`WEIGHT BASIS` == 'WET', na.rm= TRUE),
            problem = ! (nlip<= ndry & ndry <= nwet),
            .groups = 'drop') %>%
    filter(problem)
```

    ## # A tibble: 12 x 5
    ##    PARAMETER                  nlip  ndry  nwet problem
    ##    <chr>                     <int> <int> <int> <lgl>  
    ##  1 ALUMINUM                     45   115   114 TRUE   
    ##  2 CADMIUM                      45   115   114 TRUE   
    ##  3 CHROMIUM                     45   115   114 TRUE   
    ##  4 COPPER                       45   115   114 TRUE   
    ##  5 IRON                         45   115   114 TRUE   
    ##  6 LEAD                         45   115   114 TRUE   
    ##  7 LIPIDS                       91     0   139 TRUE   
    ##  8 NICKEL                       45   115   114 TRUE   
    ##  9 SELENIUM                     45   115   114 TRUE   
    ## 10 SILVER                       45   115   114 TRUE   
    ## 11 SOLIDS-TOTAL RESIDUE (TS)     0    79    36 TRUE   
    ## 12 ZINC                         45   115   114 TRUE

Two of those appear to be effectively attribution problems, since the
parameters apply to whole samples (e.g., percentage of lipids and total
solids).

It’s not clear what is going on with the metals. This appears to be the
result of samples where we have data on a DRY basis but not on a WET
basis.

``` r
SWAT_simplified %>%
  select(Code, PARAMETER, `WEIGHT BASIS`) %>%
  group_by(PARAMETER, Code) %>%
  summarize(nlip = sum(`WEIGHT BASIS` == 'LIP', na.rm= TRUE),
            ndry = sum(`WEIGHT BASIS` == 'DRY', na.rm= TRUE),
            nwet = sum(`WEIGHT BASIS` == 'WET', na.rm= TRUE),
            problem = ! (ndry <= nwet),
            .groups = 'drop') %>%
    filter(problem)
```

    ## # A tibble: 178 x 6
    ##    PARAMETER Code                 nlip  ndry  nwet problem
    ##    <chr>     <chr>               <int> <int> <int> <lgl>  
    ##  1 ALUMINUM  CBBBBB_REP_1_2006_4     1     1     0 TRUE   
    ##  2 ALUMINUM  CBBBBB_REP_2_2006_4     1     1     0 TRUE   
    ##  3 ALUMINUM  CBBBBB_REP_3_2006_4     1     1     0 TRUE   
    ##  4 ALUMINUM  CBGDCC_REP_1_2006_2     1     1     0 TRUE   
    ##  5 ALUMINUM  CBGDCC_REP_2_2006_2     1     1     0 TRUE   
    ##  6 ALUMINUM  CBGDCC_REP_3_2006_2     1     1     0 TRUE   
    ##  7 ALUMINUM  CBMCMC_REP_1_2006_3     1     1     0 TRUE   
    ##  8 ALUMINUM  CBMCMC_REP_2_2006_3     1     1     0 TRUE   
    ##  9 ALUMINUM  CBMCMC_REP_3_2006_3     1     1     0 TRUE   
    ## 10 CADMIUM   CBBBBB_REP_1_2006_4     1     1     0 TRUE   
    ## # ... with 168 more rows

It appears that all data collected in 2006 lacks metals data on a wet
weight basis. Other missing data is associated with the PARAMETER
“SOLIDS-TOTAL RESIDUE (TS)”, where it makes no difference.

This does not need correction, since the missing data will automatically
be left out of any analysis we conduct. If wet weight values were key
for an analysis, we might be able to back calculate them.

# Check That Value of LIP \<= DRY \<= WET

``` r
SWAT_simplified %>%
  select(Code, PARAMETER, `TEST METHOD`, `WEIGHT BASIS`, CONCENTRATION) %>%
  group_by(Code, PARAMETER) %>%
  summarize(method = first(`TEST METHOD`),
            vlip = mean(CONCENTRATION[`WEIGHT BASIS` == 'LIP'], na.rm= TRUE),
            vdry = mean(CONCENTRATION[`WEIGHT BASIS` == 'DRY'], na.rm= TRUE),
            vwet = mean(CONCENTRATION[`WEIGHT BASIS` == 'WET'], na.rm= TRUE),
            n = n(),
            problem = ! (vlip >= vdry & vdry >= vwet),
            .groups = 'drop') %>%
      filter(problem)
```

    ## # A tibble: 145 x 8
    ##    Code         PARAMETER               method   vlip   vdry  vwet     n problem
    ##    <chr>        <chr>                   <chr>   <dbl>  <dbl> <dbl> <int> <lgl>  
    ##  1 CBEEEE_REP_~ 2',3,4,4',5-PENTACHLOR~ E1668A  61.1   88.2   8.55     3 TRUE   
    ##  2 CBEEEE_REP_~ 2-CHLOROBIPHENYL        E1668A   2.5    3.61  0.35     3 TRUE   
    ##  3 CBEEEE_REP_~ 2,2'-DICHLOROBIPHENYL   E1668A   8.14  11.7   1.14     3 TRUE   
    ##  4 CBEEEE_REP_~ 2,2',3-TRICHLOROBIPHEN~ E1668A  26.4   38.1   3.7      3 TRUE   
    ##  5 CBEEEE_REP_~ 2,2',3,3',4-PENTACHLOR~ E1668A 319    460    44.7      3 TRUE   
    ##  6 CBEEEE_REP_~ 2,2',3,3',4,4',5-HEPTA~ E1668A 356    514    49.9      3 TRUE   
    ##  7 CBEEEE_REP_~ 2,2',3,3',4,4',5,5'-OC~ E1668A 111    160    15.5      3 TRUE   
    ##  8 CBEEEE_REP_~ 2,2',3,3',4,4',5,5',6-~ E1668A  22.7   32.8   3.18     3 TRUE   
    ##  9 CBEEEE_REP_~ 2,2',3,3',4,4',5,6'-OC~ E1668A  17.1   24.7   2.39     3 TRUE   
    ## 10 CBEEEE_REP_~ 2,2',3,3',4,4',5,6-OCT~ E1668A  14.1   20.4   1.98     3 TRUE   
    ## # ... with 135 more rows

The last six problems are all moisture values (where this is
meaningless). The others are all PCB values, apparently from one
sampling event. that suggests some sort of transcription error or
contamination problem in the lipid analysis.

Note that the PCB errors are either `TEST METHOD` “E1668A” or calculated
PCB values – totals and TEQs.

``` r
SWAT_simplified %>%
  select(Code, PARAMETER, `WEIGHT BASIS`, CONCENTRATION, `UNITS VALUE`) %>%
  filter(Code == "CBEEEE_REP_4_2009_5",
         grepl('LIPID|SOLID|MOISTURE', PARAMETER))
```

    ## # A tibble: 3 x 5
    ##   Code            PARAMETER           `WEIGHT BASIS` CONCENTRATION `UNITS VALUE`
    ##   <chr>           <chr>               <chr>                  <dbl> <chr>        
    ## 1 CBEEEE_REP_4_2~ LIPIDS              LIP                      1.4 %            
    ## 2 CBEEEE_REP_4_2~ MOISTURE            WET                     90.3 %            
    ## 3 CBEEEE_REP_4_2~ SOLIDS-TOTAL RESID~ DRY                     17.1 %

Shouldn’t `SOLIDS-TOTAL RESIDUE` be equal to `1 - MOISTURE`? It is odd
that the error is exactly 10%. That suggests a typographical error. We
can compare results to the other replicates.

``` r
SWAT_simplified %>%
  select(Code, PARAMETER, CONCENTRATION, `WEIGHT BASIS`) %>%
  filter(grepl("CBEEEE_REP_._2009_5", Code),
         grepl('LIPID|SOLID|MOISTURE', PARAMETER)) %>%
  pivot_wider(Code, names_from = `WEIGHT BASIS`, values_from = CONCENTRATION)
```

    ## # A tibble: 4 x 4
    ##   Code                  LIP   WET   DRY
    ##   <chr>               <dbl> <dbl> <dbl>
    ## 1 CBEEEE_REP_1_2009_5  1.38  83.3  17.6
    ## 2 CBEEEE_REP_2_2009_5  1.52  83.4  16.7
    ## 3 CBEEEE_REP_3_2009_5  1.37  83.6  17.2
    ## 4 CBEEEE_REP_4_2009_5  1.4   90.3  17.1

So, for all of these EXCEPT `REP_4`, the expected identity nearly
applies (within one percent): \(DRY \approx 1 - WET\). And the WET value
is about 83.4%. This suggests a typographical or other error in the WET
value for this sample.

# Hand Calculate Values for Comparison

We calculate values for one PARAMETER `2',3,4,4',5-PENTACHLOROBIPHENYL`
by hand. (From the first row of the results returned above).

``` r
# Dry-Weight Basis
8.55/(1-0.903)
```

    ## [1] 88.14433

``` r
# Dry-weight basis calculated based on DRY value 
8.55/0.171
```

    ## [1] 50

``` r
# Lipid basis calculated based on LIP value
8.55/0.014
```

    ## [1] 610.7143

The DRY basis (NEARLY) matches the value in the data. The other two
values don’t. We can conclude that:  
1\. The “WET” `WEIGHT BASIS` values were calculated from the `MOISTURE`
values.  
2\. the `SOLIDS-TOTAL RESIDUE (TS)` was not used for calculations.  
2\. The reported “LIPID” `WEIGHT BASIS` are off by a factor of ten.

We can confirm that this sample has problems by comparing results with
the other’ three FIELD replicate samples collected on the same site and
day. As these are FIELD replicates, we do not expect identical results,
but in general, results should be similar.

Here we focus only on one `PARAMETER`, but results are similar for all
PCBs. We include the `TEST METHOD` here because it appears all values
associated with this test method are suspect.

``` r
SWAT_simplified %>%
  select(Code, `SITE SEQ`, SAMPLE_DATE, `TEST METHOD`, PARAMETER,
         CONCENTRATION, `WEIGHT BASIS`) %>%
  filter(grepl("CBEEEE_REP_._2009_5", Code),
         PARAMETER == '2-CHLOROBIPHENYL') %>%
  pivot_wider( names_from = `WEIGHT BASIS`, values_from = CONCENTRATION)
```

    ## # A tibble: 4 x 8
    ##   Code  `SITE SEQ` SAMPLE_DATE         `TEST METHOD` PARAMETER   WET   DRY   LIP
    ##   <chr>      <dbl> <dttm>              <chr>         <chr>     <dbl> <dbl> <dbl>
    ## 1 CBEE~      76080 2009-11-10 00:00:00 E1668A        2-CHLORO~ 0.303  1.82  22  
    ## 2 CBEE~      76080 2009-11-10 00:00:00 E1668A        2-CHLORO~ 0.334  2.02  22.1
    ## 3 CBEE~      76080 2009-11-10 00:00:00 E1668A        2-CHLORO~ 0.325  1.98  23.7
    ## 4 CBEE~      76080 2009-11-10 00:00:00 E1668A        2-CHLORO~ 0.35   3.61   2.5

Compared to sample replicates:

1.  The error in the LIPID-based values appears to *underestimate*
    concentrations by a factor of ten. That value could in principal be
    recalculated based on (what appears to be) the correct LIPID value.

2.  The DRY-based value is not obviously impossible, but it is roughly
    1.75 times the other replicates (as expected with the likely error
    in the `MOISTURE` values. This error can *not* be corrected, since
    the `MOISTURE` value on which it is based is itself suspect.

To be Safe, we should probably drop this sample from the PCB
calculations if we rely on either the LIPID or DRY WEIGHT values.

The Problem does not appear to extend to other organic contaminants.

``` r
SWAT_simplified %>%
  select(Code, `SITE SEQ`, SAMPLE_DATE, `TEST METHOD`, PARAMETER,
         CONCENTRATION, `WEIGHT BASIS`) %>%
  filter(grepl("CBEEEE_REP_._2009_5", Code),
         grepl('NAPHTHALENE|DDT', PARAMETER)) %>%
  pivot_wider(names_from = `WEIGHT BASIS`, values_from = CONCENTRATION)
```

    ## # A tibble: 56 x 8
    ##    Code  `SITE SEQ` SAMPLE_DATE         `TEST METHOD` PARAMETER    LIP   WET
    ##    <chr>      <dbl> <dttm>              <chr>         <chr>      <dbl> <dbl>
    ##  1 CBEE~      76080 2009-11-10 00:00:00 CALCULATED W~ C4-NAPHT~ 156    2.15 
    ##  2 CBEE~      76080 2009-11-10 00:00:00 CALCULATED W~ C3-NAPHT~ 184    2.53 
    ##  3 CBEE~      76080 2009-11-10 00:00:00 CALCULATED W~ C2-NAPHT~ 159    2.19 
    ##  4 CBEE~      76080 2009-11-10 00:00:00 CALCULATED W~ C1-NAPHT~  94.5  1.3  
    ##  5 CBEE~      76080 2009-11-10 00:00:00 SW8270CM      2-METHYL~  57.3  0.788
    ##  6 CBEE~      76080 2009-11-10 00:00:00 SW8270CM      1,4,6,7-~  30.5  0.419
    ##  7 CBEE~      76080 2009-11-10 00:00:00 SW8270CM      NAPHTHAL~ 117    1.61 
    ##  8 CBEE~      76080 2009-11-10 00:00:00 SW8270CM      2,6-DIME~  31.8  0.437
    ##  9 CBEE~      76080 2009-11-10 00:00:00 SW8270CM      1-METHYL~  37.3  0.513
    ## 10 CBEE~      76080 2009-11-10 00:00:00 SW8270CM      1,2-DIME~   6.91 0.095
    ## # ... with 46 more rows, and 1 more variable: DRY <dbl>

# Identify / Remove Suspect Values

We need to remove:

1.  Data ONLY from sampling event `Code` = “CBEEEE\_REP\_4\_2009\_5”  
2.  PCB Data with `TEST METHOD` = “E1668A”  
3.  The four calculated PCB values based on those:
      - PCB TOTAL TEQ (ND=0)  
      - PCB TOTAL TEQ (ND=1/2 DL)  
      - PCB TOTAL TEQ (ND=DL)  
      - PCBs  
4.  Remove BOTH `WEIGHT BASIS` = “DRY” and `WEIGHT BASIS` = “LIPID” for
    those samples.

## The Suspect Values

``` r
SWAT_simplified %>%
  select(Code, PARAMETER, CONCENTRATION, `TEST METHOD`, `WEIGHT BASIS`) %>%
  filter(Code == "CBEEEE_REP_4_2009_5",
         `TEST METHOD` == "E1668A" | 
               PARAMETER == 'PCBS' |
               grepl('PCB TOTAL TEQ', PARAMETER),
         `WEIGHT BASIS` == 'LIP' | `WEIGHT BASIS` == 'DRY')
```

    ## # A tibble: 333 x 5
    ##    Code        PARAMETER             CONCENTRATION `TEST METHOD`  `WEIGHT BASIS`
    ##    <chr>       <chr>                         <dbl> <chr>          <chr>         
    ##  1 CBEEEE_REP~ 2,2',3,3',4,4',5,6-O~         20.4  E1668A         DRY           
    ##  2 CBEEEE_REP~ PCB TOTAL TEQ (ND=0)           1.94 CALCULATED WH~ DRY           
    ##  3 CBEEEE_REP~ PCBS                       93700    CALCULATED WH~ LIP           
    ##  4 CBEEEE_REP~ 2,2',3,3',4,4',5,6-O~         14.1  E1668A         LIP           
    ##  5 CBEEEE_REP~ 2,2',3,3',4,4',5,5'-~        111    E1668A         LIP           
    ##  6 CBEEEE_REP~ 2,2',3,3',4,4',5,5'-~        160    E1668A         DRY           
    ##  7 CBEEEE_REP~ PCBS                      135000    CALCULATED WH~ DRY           
    ##  8 CBEEEE_REP~ PCB TOTAL TEQ (ND=DL)          1.48 CALCULATED WH~ LIP           
    ##  9 CBEEEE_REP~ PCB TOTAL TEQ (ND=DL)          2.14 CALCULATED WH~ DRY           
    ## 10 CBEEEE_REP~ PCB TOTAL TEQ (ND=1/~          1.42 CALCULATED WH~ LIP           
    ## # ... with 323 more rows

## A Single Logical Test Value

So, make that selection process a single test, so it is easier to
negate.

``` r
mytest <- with(SWAT_simplified, 
               Code == "CBEEEE_REP_4_2009_5" &
               (`TEST METHOD` == "E1668A" | 
                 PARAMETER == 'PCBS' |
                 grepl('PCB TOTAL TEQ', PARAMETER)) &
               (`WEIGHT BASIS` == 'LIP' | `WEIGHT BASIS` == 'DRY'))
SWAT_simplified %>%
  select(Code, PARAMETER, CONCENTRATION, `TEST METHOD`, `WEIGHT BASIS`) %>%
  filter(mytest)
```

    ## # A tibble: 333 x 5
    ##    Code        PARAMETER             CONCENTRATION `TEST METHOD`  `WEIGHT BASIS`
    ##    <chr>       <chr>                         <dbl> <chr>          <chr>         
    ##  1 CBEEEE_REP~ 2,2',3,3',4,4',5,6-O~         20.4  E1668A         DRY           
    ##  2 CBEEEE_REP~ PCB TOTAL TEQ (ND=0)           1.94 CALCULATED WH~ DRY           
    ##  3 CBEEEE_REP~ PCBS                       93700    CALCULATED WH~ LIP           
    ##  4 CBEEEE_REP~ 2,2',3,3',4,4',5,6-O~         14.1  E1668A         LIP           
    ##  5 CBEEEE_REP~ 2,2',3,3',4,4',5,5'-~        111    E1668A         LIP           
    ##  6 CBEEEE_REP~ 2,2',3,3',4,4',5,5'-~        160    E1668A         DRY           
    ##  7 CBEEEE_REP~ PCBS                      135000    CALCULATED WH~ DRY           
    ##  8 CBEEEE_REP~ PCB TOTAL TEQ (ND=DL)          1.48 CALCULATED WH~ LIP           
    ##  9 CBEEEE_REP~ PCB TOTAL TEQ (ND=DL)          2.14 CALCULATED WH~ DRY           
    ## 10 CBEEEE_REP~ PCB TOTAL TEQ (ND=1/~          1.42 CALCULATED WH~ LIP           
    ## # ... with 323 more rows
