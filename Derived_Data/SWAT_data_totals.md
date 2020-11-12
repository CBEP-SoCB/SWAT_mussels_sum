Assemble Data Set with Totals and Sums for Organic Contaminants
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
11/06/2020

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder Reference](#establish-folder-reference)
      - [Copy Data](#copy-data)
      - [Focus on Dry Weght Basis Values
        Only](#focus-on-dry-weght-basis-values-only)
  - [Identify Pre-Calculated Totals](#identify-pre-calculated-totals)
      - [PAHs](#pahs)
      - [PCBs](#pcbs)
      - [Dioxins (and Furans)](#dioxins-and-furans)
      - [Pesticides](#pesticides)
  - [Calculate Additional Reference
    Totals](#calculate-additional-reference-totals)
      - [PCBs](#pcbs-1)
      - [Dioxins and Furans](#dioxins-and-furans-1)
      - [DDT Residues](#ddt-residues)
  - [Assemble the Totals data](#assemble-the-totals-data)
      - [Select Precalculated Totals](#select-precalculated-totals)
      - [Add “SWAT PCBs”, Dioxin and DDT
        Totals](#add-swat-pcbs-dioxin-and-ddt-totals)
  - [Save Totals Data](#save-totals-data)

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

In this Notebook, we assemble a reduced data set containing certain
analytic totals.

Our preference is to calculate totals and other derived quantities
directly, so we know exactly how derived quantities were produced, can
perform data QA/QC, and directly model non detects. Unfortunately, it
has proven difficult to reconstruct exactly how analytic totals were
calculated by DEP.

DEP staff pointed us towards the detailed documentation in the 2017-2018
SWAT data report for details, especially from page 64 onward for PAHs.
This provided a detailed list of parameters used to summarize PAHs using
common analytic totals that can be compared on a more or less consistent
basis with benchmarks from Gulfwatch and NOAA’s National Status and
Trends studies.

> Maine Department of Environmental Protection. 2019. Surface Water
> Ambient Toxics Monitoring Program. 2017-2018 Report to the Joint
> Committee on Environmental and Natural Resources. 129th Legislature,
> First Session.

Due to changes from year to year in analytic methods, data from
different years was reported to DEP in slightly inconsistent ways, so
the totals were also handled differently each year. While it appears
full details are available in the DEP report, we decided direct use of
the totals as calculated by DEP makes better sense.

We principally use the totals provided by DEP. This poses challenges for
addressing dioxins (where totals are only available on a TEQ basis, not
a mass basis), and for DDT residues (because DDT residues were not
calculated by DEP). We need the DDT residues to make our presentation
more directly comparable to results from all our other toxics data sets.

It’s fairly straightforward to calculate Dioxin and DDT residue totals.

# Load Libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts -------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

# Load Data

We load the extensively reviewed and cleaned data from the Derived\_Data
folder. (This code contains unnecessary folder references, but it makes
the code work from any of the four primary folders in the archive,
facilitating code reuse.)

## Establish Folder Reference

``` r
sibfldnm <- 'Derived_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)
fn <- 'SWAT_data_working.csv'
```

## Copy Data

``` r
swat_data <- read_csv(file.path(sibling, fn),
                      col_types = cols(
                        .default = col_character(),
                        site_seq = col_double(),
                        year = col_double(),
                        sample_date = col_datetime(format = ""),
                        concentration = col_double(),
                        qualifier = col_character(),
                        rl = col_double(),
                        mdl = col_double(),
                        dilution_factor = col_double(),
                        conc_ugg = col_double(),
                        conc_ngg = col_double(),
                        rl_ugg = col_double(),
                        rl_ngg = col_double()
                      )) %>%
  mutate(sample_date = as.Date(sample_date))
```

## Focus on Dry Weght Basis Values Only

Data in the database also includes wet weight and lipid weight basis
values. Most reference levels are based on values expressed on a dry
weight or lipid weight basis.

Restricting data to dry weight basis only eliminates data from earlier
years. Those data do not have associated moisture values, and so dry
weight basis could not be calculated. In preliminary analyses, we note
that most earlier analyses generally used different laboratories, making
direct comparison difficult.

``` r
swat_data <- swat_data %>%
  filter(weight_basis == 'DRY') %>%
  select(-weight_basis)
```

# Identify Pre-Calculated Totals

In the `SWAT_data_examination_PARAMETERS.Rmd` file, we looked at all
parameters in the source data, and identified all derived quantities,
including totals. We take a moment to identify totals here, by major
parameter groups, showing only to the versions that addressed non
detects by using half the value of the reporting limits.

## PAHs

According to the SWAT report cited above, only certain summaries are
possible for older periods of time, when fewer compounds were studied,
so there is a trade-off between completeness and coverage. We can
demonstrate that by looking at availability of summations over time.

``` r
swat_data %>%
  filter(grepl('TOTAL', parameter) &
         grepl('PAH', parameter)   &
         grepl('-H$', parameter)) %>%
  select(parameter, year, concentration) %>%
  unique() %>%
  group_by(parameter, year) %>%
  summarize(n = sum(! is.na(concentration)), .groups = 'drop') %>%
  pivot_wider(names_from = year, values_from = n)
```

    ## # A tibble: 4 x 11
    ##   parameter `2009` `2010` `2011` `2012` `2013` `2014` `2015` `2017` `2007`
    ##   <chr>      <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>
    ## 1 TOTAL PA~     24      4      8      4      4     11      8      7     NA
    ## 2 TOTAL PA~     24      4      8      4      4     11      8      7     29
    ## 3 TOTAL PA~     24      4      8      4      4     11      8      7     NA
    ## 4 TOTAL PA~     24      4      8      4      4     11      8      7     NA
    ## # ... with 1 more variable: `2008` <int>

So the only significant change is that the 2007 and 2008 data are only
reported according to PAH-19 totals.

``` r
swat_data %>%
  filter(grepl('TOTAL', parameter) &
         grepl('PAH', parameter)   &
         grepl('-H$', parameter)
         ) %>%
  select(parameter) %>%
  unique()
```

    ## # A tibble: 4 x 1
    ##   parameter    
    ##   <chr>        
    ## 1 TOTAL PAH19-H
    ## 2 TOTAL PAH-H  
    ## 3 TOTAL PAH24-H
    ## 4 TOTAL PAH40-H

## PCBs

In the `SWAT_data_examination_PCB.Rmd` file, we identified three
pre-calculated totals for PCBs, and noted that an important reference
total was NOT pre calculated.

``` r
swat_data %>%
  filter((grepl('TOTAL', parameter) &
         grepl('PCB', parameter)   &
         (grepl('-H$', parameter) |
          grepl('\\/2', parameter))) |
           parameter == 'PCBS') %>%
  select(parameter, year, concentration) %>%
  unique() %>%
  group_by(parameter, year) %>%
  summarize(n = sum(! is.na(concentration)), .groups = 'drop') %>%
  pivot_wider(names_from = year, values_from = n)
```

    ## # A tibble: 3 x 11
    ##   parameter `2007` `2008` `2009` `2010` `2011` `2012` `2013` `2014` `2015`
    ##   <chr>      <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>
    ## 1 PCB TOTA~     27      8     23      4      7      4      4     11      8
    ## 2 PCBS          NA     NA     21      4      8      4      4     11      8
    ## 3 TOTAL PC~     28      8     24      4      9      4      4     11      8
    ## # ... with 1 more variable: `2017` <int>

`PCBS` and `TOTAL PCB-H` are nearly identical, and `TOTAL PCB-H` is
available more consistently, and has higher resolution, so we will use
that in preference to the “PCBS” value.

``` r
swat_data %>%
  filter(grepl('TOTAL', parameter) &
         grepl('PCB', parameter)   &
         (grepl('-H$', parameter) |
          grepl('\\/2', parameter)))
```

    ## # A tibble: 212 x 27
    ##    site_seq sitecode site   year sample_date sample_id code  samp_pt_name lab  
    ##       <dbl> <chr>    <chr> <dbl> <date>      <chr>     <chr> <chr>        <chr>
    ##  1    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 1        AXYS~
    ##  2    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 1        AXYS~
    ##  3    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 2        AXYS~
    ##  4    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 2        AXYS~
    ##  5    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 3        AXYS~
    ##  6    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 3        AXYS~
    ##  7    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 4        AXYS~
    ##  8    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 4        AXYS~
    ##  9    76073 CBANAN   FALM~  2007 2007-10-18  CBANAN R~ CBAN~ REP 1        AXYS~
    ## 10    76073 CBANAN   FALM~  2007 2007-10-18  CBANAN R~ CBAN~ REP 1        AXYS~
    ## # ... with 202 more rows, and 18 more variables: lab_id <chr>, method <chr>,
    ## #   method_name <chr>, parameter <chr>, concentration <dbl>, units <chr>,
    ## #   lab_qualifier <chr>, qualifier <chr>, qual_description <chr>, rl <dbl>,
    ## #   mdl <dbl>, prep_method <chr>, dilution_factor <dbl>, cas_no <chr>,
    ## #   conc_ugg <dbl>, conc_ngg <dbl>, rl_ugg <dbl>, rl_ngg <dbl>

## Dioxins (and Furans)

There is only a single relevant Total for us to examine. We note that
dioxins were only studied in 2008, and only from the Presumpscot River
sites, so there is, perhaps, little reason to report on them here.

``` r
swat_data %>%
  filter(grepl('TOTAL', parameter) &
         grepl('DX', parameter)    &
         (grepl('ND=1\\/2 DL', parameter) |
             grepl('-H$', parameter))) %>%
  select(parameter) %>%
  unique()
```

    ## # A tibble: 1 x 1
    ##   parameter               
    ##   <chr>                   
    ## 1 DX TOTAL TEQ (ND=1/2 DL)

That is in toxic equivalents, not on a mass basis. Because we have not
introduced the idea of toxic equivalents elsewhere in State of the Bay,
we will not do so here. We will directly calculate our own total for
Dioxins.

## Pesticides

``` r
swat_data %>%
  filter(grepl('TOTAL', parameter) &
         grepl('PESTICIDES', parameter) &
           grepl('-H$', parameter)) %>%
  select(parameter) %>%
  unique()
```

    ## # A tibble: 1 x 1
    ##   parameter           
    ##   <chr>               
    ## 1 TOTAL PESTICIDES21-H

This collection of 21 pesticides does not match any of our other Toxics
data sources. A complete total of all pesticides in these data would be
even more extensive. While either the full list or the `P`TOTAL
PESTICIDES21-H\` may be of interest, they introduce additionl complexity
in reporting results in State of Casco Bay.

Elsewhere we have reported on DDT residues, which tend to be a
significant fraction of total pesticides. We will again follow that
practice here, although we have to calculate this total *de novo*.

# Calculate Additional Reference Totals

There is a fair amount of repeated code in the next section, so the
following could probably be simplified by creating some utility
functions, but by the time that became obvious, we had working code, and
so did not bother to refactor.

## PCBs

The logic and code for selecting the particular PCBs included in this
calculated sum is developed in the `SWAT_data_examination_PCBs.Rmd`
Notebook. In summary, we are calculating a sum of PCBs that matches a
sum used by Maine DEP to compare Maine results to (older) reference
studies at regional and national levels.

Maine DEP staff pointed us towards the documentation in a 2017-2018 SWAT
report for details of how totals were calculated. This provided a
detailed list of parameters used to summarize PCBs using analytic totals
that can be compared on a more or less consistent basis with benchmarks
from Gulfwatch and NOAA’s National Status and Trends studies.

> Maine Department of Environmental Protection. 2019. Surface Water
> Ambient Toxics Monitoring Program. 2017-2018 Report to the Joint
> Committee on Environmental and Natural Resources. 129th Legislature,
> First Session.

See the other R Notebook for details and QA/QC checks. The code here is
lightly modified from the code developed there, mostly to address the
different naming conventions.

Although we prefer to calculate ND estimates based on statistical
methods, we can not recalculate all totals in these data. If we change
the methods we use, that may throw off comparisons with reference
levels. Maine DEP calculated totals based on the conventions of
replacing non-detects with zero, half the reporting limit, or the
reporting limit. Here, we will replace non-detects with half the
reporting limit, for consistency with DEP practice.

### Load List of All PCBs

First we load a list of PCBs from `Parameter List.xlsx` and drop the
provided totals. While this step is not strictly necessary, it offers
somechances for QA/QC checks, and minimizes chances of confusion later.
Most PCBS were reported in the data by name, others (the co-eluted
mixtures) as PAH numbers in slash-delimited lists. That’s inconvenient,
and requires much of the following code to address.

``` r
pcb_list <- read_excel(file.path(sibling,"Parameter List.xlsx"), 
                             sheet = "Parameter List") %>%
  mutate(Class = factor(Class)) %>%
  arrange(Class, PARAMETER) %>%
  
  # Restrict to PCBs data
  filter(grepl('PCB', Class)) %>%
  
  # Drop TOTALS
  filter(! grepl('TOTAL', PARAMETER)) %>%
  filter(! PARAMETER == 'PCBS') %>%
  
  pull(PARAMETER) %>%
  as.character()
```

### Load PCB Nomenclature Table

This is a look up table that converts among common ways of referring to
PCBs. The added data columns just convert entries in the look up table
to match conventions in the DEP data.

``` r
pcb_translator <-  read_excel(file.path(sibling,"Parameter List.xlsx"), 
                             sheet = "PCB Nomenclature")%>%
  mutate(casrn = gsub('-','', CASRN),
         iupac_name = toupper(`IUPAC Name`))
```

### Calculate `pcb_congener name`

The `swat_list` (loaded later) a\]is expressed in PAH Congener Numbers
ONLY, which are not consistently reported in the source data.

We generate a new column of PCB names, by matching CAS number. The new
data column includes the new names where matches occur, and retains the
old names, where they already are in PCB number format (specifically for
co-eluted mixtures).

``` r
pcb_data <- swat_data %>%
  # limit data to PCBs
  filter(parameter %in% pcb_list) %>%
  
  # Add data on congener numbers
  left_join(pcb_translator, by = c("cas_no" = "casrn")) %>%
  select(-CASRN, -Descriptor, -Type, -`IUPAC Name`, -iupac_name)  %>%

  # Create pcb_congener_name data
  mutate(pcb_number = paste0('PCB-', `Congener Number`) ) %>%
  mutate(pcb_congener_name = if_else(grepl('PCB', parameter),
                            parameter,
                            pcb_number)) %>%
    
  # Address non-matches, where we want NAs, not a misleading code
  mutate(pcb_congener_name = if_else(pcb_congener_name == 'PCB-NA',
                                       NA_character_,
                                       pcb_congener_name)) %>% 
  # Cleanup
  select(-`Congener Number`, -pcb_number)
```

#### Are All PCB Congeners Represented?

We Check to confirm everything so far has worked as expected. If so,
each congener number will turn up exactly once. The magic function here
is `separate_rows()`.

``` r
pcb_data %>%
  select(pcb_congener_name) %>%
  unique() %>%
  mutate(available_nums  = sub('PCB-', '', pcb_congener_name)) %>%
  separate_rows(available_nums) %>%
  mutate(available_nums = as.numeric(available_nums)) %>%
  arrange(available_nums)
```

    ## # A tibble: 209 x 2
    ##    pcb_congener_name available_nums
    ##    <chr>                      <dbl>
    ##  1 PCB-1                          1
    ##  2 PCB-2                          2
    ##  3 PCB-3                          3
    ##  4 PCB-4                          4
    ##  5 PCB-5                          5
    ##  6 PCB-6                          6
    ##  7 PCB-7                          7
    ##  8 PCB-8                          8
    ##  9 PCB-9                          9
    ## 10 PCB-10                        10
    ## # ... with 199 more rows

### Load “SWAT PCBs” Parameter List

The list of parameters we want to include in the sum is pulled from
Table 1.3.3.1.1 in the DEP SWAT report.

``` r
swat_list <- read_excel(file.path(sibling,"Parameter List.xlsx"), 
                             sheet = "PCB_SWAT") %>%
  pull(SWAT_PCBs_35)
swat_list
```

    ##  [1] "PCB-5"           "PCB-8"           "PCB-15"          "PCB-018/030"    
    ##  [5] "PCB-026/029"     "PCB-020/028"     "PCB-050/053"     "PCB-52"         
    ##  [9] "PCB-66"          "PCB-77"          "PCB-090/101/113" "PCB-118"        
    ## [13] "PCB-126"         "PCB-132"         "PCB-153/168"     "PCB-169"        
    ## [17] "PCB-187"         "PCB-170"         "PCB-190"         "PCB-128/166"    
    ## [21] "PCB-195"         "PCB-208"         "PCB-180/193"     "PCB-206"        
    ## [25] "PCB-209"         "PCB-105"

And those names all match parameters in `pcb_congener_name`, as
required.

### Calculate “SWAT PCBs” Sum

here we actually calculate the sums, and begin to build up complete data
rows to include in the totals data.

``` r
pcb_swat <- pcb_data %>%
  # Filter to SWAT PCB codes
  filter(pcb_congener_name %in% swat_list) %>%
  
  # Address NDs, We could add code to estimate NDs by maximum likelihood.
  # We calculate based on ng/g to avoid unit confusion.  The original data
  # was in pg/g.  We correct the units in a moment.
  mutate(conc = if_else(is.na(lab_qualifier), conc_ngg,
                        if_else(lab_qualifier == 'U',
                                rl_ngg/2,
                                conc_ngg))) %>%
  
  # Calculate sums across SWAT PCBs for each sample or subsample.
  # And add parameter-specific metadata.
  group_by(code, lab_id) %>%
  summarize(concentration = sum(conc, na.rm = TRUE),
            conc_ngg = concentration,
            conc_ugg = concentration / 1000,
            parameter = 'SWAT_PCBs-H',
            units     = 'NG/G',
            method = 'Calculated',
            .groups   = 'drop') 
pcb_swat
```

    ## # A tibble: 107 x 8
    ##    code         lab_id   concentration conc_ngg conc_ugg parameter units method 
    ##    <chr>        <chr>            <dbl>    <dbl>    <dbl> <chr>     <chr> <chr>  
    ##  1 CBANAN_REP_~ L10702-1          12.6     12.6   0.0126 SWAT_PCB~ NG/G  Calcul~
    ##  2 CBANAN_REP_~ L10702-2          13.7     13.7   0.0137 SWAT_PCB~ NG/G  Calcul~
    ##  3 CBANAN_REP_~ L10702-~          34.4     34.4   0.0344 SWAT_PCB~ NG/G  Calcul~
    ##  4 CBANAN_REP_~ L10702-4          14.3     14.3   0.0143 SWAT_PCB~ NG/G  Calcul~
    ##  5 CBEEEE_REP_~ L10702-~          22.8     22.8   0.0228 SWAT_PCB~ NG/G  Calcul~
    ##  6 CBEEEE_REP_~ L13939-1          33.8     33.8   0.0338 SWAT_PCB~ NG/G  Calcul~
    ##  7 CBEEEE_REP_~ L17179-5          20.7     20.7   0.0207 SWAT_PCB~ NG/G  Calcul~
    ##  8 CBEEEE_REP_~ L20804-5          22.0     22.0   0.0220 SWAT_PCB~ NG/G  Calcul~
    ##  9 CBEEEE_REP_~ L24113-1          22.0     22.0   0.0220 SWAT_PCB~ NG/G  Calcul~
    ## 10 CBEEEE_REP_~ L28408-9          23.0     23.0   0.0230 SWAT_PCB~ NG/G  Calcul~
    ## # ... with 97 more rows

### Assemble Full Data Rows

We add in sample-specific metadata to full out the rest of the data we
need to analyze the calculated values from a moment ago.

``` r
# Select the first matching record.  We might actually get a few too many,
# Since we search for all records that match the sample code.
# But that will be addressed by the inner join.
ancillary_data <- pcb_data %>%
  filter(code %in% pcb_swat$code ) %>%
  group_by(code, lab_id) %>%
  filter(row_number() == 1) %>%
 
  # Drop any data that is sample-specific.  We simply drop those data
  # columns.  When we `bind_rows()` later, missing columns will be filled in 
  # with NA.
  select(-c(method:pcb_congener_name))

# Finally, we use the inner join to combine data
pcb_swat <- ancillary_data %>%
  inner_join(pcb_swat, by = c('code', 'lab_id'))
pcb_swat
```

    ## # A tibble: 107 x 16
    ## # Groups:   code, lab_id [107]
    ##    site_seq sitecode site   year sample_date sample_id code  samp_pt_name lab  
    ##       <dbl> <chr>    <chr> <dbl> <date>      <chr>     <chr> <chr>        <chr>
    ##  1    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 1        AXYS~
    ##  2    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 2        AXYS~
    ##  3    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 3        AXYS~
    ##  4    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 4        AXYS~
    ##  5    76073 CBANAN   FALM~  2007 2007-10-18  CBANAN R~ CBAN~ REP 1        AXYS~
    ##  6    76073 CBANAN   FALM~  2007 2007-10-18  CBANAN R~ CBAN~ REP 2        AXYS~
    ##  7    76073 CBANAN   FALM~  2007 2007-10-18  CBANAN R~ CBAN~ REP 3        AXYS~
    ##  8    76073 CBANAN   FALM~  2007 2007-10-18  CBANAN R~ CBAN~ REP 4        AXYS~
    ##  9    70678 CBMCMC   MILL~  2009 2009-11-10  CBMCMC R~ CBMC~ REP 1        AXYS~
    ## 10    70678 CBMCMC   MILL~  2009 2009-11-10  CBMCMC R~ CBMC~ REP 2        AXYS~
    ## # ... with 97 more rows, and 7 more variables: lab_id <chr>,
    ## #   concentration <dbl>, conc_ngg <dbl>, conc_ugg <dbl>, parameter <chr>,
    ## #   units <chr>, method <chr>

#### Cleanup

``` r
rm(pcb_data, pcb_translator, pcb_list, ancillary_data, swat_list)
```

## Dioxins and Furans

### List of Dioxins and Furans

First we load a list of dioxins from `Parameter List.xlsx`.

``` r
dioxins_list <- read_excel(file.path(sibling,"Parameter List.xlsx"), 
                             sheet = "Parameter List") %>%
  mutate(Class = factor(Class)) %>%
  arrange(Class, PARAMETER) %>%
  filter(Class == "Dioxin") %>%
  pull(PARAMETER) %>%
  as.character()
dioxins_list
```

    ##  [1] "1,2,3,4,6,7,8-HPCDD"                         
    ##  [2] "1,2,3,4,6,7,8-HPCDF"                         
    ##  [3] "1,2,3,4,7,8-HXCDD"                           
    ##  [4] "1,2,3,4,7,8-HXCDF"                           
    ##  [5] "1,2,3,4,7,8,9-HEPTACHLORODIBENZOFURAN(HPCDF)"
    ##  [6] "1,2,3,6,7,8-HXCDD"                           
    ##  [7] "1,2,3,6,7,8-HXCDF"                           
    ##  [8] "1,2,3,7,8-PECDD"                             
    ##  [9] "1,2,3,7,8-PECDF"                             
    ## [10] "1,2,3,7,8,9-HXCDD"                           
    ## [11] "1,2,3,7,8,9-HXCDF"                           
    ## [12] "2,3,4,6,7,8-HXCDF"                           
    ## [13] "2,3,4,7,8-PECDF"                             
    ## [14] "2,3,7,8-TCDD"                                
    ## [15] "2,3,7,8-TETRACHLORODIBENZOFURAN"             
    ## [16] "OCDD"                                        
    ## [17] "OCDF"

### Calculate Total Dioxins

We calculate a sum across all the dioxins studied, addressing
non-detects while we are at it.

Although we prefer to calculate ND estimates based on statistical
methods, we can not recalculate all totals in these data, because of
inconsistencies in how some data were reported. Maine DEP calculated
totals based on the conventions of replacing non-detects with zero, half
the reporting limit, or the reporting limit. Here, we will replace
non-detects with half the reporting limit, for consistency with DEP
practice.

``` r
dioxin_data <- swat_data %>%
  # Filter to dioxin residues
  filter(parameter %in% dioxins_list) %>%
  
  # Address NDs, working only with nanogram per gram data
  mutate(conc = if_else(is.na(lab_qualifier), conc_ngg,
                        if_else(lab_qualifier == 'U',
                                rl_ngg/2,
                                conc_ngg))) %>%
  
  # Calculate sums across parameters
  group_by(code) %>%
  summarize(concentration = sum(conc, na.rm = TRUE),
            conc_ngg = concentration,
            conc_ugg = concentration / 1000,
            parameter = 'TOTAL Dioxins',
            units     = 'NG/G',
            method = 'Calculated',
            .groups   = 'drop') 
dioxin_data
```

    ## # A tibble: 4 x 7
    ##   code             concentration conc_ngg  conc_ugg parameter    units method   
    ##   <chr>                    <dbl>    <dbl>     <dbl> <chr>        <chr> <chr>    
    ## 1 CBPRMT_REP_1_20~        0.292    0.292  0.000292  TOTAL Dioxi~ NG/G  Calculat~
    ## 2 CBPRMT_REP_2_20~        0.0942   0.0942 0.0000942 TOTAL Dioxi~ NG/G  Calculat~
    ## 3 CBPRMT_REP_3_20~        0.0900   0.0900 0.0000900 TOTAL Dioxi~ NG/G  Calculat~
    ## 4 CBPRMT_REP_4_20~        0.0886   0.0886 0.0000886 TOTAL Dioxi~ NG/G  Calculat~

Note we only have Dioxin Data from four samples, all from one site and
one year.

Now, we need to assemble full rows of data. We do that by selecting an
arbitrary row from the matching sample, stripping out all parameter
specific information, leaving only the sample-specific data, and then
joining the stripped down data data with the dioxin\_data.

### Assemble Full Data Rows

``` r
# Select the first matching record.  We might actually get a few too many,
# Since we search for all records that match the sample code.
ancillary_data <- swat_data %>%
  filter(code %in% dioxin_data$code ) %>%
  group_by(code) %>%
  filter(row_number() == 1) %>%
  
  # Now, drop any data that is sample-specific.  We simply drop those data
  # columns.  When we `bind_rows()` later, missing columns will be filled in 
  # with NA.
  
  select(-c(method:rl_ngg))

# Finally, we use the inner join to combine data
dioxin_data <- ancillary_data %>%
  inner_join(dioxin_data, by = c('code'))
dioxin_data
```

    ## # A tibble: 4 x 16
    ## # Groups:   code [4]
    ##   site_seq sitecode site   year sample_date sample_id code  samp_pt_name lab  
    ##      <dbl> <chr>    <chr> <dbl> <date>      <chr>     <chr> <chr>        <chr>
    ## 1    77495 CBPRMT   PRES~  2008 2008-11-18  CBPRMT R~ CBPR~ REP 1        AXYS~
    ## 2    77495 CBPRMT   PRES~  2008 2008-11-18  CBPRMT R~ CBPR~ REP 2        AXYS~
    ## 3    77495 CBPRMT   PRES~  2008 2008-11-18  CBPRMT R~ CBPR~ REP 3        AXYS~
    ## 4    77495 CBPRMT   PRES~  2008 2008-11-18  CBPRMT R~ CBPR~ REP 4        AXYS~
    ## # ... with 7 more variables: lab_id <chr>, concentration <dbl>, conc_ngg <dbl>,
    ## #   conc_ugg <dbl>, parameter <chr>, units <chr>, method <chr>

``` r
rm(ancillary_data)
```

## DDT Residues

We can pull the DDT, DDE, and DDD parameters with a simple regex. Once
we identify these records, we can calculate totals and add them to the
data set for further analysis.

``` r
swat_data %>%
  filter(grepl('DD[TDE]', parameter)) %>%
  select(parameter) %>%
  unique()
```

    ## # A tibble: 6 x 1
    ##   parameter
    ##   <chr>    
    ## 1 2,4'-DDD 
    ## 2 4,4'-DDD 
    ## 3 2,4'-DDE 
    ## 4 4,4'-DDE 
    ## 5 2,4'-DDT 
    ## 6 4,4'-DDT

We have non-detects reported in these data, so we need to deal with
them.

Although we prefer to calculate ND estimates based on statistical
methods, we can not recalculate all the other totals in these data,
because of inconsistencies in how they were reported. We have to rely on
the calculated totals by DEP staff, which reported totals treating ND as
zero, half the Reporting Limit (RL), or equal to the RL.

Of those three options, we have selected the option that non-detects are
replaced with one half of the detection limits. \#\#\# Calculate Total
DDT Residues

``` r
ddt_data <- swat_data %>%
  # Filter to DDT residues, (dry weight only was set earlier)
  filter(grepl('DD[TDE]', parameter)) %>%
  #filter(weight_basis == 'DRY') %>%
  
  # Address NDs
  mutate(conc = if_else(is.na(lab_qualifier), conc_ngg,
                        if_else(lab_qualifier == 'U',
                                rl_ngg/2,
                                conc_ngg))) %>%
  
  # Calculate sums across parameters
  group_by(code, lab_id) %>%
  summarize(concentration = sum(conc, na.rm = TRUE),
            conc_ngg = concentration,
            conc_ugg = concentration / 1000,
            parameter = 'Total DDT',
            units     = 'NG/G',
            method = 'Calculated',
            .groups   = 'drop') 
ddt_data
```

    ## # A tibble: 76 x 8
    ##    code         lab_id   concentration conc_ngg conc_ugg parameter units method 
    ##    <chr>        <chr>            <dbl>    <dbl>    <dbl> <chr>     <chr> <chr>  
    ##  1 CBANAN_REP_~ L10702-~          9.18     9.18  0.00918 Total DDT NG/G  Calcul~
    ##  2 CBANAN_REP_~ L10702-~         10.0     10.0   0.0100  Total DDT NG/G  Calcul~
    ##  3 CBANAN_REP_~ L10702-~         11.3     11.3   0.0113  Total DDT NG/G  Calcul~
    ##  4 CBANAN_REP_~ L10702-~         10.8     10.8   0.0108  Total DDT NG/G  Calcul~
    ##  5 CBEEEE_REP_~ L10702-~         17.4     17.4   0.0174  Total DDT NG/G  Calcul~
    ##  6 CBEEEE_REP_~ L13939-~         11.0     11.0   0.0110  Total DDT NG/G  Calcul~
    ##  7 CBEEEE_REP_~ L17179-5         14.0     14.0   0.0140  Total DDT NG/G  Calcul~
    ##  8 CBEEEE_REP_~ L10702-~         18.7     18.7   0.0187  Total DDT NG/G  Calcul~
    ##  9 CBEEEE_REP_~ L13939-2         12.6     12.6   0.0126  Total DDT NG/G  Calcul~
    ## 10 CBEEEE_REP_~ L17179-6         18.1     18.1   0.0181  Total DDT NG/G  Calcul~
    ## # ... with 66 more rows

### Assemble Full Data Rows

Now, we need to assemble full rows of data. We do that by selecting an
arbitrary row from the matching sample, stripping out all parameter
specific information, leaving only the sample-specific data, and then
joining the stripped down data data with the ddt\_data.

``` r
# Select the first matching record.  We might actually get a few too many,
# Since we search for all records that match EITHER the code or the lab_id.
# And we only need the ones that match BOTH, but that's OK for now, since we 
# will use an inner join later, which will only keep matches.
ancillary_data <- swat_data %>%
  filter(code %in% ddt_data$code & lab_id %in% ddt_data$lab_id) %>%
  group_by(code, lab_id) %>%
  filter(row_number() == 1) %>%
  
  # Now, drop any data that is sample-specific.  We simply drop those data
  # columns.  When we `bind_rows()` later, missing columns wil lbe filled in 
  # with NA.
  
  select(-c(method:rl_ngg))

# Finally, we use the inner join to combine data

ddt_data <- ancillary_data %>%
  inner_join(ddt_data, by = c('code', 'lab_id'))
ddt_data
```

    ## # A tibble: 76 x 16
    ## # Groups:   code, lab_id [76]
    ##    site_seq sitecode site   year sample_date sample_id code  samp_pt_name lab  
    ##       <dbl> <chr>    <chr> <dbl> <date>      <chr>     <chr> <chr>        <chr>
    ##  1    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 1        AXYS~
    ##  2    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 2        AXYS~
    ##  3    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 3        AXYS~
    ##  4    76091 CBJWPB   JEWE~  2007 2007-10-22  CBJWPB R~ CBJW~ REP 4        AXYS~
    ##  5    76073 CBANAN   FALM~  2007 2007-10-18  CBANAN R~ CBAN~ REP 1        AXYS~
    ##  6    76073 CBANAN   FALM~  2007 2007-10-18  CBANAN R~ CBAN~ REP 2        AXYS~
    ##  7    76073 CBANAN   FALM~  2007 2007-10-18  CBANAN R~ CBAN~ REP 3        AXYS~
    ##  8    76073 CBANAN   FALM~  2007 2007-10-18  CBANAN R~ CBAN~ REP 4        AXYS~
    ##  9    70678 CBMCMC   MILL~  2009 2009-11-10  CBMCMC R~ CBMC~ REP 1        AXYS~
    ## 10    70678 CBMCMC   MILL~  2009 2009-11-10  CBMCMC R~ CBMC~ REP 2        AXYS~
    ## # ... with 66 more rows, and 7 more variables: lab_id <chr>,
    ## #   concentration <dbl>, conc_ngg <dbl>, conc_ugg <dbl>, parameter <chr>,
    ## #   units <chr>, method <chr>

# Assemble the Totals data

## Select Precalculated Totals

Recall, we are looking only at totals expressed on a dry weight basis.

``` r
totals_data <- swat_data %>%
  filter((grepl('TOTAL', parameter) &
         grepl('PAH', parameter)   &
         grepl('-H$', parameter))                  |
           
         ((grepl('TOTAL', parameter) &
             grepl('PCB', parameter)) &
           (grepl('ND=1\\/2 DL', parameter) |
             grepl('-H$', parameter)) )           |  # We dropped "PAHS" here
           
         (grepl('TOTAL', parameter) &
         grepl('DX', parameter)    &
         (grepl('ND=1\\/2 DL', parameter) |
             grepl('-H$', parameter)))            |
           
         (grepl('TOTAL', parameter) &
          grepl('PESTICIDES', parameter) &
           grepl('-H$', parameter) ) )
```

## Add “SWAT PCBs”, Dioxin and DDT Totals

``` r
totals_data <- totals_data %>%
  bind_rows(pcb_swat) %>%
  bind_rows(dioxin_data) %>%
  bind_rows(ddt_data)
```

# Save Totals Data

``` r
write_csv(totals_data, 'SWAT_totals_working.csv')
```
