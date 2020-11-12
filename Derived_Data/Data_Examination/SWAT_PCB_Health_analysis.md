Comparison of PCBs from EGAD Mussel Toxics Data to Health Thresholds
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
11/12/2020

  - [Introduction](#introduction)
      - [PCB TEQ comparisons.](#pcb-teq-comparisons.)
  - [Related Health Standards](#related-health-standards)
  - [Load Libraries](#load-libraries)
      - [Establish Folder Reference](#establish-folder-reference)
      - [Copy Data](#copy-data)
      - [Remove duplicates](#remove-duplicates)
      - [Simplify Data and Add Unique Sample
        Codes](#simplify-data-and-add-unique-sample-codes)
      - [Add Class to the Working Simplified
        Data](#add-class-to-the-working-simplified-data)
  - [Create PCB-only Data – Wet Weight
    Basis\!](#create-pcb-only-data-wet-weight-basis)
      - [Load Location Information](#load-location-information)
      - [Data Subsets](#data-subsets)
      - [Data Since 2010](#data-since-2010)
  - [PCB Graphic](#pcb-graphic)
  - [TEQ Graphic](#teq-graphic)

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

In this Notebook, we analyze PCB contaminants found in blue mussel
(*Mytilus edulis*) tissue. Our focus is on comparison to “fish tissue
action levels” or FTALs. FTALs are expressed o a wet weight basis, which
is why this analysis is separate from other analyses of PCBs.

We focus on the following analytic totals: \* Total PCBs, \* PCB TEQs

## PCB TEQ comparisons.

TEQs (Toxic Equivalents) use weighted sums of concentrations of toxic
chemicals used to estimate overall toxicity of mixtures. The idea is
that by weighting concentrations by very rough (order of magnitude)
weights based on relative toxicity (called Toxic Equivalency Factors or
“TEFs”), you can get some idea of whether a mixture poses toxic risk
or not. TEQs are compared to published toxicity benchmarks.

# Related Health Standards

DEP staff pointed us towards the detailed documentation in the 2017-2018
SWAT data report.

> Maine Department of Environmental Protection. 2019. Surface Water
> Ambient Toxics Monitoring Program. 2017-2018 Report to the Joint
> Committee on Environmental and Natural Resources. 129th Legislature,
> First Session.

That document refers to “MCDC Fish Tissue Action Levels.” and FDA Action
Levels. After some digging, we found a document on the Maine CDC website
that corresponds to (most or all of) the FTALs reported by DEP.

> Maine Center for Disease Control. 2001. Bureau of Health Fish Tissue
> Action Levels.
> <https://www.maine.gov/dhhs/mecdc/environmental-health/eohp/fish/documents/action-levels-writeup.pdf>.
> Accessed 11/10/2020.

The Maine CDC document does not include action levels for TEQs. We
pulled related action levels from the DEP SWAT report.

The relevant Maine health reference level are these (1 ppb = 1 ng/g):

**THESE REFERENCE LEVELS ARE ON A WET WEIGHT BASIS**

| Chemical Group | Non-Cancer Action Level | Cancer Action Level  | Source |
| -------------- | ----------------------- | -------------------- | ------ |
| PCBs           | 11 ppb                  | 43 ppb               | MCDC   |
| Dioxins        | 0.0015 ppb              | 0.0019 ppb           | MCDC   |
| TEQ            |                         | 0.4 pg/g = .0004 ppb | DEP    |

# Load Libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts -------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())
```

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
rm(SWAT_data)
```

## Add Class to the Working Simplified Data

We read data from the `Parameter List.xlsx` Excel File to provide
groupings.

``` r
Parameter_List <- read_excel(file.path(parent,"Parameter List.xlsx"), 
    sheet = "Parameter List") %>%
  mutate(Class = factor(Class)) %>%
  arrange(Class, PARAMETER)

SWAT_simplified <- SWAT_simplified %>% 
  mutate(Class = Parameter_List$Class[match(PARAMETER, Parameter_List$PARAMETER)])
rm(Parameter_List)
```

# Create PCB-only Data – Wet Weight Basis\!

But want to restrict to PCB data only, expressed on a dry weight basis.
We had trouble because we lost certain parameters in deriving this data
subset, presumably when we selected only dry weigh basis samples.

``` r
pcb_data <- SWAT_simplified %>%
  filter(grepl('PCB', Class)) %>%
  filter(`WEIGHT BASIS` == 'WET') %>%
  filter(PARAMETER %in% c("PCB TOTAL TEQ (ND=1/2 DL)", "TOTAL PCB-H")) %>%
  rename_with(~tolower(gsub(' ','_', .x, fixed = TRUE)))
```

## Load Location Information

And add a short location name for figures.

``` r
locations <- read_csv(file.path(parent,"sites_spatial.csv"), 
    col_types = cols(SITESEQ = col_skip(), 
        LAT = col_skip(), LONG = col_skip())) %>%
  mutate(short_locs= c("Back Bay",
                      "Fore River",
                      "Cocktail Cove",
                      "SW Great Diamond",
                      "Navy Pier",
                      "Mill Creek",
                      "Royal River",
                      "Haraseeket",
                      "Falmouth",
                      "Mare Brook",
                      "Fore River",
                      "East End",
                      "Spring Point",
                      "Jewel Island",
                      "Presumpscot",
                      "Middle Bay",
                      "Maquoit Bay",
                      "Inner Fore",
                      "Quahog Bay",
                      "Long Island"))

pcb_data <- pcb_data %>%
  mutate(short_locs = locations$short_locs[match(sitecode,
                                                 locations$SITECODE)])
rm(locations)
```

## Data Subsets

``` r
pcb_totals_data <- pcb_data %>%
  filter(parameter == "TOTAL PCB-H") %>%
  mutate(short_locs = fct_reorder(short_locs, concentration))

lvls = levels(pcb_totals_data$short_locs)

pcb_teq_data <- pcb_data %>%
  filter(parameter == "PCB TOTAL TEQ (ND=1/2 DL)") %>%
  mutate(short_locs = factor(short_locs, levels =  lvls))
```

## Data Since 2010

Give the paucity of data and sited from 2015 through 2019, we consider
“recent” data to include the lat 10 years, or 2010 throut 2019

``` r
totals_recent_data <- pcb_totals_data %>%
    filter(year >= 2010)

teq_recent_data <- pcb_teq_data %>%
    filter(year >= 2010)
```

# PCB Graphic

``` r
plt <- ggplot(totals_recent_data,
              aes(short_locs, concentration)) +
  geom_point(aes(color = year)) +
  geom_hline(yintercept = 43*1000, lty = 3) +
  geom_hline(yintercept = 11*1000, lty = 2) +
  scale_y_log10(labels=scales::label_comma()) +
  theme_cbep(base_size = 12) +
  ylab("Total PCBs (pg/g)") +
  xlab('')
plt
```

![](SWAT_PCB_Health_analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
So PCB levels in a couple of samples are above health thresholds for
PCBs. This does not quite match, but is consistent with what DEP has
reported on page 78 of their report. The report only on site averages,
so do not highlight individual observations that exceed thresholds.

# TEQ Graphic

``` r
plt <- ggplot(teq_recent_data,
              aes(short_locs, concentration)) +
  geom_point(aes(color = year)) +
  geom_hline(yintercept = .4, lty = 3) +
  scale_y_log10(labels=scales::label_comma()) +
  theme_cbep(base_size = 12) +
  ylab("TEQ Concentration (pg/g)") +
  xlab('')
plt
```

![](SWAT_PCB_Health_analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
Same thing here. DEP reports on averages, so does not highlight the
single observation above the action level.
