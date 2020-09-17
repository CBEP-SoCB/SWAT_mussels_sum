Generate Simplified EGAD Mussel Toxics Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
9/10/2020

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder Reference](#establish-folder-reference)
      - [Copy Data](#copy-data)
      - [Remove Duplicates](#remove-duplicates)
  - [Simplify Data](#simplify-data)
      - [Remove Uninformative Data
        Categories](#remove-uninformative-data-categories)
      - [Add Unique Sampling Event
        Code](#add-unique-sampling-event-code)
  - [Remove Duplicate Samples](#remove-duplicate-samples)
  - [Rename Data Columns](#rename-data-columns)
  - [Save Resulting Data](#save-resulting-data)
  - [Save Related Metadata](#save-related-metadata)

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

In this Notebook, we apply the lessons from our detailed data review,
and generate a simplified data set that removes unused data, adds a
unique sample ID code, and removes duplicate records.

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
    ## <bytecode: 0x000000001659f648>
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

## Remove Duplicates

Many samples – nearly 20% – are members of a group of duplicates. We can
think of no valid reason why two records should be exact duplicates in
this setting, so we remove all complete duplicates.

``` r
SWAT_data <- unique(SWAT_data)
```

# Simplify Data

## Remove Uninformative Data Categories

Details provided iv “SWAT\_data\_examination\_CODES.Rmd”.

``` r
SWAT_simplified <- SWAT_data %>%
  # Eliminate uninformative identifiers
  select    (-`SAMPLE TYPE`, -`SAMPLE POINT TYPE`, -`SAMPLE LOCATION`,
             -`RESULT TYPE`, -`PARAMETER_QUALIFIER`, -`PARAMETER FILTERED`,
             -`SAMPLE FILTER`, -`DEPTH`, -`DEPTH UNITS`,
             -TREATMENT, -`METER_CALIBRATED`) %>%
  
  # Eliminate data we will not analyze
  select    (-SITE_DESCRIPTION, -ANALYSIS_DATE,
             -`QC TYPE`, -SAMPLED_BY, 
             -`SAMPLE COLLECTION METHOD`, -`UNITS DESCRIPTION`,
             - `LAB QUALIFIER`, -`SAMPLE COMMENT`, -`LAB COMMENT`,
             -`VALIDATION COMMENT`) %>%
  
  # Create Site Code and Site Name
  mutate    (SiteCode =  first(sub('.* - ','', `EGAD_SITE_NAME`)), 
             Site =  first(sub(' - .*','', `EGAD_SITE_NAME`))) %>%
  select(-EGAD_SITE_NAME)
```

## Add Unique Sampling Event Code

Details provided in “SWAT\_data\_examination\_UNIQUE.Rmd”.

``` r
SWAT_simplified <-  SWAT_simplified  %>%
  # Create Year Time Stamp and  Unique Sample ID
  mutate    (Year  = as.numeric(format(SAMPLE_DATE, '%Y')),
             sample_id = gsub(" ", "_", SAMPLE_ID)) %>%
  group_by  (Year) %>%
  mutate    (tag = as.numeric(factor(SAMPLE_DATE))) %>%
  ungroup   ()  %>%
  mutate    (Code = paste(sample_id, Year, tag, sep = '_')) %>%
  select    (-sample_id, -tag) %>%
  select(`SITE SEQ`, SiteCode, Site, Year, SAMPLE_DATE, SAMPLE_ID, Code, everything())
```

# Remove Duplicate Samples

Details provided in “SWAT\_data\_examination\_REPLICATES.Rmd”.

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

# Rename Data Columns

``` r
nms <- names(SWAT_final)
nms <- gsub(' ', '_', nms)
nms <- tolower(nms)
nms
```

    ##  [1] "site_seq"                  "sitecode"                 
    ##  [3] "site"                      "year"                     
    ##  [5] "sample_date"               "sample_id"                
    ##  [7] "code"                      "current_sample_point_name"
    ##  [9] "analysis_lab"              "analysis_lab_sample_id"   
    ## [11] "test_method"               "test_method_description"  
    ## [13] "parameter"                 "concentration"            
    ## [15] "units_value"               "validation_qualifier"     
    ## [17] "qualifier_description"     "rl"                       
    ## [19] "mdl"                       "weight_basis"             
    ## [21] "prep_method"               "dilution_factor"          
    ## [23] "cas_no"

``` r
nms[8]  <- "samp_pt_name"
nms[9]  <- "lab"
nms[10] <- "lab_id"
nms[11] <- "method"
nms[12] <- "method_name"
nms[15] <- "units"
nms[16] <- "qualifier"
nms[17] <- "qual_description"
nms
```

    ##  [1] "site_seq"         "sitecode"         "site"             "year"            
    ##  [5] "sample_date"      "sample_id"        "code"             "samp_pt_name"    
    ##  [9] "lab"              "lab_id"           "method"           "method_name"     
    ## [13] "parameter"        "concentration"    "units"            "qualifier"       
    ## [17] "qual_description" "rl"               "mdl"              "weight_basis"    
    ## [21] "prep_method"      "dilution_factor"  "cas_no"

``` r
names(SWAT_final) <- nms
```

# Save Resulting Data

``` r
write_csv(SWAT_final, 'SWAT_data_cleaned.csv')
```

# Save Related Metadata

``` r
metadata <- tibble(Col_Name = nms, Description = NA_character_)
```

``` r
metadata$Description <- 
  c("EGAD sequence number for designated SITE.",
    "Alphanumeric code for a specific SITE.",
    "Name or description of SITE.",
    "Year of sample collection.",
    "Date of sample collection.",
    "Original EGAD sample ID -- NOT unique.",
    "Derived, lengthy alphanumaric code for individual sampling events.",
    "EGAD sample point name. NOT unique. Possibly unique within sites.",
    "Laboratory conducting analyses.",
    "Internal laboratory sample ID number.",
    "Alphanumeric code for specific laboratory method.",
    "Name of method.",
    "Name of chemical or physical parameter.",
    "Value of the parameter.  Usually, but not always a concentration.",
    "Abbreviation of the units used.  Most are SI units of concentration.",
    "EGAD's `VALIDATION QUALIFIER`.  This is a subset of the `LABORATORY QUALIFIER`.",
    "Long description of meaning ofqualifiers.  Includes other qualifiers",
    "Reporting Limit.  LAb-determined value below which results are non-detects.",
    "Method Detection Limit.  Official limit of method",
    "Basis for calculating concentration (wet weight, dry weight or lipid weight).",
    "Alphanumeric code for sample preparation methods.",
    "Dilution factor for sample analysis.  It's not clear whether this is useful.",
    "Chemical Abstract Number for chemical compound or mixture.")

kable(metadata)
```

| Col\_Name         | Description                                                                    |
| :---------------- | :----------------------------------------------------------------------------- |
| site\_seq         | EGAD sequence number for designated SITE.                                      |
| sitecode          | Alphanumeric code for a specific SITE.                                         |
| site              | Name or description of SITE.                                                   |
| year              | Year of sample collection.                                                     |
| sample\_date      | Date of sample collection.                                                     |
| sample\_id        | Original EGAD sample ID – NOT unique.                                          |
| code              | Derived, lengthy alphanumaric code for individual sampling events.             |
| samp\_pt\_name    | EGAD sample point name. NOT unique. Possibly unique within sites.              |
| lab               | Laboratory conducting analyses.                                                |
| lab\_id           | Internal laboratory sample ID number.                                          |
| method            | Alphanumeric code for specific laboratory method.                              |
| method\_name      | Name of method.                                                                |
| parameter         | Name of chemical or physical parameter.                                        |
| concentration     | Value of the parameter. Usually, but not always a concentration.               |
| units             | Abbreviation of the units used. Most are SI units of concentration.            |
| qualifier         | EGAD’s `VALIDATION QUALIFIER`. This is a subset of the `LABORATORY QUALIFIER`. |
| qual\_description | Long description of meaning ofqualifiers. Includes other qualifiers            |
| rl                | Reporting Limit. LAb-determined value below which results are non-detects.     |
| mdl               | Method Detection Limit. Official limit of method                               |
| weight\_basis     | Basis for calculating concentration (wet weight, dry weight or lipid weight).  |
| prep\_method      | Alphanumeric code for sample preparation methods.                              |
| dilution\_factor  | Dilution factor for sample analysis. It’s not clear whether this is useful.    |
| cas\_no           | Chemical Abstract Number for chemical compound or mixture.                     |

``` r
write_csv(metadata, 'simple_metadata.csv')
```
