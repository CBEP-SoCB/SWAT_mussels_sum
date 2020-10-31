Generate Working Verion of EGAD Mussel Toxics Data
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
  - [Remove Suspect Data](#remove-suspect-data)
  - [Add Fixed Unit Concentration
    Columns](#add-fixed-unit-concentration-columns)
  - [Express Data in Consistent
    Units](#express-data-in-consistent-units)
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
    ## <bytecode: 0x00000000165c11d0>
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

Details provided in “SWAT\_data\_examination\_CODES.Rmd”.

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
             -`SAMPLE COMMENT`, -`LAB COMMENT`,
             -`VALIDATION COMMENT`) %>%
  
  # Create Site Code and Site Name
  mutate    (SiteCode =  sub('.* - ','', `EGAD_SITE_NAME`), 
             Site     =  sub(' - .*','', `EGAD_SITE_NAME`)) %>%
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

# Remove Suspect Data

See “SWAT\_data\_examination\_WEIGHT\_BASIS.Rmd” for justification.

``` r
mytest <- with(SWAT_final, 
               Code == "CBEEEE_REP_4_2009_5" &
               (`TEST METHOD` == "E1668A" | 
                 PARAMETER == 'PCBS' |
                 grepl('PCB TOTAL TEQ', PARAMETER)) &
               (`WEIGHT BASIS` == 'LIP' | `WEIGHT BASIS` == 'DRY'))
SWAT_final <- SWAT_final %>%
  filter(! mytest)
```

# Add Fixed Unit Concentration Columns

The following units were used for concentrations at least once in the
data. The “netexp” column provides powers of ten to convert to
concentration measured in grams per gram (a relative value of 1, or
10^0).

The logic is developed more fully in “SWAT\_data\_review UNITS.Rmd”.

``` r
conversions <- read.csv(text =
"units, netexp
PG/G,   -12
NG/KG,  -12
NG/G,     -9
UG/KG,  -9
MG/KG,  -6
UG/G,     -6")
```

We calculate values in two sets of standardized units: nanograms per
gram and micrograms per gram. For later convenience, we also calculate
reporting limits in those same units.

# Express Data in Consistent Units

In other Analyses, we have used ug/g for metals and ng/g for organic
contaminants. So we will express concentrations in those units,
extracting the correct conversion factors from the conversions table.

The logic is, we go from units to g/g by multiplying by 10^netexp, and
get from g/g to the desired units by multiplying by either 10^6 or 10^9.
Since both steps are multiplications, we add exponents.

``` r
uggexp <- 6
nggexp <- 9

SWAT_final <- SWAT_final %>% 
  mutate(theexp = conversions$netexp[match(`UNITS VALUE`, conversions$units)]) %>%
  mutate(uggconvexp = uggexp + theexp,
         nggconvexp = nggexp + theexp) %>%
  mutate(conc_ugg   = CONCENTRATION * 10^uggconvexp,
         conc_ngg   = CONCENTRATION * 10^nggconvexp,
         rl_ugg     = RL            * 10^uggconvexp,
         rl_ngg     = RL            * 10^nggconvexp) %>%
  select (-theexp, -uggconvexp, -nggconvexp)
head(SWAT_final,10)
```

    ## # A tibble: 10 x 28
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
    ## # ... with 21 more variables: CURRENT_SAMPLE_POINT_NAME <chr>, `ANALYSIS
    ## #   LAB` <chr>, ANALYSIS_LAB_SAMPLE_ID <chr>, `TEST METHOD` <chr>, `TEST METHOD
    ## #   DESCRIPTION` <chr>, PARAMETER <chr>, CONCENTRATION <dbl>, `UNITS
    ## #   VALUE` <chr>, `LAB QUALIFIER` <chr>, `VALIDATION QUALIFIER` <chr>,
    ## #   `QUALIFIER DESCRIPTION` <chr>, RL <dbl>, MDL <dbl>, `WEIGHT BASIS` <chr>,
    ## #   `PREP METHOD` <chr>, DILUTION_FACTOR <chr>, CAS_NO <chr>, conc_ugg <dbl>,
    ## #   conc_ngg <dbl>, rl_ugg <dbl>, rl_ngg <dbl>

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
    ## [15] "units_value"               "lab_qualifier"            
    ## [17] "validation_qualifier"      "qualifier_description"    
    ## [19] "rl"                        "mdl"                      
    ## [21] "weight_basis"              "prep_method"              
    ## [23] "dilution_factor"           "cas_no"                   
    ## [25] "conc_ugg"                  "conc_ngg"                 
    ## [27] "rl_ugg"                    "rl_ngg"

``` r
nms[8]  <- "samp_pt_name"
nms[9]  <- "lab"
nms[10] <- "lab_id"
nms[11] <- "method"
nms[12] <- "method_name"
nms[15] <- "units"
nms[16] <- "lab_qualifier"
nms[17] <- "qualifier"
nms[18] <- "qual_description"
nms
```

    ##  [1] "site_seq"         "sitecode"         "site"             "year"            
    ##  [5] "sample_date"      "sample_id"        "code"             "samp_pt_name"    
    ##  [9] "lab"              "lab_id"           "method"           "method_name"     
    ## [13] "parameter"        "concentration"    "units"            "lab_qualifier"   
    ## [17] "qualifier"        "qual_description" "rl"               "mdl"             
    ## [21] "weight_basis"     "prep_method"      "dilution_factor"  "cas_no"          
    ## [25] "conc_ugg"         "conc_ngg"         "rl_ugg"           "rl_ngg"

``` r
names(SWAT_final) <- nms
```

# Save Resulting Data

``` r
write_csv(SWAT_final, 'SWAT_data_working.csv')
```

# Save Related Metadata

More complete metadata on the EGAD data is included in files included in
the Original\_Data folder.

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
    "Laboratory data qualifier. Sometimes caries non-detect flags missing in the next QUALIFIER.",
    "EGAD's `VALIDATION QUALIFIER`.  This is usually a subset of the `LABORATORY QUALIFIER`.",
    "Long description of meaning of Validation qualifiers.  Includes some other qualifiers.",
    "Reporting Limit.  Lab-determined value below which results are non-detects.",
    "Method Detection Limit.  Official limit of method.  Usually much lower than Reporting Limit.",
    "Basis for calculating concentration (wet weight, dry weight or lipid weight).",
    "Alphanumeric code for sample preparation methods.",
    "Dilution factor for sample analysis.  It's not clear whether this is useful.",
    "Chemical Abstract Number for chemical compound or mixture.",
    "Concentrations, expressed in micrograms per gram (or parts per million).",
    "Concentrations expressed in nanograms per gram (or parts per billion),",
    "Reporting limits, expressed in micrograms per gram (or parts per million).",
    "Reporting limits expressed in nanograms per gram (or parts per billion),")

kable(metadata)
```

| Col\_Name         | Description                                                                                 |
| :---------------- | :------------------------------------------------------------------------------------------ |
| site\_seq         | EGAD sequence number for designated SITE.                                                   |
| sitecode          | Alphanumeric code for a specific SITE.                                                      |
| site              | Name or description of SITE.                                                                |
| year              | Year of sample collection.                                                                  |
| sample\_date      | Date of sample collection.                                                                  |
| sample\_id        | Original EGAD sample ID – NOT unique.                                                       |
| code              | Derived, lengthy alphanumaric code for individual sampling events.                          |
| samp\_pt\_name    | EGAD sample point name. NOT unique. Possibly unique within sites.                           |
| lab               | Laboratory conducting analyses.                                                             |
| lab\_id           | Internal laboratory sample ID number.                                                       |
| method            | Alphanumeric code for specific laboratory method.                                           |
| method\_name      | Name of method.                                                                             |
| parameter         | Name of chemical or physical parameter.                                                     |
| concentration     | Value of the parameter. Usually, but not always a concentration.                            |
| units             | Abbreviation of the units used. Most are SI units of concentration.                         |
| lab\_qualifier    | Laboratory data qualifier. Sometimes caries non-detect flags missing in the next QUALIFIER. |
| qualifier         | EGAD’s `VALIDATION QUALIFIER`. This is usually a subset of the `LABORATORY QUALIFIER`.      |
| qual\_description | Long description of meaning of Validation qualifiers. Includes some other qualifiers.       |
| rl                | Reporting Limit. Lab-determined value below which results are non-detects.                  |
| mdl               | Method Detection Limit. Official limit of method. Usually much lower than Reporting Limit.  |
| weight\_basis     | Basis for calculating concentration (wet weight, dry weight or lipid weight).               |
| prep\_method      | Alphanumeric code for sample preparation methods.                                           |
| dilution\_factor  | Dilution factor for sample analysis. It’s not clear whether this is useful.                 |
| cas\_no           | Chemical Abstract Number for chemical compound or mixture.                                  |
| conc\_ugg         | Concentrations, expressed in micrograms per gram (or parts per million).                    |
| conc\_ngg         | Concentrations expressed in nanograms per gram (or parts per billion),                      |
| rl\_ugg           | Reporting limits, expressed in micrograms per gram (or parts per million).                  |
| rl\_ngg           | Reporting limits expressed in nanograms per gram (or parts per billion),                    |

``` r
write_csv(metadata, 'simple_metadata.csv')
```
