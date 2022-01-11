Review of Maine DEP EGAD Mussel Tissue Toxics Spatial Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
9/10/2020

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder Reference](#establish-folder-reference)
      - [Copy Data](#copy-data)
      - [Remove duplicates](#remove-duplicates)
  - [Sites](#sites)
      - [List of Sites](#list-of-sites)
      - [Match Sites with Geographic
        Locations](#match-sites-with-geographic-locations)
  - [Sample Points](#sample-points)
      - [Examine Distances between Site and Sample
        Points](#examine-distances-between-site-and-sample-points)
      - [Map Visualization](#map-visualization)
      - [Missing Sample Point Data](#missing-sample-point-data)
      - [Write CSV file for GIS](#write-csv-file-for-gis)

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

In this notebook and accompanying notebooks, we take various slices
through the data to understand its structure.

This Notebook generates the following derived data files:

sample\_spatial.csv sites\_spatial.csv

# Load Libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------ tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts --------------------------------------- tidyverse_conflicts() --
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
    ## <bytecode: 0x000000001659be78>
    ## <environment: namespace:ggplot2>

``` r
library(LCensMeans)
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
collection events.

As far as we have been able to tell, however, there is no single
consistent sample identifier between the geographic data and toxics data
as received.

It appears we can match sites from the geospatial data based on`SITE
SEQ` or by pulling apart the “EGAD\_SITE\_NAME” and
“CURRENT\_SITE\_NAME” into a name and a code, and matching by code.

Sample locations are more complicated, sincee in many cases, the same
string is used to designate replicates at different SITEs. That is,
sample points are not unique, athough the combination of sample point,
site and year does appear to be unique.

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
samples_spatial_data <- read_excel(file.path(aunt, fn))%>%
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

So, all DEP mussel tissue SITEs are included. Presumably the other sites
have samples of other shellfish (clams, in particular were provided in
the second tab of the source Excel File).

# Sample Points

Many Sites have more than one SAMPLE POINT. The SAMPLE POINTs appear to
be the real sampling locations, each associated with a nominal SITE.
Unfortunately, recording of actual sampling locations may have been
inconsistent with real SAMPLE POINT data not always available,
especially from some earlier sample years.

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

``` r
rm(a)
```

Several Sites show (unreasonable?) large differences between Sample
Points and nominal Site locations. To make sense of this, we mapped
Sites and Sample Points in Arc GIS.

## Map Visualization

![Map of SWAT Sampling Locations and Nominal
Sites](SWAT_Toxics_Locations.jpg) The map shows clearly that sites
represent general areas, not the exact locations where samples were
collected. In most cases, the distances are trivial on a map of this
scale, but not always.

  - **CBBBBB – Back Bay**: Do not appear to be typos, but exact
    locations do not appear to have been collected in all years. Samples
    appea rto have been collected from various locations along the Back
    Bay Shoreline. identical for all samples. Locations in Samples in
    2015 all fairly consistent, but differ slightly.
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
  - **CBPRM – Presumpscot River Mouth** – Sample Points along shore,
    BOTH shores.

## Missing Sample Point Data

Some Sites do not appear to have independent location data for sample
points, or not for all sample points. That is actually easier to see in
the Excel file, but we can look at it in R by searching for NAs.

``` r
samples_spatial_data %>%
  filter(is.na(`SAMPLE POINT UTM X`)) %>%
  select(-starts_with('SITE'), - starts_with('LOCATION'))
```

    ## # A tibble: 21 x 9
    ##    CURRENT_SITE_NA~ DATA_SOURCE `SAMPLE POINT S~ `SAMPLE POINT N~
    ##    <chr>            <chr>                  <dbl> <chr>           
    ##  1 MILL CREEK - CB~ MDEP BLWQ ~            82245 REP 4           
    ##  2 MILL CREEK - CB~ MDEP BLWQ ~           132065 REP 1 2017      
    ##  3 MILL CREEK - CB~ MDEP BLWQ ~           132066 REP 2 2017      
    ##  4 MILL CREEK - CB~ MDEP BLWQ ~           132067 REP 3 2017      
    ##  5 MILL CREEK - CB~ MDEP BLWQ ~           132068 REP 4 2017      
    ##  6 BRUNSWICK MARE ~ MDEP BLWQ ~           132078 REP 1 2017      
    ##  7 BRUNSWICK MARE ~ MDEP BLWQ ~           132079 REP 2 2017      
    ##  8 BRUNSWICK MARE ~ MDEP BLWQ ~           132080 REP 3 2017      
    ##  9 BRUNSWICK MARE ~ MDEP BLWQ ~           132081 REP 4 2017      
    ## 10 EAST END BEACH ~ MDEP BLWQ ~           132062 REP 1 2017      
    ## # ... with 11 more rows, and 5 more variables: `SAMPLE POINT TYPE` <chr>,
    ## #   `SAMPLE POINT UTM X` <dbl>, `SAMPLE POINT UTM Y` <dbl>, `SAMPLE POINT
    ## #   LATITUDE` <dbl>, `SAMPLE POINT LONGITUDE` <dbl>

This affects the followingfive sites atleast once:

``` r
samples_spatial_data %>%
  filter(is.na(`SAMPLE POINT UTM X`)) %>%
  select(CURRENT_SITE_NAME) %>%
  unique()
```

    ## # A tibble: 5 x 1
    ##   CURRENT_SITE_NAME                     
    ##   <chr>                                 
    ## 1 MILL CREEK - CBMCMC                   
    ## 2 BRUNSWICK MARE BROOK DRAINAGE - CBMBBH
    ## 3 EAST END BEACH - CBEEEE               
    ## 4 PRESUMPSCOT RIVER (MOUTH)  - CBPRMT   
    ## 5 MIDDLE BAY (OUTER) - CBMBMB

In several cases, this represents missing data from just one year, for
sites that were sampled repeatedly. Unfortunately, a quick review of the
data shows that similar sample point names (for replicates) were used
for different physical locations, with UTM coordinates suggesting
samples with similar names in different years were often collected well
over 100 meters apart. We can not readily “rescue” missing data based on
data from prior years.

Accordingly, it appears that the best we can do is assign data to Sites,
even though samples for some sites are spread over hundreds of meters of
shoreline.

## Write CSV file for GIS

First, we discard site data for locations for which we have no mussel
samples. Since we used names sites to generate the SITE code, we can
check if that is NA. If it is, we have no related mussel samples.

``` r
sites_spatial_data <- sites_spatial_data %>%
  filter(! is.na(SITECODE))

#write_csv(sites_spatial_data, 'sites_spatial.csv')
```

ArcGIS was having trouble reading earlier drafts of the following file,
so we simplify here by removing spaces from column names and removing
NAs. Without this step, NAs were forcing ArcGIS to interpret data
columns as text.

``` r
samples_spatial_data %>%
  select ( -`SITE UTM X`, -`SITE UTM Y`, -`SITE LATITUDE`, -`SITE LONGITUDE`,
           - DATA_SOURCE, -LOCATION_ACCURACY, -LOCATION_METHOD) %>%
  rename_all(~gsub(' ', '', .))
```

    ## # A tibble: 183 x 9
    ##    SITESEQ CURRENT_SITE_NA~ SAMPLEPOINTSEQ SAMPLEPOINTNAME SAMPLEPOINTTYPE
    ##      <dbl> <chr>                     <dbl> <chr>           <chr>          
    ##  1   70615 HARRASEEKET RIV~          70661 A               MARINE         
    ##  2   70672 BACK BAY - CBBB~          70699 REP 1           MARINE         
    ##  3   70672 BACK BAY - CBBB~          70700 REP 2           MARINE         
    ##  4   70672 BACK BAY - CBBB~          70701 REP 3           MARINE         
    ##  5   70672 BACK BAY - CBBB~         127990 REP 1 2015      MARINE         
    ##  6   70672 BACK BAY - CBBB~         127991 REP 2 2015      MARINE         
    ##  7   70672 BACK BAY - CBBB~         127992 REP 3 2015      MARINE         
    ##  8   70672 BACK BAY - CBBB~         127993 REP 4 2015      MARINE         
    ##  9   70672 BACK BAY - CBBB~         127994 REP 5 2015      MARINE         
    ## 10   70674 FORE RIVER OUTE~          70719 1N              MARINE         
    ## # ... with 173 more rows, and 4 more variables: SAMPLEPOINTUTMX <dbl>,
    ## #   SAMPLEPOINTUTMY <dbl>, SAMPLEPOINTLATITUDE <dbl>,
    ## #   SAMPLEPOINTLONGITUDE <dbl>

``` r
#write_csv('samples_spatial.csv', na = '')
```
