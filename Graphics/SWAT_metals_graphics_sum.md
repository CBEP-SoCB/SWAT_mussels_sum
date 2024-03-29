Analysis of Metal Contaminants from EGAD Mussel Toxics Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
11/10/2020

-   [Introduction](#introduction)
    -   [Handling Non-Detects](#handling-non-detects)
-   [Load Libraries](#load-libraries)
-   [Utility Functions](#utility-functions)
    -   [Capitalise Strings](#capitalise-strings)
-   [Load Data](#load-data)
    -   [Establish Folder References](#establish-folder-references)
    -   [Copy Data](#copy-data)
    -   [Load Reference Values](#load-reference-values)
    -   [Load Location Information](#load-location-information)
-   [Data Since 2010](#data-since-2010)
    -   [Recents Graphic](#recents-graphic)
-   [Trend Analyses](#trend-analyses)
    -   [Trend Graphic](#trend-graphic)
    -   [Results Table](#results-table)
    -   [Summary Table](#summary-table)

<img
  src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
  style="position:absolute;top:10px;right:50px;" />

# Introduction

Maine’s Department of Environmental Protection (DEP) maintains a large
database of environmental data called “EGAD”. Citizens can request data
from the database through DEP staff.

CBEP requested data from DEP on levels of toxic contaminants in
shellfish tissue samples from Casco Bay. The result is a large (&gt;
100,000 line) excel spreadsheet containing data from about 40 sampling
dates from 20 locations, over a period of more than 15 years.

In this Notebook, we analyze metal contaminants found in blue mussel
(*Mytilus edulis*) tissue. To simplify presentation, we focus on two
questions: Do recent samples exceed screening values, and do any metals
show a trend in levels?

DEP staff pointed us towards the detailed documentation in the 2017-2018
SWAT data report.

> Maine Department of Environmental Protection. 2019. Surface Water
> Ambient Toxics Monitoring Program. 2017-2018 Report to the Joint
> Committee on Environmental and Natural Resources. 129th Legislature,
> First Session.

Much of the following analysis is based on that information.

## Handling Non-Detects

A “Non-Detect” is a sample where the concentration of a specific
contaminant is below the level where the laboratory methods are able to
reliably detect the contaminant against background interference,
measurement error, and instrument limitations. Statistically, a
“non-detect” is a “left censored” observation. We know the observation
falls below the reporting limit, but we don’t know by how much.

Leaving non-detects out of analyses entirely biases results, because the
lowest observations (the non-detects) are omitted from analysis. So it
is generally necessary to adopt statistical methods or some other
convention for handling non-detects.

While more sophisticated modeling approaches are available, CBEP’s
preference for addressing non-detects is to replace non-detects before
conducting analyses with an alternate value. The question then becomes,
what value should be used to replace the (unobserved) non-detect?

CBEP’s preference is to use (statistically-based) estimates of what one
would expect to have observed, if laboratory methods were better.
Specifically, for the 2020 State of Casco Bay Report, CBEP prefers to
replace non-detects with maximum likelihood estimates of the expected
value of the (unobserved) censored observations.

However, we have not done that for these analyses. We prefer to treat
non-detects from any single data source consistently, and we could not
use our preferred convention for addressing non-detects for organic
contaminants from the SWAT data. Because of differences in reporting of
PAHs from year to year, we were unable to recalculate PAH totals from
the raw observations, and instead have relied on calculations conducted
by Maine DEP. DEP provides results using three different conventions:
replace non-detects with, respectively, zero, half the reporting limit,
or the reporting limit.

Here, we follow their convention of conducting analyses based on
replacing non-detects with one half of the Reporting Limit.

For metals in these samples, non-detects are relatively rare, and
detection limits are well below observed levels, so choice of the
non-detect convention should have little effect on the qualitative
results of these analyses.

# Load Libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(htmltools)  # used by knitr called here only to avoid startup text later in document
library(knitr)      # knitr's namespace is already available, so loading it with
                    # 'library()' only avoids start-up issues when we call
                    # knitr::kable() later.

library(mblm)
library(emmeans)

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())

library(LCensMeans)
```

# Utility Functions

## Capitalise Strings

`This`convert\_caps()\` splits a string, capitalizes the first character
and uncaps the rest. FOr one word strings, this correspondes to “title
case”.

``` r
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
      sep="", collapse=" ")
}

convert_caps <- function(x_vec) {
  as_vector(lapply(x_vec, simpleCap))
}
```

# Load Data

## Establish Folder References

``` r
sibfldnm <- 'Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)
fn <- 'SWAT_metals_working.csv'

dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

## Copy Data

Getting the column types right improves load speed. Much of the data is
qualitative, and can’t be readily handled in R, so we delete it. To save
on keystrokes, we also clean up the parameter names.

``` r
swat_metals <- read_csv(file.path(sibling, fn),
                        col_types = cols(
                          .default = col_character(),
                          site_seq = col_double(),
                          year = col_double(),
                          sample_date = col_date(format = ""),
                          concentration = col_double(),
                          rl = col_double(),
                          mdl = col_double(),
                          dilution_factor = col_double(),
                          cas_no = col_double(),
                          conc_ugg = col_double(),
                          rl_ugg = col_double(),
                          flag = col_logical(),
                          conc_RL = col_double(),
                          conc_HALF = col_double(),
                          conc_ML = col_double()
                        )) %>%
  select(-site_seq, -lab, -method, -method_name,
         -prep_method, -dilution_factor, -cas_no) %>%
  mutate(parameter = convert_caps(parameter))
```

## Load Reference Values

There are few really good ways to establish toxicity benchmarks for
edible tissue. Maine DEP principally compares values of analytic totals
to prevalence benchmarks (medians and 85th percentiles) derived from the
Gulfwatch and National Status And trends monitoring programs, as
published by Leblanc et al. 2009.

> Leblanc, L.A., Krahforst, C.F., Aubé, J., Roach, S., Brun, G.,
> Harding, G., Hennigar, P., Page, D., Jones, S., Shaw, S., Stahlnecker,
> J., Schwartz, J., Taylor, D., Thorpe, B., & Wells, P. (2009).
> Eighteenth Year of the Gulf of Maine Environmental Monitoring Program.

We copied benchmark tables from (an on-line version of) Leblanc et
al. 2009 into our excel spreadsheet.

That document refers to “MCDC Fish Tissue Action Levels.” and FDA Action
Levels. After some digging, we found a document on the Maine CDC website
that corresponds to (most or all of) the FTALs reported by DEP.

**ALL HEALTH\_RELATED THRESHOLDS ARE EXPRESSED ON A WET WEIGHT BASIS**

> Maine Center for Disease Control. 2001. Bureau of Health Fish Tissue
> Action Levels.
> <https://www.maine.gov/dhhs/mecdc/environmental-health/eohp/fish/documents/action-levels-writeup.pdf>.
> Accessed 11/10/2020.

``` r
references <- read_excel(file.path(sibling,"Parameter List.xlsx"), 
                             sheet = "Metals Comparisons") %>%
  filter(! is.na(Reference_ugg)) %>%
  mutate(Reference_ugg = sub(' 2008', '', Reference_ugg))
```

We will use only two of the reference values:  
\* “NS&T 85th Percentile”  
\* “Gulfwatch 85th Percentile”

The Health-based metrics are valuable for comparison, but they are
expressed on a wet weight basis, and so can not be shown on one graph
with the other reference levels.

We add shorter names for graphics

``` r
references <- references %>%
  mutate(short_ref = case_when(
      Reference_ugg == 'NS&T 85th Percentile'       ~ 'National',
      Reference_ugg == 'Gulfwatch 85th Percentile'  ~ 'Gulf of Maine'
  ))
```

### Pivot Most Important References to Long Form

The long form annotation data allows us to add annotations to faceted
plots in a simple way. While we are at it, we create better names for
some reference thresholds.

``` r
ref_long <- references %>%
  filter(grepl('85', Reference_ugg)) %>%
  pivot_longer(cols = Silver:Zinc, names_to = 'parameter', values_to = 'value')
```

## Load Location Information

And add a short location name for figures.

``` r
locations <- read_csv(file.path(sibling,"sites_spatial.csv"), 
    col_types = cols(SITESEQ = col_skip(), 
        LAT = col_skip(), LONG = col_skip())) %>%
  filter (! is.na(SITE)) %>%  # drops unnamed sites

  mutate(short_locs= c("Back Bay",
                      "Outer Fore River",
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

swat_metals <- swat_metals %>%
  mutate(short_locs = locations$short_locs[match(sitecode,
                                                 locations$SITECODE)]) %>%
  mutate(short_locs = factor(short_locs,
                             levels = c('Navy Pier', 'Mare Brook', 'Mill Creek',
                                        'East End', 'Spring Point')))
#rm(locations)
```

# Data Since 2010

Give the paucity of data and sited from 2015 through 2019, we consider
“recent” data to include the last 10 years, or 2010 through 2019.

``` r
recent_data <- swat_metals %>%
  filter(year >= 2010)
```

## Recents Graphic

``` r
plt <- ggplot(recent_data,
              aes(short_locs, conc_HALF)) +
  geom_point(color = cbep_colors()[1], alpha = 0.5) +
  
  scale_y_log10() +
  theme_cbep(base_size = 12) +
  theme(legend.title = element_blank()) +
  
  
  facet_wrap(~parameter, scale="free_y") +
  ylab(expression("Concentration (" *mu *"g/g)")) +
  xlab('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1)) +
  theme(legend.position = 'bottom',
        panel.border = element_rect(color = 'gray', fill = NA, size = 0.25))
```

``` r
plt +
  geom_hline(mapping = aes(yintercept = value,
                 lty = short_ref, color = short_ref),
             data = ref_long ) +
 scale_color_manual(values = cbep_colors2())
```

    ## Warning: Removed 4 rows containing missing values (geom_hline).

![](SWAT_metals_graphics_sum_files/figure-gfm/recent_add_refs-1.png)<!-- -->

``` r
 ggsave('figures/metals_parameters.pdf', device = cairo_pdf, width = 7, height = 5)
```

    ## Warning: Removed 4 rows containing missing values (geom_hline).

# Trend Analyses

We have few sites with good temporal coverage. Most sites were only
sampled once. The high frequency sites (three or more sample years)
include: \* CBEEEE – \* CBMCMC \* CBSPSP

``` r
sites <- c('CBEEEE', 'CBMCMC', 'CBSPSP')
trend_data <- swat_metals %>%
  filter(sitecode %in% sites)
```

## Trend Graphic

``` r
plt <- trend_data %>%
  
  ggplot(aes(year, conc_HALF)) +
  geom_point(aes(color = short_locs), alpha = .5) +
  
  geom_smooth(aes(color = short_locs), method = 'lm', se = FALSE) +
  facet_wrap(~parameter, scale="free_y") +
  
  scale_y_log10() +
  scale_x_continuous(breaks = c(2006, 2008, 2010, 2012, 2014, 2016)) +
  scale_color_manual(values = cbep_colors()) +
  ylab(expression("Concentration (" *mu *"g/g)")) +
  xlab('') + 
  
  theme_cbep(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1)) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.border = element_rect(color = 'gray', fill = NA, size = 0.25))
plt
```

    ## `geom_smooth()` using formula 'y ~ x'

![](SWAT_metals_graphics_sum_files/figure-gfm/trend_graphic-1.png)<!-- -->

## Results Table

We pull results of linear models and robust linear models developed in
the file ‘SWAT\_metals\_analysis.Rmd’ in the Analysis Folder. There, we
looked at log linear models, robust linear models, and factorial models
looking at slopes at each of the three long-term sites. We draw
conclusions based on careful evaluation of all models, after considering
whether data are suitable for different models.

The following table summarizes results of those models. (The following
was transcribed by hand, which is error-prone, and won’t update
automatically).

``` r
trends_table <- tribble(
~Symbol, ~Element, ~`Linear Trend`, ~`Kendalls TAU`, ~ `Step Trend`,
'Ag', 'Silver',   'Declining', 'No Trend',   'Declining at Spring Point*',
'Al', 'Aluminum', 'No Trend',  'No Trend**', 'No Trend',
'As', 'Arsenic',  'No Trend',  'Increasing', 'Increasing',
'Cd', 'Cadmium',  'No Trend',  'No Trend',   'No Trend',
'Cr', 'Chromium', 'Declining', 'Declining',  'Declining at East End and Spring Point',
'Cu', 'Copper',   'No Trend',  'No Trend',   'Declining at Mill Creek***',
'Fe', 'Iron',     'Declining', 'Declining',  'Declining',
'Hg', 'Mercury',  'No Trend',  'No Trend',   'Increasing at Spring Point****',
'Ni', 'Nickle',   'No Trend',  'No Trend',   'No Trend',
'Pb', 'Lead',     'No Trend',  'No Trend',   'No Trend',
'Se', 'Selenium', 'No Trend',  'No Trend',   'No Trend',
'Zn',   'Zinc',     'No Trend',  'No Trend',   'No Trend')

knitr::kable(trends_table)
```

| Symbol | Element  | Linear Trend | Kendalls TAU | Step Trend                             |
|:-------|:---------|:-------------|:-------------|:---------------------------------------|
| Ag     | Silver   | Declining    | No Trend     | Declining at Spring Point\*            |
| Al     | Aluminum | No Trend     | No Trend\*\* | No Trend                               |
| As     | Arsenic  | No Trend     | Increasing   | Increasing                             |
| Cd     | Cadmium  | No Trend     | No Trend     | No Trend                               |
| Cr     | Chromium | Declining    | Declining    | Declining at East End and Spring Point |
| Cu     | Copper   | No Trend     | No Trend     | Declining at Mill Creek\*\*\*          |
| Fe     | Iron     | Declining    | Declining    | Declining                              |
| Hg     | Mercury  | No Trend     | No Trend     | Increasing at Spring Point\*\*\*\*     |
| Ni     | Nickle   | No Trend     | No Trend     | No Trend                               |
| Pb     | Lead     | No Trend     | No Trend     | No Trend                               |
| Se     | Selenium | No Trend     | No Trend     | No Trend                               |
| Zn     | Zinc     | No Trend     | No Trend     | No Trend                               |

-   -   Silver results are influenced by non detects. Elevated detection
        limits in 2007 and to a lesser extent in 2014 dominate the
        analysis. So trends detected in these analyses are not supported
        by observed values. This problem is an excellent example of why
        we prefer to use statistically-based estimates of censored
        values.

-   \*\* Aluminum shows a significant decline by `mblm()`, but not by
    the closely related Kendall’s Tau (implemented in he `cor()`
    function using `method = 'kendall'`). Kendall’s Tau is our preferred
    test for significance.

-   \*\*\* Copper trend at Mill Creek is due to one very high
    observation at Mill Creek in 2006. Although the trend is nominally
    statistically significant, its dependence on a single observation
    makes it less than compelling.

-   \*\*\*\* Mercury confidence interval for Spring Point does not
    include zero, but it is very close. Given uncertainty about model
    specification, we should not take the implied confidence interval
    probability (nominally 95%) very seriously. This trend should be
    considered provisional. Given the large number of comparisons we
    have run here, and the lack of support for trends in the other
    models, we should not take this too seriously.

So, results across multiple tests are fairly consistent. But we should
be careful because there are multiple comarisons here, so by chance we
expect some significant results that are probably meaningless.

## Summary Table

We can summarize results as follows:

``` r
sum_table <- tribble(
  ~Metal, ~Result,
  'Arsenic',  'Increasing',
  'Chromium', 'Declining at East End and Spring Point',
  'Iron',     'Declining',
  'Mercury',  'Weak evidence for an increase at Spring Point.'
)

knitr::kable(sum_table, caption = 'Trends in Metals in Blue Mussels from Three Sites')
```

| Metal    | Result                                         |
|:---------|:-----------------------------------------------|
| Arsenic  | Increasing                                     |
| Chromium | Declining at East End and Spring Point         |
| Iron     | Declining                                      |
| Mercury  | Weak evidence for an increase at Spring Point. |

Trends in Metals in Blue Mussels from Three Sites
