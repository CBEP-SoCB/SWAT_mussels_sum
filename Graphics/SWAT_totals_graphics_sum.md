Graphics for Organic Contaminants from EGAD Mussel Toxics Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
11/10/2020

-   [Introduction](#introduction)
-   [Load Libraries](#load-libraries)
-   [Load Data](#load-data)
    -   [Establish Folder References](#establish-folder-references)
    -   [Copy Data](#copy-data)
    -   [Load Reference Values](#load-reference-values)
    -   [Load Location Information](#load-location-information)
-   [PAHs](#pahs)
    -   [Create working data](#create-working-data)
    -   [Recent Conditions Dotplot
        Graphic](#recent-conditions-dotplot-graphic)
    -   [Trend Graphic](#trend-graphic)
-   [PCBs](#pcbs)
    -   [Create Working Data](#create-working-data-1)
    -   [Recent Conditions Dotplot](#recent-conditions-dotplot)
    -   [Trend Graphic (Unused)](#trend-graphic-unused)
-   [Combined Graphic](#combined-graphic)
    -   [Create Combo Data](#create-combo-data)
    -   [Recent Conditions Dotplot](#recent-conditions-dotplot-1)
    -   [Add Annotations](#add-annotations-1)

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

In this Notebook, we analyze organic contaminants found in blue mussel
(*Mytilus edulis*) tissue. To simplify presentation of levels of organic
contaminants, we focus here on various sums of similar organic
contaminants that can be related to reference values..

We focus on the following analytic totals:

-   Total PAHs,
-   SWAT PCBs

Because they are readily compared to

For additional detail on statistical analysis and alternate graphics,
see the related “SWAT\_totals\_analysis.rmd” file in the “Analysis”
folder.

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

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())

library(LCensMeans)
```

# Load Data

## Establish Folder References

``` r
sibfldnm <- 'Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)
fn <- 'SWAT_totals_working.csv'

dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

## Copy Data

This is a larger data file that takes some time to load. Getting the
column types right dramatically improves load speed. Much of the data is
qualitative, and can’t be handled in R, so we delete it.

To save on keystrokes, we also clean up the parameter names.

``` r
swat_totals <- read_csv(file.path(sibling, fn),
                        col_type = cols(
                          .default = col_character(),
                          site_seq = col_double(),
                          year = col_integer(),
                          sample_date = col_date(format = ""),
                          concentration = col_double(),
                          lab_qualifier = col_logical(),
                          dilution_factor = col_double(),
                          conc_ugg = col_double(),
                          conc_ngg = col_double(),
                          rl_ugg = col_skip(),
                          rl_ngg = col_skip()
                        )) %>%
  select(-site_seq, -lab, -method, -method_name,
         -prep_method, -dilution_factor, -cas_no) %>%

  mutate(parameter = sub('-H', '', parameter)) %>%
  mutate(parameter = sub(' \\(ND=1/2 DL\\)', '', parameter)) %>%
  mutate(parameter = sub('TOTAL ', '', parameter)) %>%
  mutate(parameter = tolower(parameter)) %>%
  mutate(parameter = sub(' ', '_', parameter))
```

``` r
unique(swat_totals$parameter)
```

    ##  [1] "pah19"        "pesticides21" "pcb_teq"      "pcb"          "pah"         
    ##  [6] "pah24"        "pah40"        "dx_teq"       "swat_pcbs"    "dioxins"     
    ## [11] "total_ddt"

## Load Reference Values

There is no good way to establish toxicity benchmarks for edible tissue.
Maine DEP instead compares values of analytic totals to prevalence
benchmarks (medians and 85th percentiles) derived from the Gulfwatch and
National Status And trends monitoring programs, as published by Leblanc
et al. 2009.

> Leblanc, L.A., Krahforst, C.F., Aubé, J., Roach, S., Brun, G.,
> Harding, G., Hennigar, P., Page, D., Jones, S., Shaw, S., Stahlnecker,
> J., Schwartz, J., Taylor, D., Thorpe, B., & Wells, P. (2009).
> Eighteenth Year of the Gulf of Maine Environmental Monitoring Program.

We copied benchmark tables from (an on-line version of) Leblanc et
al. 2009 into our excel spreadsheet, and read those benchmarks in here.

``` r
references <- read_excel(file.path(sibling,"Parameter List.xlsx"), 
                             sheet = "Organic Comparisons",
                         range = 'a3:f8') %>%
  rename('Reference_ngg' = ...1) %>%
  filter(! is.na(Reference_ngg)) %>%
  mutate(Reference_ngg = sub(' 2008', '', Reference_ngg))
```

    ## New names:
    ## * `` -> ...1

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

swat_totals <- swat_totals %>%
  mutate(short_locs = locations$short_locs[match(sitecode,
                                                 locations$SITECODE)]) %>%
  mutate(short_locs = factor(short_locs,
                             levels = c('Navy Pier', 'Mare Brook', 'Mill Creek',
                                        'East End', 'Spring Point')))
#rm(locations)
```

### Add Short Location Names

Adding short location names.

``` r
swat_totals <- swat_totals %>%
  mutate(short_locs = locations$short_locs[match(sitecode, locations$SITECODE)])
```

# PAHs

We focus on PAH 40, since it matches a national and regional benchmark,
and approximates total PAHs fairly well. The sum is highly correlated
with total PAHs, and amounts to about 75 or 80 percent of the total. Our
primary interest here to compare observations to benchmarks.

## Create working data

``` r
pah_data <- swat_totals %>%
  filter(grepl('pah', parameter))

pah_recent_data <- pah_data %>%
  filter(year >= 2010)

sites <- c('CBEEEE', 'CBMCMC', 'CBSPSP')
pah_trend_data <- swat_totals %>%
  filter(grepl('pah', parameter)) %>%
  filter(sitecode %in% sites)
```

## Recent Conditions Dotplot Graphic

The default choice of `binwidth` works. Exploring other `binwidth`s
shows that `binwidth` is:  
\* based on the log of the y axis, so needs to be small to work at all
(on the order of 0.05); and  
\* Influences not only placement, but also graphic size of the “dots”.

The result is that to ensure graphic similarity between the PAH and PCB
graphics, it is most convenient to stick with the default settings,
despite the warning message.

``` r
plt <- pah_recent_data %>%
  filter(parameter == 'pah40') %>%
  
  ggplot(aes(fct_reorder(short_locs, conc_ngg), conc_ngg)) +
 
  geom_dotplot(method = 'histodot',
               binaxis='y',
               stackdir='center',
               binpositions="all",
               fill = cbep_colors()[1]
               ) +
  
  scale_fill_gradient(name = 'Year',
                        low = cbep_colors2()[1],
                        high = cbep_colors2()[4]) +
  scale_y_log10() +
  theme_cbep(base_size = 12)  +
  ylab('Sum of 40 PAHs (ng/g)') +
  xlab('')
```

### Add Annotations

``` r
plt +
  geom_hline(yintercept = 1674, lty = 2,
             color = cbep_colors2()[1],
             lwd = 0.5) +
  geom_hline(yintercept = 618, lty = 1,
             color = cbep_colors2()[1],
             lwd = 0.5) +
  
  annotate('text', x = 0.5, y = 1800, label = 'National',
            hjust = 0,
            size = 3) +

  annotate('text', x = 0.5, y = 670, label = 'Gulf of Maine',
            hjust = 0,
            size = 3)
```

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with `binwidth`.

![](SWAT_totals_graphics_sum_files/figure-gfm/pah_add_annotations-1.png)<!-- -->

``` r
ggsave('figures/pah_40_recent.pdf', device = cairo_pdf, width = 5, height = 4)
```

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with `binwidth`.

## Trend Graphic

``` r
plt <- pah_trend_data %>%
  filter(parameter == 'pah40') %>%
  
  ggplot(aes(year, conc_ngg)) +
  geom_point(aes(color = short_locs), size = 3, alpha = .5) +
  
  geom_smooth(aes(color = short_locs),
              method = 'lm', 
              formula = y~x,
              se = FALSE) +
  scale_y_log10() +
  
  theme_cbep(base_size = 12)  +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = cbep_colors()) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
  
  ylab('PAHs (ng/g)') +
  xlab('Year')
```

``` r
plt +
  geom_hline(yintercept = 1674, lty = 2,
             color = cbep_colors2()[1],
             lwd = 0.5) +
  geom_hline(yintercept = 618, lty = 1,
             color = cbep_colors2()[1],
             lwd = 0.5) +
  
  annotate('text', x = 2009, y = 1800, label = 'National',
            hjust = 0,
            size = 3) +

  annotate('text', x = 2009, y = 670, label = 'Gulf of Maine',
            hjust = 0,
            size = 3)
```

![](SWAT_totals_graphics_sum_files/figure-gfm/pah_trend_graphic_annot-1.png)<!-- -->

``` r
ggsave('figures/pah_40_trend.pdf', device = cairo_pdf, width = 6, height = 4)
```

# PCBs

## Create Working Data

``` r
pcb_data <- swat_totals %>%
  filter(grepl('pcb', parameter))

pcb_recent_data <- pcb_data %>%
  filter(year > 2009)

pcb_trend_data <- pcb_data %>%
  filter(! grepl('teq', parameter)) %>%
  filter(sitecode %in% sites)
```

## Recent Conditions Dotplot

``` r
plt <- pcb_recent_data %>%
  filter(parameter == 'swat_pcbs') %>%
  
  ggplot(aes(fct_reorder(short_locs, conc_ngg), conc_ngg)) +
 
  geom_dotplot(method = 'histodot',
               binaxis='y',
               stackdir='center',
               binpositions="all",
               fill = cbep_colors()[5]
               ) +
  
  scale_fill_gradient(name = 'Year',
                        low = cbep_colors2()[1],
                        high = cbep_colors2()[4]) +
  scale_y_log10() +
  theme_cbep(base_size = 12)  +
  ylab('SWAT PCBs (ng/g)') +
  xlab('')
plt
```

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with `binwidth`.

![](SWAT_totals_graphics_sum_files/figure-gfm/pcb_graphic-1.png)<!-- -->

``` r
plt +
  geom_hline(yintercept = 141, lty = 2,
             color = cbep_colors2()[1],
             lwd = 0.5) +
  geom_hline(yintercept = 35.4, lty = 1,
             color = cbep_colors2()[1],
             lwd = 0.5) +
  
  annotate('text', x = 0.5, y = 160, label = 'National',
            hjust = 0,
            size = 3) +

  annotate('text', x = 0.5, y = 40, label = 'Gulf of Maine',
            hjust = 0,
            size = 3)
```

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with `binwidth`.

![](SWAT_totals_graphics_sum_files/figure-gfm/pcb_add_annotations-1.png)<!-- -->

``` r
ggsave('figures/swat_pcbs_recent.pdf', device = cairo_pdf, width = 5, height = 4)
```

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with `binwidth`.

## Trend Graphic (Unused)

``` r
plt <- pcb_trend_data %>%
  filter(parameter == 'swat_pcbs') %>%
  
  ggplot(aes(year, conc_ngg, color = short_locs)) +
  geom_point(size = 3, alpha = .5) +
  
  geom_smooth(method = 'lm', se = FALSE) +
  scale_y_log10() +
  
  theme_cbep(base_size = 12)  +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = cbep_colors()) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
  
  ylab('SWAT PCBs (ng/g)') +
  xlab('Year')
```

``` r
plt +
  geom_hline(yintercept = 141, lty = 2,
             color = cbep_colors2()[1],
             lwd = 0.5) +
  geom_hline(yintercept = 35.4, lty = 1,
             color = cbep_colors2()[1],
             lwd = 0.5) +
  
  annotate('text', x = 2009, y = 160, label = 'National',
            hjust = 0,
            size = 3) +

  annotate('text', x = 2009, y = 40, label = 'Gulf of Maine',
            hjust = 0,
            size = 3)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](SWAT_totals_graphics_sum_files/figure-gfm/pcb_trend_add_annotations-1.png)<!-- -->

``` r
ggsave('figures/swat_pcbs_trend.pdf', device = cairo_pdf, width = 6, height = 4)
```

    ## `geom_smooth()` using formula 'y ~ x'

# Combined Graphic

The following code reorganizes data frames into the long format needed
by ggplot to produce a faceted graphic. The facets will be based on the
parameter name, so parameter values must match across the data and
reference data frames.

We filter the references to only the 85th percentile values. Since we
are only showing the 85th percentile values, we can use shorter, more
informative labels. We try a couple of alternatives.

## Create Combo Data

``` r
combo_data <- pah_recent_data %>%
  bind_rows(pcb_recent_data) %>%
  filter(parameter %in% c('swat_pcbs', 'pah40'))%>%
  mutate(short_locs = factor(short_locs,
                             levels = c('Navy Pier', 'Mare Brook', 'Mill Creek',
                                        'East End', 'Spring Point')))

combo_refs <- references %>%
  select(Reference_ngg, PAH40, PCB21) %>%
  filter(grepl('85', Reference_ngg)) %>%
  mutate(short_ref = sub(' 85th Percentile', '', Reference_ngg)) %>%
  mutate(alt_ref = factor(short_ref,
                          levels = c('NS&T', 'Gulfwatch'),
                          labels = c('National', 'Gulf of Maine'))) %>%
  rename(pah40 = PAH40,
         swat_pcbs = PCB21) %>%
  pivot_longer(c(pah40, swat_pcbs),
               names_to = "parameter",
               values_to = "value")
```

## Recent Conditions Dotplot

``` r
# Establish named labels
  labs <- c('PAHs', 'PCBs')
  names(labs) <- c("pah40", "swat_pcbs")
  
plt <- combo_data %>%
  ggplot(aes(short_locs, conc_ngg)) +
 
  geom_dotplot(aes(fill = parameter), 
               method = 'histodot',
               binaxis='y',
               stackdir='center',
               binpositions="all") +
  
  facet_wrap(~parameter, ncol = 2,
             scales = 'free_y',
             labeller = labeller(parameter = labs)) +
  
  scale_fill_manual(values = cbep_colors(),
                    name = '',
                    labels = c('PAHs', 'PCBs')) +

  scale_y_log10() +
  theme_cbep(base_size = 12)  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25),
        strip.text = element_text(size = 12),
        legend.position = 'none') +
  ylab('Concentration (ng/g)') +
  xlab('')
```

## Add Annotations

``` r
plt +
  geom_hline(data = combo_refs, 
             aes(yintercept = value, lty = alt_ref),
             color = cbep_colors2()[1],
             lwd = 0.5) +
  
  
  geom_text(data = combo_refs, 
            aes(x = 0.25, y = value, label = alt_ref),
            hjust = 0,
            size = 3,
            nudge_y = .05)
```

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with `binwidth`.

![](SWAT_totals_graphics_sum_files/figure-gfm/combo_add_annotations-1.png)<!-- -->

``` r
ggsave('figures/combo_recent.pdf', device = cairo_pdf, width = 5, height = 4)  
```

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with `binwidth`.
