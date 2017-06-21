[![Travis-CI Build Status](https://travis-ci.org/koncina/elisar.svg?branch=master)](https://travis-ci.org/koncina/elisar) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/koncina/elisar?branch=master&svg=true)](https://ci.appveyor.com/project/koncina/elisar) [![codecov](https://codecov.io/gh/koncina/elisar/branch/master/graph/badge.svg)](https://codecov.io/gh/koncina/elisar)

> ElisaR can handle Tecan Sunrise excel exports which were modified to include the plate layout and sample identifications. It will perform a 4PL regression (using the `drc` library) and return a dataframe containing the concentrations corresponding to the O.D. values.

Installation
------------

    devtools::install_git('https://github.com/koncina/elisar.git')

Usage
-----

### Prepare the Excel files

1.  Export Tecan sunrise results as MS Excel `.xls` files.
2.  Open the file (an example is shown in the screenshots below), duplicate the sheet and optionally rename it (e.g. `id`).
3.  On the duplicated sheet replace the O.D. values by unique identifiers for each sample and standard.

-   Unused wells can be specified by the keyword `empty` (case insensitive)
-   The blank value is specified by the keyword `blank` (case insensitive)
-   The standard values are constructed with a common leading `std.key` id (defaults to `STD` but can be adjusted in the `elisa.analyse()` function) merged to a trailing concentration value. For example: 250, 500 and 1000 pg/ml standard points would be encoded as STD250, STD500 and STD1000 (see wells in rows A-G and columns 11 to 12 in the second screenshot below).

1.  It is possible to extend the identifications by placing a second table below the layout. The table should contain headers and requires the mandatory column `id` which should list all IDs reported in the layout. One can add as much columns as required to fully describe the data.

#### Screenshots

\*Original Tecan Excel <file:*> ![alt text](example/01.png)

*Modified Tecan Excel file to include sample identifications:* ![alt text](example/02.png)

### Import the file in *R*

``` r
library(elisar)
example <- system.file("extdata", "example.xls", package = "elisar")
input <- read_plate(example)
input
```

    ## # A tibble: 96 x 12
    ##    element_id        file       sheet_name sheet_pos format   row   col
    ##         <int>       <chr>            <chr>     <int>  <int> <chr> <chr>
    ##  1          1 example.xls Magellan Sheet 1         1     96     A     1
    ##  2          1 example.xls Magellan Sheet 1         1     96     B     1
    ##  3          1 example.xls Magellan Sheet 1         1     96     C     1
    ##  4          1 example.xls Magellan Sheet 1         1     96     D     1
    ##  5          1 example.xls Magellan Sheet 1         1     96     E     1
    ##  6          1 example.xls Magellan Sheet 1         1     96     F     1
    ##  7          1 example.xls Magellan Sheet 1         1     96     G     1
    ##  8          1 example.xls Magellan Sheet 1         1     96     H     1
    ##  9          1 example.xls Magellan Sheet 1         1     96     A     2
    ## 10          1 example.xls Magellan Sheet 1         1     96     B     2
    ## # ... with 86 more rows, and 5 more variables: id <chr>,
    ## #   description <chr>, treatment <chr>, medium <chr>, value <dbl>

### Perform the regression

``` r
df <- elisa_analyse(input)
df
```

    ## # A tibble: 34 x 20
    ##    .group drm_slope drm_lower drm_upper drm_ed50 out_of_range element_id
    ##     <int>     <dbl>     <dbl>     <dbl>    <dbl>        <lgl>      <int>
    ##  1      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1
    ##  2      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1
    ##  3      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1
    ##  4      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1
    ##  5      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1
    ##  6      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1
    ##  7      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1
    ##  8      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1
    ##  9      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1
    ## 10      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1
    ## # ... with 24 more rows, and 13 more variables: file <chr>,
    ## #   sheet_name <chr>, sheet_pos <int>, format <int>, row <chr>, col <chr>,
    ## #   id <chr>, description <chr>, treatment <chr>, medium <chr>,
    ## #   value <dbl>, estimate <dbl>, estimate_sd <dbl>

The `elisa.analyse()` function performs a 4 parameter logistic regression (using `drc::drm()`) and returns a dataframe with the calculated concentration values. **Note** that a warning is displayed when O.D. values are not within the range of standard points. These values are tagged as FALSE in the `.valid` column.

``` r
head(df)
```

    ## # A tibble: 6 x 20
    ##   .group drm_slope drm_lower drm_upper drm_ed50 out_of_range element_id        file       sheet_name sheet_pos format   row   col    id description treatment medium value   estimate estimate_sd
    ##    <int>     <dbl>     <dbl>     <dbl>    <dbl>        <lgl>      <int>       <chr>            <chr>     <int>  <int> <chr> <chr> <chr>       <chr>     <chr>  <chr> <dbl>      <dbl>       <dbl>
    ## 1      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1 example.xls Magellan Sheet 1         1     96     G     1   M1A        M1_A         A     M1 0.516 159.998395    9.880593
    ## 2      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1 example.xls Magellan Sheet 1         1     96     H     1   M2A        M2_A         A     M2 0.231  62.798726    6.192559
    ## 3      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1 example.xls Magellan Sheet 1         1     96     G     2   M1A        M1_A         A     M1 0.251  70.308342    6.329615
    ## 4      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1 example.xls Magellan Sheet 1         1     96     H     2   M2A        M2_A         A     M2 0.180  42.292176    5.570530
    ## 5      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1 example.xls Magellan Sheet 1         1     96     G     3   M1A        M1_A         A     M1 0.112   5.933014    1.879901
    ## 6      1  -1.38447 0.1070846  3.200827 622.9272        FALSE          1 example.xls Magellan Sheet 1         1     96     H     3   M2A        M2_A         A     M2 0.092   0.000000          NA

The `elisa.standard()` function extracts the standard points from the dataframe (converting the dose values encoded in the id column to numbers)

``` r
get_standard(df)
```

    ## # A tibble: 14 x 6
    ##           file   col   row        id    .dose value
    ##          <chr> <chr> <chr>     <chr>    <dbl> <dbl>
    ##  1 example.xls    11     A   STD1000 1000.000 2.110
    ##  2 example.xls    11     B    STD500  500.000 1.480
    ##  3 example.xls    11     C    STD250  250.000 0.728
    ##  4 example.xls    11     D    STD125  125.000 0.460
    ##  5 example.xls    11     E   STD62.5   62.500 0.259
    ##  6 example.xls    11     F  STD31.25   31.250 0.145
    ##  7 example.xls    11     G STD15.625   15.625 0.132
    ##  8 example.xls    12     A   STD1000 1000.000 2.166
    ##  9 example.xls    12     B    STD500  500.000 1.407
    ## 10 example.xls    12     C    STD250  250.000 0.760
    ## 11 example.xls    12     D    STD125  125.000 0.425
    ## 12 example.xls    12     E   STD62.5   62.500 0.230
    ## 13 example.xls    12     F  STD31.25   31.250 0.138
    ## 14 example.xls    12     G STD15.625   15.625 0.106

The `elisa.standard()` output can easily be integrated in `ggplot()` to render the regression curve (using `elisar::stat_4pl()` to draw the `drc::drm()` 4PL regression model).

``` r
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(scales)
df %>%
  get_standard() %>%
  rename(od = value) %>%
  ggplot(aes(x = .dose, y = log10(od))) +
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  geom_point(size = 3) +
  stat_4pl(color = "red", size = 1) +
  xlab("Concentration in pg/ml") +
  theme_bw()
```

![](README_files/figure-markdown_github/standard-1.png)

### Options for the regression

Some options of the `elisa.analyse()` function can be adjusted. Refer to the help page to list them (`?elisa.analyse`).
