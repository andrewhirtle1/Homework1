Homework1
================
Andrew Hirtle

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(nycflights13)
library(ggplot2)
```

### 1

``` r
flights %>%
  filter(is.na(dep_time))
```

    ## # A tibble: 8,255 × 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1       NA           1630        NA       NA           1815
    ##  2  2013     1     1       NA           1935        NA       NA           2240
    ##  3  2013     1     1       NA           1500        NA       NA           1825
    ##  4  2013     1     1       NA            600        NA       NA            901
    ##  5  2013     1     2       NA           1540        NA       NA           1747
    ##  6  2013     1     2       NA           1620        NA       NA           1746
    ##  7  2013     1     2       NA           1355        NA       NA           1459
    ##  8  2013     1     2       NA           1420        NA       NA           1644
    ##  9  2013     1     2       NA           1321        NA       NA           1536
    ## 10  2013     1     2       NA           1545        NA       NA           1910
    ## # … with 8,245 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### The other variables missing are departure delay, arrival time, arrival delay, and air time. These rows might represent cancelled or severly delayed flights.

### 2

``` r
flights %>%
  filter(!is.na(dep_time)) %>%
  mutate(dep_time = dep_time%/%100*60 + dep_time%%100) %>%
  mutate(sched_dep_time = sched_dep_time%/%100*60 + sched_dep_time%%100)
```

    ## # A tibble: 328,521 × 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <dbl>          <dbl>     <dbl>    <int>          <int>
    ##  1  2013     1     1      317            315         2      830            819
    ##  2  2013     1     1      333            329         4      850            830
    ##  3  2013     1     1      342            340         2      923            850
    ##  4  2013     1     1      344            345        -1     1004           1022
    ##  5  2013     1     1      354            360        -6      812            837
    ##  6  2013     1     1      354            358        -4      740            728
    ##  7  2013     1     1      355            360        -5      913            854
    ##  8  2013     1     1      357            360        -3      709            723
    ##  9  2013     1     1      357            360        -3      838            846
    ## 10  2013     1     1      358            360        -2      753            745
    ## # … with 328,511 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### 3

``` r
Cancelled <-flights%>%
  filter(is.na(dep_time)) %>%
  group_by(month,day)%>%
  summarise(cancelled=n()) 
```

    ## `summarise()` has grouped output by 'month'. You can override using the `.groups` argument.

``` r
Avg_Delay <- flights %>%
  filter(!is.na(arr_delay))%>%
  group_by(month,day)%>%
  summarise(avg_delay = mean(arr_delay)) 
```

    ## `summarise()` has grouped output by 'month'. You can override using the `.groups` argument.

``` r
final <- inner_join(Cancelled, Avg_Delay)
```

    ## Joining, by = c("month", "day")

``` r
ggplot(final, (aes(x=avg_delay, y=cancelled))) +
  geom_point()
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### The two values seem to be positively correlated. This would make sense that days where there are more delays there are also more cancellations because the two are related. Often a flight will be delayed alot before it is cancelled completely.
