README
================

# UFC Scraper and Dashboard

This project aims to:

1.  Scrape all available data on UFC fights and fighters:

    1.  Fight and fighter data comes form www.ufcstats.com.
    2.  Betting odds data comes from \_????\_

2.  Display and visualize this data within an R shiny dashboard.

Please note that is work is not intended for commercial gain. It is a
task that was set to myself as a means to learn web-scraping and shiny
dash-boarding skills. I decided to go with UFC data because I am a fan
of MMA and the UFC and I was not satisfied by their official data
visualization offerings.

I am new to both web-scraping and dash-boarding, so if anyone happens
across any errors or areas in need of improvement, please let me know at
<nathan.potgieter56@gmail.com>.

The userdefined functions used in this project are located in the
functions.R file.

## Web-Scraper

The following packages are used in the web-scraping process:

``` r
if( !require("pacman")) install.packages("pacman")
pacman::p_load(rvest, tidyverse, anytime)
```

Here are the data-sets created:

-   Fighter Data

``` r
load("data/UFC_Fighters.rda")
as_tibble(df_fighters_full)
```

    ## # A tibble: 3,673 x 20
    ##    First_Name Last_Name Nickname Height Weight Reach Stance  Wins Losses Other
    ##    <chr>      <chr>     <chr>     <dbl> <chr>  <dbl> <chr>  <int>  <int> <int>
    ##  1 Tom        Aaron     ""        NA    155     NA   ""         5      3     0
    ##  2 Danny      Abbadi    "The As~   1.80 155     NA   "Orth~     4      6     0
    ##  3 David      Abbott    "Tank"     1.83 265     NA   "Swit~    10     15     0
    ##  4 Shamil     Abdurakh~ "Abrek"    1.91 235     19.3 "Orth~    20      5     0
    ##  5 Hiroyuki   Abe       "Abe An~   1.68 145     NA   "Orth~     8     15     3
    ##  6 Daichi     Abe       ""         1.80 170     18.0 "Orth~     6      2     0
    ##  7 Papy       Abedi     "Makamb~   1.80 185     NA   "Sout~    10      4     0
    ##  8 Ricardo    Abreu     "Dement~   1.80 185     NA   "Orth~     5      1     0
    ##  9 Klidson    Abreu     "White ~   1.83 205     18.8 "Orth~    15      4     0
    ## 10 Daniel     Acacio    ""         1.73 180     NA   "Orth~    30     18     0
    ## # ... with 3,663 more rows, and 10 more variables: url <chr>, DOB <date>,
    ## #   SLpM <dbl>, Str_Acc <dbl>, SApM <dbl>, Str_Def <dbl>, TD_Avg <dbl>,
    ## #   TD_Acc <dbl>, TD_Def <dbl>, Sub_Avg <dbl>

-   Fight Data

### Fetching Fighter Data

The ‘scrape\_fighters\_raw’ function scrapes all the fighter data from a
given url.

``` r
source("functions.R")
fighters_raw <- scrape_fighters_raw("http://www.ufcstats.com/statistics/fighters?char=a&page=all")
save("fighters_raw", file = "data/fighters_raw.rda")
```

Only fighters with last name beginning with A are provided under this
url. Therefore, we have to loop this process over each letter in the
alphabet to get the complete dataset.

``` r
# This creates a list of 26 urls, 
# once for each letter of the alphabet
url <- "http://www.ufcstats.com/statistics/fighters?char=a&page=all"
lin <- gsub("a&page=all", "", url)
links <- letters %>% map( ~ paste0(lin, ., "&page=all"))

# This scrapes the figher data from each link and 
# reduces each table into a single tibble
df_fighters_raw <- links %>%
    map(scrape_fighters_raw) %>%
    reduce(union) %>% as_tibble()
# save(df_fighters_raw, file = "df_fighters_raw.rda")
```

### Cleaning Fighter Data

``` r
# Cleaning df_fighters_raw
load("df_fighters_raw.rda")

df_fighters <-
df_fighters_raw %>%
mutate( 
    across(all_of(c("Height","Weight", "Reach")), ~gsub("[[:punct:]]", "", .)),
    across(where(is.character), ~gsub("[[:space:]]", "", .)),
    across(where(is.character), ~gsub("lbs", "", .)),
    across(where(is.character), ~ gsub("([a-z])([A-Z])", "\\1 \\2", .)),
    across(where(is.character), trimws),
    ht_ft = as.numeric(substring(Height, 1, 1)),
    ht_inch = as.numeric(substring(Height, 2)),
    Height = (0.3048*ht_ft) + (0.0254*ht_inch),
    Reach = 0.0254*as.numeric(Reach)
    ) %>% select(-ht_ft, - ht_inch)
```

## Fetching Additional Data for Each Fighter

By now, if you have been running this code localy, you may have noticed
the inclusion of a url column in **df\_fighters** and
**df\_fighters\_raw**. This url’s was scraped in tandem with the other
fighter data. These url’s allow us to view additional career stats for
each fighter, see for example :
<http://www.ufcstats.com/fighter-details/9e8f6c728eb01124>.

In this next step I loop another scraping function (X in functions.R)
over each of these url’s and then join the resulting output to
**df\_fighters.**

The additional features include:

-   DOB - Date of birth.
-   SLpm - Significant Strikes Landed per Minute.
-   SApM - Significant Striking Accuracy.
-   SApM - Significant Strikes Absorbed per Minute.
-   Str\_Def - Significant Strike Defence (the % of opponents strikes
    that did not land).
-   TD\_Avg - Average Takedowns Landed per 15 minutes.
-   TD\_Acc - Takedown Accuracy.
-   TD\_Def - Takedown Defense (the % of opponents TD attempts that did
    not land).
-   Sub\_Avg - Average Submissions Attempted per 15 minutes.

``` r
source("functions.R")

# Warning this step takes +-30 minuets.
fighter_stats_raw <- df_fighters$url %>% 
    map(scrape_fighter_stats_raw)
# save(fighter_stats_raw, file = "data/df_fighter_stats_raw.rda")
```

Now to format the new data

``` r
load("data/df_fighter_stats_raw.rda")

fighter_stats <-
fighter_stats_raw %>% reduce(union) %>%
    mutate(DOB = as.Date(anytime(DOB)),
           across(contains("Acc")|contains("Def"), ~ gsub("%|\\.", "", .)),
           across(contains("Acc")|contains("Def"), ~ as.numeric(.)/100),
           across(-c("url", "DOB"), as.numeric))
```

Joining **fighter\_stats** with **df\_fighters**

``` r
df_fighters_full <-
df_fighters %>%
    inner_join( fighter_stats,
               by = c("url" = "url"))
# save(df_fighters_full, file = "data/UFC_Fighters.rda")
```

### Fetching Fight Data

The code below scrapes the Name/date, Location and URL for every UFC
event that has occurred to date as well as for the most recent upcoming
event. All this information is located
[here](http://www.ufcstats.com/statistics/events/completed?page=all).

``` r
# Scraping links to fight event pages
link <- "http://www.ufcstats.com/statistics/events/completed?page=all"
site <- read_html(link) # All the HTML and CSS code from link.

fight_events_dirty <-  site %>%
    html_table(fill = T) %>%
    .[[1]] %>% .[-1,] %>%
    as_tibble() %>%
    mutate(across(where(is.character), noquote),
           across(where(is.character), ~gsub("[[:punct:]]", "", .)),
           across(where(is.character), ~gsub("[[:space:]]", "", .))) %>%
    mutate( url = site %>% # feed `main.page` to the next step
            html_nodes(".b-statistics__table-content") %>% # get the CSS nodes
            html_nodes("a") %>%
            html_attr("href")
    )
# save(fight_events_dirty, file = "data/fight_events_dirty.rda")
```

Cleaning this data:

``` r
load("data/fight_events_dirty.rda")
```
