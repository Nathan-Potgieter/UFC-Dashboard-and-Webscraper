# This function extracts all the raw fighter info from URLs like:
# "http://www.ufcstats.com/statistics/fighters?char=a&page=all"
scrape_fighters_raw <- function(url) {

    fighters <- read_html(url)
    df_fighters <- fighters %>%
        html_table(fill = T) %>% .[[1]] %>%
        as_tibble() %>% select(-Belt) %>%
        setNames(c("First_Name", "Last_Name", "Nickname",
                   "Height", "Weight", "Reach", "Stance",
                   "Wins", "Losses", "Other")) %>% .[-1,] %>%
        mutate(Fighter_URL = fighters %>%
                   html_nodes("a") %>%
                   html_attr("href") %>%
                   map(~ .[grepl("http://www.ufcstats.com/fighter-", .)]) %>%
                   reduce(union) )

    return(df_fighters)

}

# This function extracts the raw fighter career stats from URLs like:
# url = "http://www.ufcstats.com/statistics/fighters?char=a&page=all"
scrape_fighter_stats_raw <- function(url) {

    site <- read_html(url)

    stats <- site %>% html_nodes(".b-list__box-list-item") %>%
        html_text() %>% gsub("[[:space:]]", "", .) %>%
        .[grepl("DOB|SLpM|Str|SApM|TDAvg|TDAcc|TDDef|Sub", x = .)]

    career_stats <-
        tibble(Fighter_URL = url, DOB = stats[1], SLpM = stats[2],
               Str_Acc = stats[3], SApM = stats[4], Str_Def = stats[5],
               TD_Avg = stats[6], TD_Acc = stats[7], TD_Def = stats[8],
               Sub_Avg =stats[9]) %>%
        mutate(across(-Fighter_URL, ~ gsub(pattern = "DOB:|SLpM:|Str.|Sub.|Avg.|Acc.|Def|SApM:|TDAvg.|TD|TDDef:|Sub:|:", "", .) ),
               across(-Fighter_URL, str_trim) )
    return(career_stats)

}

# This function extracts the raw event data from URLs like:
# link <- "http://ufcstats.com/event-details/b0550072e5f0afa7"
scrape_events <- function(link) {

    site <- read_html(link)

    fight_event <-  site %>%
        html_table(fill = T) %>% .[[1]] %>%
        as_tibble(.name_repair = "minimal") %>%
        set_names(c("Win_Loss", "Fighter", "Kd", "Str", "Td", "Sub",
                    "Weight_Class", "Method", "Round", "Time")) %>%
        mutate(Event_URL = link,
               Fight_URL = site %>% html_nodes("tbody tr") %>%
                   html_attr("data-link"))

    return(fight_event)
}
scrape_events("http://ufcstats.com/event-details/b0550072e5f0afa7")


# This function extracts the raw fight data from URLs like:
# link <- "http://www.ufcstats.com/fight-details/6cd44e1b2d093ea4"
scrape_fight <- function(link) {

    site <- read_html(link)

    fight_stats <-  site %>%
        html_table() %>%
        set_names("Total Strikes", "Total Strikes: Per Round",
                  "Significant Strikes", "Significant Strikes: Per Round")

    union(fight_stats$`Total Strikes`,
          fight_stats$`Total Strikes: Per Round` %>%
              set_names(names(fight_stats$`Total Strikes`))) %>%
        mutate(Round = c("All", paste("Round",1:(n()-1)))) %>%
        select(Round, Fighter, `Sig. str.`:Ctrl) %>%
        left_join(
            union(fight_stats$`Significant Strikes`,
                  fight_stats$`Significant Strikes: Per Round`) %>%
                mutate(Round = c("All", paste("Round",1:(n()-1)))) %>%
                select(Round, Fighter, Head:Ground)
        )



}
# scrape_fight("http://www.ufcstats.com/fight-details/6cd44e1b2d093ea4")
