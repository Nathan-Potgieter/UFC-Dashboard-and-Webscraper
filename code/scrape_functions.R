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

# Scraping links to fight event pages
scrape_event_list <- function() {

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
}

clean_event_list <- function(event_list_raw) {

    month_names <- "January|February|March|April|May|June|July|August|September|October|November|December"

    event_list <-
        event_list_raw %>%
        set_names(c("Event", "Location", "Event_URL")) %>%
        mutate(Month = str_extract(Event, month_names),
               Event = str_remove(Event, month_names)) %>%
        separate(Event, into = c("Event", "Date"), sep = -6) %>%
        separate(Date, into = c("Day", "Year"), sep = -4) %>%
        unite(all_of(c("Month", "Day", "Year")), col = "Date", sep = "-") %>%
        mutate(Date = as.Date(anytime(Date)),
               across(all_of(c("Event", "Location")), ~ gsub("([a-z])([A-Z])", "\\1 \\2", .)),
               Event = sub("UFCF", "UFC F", Event),
               Event = gsub('(\\d|\\d\\d|\\d\\d\\d)', ' \\1 ', Event),
               Event = gsub("vs ", " vs ", Event),
               Event = trimws(Event, which = "both") ) %>%
        select(Date, Event, Location, Event_URL)

    return(event_list)

}


# This function extracts the raw event data from URLs like:
# link <- "http://ufcstats.com/event-details/b0550072e5f0afa7"
scrape_events <- function(link, progress = F) {

    if (progress == T) {
        pb$tick()
    }

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
# scrape_events("http://ufcstats.com/event-details/b0550072e5f0afa7")

clean_events <- function(events_raw) {

    events_raw %>% mutate(
        Win_Loss = if_else(is.na(Win_Loss), "TBC   TBC",
                           if_else(grepl("win", Win_Loss), "Win   Loss", "NC   NC")),
        Fight = Fighter,
        .before = Win_Loss
    ) %>%
        separate_rows(c("Win_Loss", "Fighter", "Kd", "Str", "Td", "Sub"), sep = "\\s{2,}") %>%
        separate(
            Method,
            into = c("Method", "Method_Detail"),
            sep = '\\s+',
            fill = "right",
            extra = "merge"
        ) %>%
        mutate(
            Fight = sub('\\n', " vs", Fight),
            Fight = gsub('\\s+', " ", Fight),
            across(all_of(c("Kd", "Str", "Td", "Str", "Td", "Sub")), as.integer)
        )

}



# This function extracts the raw fight data from URLs like:
# link <- "http://www.ufcstats.com/fight-details/6cd44e1b2d093ea4"
# link <- "http://ufcstats.com/fight-details/c6b040021b4ceb0f"
# link <- "http://ufcstats.com/fight-details/7d4c160dab6eae73"
scrape_fight <- function(link, progress = F) {

    if (progress == T) {
        pb$tick()
    }

    site <- read_html(link)

    fight_stats <-  site %>% html_table() %>%
        set_names(
            "Total Strikes",
            "Total Strikes: Per Round",
            "Significant Strikes",
            "Significant Strikes: Per Round"
        )

    fight <-
        bind_rows(
            fight_stats$`Total Strikes`,
            fight_stats$`Total Strikes: Per Round` %>%
                set_names(names(fight_stats$`Total Strikes`))
        ) %>%
        mutate(Round = c("All", paste("Round", 1:(n() - 1)))) %>%
        select(Round, Fighter, `Sig. str.`:Ctrl) %>%
        full_join(
            bind_rows(
                fight_stats$`Significant Strikes`,
                fight_stats$`Significant Strikes: Per Round`
            ) %>%
                mutate(Round = c("All", paste("Round", 1:(n() - 1)))) %>%
                select(Round, Fighter, Head:Ground), # Error
            by = c("Round", "Fighter")
        ) %>% mutate(Fight_URL = link)

    return(fight)

}
# scrape_fight("http://www.ufcstats.com/fight-details/6cd44e1b2d093ea4")


clean_fight <- function(fight_data_raw) {

    fight_data <- fight_data_raw %>%
        separate_rows(-c(Round, Fight_URL), sep = "\\s{2,}") %>%
        set_names(c("Round", "Fighter", "Sig_Strikes", "Sig_Strike_Acc", "Total_Strikes",
                    "Takedowns", "Takedown_Acc", "Submitions_Att", "Reversals", "Control_Time",
                    "Head", "Body", "Leg", "Distance", "Clinch", "Ground", "Fight_URL")) %>%
        separate_rows(-c(Round, Fighter, Sig_Strike_Acc, Takedown_Acc, Submitions_Att,
                         Reversals, Control_Time), sep = "of") %>%
        mutate(Strike_Type = rep(c("Landed", "Attempted"), n()/2), .after = Fighter) %>%
        mutate(across(everything(), trimws),
               across(c(Sig_Strikes, Total_Strikes, Takedowns, Submitions_Att, Reversals,
                        Clinch, Head:Ground), as.integer),
               across(contains("Acc"), ~as.numeric(gsub("%|-", "", .))/100)
               # Control_Time = as.period(ms(Control_Time), "minuets")
               )

    return(fight_data)

}


