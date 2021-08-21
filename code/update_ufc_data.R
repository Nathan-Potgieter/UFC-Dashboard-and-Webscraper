rm(list = ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, dplyr, tidyr, progress, anytime)
source("code/scrape_functions.R")

# Scrape list of all UFC events
event_list_raw_new <- scrape_event_list()
event_list_new <- event_list_raw_new %>% clean_event_list()
# event_list_raw <- event_list_raw[1:10,]

# save(event_list_raw, file = "raw webdata/event_list_raw.rda")
# load("raw webdata/event_list_raw.rda")

# Loading existing list of events
load("raw webdata/event_list_raw.rda")
event_list <- event_list_raw %>% clean_event_list() %>% filter(Date < first(Date))

event_update_list <- anti_join(event_list_new, event_list)
event_list_raw <- event_list_raw_new

# save(event_list_raw, file = "raw webdata/event_list_raw.rda")

# Scrape UFC event Data
load("raw webdata/events_raw.rda")
pb <- progress_bar$new(total = nrow(event_update_list),
                       format = ":elapsed [:bar] :percent ETA :eta")
events_raw_new <- event_update_list$Event_URL %>% map(scrape_events, progress = T)
events_raw_new <- events_raw_new %>% reduce(bind_rows)

events_raw <- full_join(events_raw_new, events_raw)

save(events_raw, file = "raw webdata/events_raw.rda")
# load("raw webdata/events_raw.rda")

events <- clean_events(events_raw)

# Scrape fight Data from UFC events
load("raw webdata/fight_data_raw.rda")
urls <- anti_join(events, fight_data_raw,
                  by = c("Fight_URL" = "Fight_URL")) %>%
    filter(Win_Loss != "TBC") %>% .$Fight_URL %>% unique()


#urls <- urls[1:500]
pb <- progress_bar$new(total = length(urls),
                       format = ":elapsed [:bar] :percent ETA :eta")
fight_data_raw_new <- urls %>% map(scrape_fight, progress = T)
fight_data_raw_new <- fight_data_raw_new %>% reduce(bind_rows)

fight_data_raw <- full_join(fight_data_raw_new, fight_data_raw) #%>% distinct()
save(fight_data_raw, file = "raw webdata/fight_data_raw.rda")

# load("raw webdata/fight_data_raw.rda")

# STill need to add a loop to update fighter data for fighter who have fought reecnetly
##############################################################################



fight_data <- fight_data_raw %>% clean_fight()

load("raw webdata/event_list_raw.rda")
load("raw webdata/events_raw.rda")
load("raw webdata/fight_data_raw.rda")

UFC_Events <- left_join(clean_event_list(event_list_raw), clean_events(events_raw))
UFC_Fights <- clean_fight(fight_data_raw)
write_csv(UFC_Events, "data/UFC_Events.csv")
write_csv(UFC_Fights, "data/UFC_Fights.csv")
