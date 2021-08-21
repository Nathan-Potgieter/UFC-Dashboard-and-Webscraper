# clean fights v2
x <- fight_data_raw %>%
    separate_rows(-c(Round, Fight_URL), sep = "\\s{2,}") %>%
    set_names(c("Round", "Fighter", "Total_Sig_Strikes", "Sig_Strike_Acc", "Total_Strikes",
                "Takedowns", "Takedown_Acc", "Submitions", "Reversals", "Control_Time",
                "Head", "Body", "Leg", "Distance", "Clinch", "Ground", "Fight_URL")) %>%
    separate_rows(-c(Round, Fighter, Sig_Strike_Acc, Takedown_Acc, Submitions,
                     Reversals, Control_Time), sep = "of") %>%
    mutate(Landed_vs_Attempted = rep(c("Landed", "Attempted"), n()/2), .after = Fighter) %>%
    mutate(Submitions = ifelse(Landed_vs_Attempted == "Attempted", Submitions, NA),
           Reversals = ifelse(Landed_vs_Attempted == "Landed", Reversals, NA))

x %>% pivot_longer(c(Head:Ground),
                   names_to = "Sig_Strike_Location",
                   values_to = "Sig_Strikes") %>% View()


    pivot_longer(contains("Acc"),
                   names_to = "Strike_Accuracy_Type",
                   values_to = "Accuracy") %>% View()



mutate(across(everything(), trimws),
       across(c(Sig_Strikes, Total_Strikes, Takedowns, Submitions_Att, Reversals,
                Clinch, Head:Ground), as.integer),
       across(contains("Acc"), ~as.numeric(gsub("%|-", "", .))/100)
       # Control_Time = as.period(ms(Control_Time), "minuets")
)

