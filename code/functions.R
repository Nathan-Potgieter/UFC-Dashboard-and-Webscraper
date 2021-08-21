

# Method barplot function
methods_plot <- function(filter = c("ALL", "KO/TKO", "SUB"),
                         n = 10,
                         log_axis = F) {

    events %>% filter(!is.na(Method)) %>%
        distinct(Date, Fight, Method = trimws(gsub("NA", "", paste(Method,Method_Detail)) )) %>%
        filter(case_when(
            filter == "SUB" ~ grepl("SUB", Method),
            filter == "KO/TKO" ~ grepl("KO|TKO", Method),
            filter == "ALL" ~ Method == Method )) %>%
        {if(n < 0) filter(., !grepl("Overturned", Method), Method != "SUB")
            else filter(., Method == Method)} %>%
        mutate(Method = fct_lump_n( fct_rev(fct_infreq(Method)), n) ) %>%
        {if(n < 0) filter(., Method != "Other")
            else filter(., Method == Method)} %>%
        ggplot(aes(x = Method)) +
        geom_bar( aes(fill = Method ), show.legend = F) +
        {if(log_axis == T) scale_y_log10()} +
        coord_flip() +
        theme_bw()

}

# Fight Win/Loss streak plot
streak_plot <- function(fighter, from_date, to_date) {

    format_quarters <- function(x) {
        x <- as.yearqtr(x)
        year <- as.integer(x)
        quart <- as.integer(format(x, "%q"))

        paste(c("Q1","Q2","Q3","Q4")[quart],
              year)
    }

    streaks <- events %>%
        select(Date, Fighter, Win_Loss) %>%
        filter(Date < first(Date) & Date >= from_date & Date <= to_date) %>%
        arrange(Date) %>%
        group_by(Fighter) %>%
        filter(n() > 2,
               !is.na(Win_Loss)) %>%
        mutate(Streak = sequence(rle(Win_Loss)$lengths),
               Streak = ifelse(Win_Loss == "Loss", - Streak, Streak))

    cols <- c("Win" = "#3CB371", "Loss" = "red", "NC" = "lightblue")
    streaks %>%
        filter(Fighter == fighter) %>%
        ggplot(aes(x = Date, y = Streak, color = Win_Loss)) +
        geom_point(alpha = 0.9, size = 3.5) +
        geom_segment(aes(x=Date, xend=Date, y=0, yend=Streak)) +
        scale_color_manual(values = cols) +
        labs(title = "Fighter Win/Loss Streak") +
        scale_x_date(breaks = date_breaks("3 months"),
                     labels = format_quarters) +
        theme_bw() + labs(x = "") +
        theme(legend.position = "left",
              axis.text.x = element_text(size = 11, angle=45, vjust = 0.5))
}

# Strikes, TD, Kd landed/absorbed plot
landed_absorbed_plot <- function(fighter) {

    plot_data <-
        events %>%
        select(Date, Fight, Fighter, Kd, Str, Td, Win_Loss) %>%
        filter(Date<first(Date)) %>%
        group_by(Fight) %>%
        mutate(Kd_Abs = rev(Kd),
               Str_Abs = rev(Str),
               Td_Abs = rev(Td)) %>%
        ungroup() %>% #select(-Date, -Fight) %>%
        gather(Method, Number,  -Date, -Fight, -Fighter, -Win_Loss) %>%
        mutate(Direction = if_else(grepl("_Abs", Method), "Absorbed", "Landed")) %>%
        group_by(Method) %>%
        mutate(Number = scale(Number)) %>%
        filter(Fighter == fighter) %>% # Change to filter to improve performance
        summarise(Fighter = unique(Fighter),
                  Direction = unique(Direction),
                  Number = mean(Number)) %>%
        mutate(method = case_when(
            grepl("Kd", Method) ~ "Kd",
            grepl("Str", Method) ~ "Str",
            grepl("Td", Method) ~ "Td"),
            Method = case_when(
                Method == "Kd"  ~ "Knockdowns Landed",
                Method == "Kd_Abs" ~ "Knockdowns Absorbed",
                Method == "Str" ~ "Strikes Landed",
                Method =="Str_Abs" ~ "Strikes Absorbed",
                Method == "Td" ~ "Takedowns Landed",
                Method == "Td_Abs" ~ "Takedowns Absorbed",
            )
        )


    ylim <- max(abs(plot_data$Number))
    plot_data %>%
        ggplot(aes(x = Method, y = Number, fill = Direction)) +
        geom_col(color = "black") + theme_bw() +
        labs(title = "Takedowns, Strikes and Knock-downs",
             subtitle = "Scaled Number Landed vs Absorbed", x = "", y = "Scaled Value") +
        scale_fill_manual(values = c("Landed" = "#3CB371", "Absorbed" = "red")) +
        coord_flip(ylim = c(-ylim,ylim))  +
        theme(legend.position = "none",
              axis.text.y = element_text(size = 11))


}

strike_location_plot <- function(fighter, avl, tvs) {

    plot_data <- fights %>%
        filter(Round == "All", Strike_Type == avl) %>%
        select(Fight_URL, Fighter, Strike_Type, Head:Ground) %>%
        rename(Target_Head = Head, Target_Body = Body,
               Target_Leg = Leg, Position_Distance = Distance,
               Position_Clinch = Clinch, Position_Ground = Ground) %>%
        pivot_longer(Target_Head:Position_Ground,
                     names_to = c("Target", "Position"),
                     values_to = "Sig_Strikes",
                     names_sep = "_") %>%
        group_by(Fight_URL, Strike_Type, Target, Position) %>%
        mutate(Sig_Strikes_Sus = rev(Sig_Strikes)) %>%
        filter(Fighter == fighter) %>%
        rename(Thrown = Sig_Strikes, Sustained = Sig_Strikes_Sus) %>%
        pivot_longer(Thrown:Sustained,
                     names_to = "Thrown_vs_Sustained",
                     values_to = "Sig_Strikes") %>%
        group_by(Fighter, Strike_Type, Target, Position, Thrown_vs_Sustained) %>%
        summarise(Sig_Strikes = sum(Sig_Strikes)) %>%
        ungroup()

    cols <- c("Head" = "#FC4E07", "Body" = "#ab5500", "Leg" = "#E7B800",
              "Distance" = "#6699CC", "Clinch" = "#4DAC26", "Ground" = "#80CDC1")
    plot_data %>%
        filter(Thrown_vs_Sustained == tvs) %>%
        mutate(Target = factor(Target, levels = rev(c("Target", "Position"))),
               Position = factor(Position, levels = c("Leg", "Body", "Head", "Ground", "Clinch", "Distance"))
        ) %>% ggplot(aes(x = Target, y = Sig_Strikes, fill = Position)) +
        geom_col(color = "black", position = "stack") + # "dodge"
        geom_text(aes(label = Sig_Strikes), position = position_stack(vjust = .5)) +
        scale_y_continuous( breaks = waiver(), n.breaks = 6) +
        scale_fill_manual(values = cols) +
        theme_bw() +
        theme(legend.position = "top", legend.title = element_blank()) +
        labs(title = glue::glue("Significant Strikes {tvs}- {fighter}"),
             subtitle = glue::glue("Total {avl} by Target and Fight Position"), x = "", y = "") +
        guides(fill = guide_legend(direction = "horizontal", byrow = T)) +
        coord_flip()

}
