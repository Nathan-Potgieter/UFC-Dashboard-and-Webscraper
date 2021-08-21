if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)
source()

events <- read_csv("data/UFC_Events.csv")
fights <- read_csv("data/UFC_Fights.csv")
fighters <- read_csv("data/UFC_Fighters.csv")
#load("data/fight")

m = c("SUB", "KO/TKO", "ALL")[3]
n = -10
log_axis = F

methods_plot <- function(filter = c("ALL", "KO/TKO", "SUB"),
                         n = 10,
                         log_axis = F) {

    events %>% filter(!is.na(Method)) %>%
        distinct(Date, Fight, Method = trimws(gsub("NA", "", paste(Method,Method_Detail)) )) %>%
        filter(case_when(
            m == "SUB" ~ grepl("SUB", Method),
            m == "KO/TKO" ~ grepl("KO|TKO", Method),
            m == "ALL" ~ Method == Method )) %>%
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


# Calculating and plotting fighter streaks

streaks <- events %>%
    select(Date, Fighter, Win_Loss) %>%
    filter(Date < first(Date)) %>%
    arrange(Date) %>%
    group_by(Fighter) %>%
    mutate(Streak = sequence(rle(Win_Loss)$lengths),
           Streak = ifelse(Win_Loss == "Loss", - Streak, Streak))


f <- as.vector(unique(events$Fighter))[1]

streak_plot <- function(f) {

    format_quarters <- function(x) {
        x <- as.yearqtr(x)
        year <- as.integer(x)
        quart <- as.integer(format(x, "%q"))

        paste(c("Q1","Q2","Q3","Q4")[quart],
              year)
    }

    cols <- c("Win" = "#3CB371", "Loss" = "red", "NC" = "lightblue")
    streaks %>%
        filter(Fighter == f) %>%
        ggplot(aes(x = Date, y = Streak, color = Win_Loss)) +
        geom_point(alpha = 0.9, size = 3.5) +
        geom_segment(aes(x=Date, xend=Date, y=0, yend=Streak)) +
        scale_color_manual(values = cols) +
        labs(title = "Fighter Win/Loss Streak") +
        scale_x_date(breaks = date_breaks("3 months"),
                     labels = format_quarters) +
        theme_bw() +
        theme(axis.text.x = element_text(face = "bold", angle=45, vjust = 0.5))
}

streak_plot("Tony Ferguson")


# Bar plot of fighter record.
fighters %>% select(Name, Wins, Losses)
    filter(Name == "Greg Hardy")

# Bar plot of fighter record.



# Attacks landed/absorbed.
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
        coord_flip(ylim = c(-ylim,ylim))  +
        theme(legend.position = "none",
              axis.text.y = element_text(face = "bold", size = 8))


}
fighter = "Khabib Nurmagomedov"
landed_absorbed_plot(fighter)






events %>%
    select(Date, Fight, Fighter, Kd, Str, Td, Win_Loss) %>%
    filter(Date<first(Date)) %>%
    group_by(Fight) %>%
    mutate(Kd_Abs = rev(Kd),
           Str_Abs = rev(Str),
           Td_Abs = rev(Td)) %>%
    ungroup() %>% #select(-Date, -Fight) %>%
    gather(Kd, Num_Kd,  -Date, -Fight, -Fighter,-Str, -Td, -Win_Loss, -Str_Abs, -Td_Abs) %>%
    gather(Str, Num_Str, -Date, -Fight, -Fighter, -Td, -Win_Loss, -Td_Abs, -Kd, -Num_Kd) %>%
    gather(Td, Num_Td, -Date, -Fight, -Fighter, -Win_Loss, -Kd, -Num_Kd, -Str, -Num_Str) %>%
    mutate(across(all_of(c("Kd", "Str", "Td")), ~if_else(grepl("_Abs", .), "Absorbed", "Landed")))




#Fight Plot
events %>% left_join(fighters,
                     by = c("Fighter" = "Name")) %>%
    filter(Date<first(Date),
           Weight_Class != "Super Heavyweight",
           Weight_Class != "Open Weight") %>%
    group_by(Weight_Class)  %>%
    summarise(Weight = median(Weight, na.rm = T),
              Strikes = mean(Str, na.rm = T),
              "Knock Downs" = mean(Kd, na.rm = T),
              "Take Downs" = mean(Td, na.rm = T),
              Submitions = mean(Sub, na.rm = T)) %>%
    arrange(desc(Weight))


fight_stats <- events %>% select(Date, Fight, Fighter, Weight_Class, Kd:Sub) %>%
    left_join(fighters %>% select(Name, Weight),
                     by = c("Fighter" = "Name")) %>%
    filter(Date<first(Date),
           !grepl("Super Heavyweight",.$Weight_Class) &
           grepl("Heavyweight", .$Weight_Class, fixed = T)
           )

fight_stats %>%
    ggplot(aes(x = sqrt(Str), fill = Weight_Class)) +
    geom_density(alpha = 0.24)




events %>%
    select(Date, Fighter, Win_Loss) %>%
    filter(Date < first(Date)) %>%
    arrange(Date) %>%
    group_by(Fighter) %>%
    filter(n() > 2,
           !is.na(Win_Loss)) %>%
    mutate(Streak = sequence(rle(Win_Loss)$lengths),
           Streak = ifelse(Win_Loss == "Loss", - Streak, Streak))



# fights Plots

pie_chart_data <- fights %>% filter(Round == "All",
                      Fighter == "Khabib Nurmagomedov",
                      Strike_Type == "Landed") %>%
    select(Fight_URL, Fighter, Strike_Type, Distance:Ground) %>%
    pivot_longer(Distance:Ground) %>%
    group_by(Fighter, Strike_Type, name) %>%
    summarise(Total_Strikes = sum(value) ) %>%
    group_by(Strike_Type) %>%
    mutate(Prop = Total_Strikes/sum(Total_Strikes),
           lab.ypos = cumsum(Prop) - 0.5*Prop)

pie_chart_data %>%
    ggplot(aes(x = name, y = Prop, fill = name)) +
    geom_bar(width = 1, stat = "identity", color = "black") +
    coord_polar("y", start = 0)+
    geom_text(aes(y = lab.ypos, label = round(Prop, 2)), color = "black")+
    theme_void()



# EDA
# stat = c("Sig_Strike_Acc", "Takedown_Acc")
# n = 10
#
# fights %>% group_by(Fighter) %>%
#     { if (stat[1] == "Sig_Strike_Acc") filter(n() > n)
#         else filter(sum(Takedowns, na.rm = T) > n)
#         } %>%
#     summarise(Accuracy = mean(Sig_Strike_Acc, na.rm = T)) %>%
#     arrange(desc(Accuracy))

plot_data <- fights %>%
    filter(Round == "All") %>% # , Fighter == "Cody McKenzie"
    select(Strike_Type, Head, Body, Leg) %>%
    pivot_longer(Head:Leg) %>%
    group_by(name, Strike_Type) %>%
    summarise(Total = sum(value, na.rm = T))

plot_data %>%
    ggplot(aes(x = name, y = Total, fill = Strike_Type)) +
    geom_col() +
    coord_flip()


fights %>%
    filter(Round == "All") %>%
    select(Fight_URL, Fighter, Strike_Type, Sig_Strikes, Head:Ground) %>%
    rename(Total = Sig_Strikes) %>%
    pivot_wider(id_cols = c(Fight_URL, Fighter),
                names_from = Strike_Type,
                values_from = c(Total:Ground),
                names_sep = "_") %>%
    pivot_longer(Total_Landed:Ground_Attempted,
                 names_to = "Strike_Type",
                 values_to = "Sig_Strikes")

function() {

    avl <- "Landed"
    f <- unique(fights$Fighter)[sample(1:length(unique(fights$Fighter)), 1)]

    plot_data <- fights %>%
        filter(Round == "All", Strike_Type == avl) %>%
        select(Fight_URL, Fighter, Strike_Type, Sig_Strikes, Head:Ground) %>%
        rename(Total = Sig_Strikes) %>%
        pivot_longer(Total:Ground,
                     names_to = "Strike_Location",
                     values_to = "Sig_Strikes") %>%
        group_by(Fight_URL, Strike_Type, Strike_Location) %>%
        mutate(Sig_Strikes_Sus = rev(Sig_Strikes)) %>%
        filter(Fighter == f) %>%
        rename(Thrown = Sig_Strikes, Sustained = Sig_Strikes_Sus) %>%
        pivot_longer(Thrown:Sustained,
                     names_to = "Thrown_vs_Sustained",
                     values_to = "Sig_Strikes") %>%
        group_by(Strike_Type, Strike_Location, Thrown_vs_Sustained) %>%
        summarise(Sig_Strikes = sum(Sig_Strikes))

    cols <- c("Thrown" = "#3CB371", "Sustained" = "red")
    plot_data %>%
        mutate(Strike_Location = factor(Strike_Location,
                                        levels = rev(c("Head", "Body", "Leg", "Distance",
                                                       "Clinch", "Ground", "Total")))) %>%
        ggplot() +
        geom_col(aes(x = Strike_Location, y = Sig_Strikes, fill = Thrown_vs_Sustained),
                 position = "dodge", color = "black") +
        coord_flip() +
        theme_bw() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        labs(title = glue::glue("Total Significant Strikes {avl} - {f}"), x = "", y = "") +
        scale_fill_manual(values = cols)
}




# f <- unique(fights$Fighter)[sample(1:length(unique(fights$Fighter)), 1)]
# avl <- "Attempted"
# tvs <- "Sustained"
# strike_location_plot(f,avl,tvs)
strike_location_plot <- function(f, avl, tvs) {
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
        filter(Fighter == f) %>%
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
        mutate(Target = factor(Target, levels = rev(c("Position", "Target"))),
               Position = factor(Position, levels = c("Head", "Body", "Leg", "Distance", "Clinch", "Ground"))
        ) %>% ggplot(aes(x = Target, y = Sig_Strikes, fill = Position)) +
        geom_col(color = "black", position = "stack") + # "dodge"
        geom_text(aes(label = Sig_Strikes), position = position_stack(vjust = .5)) +
        scale_y_continuous( breaks = waiver(), n.breaks = 6) +
        scale_fill_manual(values = cols) +
        theme_bw() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        labs(title = glue::glue("Significant Strikes {tvs}- {f}"),
             subtitle = glue::glue("Total {avl} by Target and Fight Position"), x = "", y = "") +
        guides(fill = guide_legend(direction = "horizontal", byrow = T)) +
        coord_flip()

}



# Accuracy

# Defense