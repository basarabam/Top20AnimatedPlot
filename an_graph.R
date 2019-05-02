#Loading packages
library(tidyverse)
library(lubridate)
library(countrycode)
library(gganimate)
library(ggflags)
library(tidyr)
library(viridis)

#Reading data
data_1 <- read_csv("results.csv")

#DATA PROCESSING
#-------------------------------------------------------
#Gathering scores
data <- data_1 %>%
        gather(score_playgr, score, -c(1:3, 6:9)) %>%
        gather(team_playgr, team, -c(1, 4:9))

#Cleaning gatherd values for removing duplicates       
data$team_playgr <- str_sub(data$team_playgr, 1, 4)
data$score_playgr <-str_sub(data$score_playgr, 1, 4)        

# Removing duplicates and adding year and month columns
data <- data %>%
        filter(team_playgr == score_playgr) %>%
        mutate(Year = year(date),
               Month = month(date)) %>%
        select(-score_playgr) %>%
        mutate_if(is.character, as.factor)

#Unique Years 
years_all <- unique(data$Year)

#Unique Countries
countries_unique <- data.frame(unique(data$team), stringsAsFactors = FALSE)
colnames(countries_unique) <- "team"
#Adding countrycode to countries, there is going to be a warning
#function country code cant find all countries and teritories
countries_unique$ccode <- tolower(countrycode(countries_unique$team, "country.name", "iso2c"))

#To add missing countrie codes, we need to add it by hand
missing_codes <- data.frame(team = c("England", "Scotland", "Wales",
                                     "Northern Ireland", "Yugoslavia",
                                     "Czechoslovakia"), 
                            ccode = c("gb-eng", "gb-sct", "gb-wls", 
                                      "gb-nir", "yu", "cz"), stringsAsFactors = FALSE)

#Adding missing codes(there must be a batter solution!)
countries_unique <- countries_unique %>% 
        left_join(missing_codes, by = c("team" = "team")) %>%
        mutate(ccode.x = replace_na(ccode.x, ""),
               ccode.y = replace_na(ccode.y, ""))%>%
        unite("ccode", c(ccode.x, ccode.y), sep = "") %>%
        mutate_if(is.character, as.factor)
        
# Creating data frame with each country for each year, to avoid animation jumps
all_y_c <- data.frame(expand.grid(Year = years_all, team = countries_unique$team)) %>%
        arrange(Year, team)

# Transforming dataset for ploting
data_test <- data %>%
        group_by(Year, team) %>%
        summarize(sum_score_y = sum(score)) %>%
        right_join(all_y_c, by = c("Year" = "Year", "team" = "team")) %>%
        mutate_at(c("sum_score_y"), ~replace(.,is.na(.),0)) %>%
        group_by(team, Year) %>%
        summarise(sum_t_goals = sum(sum_score_y)) %>%
        mutate(cum_y_goals = cumsum(sum_t_goals)) %>%
        #removing rows before first game
        filter(cum_y_goals != 0)

# Ranking function by year and adding counry code
ranking_f_y<- function(n){
        data_g <- data_test %>% 
                group_by(Year) %>%
                mutate(rank = min_rank(-cum_y_goals) * 1) %>%
                ungroup() %>%
                arrange(Year) %>%
                filter(rank <= n) %>%
                left_join(countries_unique, by = c("team" = "team")) %>%
                mutate(ccode = as.character(ccode),
                       team = as.character(team))
        
        data_g
}

#Ranking and coding countries
data_graph <- ranking_f_y(20)

# PLOTTING DATA
#-----------------------------------------

# Plotting 
p <- ggplot(data_graph, aes(rank, group = team, country = ccode, 
                     color = as.factor(team), fill = as.factor(team))) +
        geom_tile(aes(y = cum_y_goals/2,
                      height = cum_y_goals,
                      width = 0.9, fill = as.factor(team)), alpha = 0.6, color = NA)+
        geom_text(aes(y = 0, label = paste(team, " ")), vjust = 0.5, hjust = 1) +
        geom_text(aes(y = cum_y_goals + 275, label = paste(cum_y_goals, " ")))+
        geom_flag(aes(y = cum_y_goals + 85), size = 8)+
        scale_fill_viridis_d(option = "plasma", alpha = 1) +
        scale_color_viridis_d(option = "plasma", alpha = 1)+
        coord_flip(clip = "off", expand = FALSE) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_reverse() +
        theme_minimal() +
        guides(color = FALSE, fill = FALSE) +
        labs(subtitle=paste0("Year: ",'{closest_state}'),
             title = "Top 20 National football teams by goals scored",
             caption = "Data source: https://www.kaggle.com/martj42/international-football-results-from-1872-to-2017\nPlot author: Milos Basaraba",
             x = "", y = "") +
        theme(plot.title = element_text(hjust = 0, vjust = 0, size = 12, color = plasma(1, alpha = 1)),
              plot.subtitle = element_text(hjust = 0, vjust = 0, size = 14, color = plasma(1, alpha = 1, begin = 0.4)),
              plot.caption = element_text(hjust = 0, vjust = 0, size = 8, color = plasma(1, alpha = 1, begin = 0.8)),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.ticks.y = element_blank(),  # These relate to the axes post-flip
              axis.text.y  = element_blank(),  # These relate to the axes post-flip
              axis.text.x = element_blank(),
              plot.margin = margin(1,1,1,4, "cm")) +
        #Adding transition states
        transition_states(Year, transition_length = 8, state_length = 1) +
        enter_fade() + 
        exit_shrink() +
        ease_aes('cubic-in-out')

# Animating plot
anim <- animate(p, fps = 25, duration = 60, width = 600, height = 400)

# Saving animation as gif
save_animation(anim, "output1.gif")
