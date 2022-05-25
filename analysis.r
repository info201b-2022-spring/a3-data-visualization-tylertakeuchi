library(tidyverse)
library(dplyr)
library(ggplot2)
library(usmap)

incar_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

incar_trends_prop <- incar_trends %>%
                     group_by(year) %>%
                     subset(!is.na(black_jail_pop)) %>%
                     subset(!is.na(total_jail_pop)) %>%
                     mutate(black_jail_prop = black_jail_pop/total_jail_pop)
                   
black_jail_mean_time <- incar_trends_prop %>%
                        subset(black_jail_prop >= 0) %>% 
                        filter(year >= 1985) %>%
                        summarise(black_jail_mean_time = mean(black_jail_prop))
  
filtered_2018 <- incar_trends %>%
                filter(year == 2018)

total_black_pop_15to64 <- filtered_2018 %>%
                          summarise(total_black_pop_15to64 = sum(black_pop_15to64))

total_black_jail_pop_15to64 <- filtered_2018 %>%
                              subset(!is.na(black_jail_pop)) %>%
                              summarise(total_black_jail_pop_15to64 = sum(black_jail_pop)) 
total_pop_2018 <- filtered_2018 %>%
                  summarise(total_pop_2018 = sum(total_pop_15to64))

total_jail_pop_2018 <- filtered_2018 %>%
                      subset(!is.na(total_jail_pop)) %>%
                      summarise(total_jail_pop_2018 = sum(total_jail_pop))

rural_black_pop <- filtered_2018 %>%
                          filter(urbanicity == "rural") %>%
                          subset(!is.na(black_pop_15to64)) %>%
                          summarise(rural_black_proportion = sum(black_pop_15to64)) 
rural_population <- filtered_2018 %>%
                    filter(urbanicity == "rural") %>%
                    subset(!is.na(total_pop_15to64)) %>%
                    summarise(rural_population = sum(total_pop_15to64))

rural_black_proportion <- rural_black_pop/rural_population

rural_black_jail_pop <- filtered_2018 %>%
                        filter(urbanicity == "rural") %>%
                        subset(!is.na(black_jail_pop)) %>%
                        summarise(rural_black_jail_pop = sum(black_jail_pop)) 
rural_jail_pop <- filtered_2018 %>%
                  filter(urbanicity == "rural") %>%
                  subset(!is.na(total_jail_pop)) %>%
                  summarise(rural_jail_pop = sum(total_jail_pop))

rural_black_jail_proportion <- rural_black_jail_pop/rural_jail_pop

black_proportion_pop <- total_black_pop_15to64/total_pop_2018

black_jail_proportion <- total_black_jail_pop_15to64/total_jail_pop_2018

medium_large_total_jail_pop <- filtered_2018 %>%
                                      filter(urbanicity != "rural") %>%
                                      subset(!is.na(total_jail_pop)) %>%
                                      summarise(total_jail_pop_2018 = sum(total_jail_pop))

medium_large_black_jail_pop <- filtered_2018 %>%
                               filter(urbanicity != "rural") %>%
                               subset(!is.na(black_jail_pop)) %>%
                               summarise(total_black_jail_pop_15to64 = sum(black_jail_pop)) 

medium_large_black_jail_proportion <- medium_large_black_jail_pop/medium_large_total_jail_pop

line_df <- incar_trends_prop %>%
               filter(state %in% c("CA", "FL")) %>%
               filter(year >= 1985)
               
line_graph <-  line_df %>%
  ggplot( aes(x=year, y=black_jail_prop, group=state, color=state)) +
  geom_line() + ggtitle("Black Proportion Incarceration \nBetween California and Florida")


line_graph

data <- data.frame(
  Urbanicity=c("Medium/Large Cities","Rural"),
  Proportion=c(0.3602962, 0.2421117)
)

rural_vs_large <- ggplot(data, aes(x=Urbanicity, y=Proportion)) + 
  geom_bar(stat = "identity", fill = "#FF6666") + ggtitle("Rural vs. Medium/Large City Black \nIncarceration Proportion") 
  
rural_vs_large

avg_black_prop <- incar_trends_prop %>%
                  group_by(state) %>%
                  summarize(avg_black_prop = mean(black_jail_prop, na.rm = TRUE))
                            

avg_black_prop_map <- plot_usmap(data = avg_black_prop, values = "avg_black_prop") +
  labs(title = "Average Black Proportion In US Jails",
       subtitle = "Data from 1985-2018") +
  scale_fill_continuous(low = "white", high = "red")
avg_black_prop_map
