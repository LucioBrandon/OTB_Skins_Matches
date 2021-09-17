library(tidyverse)

#Reading in Data
data <- read_csv("Data/OTB_SKINS_Winnings.csv")

#Sample Splits
Match_Info <- data %>%
                filter(`Hole Number` == 'Info') %>%
                select(Year:Course , `Player 1`:Host ) %>%
                gather( Test,Player, `Player 1`:`Player 4`, factor_key = TRUE ) #Turn to long form

Player_Stats <- data %>%
                  filter(!is.na(Winner)) %>%
                  group_by(Course,Winner) %>%
                  summarise(count = n(), Total_Winnings = sum(Amount))

Match_Stat <- data %>%
                filter(!is.na(Winner)) %>%
                group_by(Winner, `Tournament`) %>%
                summarise(count = n(), Total_Winnings = sum(Amount))

# Player_Stats %>%
#   filter(Winner == 'Kevin Jones') %>%
#   ggplot(aes(x = Course, y = Total_Winnings)) +
#     geom_col()+
#     coord_flip()

Earnings <- Player_Stats %>% group_by(Winner) %>%
        summarise(Money = sum(Total_Winnings))