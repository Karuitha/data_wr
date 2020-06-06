##Exercise on data wrangling ----
install.packages("Lahman")
library(tidyverse)
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

##Check full table 
Master %>% as_tibble()

top_names <- top %>% left_join(Master) %>% 
  select(playerID, nameFirst, nameLast, HR)
top_names

top_salary <- Salaries %>% filter(yearID == 2016) %>% right_join(top_names)%>% 
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

head(AwardsPlayers)
AwardsPlayers %>% filter(yearID == 2016) %>% group_by(playerID, awardID) %>% filter(n() >= 1) %>%
  pull(playerID) %>% intersect(top_names$playerID)

AwardsPlayers %>% filter(yearID == 2016) %>% group_by(playerID, awardID) %>% filter(n() >= 1) %>%
  pull(playerID) %>% setdiff(top_names$playerID)

diamonds
