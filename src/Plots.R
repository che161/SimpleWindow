library(tidyverse)

allstate <- read.csv("data/Result2_Orig_Clean_AllState.csv")
allstate

abs_allstate <- allstate %>% mutate(AbsDiff = abs(StarDiff))
by_state <- group_by(abs_allstate, by = State)
by_state
diff <- allstate %>% group_by(State) %>% mutate(AbsDiff = abs(StarDiff)) %>% 
  summarise(MaxStarDiff= max(StarDiff), MinStarDiff= min(StarDiff),MeanStarDiff= mean(AbsDiff) )
diff
number_dwellings <- allstate %>% group_by(State) %>% summarise(DwellingNo = n() )
number_dwellings

# Using ggplot2
ggplot(
  data = number_dwellings, 
  mapping = aes(x = State, y = DwellingNo, fill = DwellingNo, label = DwellingNo )
) +
  geom_col(width = 0.7) +   geom_col(fill = "red", width = 0.75) +
  geom_text() +
  labs(title = "Figure 1",
       x = "State",
       y = "Number of Dwellings"
  )

ggplot(data = number_dwellings)
?ggplot
ggplot(
  data = number_dwellings, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)
) 
?
