library(tidyverse)

allstate <- read.csv("data/Result2_Orig_Clean_AllState.csv")
allstate

abs_allstate <- allstate %>% mutate(AbsDiff = abs(StarDiff))
by_state <- group_by(abs_allstate, by = State)
by_state
summarise(by_state,MaxStarDiff= max(StarDiff), MinStarDiff= min(StarDiff),MeanStarDiff= mean(AbsDiff) )


# Using ggplot2
ggplot(data = AllState)
?ggplot
ggplot(
  data = AllState, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)
) 

?mean
