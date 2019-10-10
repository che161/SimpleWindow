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

stardiff <- group_by(allstate, State, StarDiff) %>% summarise(DwellingNo = n() )
stardiff
averagediff <- group_by(abs_allstate, State) %>% summarise(MeanDiff = mean(AbsDiff) )
averagediff
# Using ggplot2
ggplot(
  data = number_dwellings, 
  mapping = aes(x = State, y = DwellingNo,  label = DwellingNo,
                col.lab="red", cex.axis = 3, cex.lab = 4)
) +
  geom_col(width = 0.7) +   geom_col(fill = "green", width = 0.75) +
  geom_text(aes(label = DwellingNo), colour ="blue", vjust = -0.5) +
  labs(title = "Figure 1",
       x = "State",
       y = "Number of Dwellings"
  ) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))
  

ggplot(
  data = stardiff, 
  mapping = aes(x = StarDiff, y = DwellingNo, group = State, label = DwellingNo )
) +
  geom_col(fill = "green",width = 0.05) +   
  geom_text(aes(label = DwellingNo), colour = "blue", vjust = -0.15) +
  labs(title = "Figure 2",
       x = "Star Rating Difference",
       y = "Number of Dwellings"
  ) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))+
    facet_wrap( ~ State)

ggplot(
  data = averagediff, 
  mapping = aes(x = State, y = MeanDiff, label = sprintf("%.02f %%", MeanDiff),
                col.lab="red", cex.axis = 3, cex.lab = 4)
) +
  geom_col(width = 0.7) +   geom_col(fill = "green", width = 0.75) +
  geom_text(aes(label = sprintf("%.02f", MeanDiff)), colour ="blue", vjust = -0.5) +
  labs(title = "Figure 3",
       x = "State",
       y = "Mean Star Rating Difference"
  ) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))


