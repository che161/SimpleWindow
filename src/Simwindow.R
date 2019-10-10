library(tidyverse)
overheat_result2 <- read_fwf("data/NT/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
        StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
        CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2_error <- read_fwf("data/NT/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
        StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
        CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2
overheat_result2_error
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

overheatcsv_orig <- read.csv("data/NT/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/NT/Result2_Orig_Clean.csv")

overheatcsv <- read.csv("data/NT/Result2.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv

overheat_clean <- overheatcsv %>% semi_join(validRow, by = "Nvalid") %>% 
  write_csv("res/NT/Result2_Clean.csv")

overheat_star_Orig <- select(overheatorig_clean, StarRating)
overheat_star_Orig
overheat_star <- select(overheat_clean, StarRating)
overheat_star

overheat_stardiff <- overheat_star_Orig %>% 
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#CCCCCCCCCCCCCCCCCCCC     Merge clean data  ccccccccccccccccccccccccccccccccccccccccccccccccccccc

?anti_join


#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#CCCCCCCCCCCCCCCCCCCC          PLOT         ccccccccccccccccccccccccccccccccccccccccccccccccccccc

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
figure1 <- ggplot(
  data = number_dwellings, 
  mapping = aes(x = State, y = DwellingNo,  label = DwellingNo,
                col.lab="red", cex.axis = 3, cex.lab = 4)
) +
  geom_col(fill = "green", width = 0.5) +
  geom_text(aes(label = DwellingNo), colour ="blue", vjust = -0.5, fontface = "bold") +
  labs(title = "Figure 1. Number of dwellings simmulated in each State",
       x = "State",
       y = "Number of Dwellings"
  ) +
  ylim(0, 600) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))
figure1  
ggsave("fig/Figure_1.png", plot = figure1)

figure2 <- ggplot(
  data = stardiff, 
  mapping = aes(x = StarDiff, y = DwellingNo, group = State, label = DwellingNo )
) +
  geom_col(fill = "green",width = 0.05) +   
  geom_text(aes(label = DwellingNo), colour = "blue", vjust = -0.15, fontface = "bold") +
  labs(title = "Figure 2. Star rating difference distribution in each State",
       x = "Star Rating Difference",
       y = "Number of Dwellings"
  ) +
  ylim(0, 220) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))+
  facet_wrap( ~ State)
figure2
ggsave("fig/Figure_2.png", plot = figure2)

figure3 <- ggplot(
  data = averagediff, 
  mapping = aes(x = State, y = MeanDiff, label = sprintf("%.02f %%", MeanDiff),
                col.lab="red", cex.axis = 3, cex.lab = 4, fontface = "bold")
) +
  geom_col(fill = "green", width = 0.5) +
  geom_text(aes(label = sprintf("%.02f", MeanDiff)), colour ="blue", vjust = -0.5) +
  labs(title = "Figure 3. Mean star raing difference in each State",
       x = "State",
       y = "Mean Star Rating Difference"
  ) + 
  ylim(0, 0.24) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))
figure3  
ggsave("fig/Figure_3.png", plot = figure3)

?sprintf

