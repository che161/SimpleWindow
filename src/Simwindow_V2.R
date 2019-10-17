library(tidyverse)
#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#CCCCCCCCCCCCCCCCC       Clean data       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
# Clean data for NT
# First read in Result2.txt which contains the information for all houses collected from the state 
overheat_result2 <- read_fwf("data/NT/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
        StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
        CZ = 2, BL = 1,SubDir = 97, ScratchName = NA)) 

# Second read in Result2_error.txt which contains the information for all houses collected from the state
# which has issues in the data 
overheat_result2_error <- read_fwf("data/NT/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
        StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
        CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2
overheat_result2_error

# Figure out the those house numbers which do not have issues. 
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

# Now read in the performance data for each house using the existing window model in the state
overheatcsv_orig <- read.csv("data/NT/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

# Clean the performance data just contain those house which do not have issues.
overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/NT/Result2_Orig_Clean.csv")

# Now read in the performance data for each house using the new window model in the state
overheatcsv <- read.csv("data/NT/Result2.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv

# Clean the performance data just contain those house which do not have issues.
overheat_clean <- overheatcsv %>% semi_join(validRow, by = "Nvalid") %>% 
  write_csv("res/NT/Result2_Clean.csv")

# Only keep the hose number and the star rating column.
overheat_star_Orig <- select(overheatorig_clean, Nvalid, StarRating)
overheat_star_Orig
overheat_star <- select(overheat_clean, Nvalid, StarRating)
overheat_star

# Now join the tables for existing and the new window model results, and
# work out the change in the star rating due to the change in the window model
# and also create a column with state name
overheat_stardiff <- overheat_star_Orig %>% inner_join(overheat_star, by = "Nvalid") %>% 
  mutate(StarDiff = StarRating.x - StarRating.y, State = "NT") %>% 
  rename(StarratingOrig = StarRating.x, Starrating = StarRating.y) %>%
  select(State,StarDiff) %>% 
  write_csv("res/NT/Result2_StarDiff.csv")

# Assign to a NT data form
overheat_stardiff_NT <- overheat_stardiff
overheat_stardiff_NT

#Now copy and work out for other states and territories
# Clean data for ACT
overheat_result2 <- read_fwf("data/ACT/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                            StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                            CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2_error <- read_fwf("data/ACT/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                            StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                            CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2
overheat_result2_error
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

overheatcsv_orig <- read.csv("data/ACT/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/ACT/Result2_Orig_Clean.csv")

overheatcsv <- read.csv("data/ACT/Result2.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv

overheat_clean <- overheatcsv %>% semi_join(validRow, by = "Nvalid") %>% 
  write_csv("res/ACT/Result2_Clean.csv")

overheat_star_Orig <- select(overheatorig_clean, Nvalid, StarRating)
overheat_star_Orig
overheat_star <- select(overheat_clean, Nvalid, StarRating)
overheat_star

overheat_stardiff <- overheat_star_Orig %>% inner_join(overheat_star, by = "Nvalid") %>% 
  mutate(StarDiff = StarRating.x - StarRating.y, State = "ACT") %>% 
  rename(StarratingOrig = StarRating.x, Starrating = StarRating.y) %>%
  select(State,StarDiff) %>% 
  write_csv("res/ACT/Result2_StarDiff.csv")

overheat_stardiff_ACT <- overheat_stardiff
overheat_stardiff_ACT

# Clean data for TAS
overheat_result2 <- read_fwf("data/TAS/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                            StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                            CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2_error <- read_fwf("data/TAS/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                           StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                           CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2
overheat_result2_error
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

overheatcsv_orig <- read.csv("data/TAS/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/TAS/Result2_Orig_Clean.csv")

overheatcsv <- read.csv("data/TAS/Result2.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv

overheat_clean <- overheatcsv %>% semi_join(validRow, by = "Nvalid") %>% 
  write_csv("res/TAS/Result2_Clean.csv")

overheat_star_Orig <- select(overheatorig_clean, Nvalid, StarRating)
overheat_star_Orig
overheat_star <- select(overheat_clean, Nvalid, StarRating)
overheat_star

overheat_stardiff <- overheat_star_Orig %>% inner_join(overheat_star, by = "Nvalid") %>% 
  mutate(StarDiff = StarRating.x - StarRating.y, State = "TAS") %>% 
  rename(StarratingOrig = StarRating.x, Starrating = StarRating.y) %>%
  select(State,StarDiff) %>% 
  write_csv("res/TAS/Result2_StarDiff.csv")

overheat_stardiff_TAS <- overheat_stardiff
overheat_stardiff_TAS

# Clean data for QLD
overheat_result2 <- read_fwf("data/QLD/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                             StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                             CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2_error <- read_fwf("data/QLD/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                             StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                            CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2
overheat_result2_error
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

overheatcsv_orig <- read.csv("data/QLD/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/QLD/Result2_Orig_Clean.csv")

overheatcsv <- read.csv("data/QLD/Result2.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv

overheat_clean <- overheatcsv %>% semi_join(validRow, by = "Nvalid") %>% 
  write_csv("res/QLD/Result2_Clean.csv")

overheat_star_Orig <- select(overheatorig_clean, Nvalid, StarRating)
overheat_star_Orig
overheat_star <- select(overheat_clean, Nvalid, StarRating)
overheat_star

overheat_stardiff <- overheat_star_Orig %>% inner_join(overheat_star, by = "Nvalid") %>% 
  mutate(StarDiff = StarRating.x - StarRating.y, State = "QLD") %>% 
  rename(StarratingOrig = StarRating.x, Starrating = StarRating.y) %>%
  select(State,StarDiff) %>% 
  write_csv("res/QLD/Result2_StarDiff.csv")

overheat_stardiff_QLD <- overheat_stardiff
overheat_stardiff_QLD

# Clean data for WA
overheat_result2 <- read_fwf("data/WA/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                             StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                             CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2_error <- read_fwf("data/WA/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                         StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                                         CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2
overheat_result2_error
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

overheatcsv_orig <- read.csv("data/WA/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/WA/Result2_Orig_Clean.csv")

overheatcsv <- read.csv("data/WA/Result2.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv

overheat_clean <- overheatcsv %>% semi_join(validRow, by = "Nvalid") %>% 
  write_csv("res/WA/Result2_Clean.csv")

overheat_star_Orig <- select(overheatorig_clean, Nvalid, StarRating)
overheat_star_Orig
overheat_star <- select(overheat_clean, Nvalid, StarRating)
overheat_star

overheat_stardiff <- overheat_star_Orig %>% inner_join(overheat_star, by = "Nvalid") %>% 
  mutate(StarDiff = StarRating.x - StarRating.y, State = "WA") %>% 
  rename(StarratingOrig = StarRating.x, Starrating = StarRating.y) %>%
  select(State,StarDiff) %>% 
  write_csv("res/WA/Result2_StarDiff.csv")

overheat_stardiff_WA <- overheat_stardiff
overheat_stardiff_WA

# Clean data for SA
overheat_result2 <- read_fwf("data/SA/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                             StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                             CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2_error <- read_fwf("data/SA/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                         StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                                         CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2
overheat_result2_error
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

overheatcsv_orig <- read.csv("data/SA/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/SA/Result2_Orig_Clean.csv")

overheatcsv <- read.csv("data/SA/Result2.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv

overheat_clean <- overheatcsv %>% semi_join(validRow, by = "Nvalid") %>% 
  write_csv("res/SA/Result2_Clean.csv")

overheat_star_Orig <- select(overheatorig_clean, Nvalid, StarRating)
overheat_star_Orig
overheat_star <- select(overheat_clean, Nvalid, StarRating)
overheat_star

overheat_stardiff <- overheat_star_Orig %>% inner_join(overheat_star, by = "Nvalid") %>% 
  mutate(StarDiff = StarRating.x - StarRating.y, State = "SA") %>% 
  rename(StarratingOrig = StarRating.x, Starrating = StarRating.y) %>%
  select(State,StarDiff) %>% 
  write_csv("res/SA/Result2_StarDiff.csv")

overheat_stardiff_SA <- overheat_stardiff
overheat_stardiff_SA

# Clean data for VIC
overheat_result2 <- read_fwf("data/VIC/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                             StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                             CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2_error <- read_fwf("data/VIC/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                         StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                                         CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2
overheat_result2_error
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

overheatcsv_orig <- read.csv("data/VIC/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%  
  write_csv("res/VIC/Result2_Orig_Clean.csv")

overheatorig_clean2 <- overheatorig_clean %>% filter(StarRating != "*****",StarRating != "0" ) %>%
  filter(StarRating != "0" ) %>%
  mutate(StarRating = as.numeric(StarRating)) %>% 
  filter(StarRating > 0.1 ) %>%  
  write_csv("res/VIC/Result2_Orig_Clean.csv")

overheatorig_clean <- overheatorig_clean2 %>%  
  write_csv("res/VIC/Result2_Orig_Clean.csv")

overheatcsv <- read.csv("data/VIC/Result2.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv

overheat_clean <- overheatcsv %>% semi_join(validRow, by = "Nvalid") %>%  
  write_csv("res/VIC/Result2_Clean.csv")

overheat_clean2 <- overheat_clean %>% filter(StarRating != "*****",StarRating != "0" ) %>%
  filter(StarRating != "0" ) %>%
  mutate(StarRating = as.numeric(StarRating)) %>% 
  filter(StarRating > 0.1 ) %>%  
  write_csv("res/VIC/Result2_Clean.csv")

overheat_clean <-overheat_clean2 %>%  
  write_csv("res/VIC/Result2_Clean.csv")

overheat_star_Orig <- select(overheatorig_clean, Nvalid, StarRating)
overheat_star_Orig
overheat_star <- select(overheat_clean, Nvalid, StarRating)
overheat_star
overheat_star_Orig <- overheat_star_Orig %>% filter(StarRating != "*****") %>%
  filter(StarRating != "0") %>%
  mutate(StarRating = as.numeric(StarRating))
overheat_star <- overheat_star %>% filter(StarRating != "*****",StarRating != "0" ) %>%
  filter(StarRating != "0" ) %>%
  mutate(StarRating = as.numeric(StarRating)) 

overheat_stardiff <- overheat_star_Orig %>% inner_join(overheat_star, by = "Nvalid") %>% 
  mutate(StarDiff = StarRating.x - StarRating.y, State = "VIC") %>% 
  rename(StarratingOrig = StarRating.x, Starrating = StarRating.y) %>%
  select(State,StarDiff) %>% 
  write_csv("res/VIC/Result2_StarDiff.csv")

overheat_stardiff_VIC <- overheat_stardiff
overheat_stardiff_VIC

# Clean data for NSW
overheat_result2 <- read_fwf("data/NSW/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                             StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                             CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2_error <- read_fwf("data/NSW/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                         StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                                         CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2
overheat_result2_error
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

overheatcsv_orig <- read.csv("data/NSW/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%  
  write_csv("res/NSW/Result2_Orig_Clean.csv")

overheatorig_clean2 <- overheatorig_clean %>% filter(StarRating != "*****",StarRating != "0" ) %>%
  filter(StarRating != "0" ) %>%
  mutate(StarRating = as.numeric(StarRating)) %>% 
  filter(StarRating > 0.1 ) %>%  filter(StarRating < 10.1 ) %>% 
  write_csv("res/NSW/Result2_Orig_Clean.csv")

overheatorig_clean <- overheatorig_clean2 %>%  
  write_csv("res/NSW/Result2_Orig_Clean.csv")

overheatcsv <- read.csv("data/NSW/Result2.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv

overheat_clean <- overheatcsv %>% semi_join(validRow, by = "Nvalid") %>%  
  write_csv("res/NSW/Result2_Clean.csv")

overheat_clean2 <- overheat_clean %>% filter(StarRating != "*****",StarRating != "0" ) %>%
  filter(StarRating != "0" ) %>%
  mutate(StarRating = as.numeric(StarRating)) %>% 
  filter(StarRating > 0.1 ) %>%  filter(StarRating < 10.1 ) %>% 
  write_csv("res/NSW/Result2_Clean.csv")

overheat_clean <-overheat_clean2 %>%  
  write_csv("res/NSW/Result2_Clean.csv")

overheat_star_Orig <- select(overheatorig_clean, Nvalid, StarRating)
overheat_star_Orig
overheat_star <- select(overheat_clean, Nvalid, StarRating)
overheat_star
#overheat_star_Orig <- overheat_star_Orig %>% filter(StarRating != "*****") %>%
#  filter(StarRating != "0") 
#  mutate(StarRating = as.numeric(StarRating))
#overheat_star <- overheat_star %>% filter(StarRating != "*****",StarRating != "0" ) %>%
#  filter(StarRating != "0" ) %>%
#  mutate(StarRating = as.numeric(StarRating)) 

overheat_stardiff <- overheat_star_Orig %>% inner_join(overheat_star, by = "Nvalid") %>% 
  mutate(StarDiff = StarRating.x - StarRating.y, State = "NSW") %>% 
  rename(StarratingOrig = StarRating.x, Starrating = StarRating.y) %>%
  select(State,StarDiff) %>% 
  write_csv("res/NSW/Result2_StarDiff.csv")

overheat_stardiff_NSW <- overheat_stardiff
overheat_stardiff_NSW

#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#CCCCCCCCCCCCCCCCCCCC     Merge clean data  ccccccccccccccccccccccccccccccccccccccccccccccccccccc

bindAll <- bind_rows(overheat_stardiff_NT, overheat_stardiff_SA, 
                     overheat_stardiff_TAS, overheat_stardiff_QLD,
                     overheat_stardiff_WA,overheat_stardiff_ACT,
                     overheat_stardiff_VIC,overheat_stardiff_NSW) 
bindAll
bindAll_Diff <- bindAll %>% select(StarDiff)
bindAll_Diff
bindAll_Diff <- format(round(bindAll_Diff,1)) %>% 
  write_csv("res/Result2_Orig_Clean_AllState.csv")
bindAll_State <- bindAll %>% select(State)
bindAll_State 
bindAll_Diff
bindAll_State$StarDiff <- bindAll_Diff$StarDiff
bindAll_State 

allstate <- bindAll_State 
bindAll_State %>% write_csv("res/Result2_Orig_Clean_AllState.csv")
bindAll_State
?format.data.frame
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#CCCCCCCCCCCCCCCCCCCC          PLOT         ccccccccccccccccccccccccccccccccccccccccccccccccccccc
#allstate <- read.csv("res/Result2_Orig_Clean_AllState.csv") 
#allstate
#view(allstate)
allstate <- allstate %>% mutate(StarDiff = as.numeric(StarDiff))
abs_allstate <- allstate %>% mutate(AbsDiff = abs(StarDiff))
abs_allstate
by_state <- group_by(abs_allstate, by = State)
by_state

diff <- allstate %>% group_by(State) %>% mutate(AbsDiff = abs(StarDiff)) %>% 
  summarise(MaxStarDiff= max(StarDiff), MinStarDiff= min(StarDiff),MeanStarDiff= mean(StarDiff),MeanAbsStarDiff= mean(AbsDiff) )
diff
number_dwellings <- allstate %>% group_by(State) %>% summarise(DwellingNo = n() )
number_dwellings

stardiff <- group_by(allstate, State, StarDiff) %>% summarise(DwellingNo = n() )
stardiff 

?as.numeric

#view(stardiff)
averagediff <- group_by(abs_allstate, State) %>% summarise(MeanStarDiff= mean(StarDiff),MeanAbsStarDiff= mean(AbsDiff) ) 
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
  ylim(0, 20000) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))
figure1  
ggsave("fig/Figure_1.png", plot = figure1)


# Plot the figure in two groups due to large number of houses in VIC and NSW
stardiff
stardiff_VIC_NSW <- stardiff %>% filter(State =="VIC"|State =="NSW")
stardiff_OtherStates <- stardiff %>% filter(State !="VIC",State !="NSW")
figure2a <- ggplot(
  data = stardiff_VIC_NSW, 
  mapping = aes(x = StarDiff, y = DwellingNo, group = State)
) +
  geom_col(fill = "green",width = 0.05) +   
  geom_text(aes(label = DwellingNo), colour = "blue", vjust = -0.15, fontface = "bold", size = 4) +
  labs(title = "Figure 2a. Star rating difference distribution in each State",
       x = "Star Rating Difference",
       y = "Number of Dwellings"
  ) +
  ylim(0, 4000) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))+
  facet_wrap( ~ State)
figure2a
ggsave("fig/Figure_2a.png", plot = figure2a)

figure2b <- ggplot(
  data = stardiff_OtherStates, 
  mapping = aes(x = StarDiff, y = DwellingNo, group = State)
) +
  geom_col(fill = "green",width = 0.05) +   
  geom_text(aes(label = DwellingNo), colour = "blue", vjust = -0.15, fontface = "bold", size = 4) +
  labs(title = "Figure 2. Star rating difference distribution in each State",
       x = "Star Rating Difference",
       y = "Number of Dwellings"
  ) +
  ylim(0, 250) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))+
  facet_wrap( ~ State)
figure2b
ggsave("fig/Figure_2b.png", plot = figure2b)

#plot averaged absolute star rating difference

figure3 <- ggplot(
  data = averagediff, 
  mapping = aes(x = State, y = MeanAbsStarDiff, label = sprintf("%.02f %%", MeanAbsStarDiff),
                col.lab="red", cex.axis = 3, cex.lab = 4, fontface = "bold")
) +
  geom_col(fill = "green", width = 0.5) +
  geom_text(aes(label = sprintf("%.03f", MeanAbsStarDiff)), colour ="blue", vjust = -0.5) +
  labs(title = "Figure 3. Mean ABSOLUTE star raing difference in each State",
       x = "State",
       y = "Mean Star Rating Difference"
  ) + 
  ylim(0, 0.24) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))
figure3  
ggsave("fig/Figure_3.png", plot = figure3)

#plot averaged star rating difference
figure4 <- ggplot(
  data = averagediff, 
  mapping = aes(x = State, y = MeanStarDiff, label = sprintf("%.02f %%", MeanStarDiff),
                col.lab="red", cex.axis = 3, cex.lab = 4, fontface = "bold")
) +
  geom_col(fill = "green", width = 0.5) +
  geom_text(aes(label = sprintf("%.03f", MeanStarDiff)), colour ="blue", vjust = -0.5) +
  labs(title = "Figure 4. Mean star raing difference in each State",
       x = "State",
       y = "Mean Star Rating Difference"
  ) + 
  ylim(-0.16, 0.16) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))
figure4  
ggsave("fig/Figure_4.png", plot = figure4)
