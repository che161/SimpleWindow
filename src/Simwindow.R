library(tidyverse)
overheat_result2 <- read_fwf("data/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                         StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20, CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2_error <- read_fwf("data/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                     StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20, CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2
overheat_result2_error
validRow <- overheat_result2 %>% anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

overheatcsv_orig <- read.csv("data/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>% write_csv("res/Result2_Orig_Clean.csv")

overheatcsv <- read.csv("data/Result2.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv

overheatcsv %>% semi_join(validRow, by = "Nvalid") %>% write_csv("res/Result2_Clean.csv")




?read.csv
?anti_join
