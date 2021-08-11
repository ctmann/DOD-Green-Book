
#' Table 7.5 End Strength FTEs
#' 
# How to Update this File -------------------------------------------------
#' 
#'   Set working Directory to current year:
          setwd("./DOD-Green-Book/FY2022")
#'   
#'   Update name of Chapter 7 raw data folder, files
          raw.data.folder <- "./Data/Raw/"
#' 
#'   Update Name of specific files
          my.filename <- "FY22 PB Green Book Chap 7.xlsx"
          
# Shape data
my.data.rows <- c(6:89)
my.data.cols <- c(1,3:8, 10:14)

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)

#' # Import  Data ------------------------------------------------------------
my.filename <- paste0(raw.data.folder,my.filename)

end.strength <- read_excel(my.filename,'7-5')

# Shape Dataset: This Varies from year to to year!
end.strength <- end.strength[my.data.rows, my.data.cols]

names(end.strength) <- c(
  "FY",
  "Army,Active",
  "Navy,Active",
  "Marine Corps,Active",
  "Air Force,Active",
  "Space Force,Active",
  "Full Time Guard and Reserve,Active",
  "Army,Civilians",
  "Navy and Marine Corps,Civilians",
  "Air Force,Civilians",
  "Space.Force,Civilians",
  "Defense Agencies,Civilians")
        

# Tidy
end.strength.data.tdy <- gather(end.strength, Service, Personnel, -FY)

end.strength.data.tdy <-end.strength.data.tdy %>% 
  mutate(FY = str_extract(FY, "[0-9]{4}") )

#Convert to Numerics
end.strength.data.tdy$Personnel <- as.numeric(end.strength.data.tdy$Personnel)
end.strength.data.tdy <- end.strength.data.tdy %>% 
  mutate(Personnel = as.numeric(Personnel*1e3))

# Separate into Active/Civilian
end.strength.data.tdy <- end.strength.data.tdy %>% 
  separate(col = Service, into = c("Service", "Active.or.Civilians"), ",")



# Export ------------------------------------------------------------------

mylocation <- "./Data/Processed"
myfilename <- "tbl.7.5_DOD.Manpower"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")

my.file <- sprintf("%s/%s_%s.csv", mylocation, myfilename, mydate)
write_csv(end.strength.data.tdy, my.file)


