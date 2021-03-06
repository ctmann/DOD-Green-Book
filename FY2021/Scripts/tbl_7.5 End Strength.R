#' Original DoD Comptroller zip file downloaded here:
#' http://comptroller.defense.gov/BudgetMaterials.aspx
#' To View as pd:
#' http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2019/FY19_Green_Book.pdf
#' 
#' To download as zip
#' http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2019/FY_2019_Green_Book.zip
#' 
#' Table 7.5 End Strength FTEs
#' 
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)
library(nettles.toolbox)

#' # Import  Data ------------------------------------------------------------
x <- "http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2019/FY_2019_Green_Book.zip"
y <- "FY19 PB Green Book Chap 7.xlsx"


nettle_downzip <- function(zip.url, zip.file){
    my.temporary.zipped.file <- tempfile()   # Zip file will go in here
    my.temporarary.zipped.folder <- tempdir() # Unzipped file will go in here
    download.file(zip.url, dest = my.temporary.zipped.file) # Download Source Data to Temp file
    unzip(my.temporary.zipped.file, exdir = my.temporarary.zipped.folder) # Unzip to Temp directory
    location.of.unzipped.file <- paste0(my.temporarary.zipped.folder,"/", zip.file)
    return(location.of.unzipped.file )
    }

my.filename <- nettle_downzip(x,y)

end.strength <- read_excel(my.filename,'7-5')

# Shape Dataset: This Varies from year to to year!
end.strength.data <- end.strength[c(5:83, 86, 89), c(1,3:7, 9:12)]


# Names

names(end.strength.data) <- c(
  "FY",
  "Army,Active",
  "Navy,Active",
  "Marine Corps,Active",
  "Air Force,Active",
  "Full Time Guard and Reserve,Active",
  "Army, Civilians",
  "Navy and Marine Corps,Civilians",
  "Air Force,Civilians",
  "Defense Agencies,Civilians"
        )
end.strength.data <- end.strength.data[-1,]


# Tidy
end.strength.data.tdy <- gather(end.strength.data, Service, Personnel, -FY)
#View(end.strength.data.tdy)

# Remove Dots, Trim, Remove "Total" text
end.strength.data.tdy$FY <-  str_replace_all(end.strength.data.tdy$FY, "[.]","")
end.strength.data.tdy$FY <- str_trim(end.strength.data.tdy$FY, "both")
end.strength.data.tdy$FY <- str_sub(end.strength.data.tdy$FY, start = 1, end = 4)
     
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


