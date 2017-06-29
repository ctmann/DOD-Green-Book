#' Original DoD Comptroller zip file downloaded here:
#' http://comptroller.defense.gov/BudgetMaterials.aspx
#' To View as pd:
#' http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2017/FY17_Green_Book.pdf
#' 
#' Table 7.5 End Strength FTEs
#' 
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)
library(nettles.toolbox)

setwd("~/Documents/R Programming/Repositories/DOD-Green-Book/FY2017")

end.strength <- read_excel('./Data/Raw/FY17 PB Green Book Chap 7.xlsx','7-5')

# Cut Dataset
end.strength.data <- end.strength[c(5:83, 85:86),c(1,3:7, 9:12)]

# Fix Rownames
names(end.strength.data) <- end.strength.data[1,]
end.strength.data <- end.strength.data[-1,]
names(end.strength.data)[1]<- "FY"

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
View(end.strength.data)

# Tidy
end.strength.data.tdy <- gather(end.strength.data, Service, Personnel, -FY)
#View(end.strength.data.tdy)

# Remove Dots
end.strength.data.tdy$FY <-  str_replace_all(end.strength.data.tdy$FY, "[.]","")
end.strength.data.tdy$FY <- str_trim(end.strength.data.tdy$FY, "both")
     
#Convert to Numerics
end.strength.data.tdy$Personnel <- as.numeric(end.strength.data.tdy$Personnel)
end.strength.data.tdy <- end.strength.data.tdy %>% 
  mutate(Personnel = as.numeric(Personnel*1e3))

# Separate into Active/Civilian
end.strength.data.tdy <- end.strength.data.tdy %>% 
  separate(col = Service, into = c("Service", "Active.or.Civilians"), ",")


# Export ------------------------------------------------------------------

nettle_export <- function(df, my.filename, my.data.subfolder = "Processed", ...){
  df <- dplyr::tbl_df(df)
  my.data.folder.location <- paste0(getwd(), "/Data")
  my.data.subfolder <- "Processed"
  my.filename <- my.filename
  my.timestamp <- paste('Updated', format(Sys.time(), format = ".%Y-%m-%d.%H%M") , sep = "")
  export.this <- sprintf("%s/%s/%s_%s.csv", my.data.folder.location, my.data.subfolder, my.filename, my.timestamp)
  return(readr::write_csv(df, export.this))
}

nettle_export(end.strength.data.tdy, "tbl.7-5 DOD Manpower")
