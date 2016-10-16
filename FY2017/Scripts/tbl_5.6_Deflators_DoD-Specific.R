#' Original DoD Comptroller zip file downloaded here:
#' http://comptroller.defense.gov/BudgetMaterials.aspx
#' To View as pd:
#' http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2017/FY17_Green_Book.pdf
#' 

#' #' Table 5-6: DoD dod.deflators Budget Authority by Public Law 
#' Title (FY 1970 to FY 2021)
#' 
# Libraries ---------------------------------------------------------------

library(tidyr)
library(dplyr)
library(readxl)
library(zoo)
library(stringr)
library(readr)
library(ggplot2)

#' # Import  Data ------------------------------------------------------------

# Create filename for import purposes
# Note: Data is unzipped and stored in a Data/Raw subfolder as an Excel doc
filename <- "../Data/Raw/FY17 PB Green Book Chap 5.xlsx"

# Examine file
#excel_sheets(filename)[6]

# Import tbl 5-5 on sheet 6 of workbook
dod.deflator <- tbl_df(read_excel(path = filename, sheet = 6, skip = 4))

# Make all colnames r-friendly
colnames(dod.deflator) <- make.names(colnames(dod.deflator))

# Add Columns -------------------------------------------------------------

# Change FY column name
colnames(dod.deflator)[1] <- "FY"

# New Column: dod.deflator Name
dod.deflator$Source <- names(read_excel(path = filename, sheet = 6))[1]

# New Column: dod.deflator Base Year
dod.deflator$Base.FY <- 2017


# Clean and Tidy -------------------------------------------------------------------

# Remove dots from FY column and convert to numeric
dod.deflator$FY <- as.numeric(str_trim(gsub("[.]+", "", dod.deflator$FY)))


# Tidy
dod.deflator <- dod.deflator %>%
  gather(key = Public.Law.Title, dod.deflator.Value, -FY, -Source, -Base.FY) 


# Complete and Export as .csv -----------------------------------------------------

# Filename
mylocation <- "../Data/Processed"
myfilename <- "tbl.5.6_DOD.Deflators.BA.by.Public.Law"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")

my.file <- sprintf("%s/%s_%s.csv", mylocation, myfilename, mydate)

# Export
write_csv(dod.deflator, my.file)


















