#' Original DoD Comptroller zip file downloaded here:
#' http://comptroller.defense.gov/BudgetMaterials.aspx
#' To View as pd:
#' http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2017/FY17_Green_Book.pdf
#' 
#' Table 6.8 DOD BA By Title
#' 
# Libraries ---------------------------------------------------------------

library(tidyr)
library(dplyr)
library(readxl)
library(stringr)

# Import  Data ------------------------------------------------------------

# Create filename for import purposes
# Note: Data is unzipped and stored in a Data/Raw subfolder as an Excel doc

filename <- "../Data/Raw/FY17 6-8_DoD BA by Title.xlsx"

#excel_sheets(filename)
df.raw <- read_excel(filename, skip = 4)

# Flatten -----------------------------------------------------------------
# Shape Subset for Current Dollars, ignore rest
df <- df.raw[2:10, -2:-3]

# Flatten
df.flat <- gather(df, Fiscal.Year, Amount, -1) 

# Fixing ------------------------------------------------------------------

# Dollars in millions
df.flat$Amount <- df.flat$Amount * 1e6

# Remove trailing dots (non-alphanumeric)
df.flat$`Public Law Title` <- str_trim(gsub("[0-9.]+", "", df.flat$`Public Law Title`))

# Remove 'FY' from Fiscal.Year column
df.flat <- separate(df.flat, Fiscal.Year, c('trash', 'FY'), convert = TRUE )
df.flat <- df.flat[,-2]

df.flat$Deflator.Type <- "Current.Dollars"
df.flat$Source <- "Table 6.8 DOD BA By Title"


# Export ------------------------------------------------------------------
# Filename
mylocation <- "../Data/Processed"
myfilename <- "tbl.6.8_DOD.BA.By.Title"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")

my.file <- sprintf("%s/%s_%s.csv", mylocation, myfilename, mydate)
write_csv(df.flat, my.file)
















