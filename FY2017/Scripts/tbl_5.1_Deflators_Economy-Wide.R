#' Original DoD Comptroller zip file downloaded here:
#' http://comptroller.defense.gov/BudgetMaterials.aspx
#' To View as pd:
#' http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2017/FY17_Green_Book.pdf
#' 

#' #' Table 5-1: Department of Defense and Selected Economy-Wide Indices
#' (1970-2021)
#' Base Year = 2017
#' 
# Libraries ---------------------------------------------------------------

library(tidyr)
library(dplyr)
library(readxl)
library(zoo)
library(stringr)
library(readr)
library(ggplot2)
library(readr)

#' # Import  Data ------------------------------------------------------------

#Create Temporary Scaffolding
my.temporary.zipped.file <- tempfile()
my.temporarary.zipped.folder <- tempdir()

# Declare Source Data Origin
url <- "http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2017/FY_2017_Green_Book.zip"
spreadsheet.name <- "FY17 PB Green Book Chap 5.xlsx"

#Download Source Data to Temp Location
download.file(url = url, dest = my.temporary.zipped.file)
unzip(my.temporary.zipped.file, exdir = my.temporarary.zipped.folder)

# Create Name of extracted file
filename <- sprintf('%s/%s', my.temporarary.zipped.folder, spreadsheet.name) 


# Import tbl 5-1 on sheet 1 of workbook
econ.deflator <- read_excel(path = filename, sheet = 1, skip = 3)


# Cleaning and Shaping ----------------------------------------------------------------


# Shaping
  # Make all colnames r-friendly
  colnames(econ.deflator) <- make.names(colnames(econ.deflator))
  
  # Shape the Width: Select Columns to use
  econ.deflator <- econ.deflator %>% 
    select(Fiscal.Year, Gross.Domestic.Product1,Consumer.Price.Index..CPI.W.2,Dept.of.Defense.Non.Pay, Dept.of.Defense.Purchases3, Total.Department.of.Defense)
  
  # Shape the Length: omit bottom notes (save notes for later)
  # Save Notes
  econ.deflator.notes <- econ.deflator[54:56,1]
  
  # Cut off bottom rows
  econ.deflator <- econ.deflator[1:52,]
  
  # Convert to Table df
  econ.deflator <- tbl_df(econ.deflator)

# Clean: FY Column
# Remove dots and spaces, convert to numeric
  econ.deflator$Fiscal.Year <- as.numeric(str_replace_all(econ.deflator$Fiscal.Year, "[ ,.]", ""))
  
# Rename FY with dplyr! Awesome. (note..the new variable precedes the old one)
  econ.deflator <- rename(econ.deflator, FY = Fiscal.Year)

# Add 'No Deflator' Column
econ.deflator$No.Deflator <- 100

# Gather and Tidy
  econ.deflator <- gather(econ.deflator, econ.deflator.name, econ.deflator.value, -FY, convert = TRUE)


# Add MetaData ------------------------------------------------------------
# Add Base year column and source
  econ.deflator$Deflator.Base.Year <- 2017
  econ.deflator$Deflator.Source <- "Table 5-1: Department of Defense and Selected Economy-Wide Indices"


# Assign Notes
econ.deflator <- econ.deflator %>% 
  mutate(Deflator.Notes = ifelse(grepl("Gross.Domestic.Product1", econ.deflator$econ.deflator.name), yes = as.character(econ.deflator.notes[1,1]), 
                 ifelse(grepl("Consumer.Price.Index..CPI.W.2", econ.deflator.name), yes = as.character(econ.deflator.notes[2,1]),
                 ifelse(grepl("Dept.of.Defense.Purchases3", econ.deflator.name), yes = as.character(econ.deflator.notes[3,1]), no = "None")))) 
         
# Remove numbers the name column
econ.deflator$econ.deflator.name <- str_replace_all(econ.deflator$econ.deflator.name, "[0-9]", "")

# Correct CPI variable 
econ.deflator$econ.deflator.name <- gsub(pattern = "Consumer.Price.Index..CPI.W.", replacement = "Consumer.Price.Index.CPI.W", x = econ.deflator$econ.deflator.name)



# Data Corrections --------------------------------------------------------
econ.deflator<- econ.deflator %>% 
  mutate(econ.deflator.value = econ.deflator.value/100)


# Export as .csv ------------------------------------------------------------------

# Filename
mylocation <- "../Data/Processed"
myfilename <- "tbl.5.1_Deflators.Economy.Wide"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")

my.file <- sprintf("%s/%s_%s.csv", mylocation, myfilename, mydate)

# Export
 write_csv(econ.deflator, my.file)


  
  
  
  
  



















