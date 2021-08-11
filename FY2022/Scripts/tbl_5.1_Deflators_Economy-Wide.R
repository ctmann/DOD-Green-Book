#' # How to Update this File -------------------------------------------------
#' 
#'   Set working Directory to current year:
          setwd("./DOD-Green-Book/FY2022")
#'   
#'   Update name of Chapter 6 raw data folder, files
          raw.data.folder <- "./Data/Raw/"
#' 
#'   Update Name of specific files
          my.filename <- "FY22 PB Green Book Chap 5.xlsx"

#' #' Table 5-1: Department of Defense and Selected Economy-Wide Indices
#' (1970-2022)
#' Base Year = 2022
#' 
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(zoo)


#' # Import  Data ------------------------------------------------------------

my.filename <- paste0(raw.data.folder,my.filename)

# Import tbl 5-1 on sheet 1 of workbook
econ.deflator <- read_excel(path = my.filename, sheet = 1, skip = 3)


# Cleaning and Shaping ----------------------------------------------------------------

# Shaping
  # Make all colnames r-friendly
  colnames(econ.deflator) <- make.names(colnames(econ.deflator))
  
  # Shape the Width: Select Columns to use
  econ.deflator <- econ.deflator %>% 
    select(1,3:7)
  
  # Cut off bottom rows
  econ.deflator <- econ.deflator[1:57,]
  
  # Convert to Table df
  econ.deflator <- tbl_df(econ.deflator)

# Clean: FY Column
# Remove dots and spaces, convert to factor
  econ.deflator$Fiscal.Year <- as.factor(str_replace_all(econ.deflator$Fiscal.Year, "[ ,.]", ""))
  
# Rename 
  econ.deflator <- rename(econ.deflator, FY = Fiscal.Year)

# Add 'No Deflator' Column
econ.deflator$No.Deflator <- 100

# Gather and Tidy
  econ.deflator <- gather(econ.deflator, econ.deflator.name, econ.deflator.value, -FY)


# Add MetaData ------------------------------------------------------------
# Add Base year column and source
  econ.deflator$Deflator.Base.Year <- factor(2018)
  econ.deflator$Deflator.Source <- "Table 5-1: Department of Defense and Selected Economy-Wide Indices"


# Remove numbers the name column
econ.deflator$econ.deflator.name <- str_replace_all(econ.deflator$econ.deflator.name, "[0-9]", "")

# Correct CPI variable 
econ.deflator$econ.deflator.name <- gsub(pattern = "Consumer.Price.Index..CPI.W.", replacement = "Consumer.Price.Index.CPI.W", x = econ.deflator$econ.deflator.name)



# Data Corrections --------------------------------------------------------
econ.deflator <- econ.deflator %>% 
  mutate(econ.deflator.value = econ.deflator.value/100)


# Export as .csv ------------------------------------------------------------------

# Filename
mylocation <- "./Data/Processed"
myfilename <- "tbl.5.1_Deflators.Economy.Wide"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")

my.file <- sprintf("%s/%s_%s.csv", mylocation, myfilename, mydate)

# Export
write_excel_csv(econ.deflator, my.file)



















