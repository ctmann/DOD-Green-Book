#' Original DoD Comptroller zip file downloaded here:
#' http://comptroller.defense.gov/BudgetMaterials.aspx
#' To View as pd:
#' http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2017/FY17_Green_Book.pdf
#' 
#' Table 6.19-6.21 DOD BA By by Service
#' 
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(stringr)


# Download ----------------------------------------------------------------
#Create Temporary Scaffolding - download to temp dir
  my.temporary.zipped.file <- tempfile()
  my.temporarary.zipped.folder <- tempdir()

# Download Location
  url <- "http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2017/FY_2017_Green_Book.zip"
#Download Source Data to Temp Location
  download.file(url = url, dest = my.temporary.zipped.file)
  unzip(my.temporary.zipped.file, exdir = my.temporarary.zipped.folder)

  
# Extract -----------------------------------------------------------------
# Must know spreadsheet name before extraction
  spreadsheet.name <- "FY17 PB Green Book Chap 6/FY17 6-19_Army BA by Title.xlsx"

# Create Name of extracted file
  filename <- sprintf('%s/%s', my.temporarary.zipped.folder, spreadsheet.name) 

# Extract
# Due to missing column names, read_excel requires col_names = False
tbl.6_19 <-  read_excel(filename, 
      na = "0", skip = 4, col_names = FALSE)

# Basic Shaping (remove blank cols)
tbl.6_19 <- tbl.6_19 %>% 
  select(-2,-3) 

# Easy to create Universal Col Header
  n <-  1948:2021
  my.col.header <- c("Public Law Title", n, "deflator.type")

# Remove trailing dots from first col
tbl.6_19[,1] <- str_trim(str_replace_all(tbl.6_19$X0, "[0-9.]+", ""))

# Shape -------------------------------------------------------------------
# Current Dollars Dataset
tbl.6_19.cur <- tbl.6_19 %>% 
  slice(1:10) %>% 
  slice(-1:-2) %>% 
  mutate(delator.type = "Current")
  
View(tbl.6_19.cur)

# Rename Col Headers
names(tbl.6_19.cur) <- my.col.header

# Repeat for Constant
tbl.6_19.cons <- tbl.6_19 %>% 
  slice(13:20) %>% 
    mutate(delator.type = "Constant")
# Rename Col Headers
  names(tbl.6_19.cons) <- my.col.header
#View(tbl.6_19.cons)

# Combine Current and Constant Datasets
tbl.6_19.comb <- bind_rows(tbl.6_19.cons, tbl.6_19.cur) 
View(tbl.6_19.comb)

# Tidy -----------------------------------------------------------------
tbl.6_19.tidy <- tbl.6_19.comb %>% 
  gather(FY, Amount, -`Public Law Title`,-deflator.type) 
#glimpse(tbl.6_19.tidy)

# Format Fixing ------------------------------------------------------------------

# Convert String to Millions
tbl.6_19.tidy$Amount <- as.numeric(tbl.6_19.tidy$Amount)
tbl.6_19.tidy$Amount <- tbl.6_19.tidy$Amount *1e6

# Replace NAs with 0
tbl.6_19.tidy <- tbl.6_19.tidy %>% 
  replace_na(list(Amount = 0))


# Final Meta --------------------------------------------------------------
# Add Data Notes
tbl.6_19.tidy$data.notes <- "All enacted war and supplemental funding is included"

# Add Source Notes
tbl.6_19.tidy$data.notes <- "FY17 6-19_Army BA by Title"

View(tbl.6_19.tidy)


# Export ------------------------------------------------------------------
# Filename
mylocation <- "../Data/Processed"
myfilename <- "tbl.6.8_DOD.BA.By.Title"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")

my.file <- sprintf("%s/%s_%s.csv", mylocation, myfilename, mydate)
write_csv(df.flat, my.file)
















