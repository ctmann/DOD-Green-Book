#FY2020 update: missing negative values (total receipts)
# because "total" probably filtered out

#' Original DoD Comptroller zip file downloaded here:
#' http://comptroller.defense.gov/BudgetMaterials.aspx
#' To View as pd:
#' http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2019/FY19_Green_Book.pdf
#' 
#' To download as zip
#' http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2019/FY_2019_Green_Book.zip
#' 
#' 
#' Table 6.19-6.21 DOD BA By by Service and Title
#' 
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)
library(nettles.toolbox)

# Download Function -----------------------------------------------------------

#' # Import  Data ------------------------------------------------------------
x <- "http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2020/FY_2020_Green_Book.zip"
y <- "FY20 PB Green Book Chap 6/" 

nettle_downzip <- function(zip.url, zip.file){
    my.temporary.zipped.file <- tempfile()   # Zip file will go in here
    my.temporarary.zipped.folder <- tempdir() # Unzipped file will go in here
    download.file(zip.url, dest = my.temporary.zipped.file) # Download Source Data to Temp file
    unzip(my.temporary.zipped.file, exdir = my.temporarary.zipped.folder) # Unzip to Temp directory
    location.of.unzipped.file <- paste0(my.temporarary.zipped.folder,"/", zip.file)
    return(location.of.unzipped.file )
    }

my.filename <- "./Data/Raw/FY_2020_Green_Book/FY20 PB Green Book Chap 6/"

# Clean Function ----------------------------------------------------------

nettle_clean <- function(location.of.file, my.table.number, dod.service){

  # Due to missing column names, read_excel requires col_names = False
  orig <-  read_excel(location.of.file, 
        na = "0", skip = 4, col_names = FALSE)

  # Basic Shaping (remove blank cols)
  orig <- orig %>% 
    select(-2,-3) 

  # Easy to create Universal Col Header
    n <-  1948:2024
    my.col.header <- c("Public Law Title", n, "deflator.type")

  # Remove trailing dots from first col
    orig[,1] <- str_trim(str_replace_all(orig$X__1, "[0-9.]+", ""))

  ## End orig
  
  ## Begin current  
    current <- orig %>% 
      slice(1:9) %>% 
      slice(-1) %>% 
      mutate(delator.type = "Current")
  
  # Rename Col Headers
  names(current) <- my.col.header

  ##< End Current>
  

  ### Begin Constant
  constant  <- orig %>% 
    slice(12:19) %>% 
      mutate(delator.type = "Constant")
  
  # Rename Col Headers
    names(constant) <- my.col.header
  
    ##<End Constant
 
    
  # Combine Current and Constant Datasets
  combined <- bind_rows(constant, current)
  
  
  # Tidy -----------------------------------------------------------------
  tidied <- combined %>% 
    gather(FY, Amount, -`Public Law Title`,-deflator.type) 
  
  # Format Fixing ------------------------------------------------------------------
  # Convert String to Millions
  tidied$Amount <- as.numeric(tidied$Amount)
  tidied$Amount <- tidied$Amount *1e6
  
  # Replace NAs with 0
  tidied <- tidied %>% 
    replace_na(list(Amount = 0))
  
  # Meta --------------------------------------------------------------
  # Add Source Notes
  tidied$data.notes <- "All enacted war and supplemental funding is includedl; Includes both discretionary and mandatory "
  tidied$source.table <- my.table.number
  tidied$Service <- my.dod.service
  
    return(tidied)
  }
    

# Use  -----------------------------------------------------------------

# Download the file with nettle function (Assume Identical Shape of Separate Tabs)
  #   nettle_clean requires four inputs
  #   the last two are for print purposes
  #   Inputs: zip.url, spreadsheet.name, my.table.number, dod.service

  # Download 6-19
    location.of.file <- paste0(my.filename,"FY20 PB Green Book Table 6-19.xlsx")
    my.table.number <- "tbl.6-19"
    my.dod.service <- "Army"
    army <- nettle_clean(location.of.file = location.of.file, my.table.number, my.dod.service)
  
  # Download 6-20  
    location.of.file <- paste0(my.filename,"FY20 PB Green Book Table 6-20.xlsx")
    my.table.number <- "tbl.6-20"
    my.dod.service <- "Navy"
    navy <- nettle_clean(location.of.file = location.of.file, my.table.number, my.dod.service)
  
  # Download 6-21  
    location.of.file <- paste0(my.filename,"FY20 PB Green Book Table 6-21.xlsx")
    my.table.number <- "tbl.6-21"
    my.dod.service <- "Air.Force"
    air.force <- nettle_clean(location.of.file = location.of.file, my.table.number, my.dod.service)
  

# tidy --------------------------------------------------------------------

ba.by.title <- bind_rows(army, navy, air.force) 

    #Rearrange to sensible order
    ba.by.title <- ba.by.title %>% 
      select(Service,
             `Public Law Title`,
             FY,
             deflator.type,
             Amount,
             source.table,
             data.notes)

# Complete and Export as .csv -----------------------------------------------------

# Filename
mylocation <- "./Data/Processed"
myfilename <- "tbl.6.19-6.21_BA.by.Service.and.Title"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")

my.file <- sprintf("%s/%s_%s.csv", mylocation, myfilename, mydate)

# Export
write_csv(ba.by.title, my.file)











