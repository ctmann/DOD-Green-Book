#' Original DoD Comptroller zip file downloaded here:
#' http://comptroller.defense.gov/BudgetMaterials.aspx
#' To View as pd:
#' http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2017/FY17_Green_Book.pdf
#' 
#' Table 6.19-6.21 DOD BA By by Service and Title
#' 
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)
library(nettles.toolbox)


# Download Function -----------------------------------------------------------

# Sub Function to create filename from downloaded location
  nettle_file.creator <- function(my.zip.folder, table.file.name){
   my.complete.filename <- paste0(my.zip.folder,"/FY17 PB Green Book Chap 6/", table.file.name)
    return(my.complete.filename)
  }
  
# Download Function: Outputs file location
nettle_dl.zip <- function(zip.url, table.file.name){
  #Create Temporary Scaffolding - download to temp dir
    my.temporary.zipped.file <- tempfile()
    my.temporarary.zipped.folder <- tempdir()
  # Download Location
  url <- zip.url
#Download Source Data to Temp Location
  download.file(url = url, dest = my.temporary.zipped.file)
  unzip(my.temporary.zipped.file, exdir = my.temporarary.zipped.folder)
#return(my.temporarary.zipped.folder)
  
#- Subfunction to output name of file  
  return(nettle_file.creator(my.temporarary.zipped.folder, table.file.name))
  
}



# Clean Function ----------------------------------------------------------

  nettle_clean <- function(zip.url, spreadsheet.name, my.table.number, dod.service){
    
    filename <- nettle_dl.zip(zip.url,table.file.name = spreadsheet.name )
  
    
#    my.temporarary.zipped.folder <- nettle_dl.zip(zip.location)  
  # Create Name of extracted file
 #   filename <- sprintf('%s/%s', my.temporarary.zipped.folder, spreadsheet.name) 

  # Due to missing column names, read_excel requires col_names = False
  orig <-  read_excel(filename, 
        na = "0", skip = 4, col_names = FALSE)

  # Basic Shaping (remove blank cols)
  orig <- orig %>% 
    select(-2,-3) 

  # Easy to create Universal Col Header
    n <-  1948:2021
    my.col.header <- c("Public Law Title", n, "deflator.type")

  # Remove trailing dots from first col
    orig[,1] <- str_trim(str_replace_all(orig$X0, "[0-9.]+", ""))

  ## End orig
  
  ## Begin current  
    current <- orig %>% 
      slice(1:10) %>% 
      slice(-1:-2) %>% 
      mutate(delator.type = "Current")
  
  # Rename Col Headers
  names(current) <- my.col.header

  ## End Current
  
  ### Begin Constant
  constant  <- orig %>% 
    slice(13:20) %>% 
      mutate(delator.type = "Constant")
  
  # Rename Col Headers
    names(constant) <- my.col.header
  
    ## End Constant
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
  tidied$data.notes <- "All enacted war and supplemental funding is included"
  tidied$source.table <- my.table.number
  tidied$soure.file <- spreadsheet.name
  tidied$Service <- dod.service
  
  return(tidied)
  }
    

# Use  -----------------------------------------------------------------

# Download the file with nettle function
  #   nettle_clean requires four inputs
  #   the last two are for print purposes
  #   Inputs: zip.url, spreadsheet.name, my.table.number, dod.service

  # Download 6-19
    zurl <- "http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2017/FY_2017_Green_Book.zip"
    my.spreadsheet <- "FY17 6-19_Army BA by Title.xlsx"
    my.table.number <- "tbl.6-19"
    my.dod.service <- "Army"
  
    army <- nettle_clean(zurl, my.spreadsheet, my.table.number, my.dod.service)
  
  # Download 6-20  
    zurl <- "http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2017/FY_2017_Green_Book.zip"
    my.spreadsheet <- "FY17 6-20_Navy BA by Title.xlsx"
    my.table.number <- "tbl.6-20"
    my.dod.service <- "Navy"
  
    navy <- nettle_clean(zurl, my.spreadsheet, my.table.number, my.dod.service)
  
  
  # Download 6-21  
    zurl <- "http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2017/FY_2017_Green_Book.zip"
    my.spreadsheet <- "FY17 6-21_Air Force BA by Title.xlsx"
    my.table.number <- "tbl.6-21"
    my.dod.service <- "Air.Force"
  
    air.force <- nettle_clean(zurl, my.spreadsheet, my.table.number, my.dod.service)
    

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
             data.notes,
             source.file = soure.file)

# Export ------------------------------------------------------------------
nettle_export(ba.by.title,"6.19-6.21_BA.by.Service.and.Title")
    









