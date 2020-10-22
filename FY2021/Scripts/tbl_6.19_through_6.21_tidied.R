
#' Table 6.19-6.21 DOD BA By by Service and Title
#' 
#' 
#' Download raw files to temp folder locally for processing
#' Raw files stored in subfolder are not necessary
#' 
#' # How to Update this File -------------------------------------------------
#' 
#'   1) Set working Directory to current year:
          setwd("./DOD-Green-Book/FY2021")
#' 
#'   2) Download Comptroller data to Raw folder (manually).
#'   
#'   3) Update name of Chapter 6 raw data folder, files
          chapter.6.raw.data.folder <- "./Data/Raw/FY21 PB Green Book Chap 6/"
#' 
#'   4) Update Name of specific files
          tbl.6.19.Army <- "FY21 PB Green Book Table 6-19.xlsx"
          tbl.6.20.Navy <- "FY21 PB Green Book Table 6-20.xlsx"
          tbl.6.21.USAF <- "FY21 PB Green Book Table 6-21.xlsx"

#'   5) Set current year
          current.FY <- 2021

#'  6) Shape of each df may change. Currently:
#'       Current dollars:  7 (MILPERS) :14 (Trust, Receipts, and Other)
#'       Constant Dollars: 16 (MILPERS):24 (Trust, Receipts, and Other)

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)

# Comm Vars, Functions ---------------------------------------------------------------
    # None
          
  #Testing Purposes          
  #file.name <- paste0(file.location, tbl.6.19.Army)


# Create Clean Function ----------------------------------------------------------

nettle_clean <- function(file.name, 
                         my.table.number, 
                         dod.service,
                         current.FY){
  
  # Due to missing column names, read_excel requires col_names = False
    orig <-  read_excel(paste0(file.name), 
    na = "0", skip = 4, col_names = TRUE)
  
    ##--Shaping Paramters may change (Current/Constant)

  # Basic Shaping (remove blank cols)
    orig <- orig %>% 
    select(-2,-3) 

  # create Universal Col Header
    my.col.header <- c("Public Law Title", fiscal.years)
    names(orig) <- my.col.header

  ## End orig---
    
  # !!DANGER: ASSUMES DF ROWS (CURRENT/CONSTANT) SAME SIZE FROM YEAR TO YEAR  
    current <- orig %>% 
      slice(2:9) %>% #< This assumes no more accounts will be added. In FY2020, one was.
      mutate(deflator.type = "current")

  ## Splice off "Constant Dollars" amounts
  constant  <- orig %>% 
    slice(12:19) %>% #< This assumes no more accounts will be added. In FY2020, one was.
      mutate(deflator.type = "constant")

  ##--COMBINE Current/Constant
  
  # Combine Current and Constant Datasets
  combined <- bind_rows(constant, current)

  # Tidy 
  tidied <- combined %>% 
    gather(FY, Amount, -`Public Law Title`,-deflator.type) 
  
  # Format Fixing 
  # Convert String to Millions
  tidied$Amount <- as.numeric(tidied$Amount)
  tidied$Amount <- tidied$Amount *1e6
  
  # Replace NAs with 0
  tidied <- tidied %>% 
    replace_na(list(Amount = 0))
  
  # Meta --------------------------------------------------------------
  # Add Source Notes
  tidied$data.notes <- "All enacted war and supplemental funding is included; Includes both discretionary and mandatory "
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
    file.name <- paste0(chapter.6.raw.data.folder,tbl.6.19.Army)
    my.table.number <- "tbl.6-19"
    my.dod.service <- "Army"
    army <- nettle_clean(file.name = file.name, 
                         my.table.number, 
                         my.dod.service)
  
  # Download 6-20  
    file.name <- paste0(chapter.6.raw.data.folder,tbl.6.20.Navy)
    my.table.number <- "tbl.6-20"
    my.dod.service <- "Navy"
    navy <- nettle_clean(file.name = file.name, 
                         my.table.number, 
                         my.dod.service)
  
  # Download 6-21  
    file.name <- paste0(chapter.6.raw.data.folder, tbl.6.21.USAF)
    my.table.number <- "tbl.6-21"
    my.dod.service <- "Air.Force"
    air.force <- nettle_clean(file.name = file.name, 
                         my.table.number, 
                         my.dod.service)
  
  
# tidy --------------------------------------------------------------------

ba.by.title <- bind_rows(army, navy, air.force) %>% 
      filter(complete.cases(.)) %>% 
      mutate(`Public Law Title` = str_replace_all(`Public Law Title`, "\\.","") %>% str_trim() ) 

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
myfilename <- "GBook_tbl.6.19-6.21_BA.by.Service.and.Title"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")

my.file <- sprintf("%s/%s_%s.csv", mylocation, myfilename, mydate)

# Export
write_csv(ba.by.title, my.file)











