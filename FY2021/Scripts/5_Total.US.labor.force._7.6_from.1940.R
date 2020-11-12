#' Chapter 7: 
# Info --------------------------------------------------------------------
#' 
#' 7.6 U.S. labor force
#' 
#' TOTALS COLUMNS
#'   Total DOD Employment = Active Duty military + DOD Civilian Direct Hires
#'   
#'   Total Federal Civilian Employment = DOD Civilian Direct Hires + 
#'                                       Other Federal Civil Service
#'   
#'   Total Federal Employment =  Active Duty military + 
#'                               DOD Civilian Direct Hires + 
#'                               Other Federal Civil Service + 
#'                               Coast Guard
#' 
#'   Total US Employment = Total Federal Employment + (see above)
#'                         State and Local Gov't + 
#'                         U.S. Private Sector Employment
#' 
#'   Total US Labor Force = not sure? remove this and all unemployment. There are better sources.
#' 
# # Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(stringr)

#' # How to Update this File -------------------------------------------------
#'
#'  #-#-#-#-#-#-#-#- Change this stuff #-#-#-#-#-#-#-#-#-#-#-#-
#'   -> Download Comptroller data to Raw folder (manually).
#'
#'  -> Identify current year
       current.fy <- 2021  
#'  
#'  
#'   -> Change year, verify file prefix (names generated automatically in tibble)
        raw.filename <- "./Data/Raw/FY21 PB Green Book Chap 7.xlsx"
        tab.name     <- "7-6"
#'
#'   ->Set working Directory to current year:
        setwd("./DOD-Green-Book/FY2021")
        
#'   -> Verify Export name
        export.filename <- "..."  
        
#'  #-#-#-#-#-#-#-#- This probably will NOT change #-#-#-#-#-#-#-#-#-#-#-#-
#' Includes table ranges and names
#' 
      #' Verify Tables #-#-#-#-#-#-#
      #' 
              #'  Tables 7.6
                      my.colrange <- c( 7:13)  #<-- Current dataset

                      my.colnames <- c (    "FY",
                                            "active.duty.military",
                                            "dod.civilian.direct.hires",
                                            "total.dod.employment",
                                            "other.federal.civil.service",
                                            "total.federal.civilian.employment",
                                            "coast.guard",
                                            "total.federal.employment",
                                            "state.and.local.government",
                                            "total.us.public.employment",
                                            "us.private.sector.employment",
                                            "total.us.employment",
                                            "us.unemployment",
                                            "total.us.labor.force")
      #'
        
# Common Vars -------------------------------------------------------------
first.fy <- 1940
                      
# Import ------------------------------------------------------------------
df.1 <- read_excel(raw.filename,
                   sheet = tab.name,
                   skip= 5,
                   col_names=F) 

# Reshaping ---------------------------------------------------------------
df.1 <- df.1[,-2]
                      
df.1 <- df.1[grepl("^[0-9]{4}", df.1$...1) & 
        !grepl(".*(Base|OCO)", df.1$...1), ]  

# renaming
df.1[ ,1 ]  <- as.character(first.fy:current.fy) 
names(df.1) <- my.colnames

#remove columns with Total in name
df.1 <- df.1[, !grepl("total", names(df.1) ) ]

#remove untrustworthy us.unemployment
df.1 <- df.1[, !grepl("us.unemployment", names(df.1) ) ]

# NAs to zeros
df.1[is.na(df.1)] <- 0


df.2 <- gather(df.1, key = employment.category, 
               value = amount, 
               -FY) %>% 
        mutate(amount = amount *1e3)






                        

                      
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        