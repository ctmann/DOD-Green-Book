
# Info --------------------------------------------------------------------
#' Table 6.14, 6.15 DOD Civilian

#' # How to Update this File -------------------------------------------------
#'
#' #-#-#-#-#-#-# CHANGE #-#-#-#-#-#-#-#-#-#-#-#-#-#
      
    current.FY <- 2020
    first.row.of.data <- 6 #omit col.header
#'
#' #-#-#-#-#-#-# THIS WILL NOT CHANGE #-#-#-#-#-#-#-#-#-#-#-#-#-#
#' 
#'   Download Comptroller data to Raw folder (manually).
#'
#'   Set working Directory to current year:
        setwd("./DOD-Green-Book/FY2021")
#'
#'   Chapter 6 raw data folder, files
        chapter.6.raw.data.folder <- "./Data/Raw/FY21 PB Green Book Chap 6/"
#' 
#'  Identify rows for constant and current
        col.names <- c(     "fy",
                            "us|direct.hire|general.schedule",
                            "us|direct.hire|wage.board",
                            "foreign|direct.hire|foreign.nationals.direct.hire",
                            "foreign|indirect.hire|foreign.national.indirect.hire")

#'  Preferred Export filename
    export.filename <- "5_defense.civilian.pay.since.1948"
    
#' #-#-#-#-#-#-# This MAY change  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#'       Update Name of specific files
         my.filename.1 <- "FY21 PB Green Book Table 6-14.xlsx" #<-- current
         my.filename.2 <- "FY21 PB Green Book Table 6-15.xlsx" #<-- constant
         
#       columns to be omitted (NA, total cols)
         omit.these.cols <- c(-2, #NAs
                              -5,  #Total US
                              -7,  #Total Direct Hires
                              -9)  #Total Civlian Pay


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(stringr) 
         
# Common Vars and Functions -----------------------------------------------

# filename        
import.file.name.1 <- paste0(chapter.6.raw.data.folder, 
                    my.filename.1)
import.file.name.2 <- paste0(chapter.6.raw.data.folder, 
                    my.filename.2)          


#ranges
  row.limit <-(current.FY + 6) - 1970  #<-begin in FY1970 
  my.range <- str_c("A",(first.row.of.data), 
                    ":", 
                    "I", row.limit + (first.row.of.data-1) )
  
  
# export labelling
mylocation <- "./Data/Processed"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")
my.export.filename <- sprintf("%s/%s_%s.csv", mylocation, export.filename, mydate)

# Methodology -------------------------------------------------------------
#' import based on range, bind both datasets, then clean
         
          #Import: assumes df are identical    
          df.1 <- bind_rows(
            read_excel(import.file.name.1,
                         range = my.range,
                         col_names = F),
            read_excel(import.file.name.2,
                        range = my.range,
                        col_names=F)
                            )
 # Reshaping-#-#-#-#-#-#-#--#-#-#--#-#-#-          
          # Shaping: Remove unneeded  cols
          df.1 <- df.1[, omit.these.cols] 
          
           # Rename (see above)
          names(df.1) <- col.names
          
# Add Clarifying and Meta Cols -#-#-#-#-#-#-#--#-#-#--#-#-#-

          #Assign FY 
          df.1[,1] <- str_c(rep(str_c(1970:(current.FY + 5) ),2) )

          #current/constant
          repeat.this.many.times <- (current.FY + 6)-1970
          current.constant.data <- c(rep("current",  repeat.this.many.times),
                                   rep("constant", repeat.this.many.times) )
          df.1$current.constant <- current.constant.data

          #Source.table
          source.table.data <- c(rep("tbl_6-14",  repeat.this.many.times),
                            rep("tbl_6-15", repeat.this.many.times) )
          df.1$source.table <- source.table.data

          
# Rearrange and tidy
          df.1 <- df.1 %>% 
            select(source.table,
                   fy,
                   current.constant,
                   everything() )
          
          df.2 <- df.1 %>% 
            pivot_longer(
              cols = contains("|"),
              names_to = "pay.type",
              values_to = "amount")
            
          #separate by delimiter
          df.2 <- df.2 %>% 
            separate(pay.type,
                     into= c("usa.or.foreign",
                             "direct.indirect.hire",
                             "pay.type"),
                     sep="\\|")
          
            #dollars.in.thousands
          df.2$amount <- df.2$amount * 1e3
          

  
  
  
  
  
  
  
  
  
  
  
  
  
