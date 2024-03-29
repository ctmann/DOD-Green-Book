#' Military Dept. from 1948
#' Includes discretionary and mandatory
#' 
#' 6.03 TOA
#' 6.10 BA
#' 6.13 Outlays
#' 
#' # How to Update this File -------------------------------------------------

#' #-#-#-#-#-#-# THIS WILL PROBABLY NOT CHANGE #-#-#-#-#-#-#-#-#-#-#-#-#-#
#' 
#'   Download Comptroller data to Raw folder (manually).
#'
#'   Set working Directory to current year:
        setwd("./DOD-Green-Book/FY2022")
#'
#'   Chapter 6 raw data folder, files
        chapter.6.raw.data.folder <- "./Data/Raw/FY22 PB Green Book Chap 6/"
#' 
#'  Identify rows for constant and current
        row.range <-  c( 7:10, #<--  Current
                        13:16) #<--  Curent Non.Pay

        row.names <-  rep(c("army",
                             "navy",
                             "air.force",
                             "defense.wide"),
                                           2)
#' name of first column
        first.col.name <- "military.dept"
        
#'  Preferred Export filename
    export.filename <- "3_Military.Dept.since.1948_BA.Outlays.TOA"
      
                
#' #-#-#-#-#-#-# Indvidual File Info May Change #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#'       Update Name of specific files
         my.filename.1 <- "FY22 PB Green Book Table 6-3.xlsx" #<--   TOA by military dept
         my.filename.2 <- "FY22 PB Green Book Table 6-10.xlsx" #<--  BA by military dept
         my.filename.3 <- "FY22 PB Green Book Table 6-13.xlsx" #<--  Outlays by military dept

 #'       Update Name of specific files
         budget.type.1        <-     "TOA"
         budget.type.2        <-     "BA"
         budget.type.3        <-     "Outlays"

#'       Update Name of specific files
         source.table.1       <-     "tbl.6.03.TOA.by.Public.Law" 
         source.table.2       <-     "tbl.6.10.BA.by.Public.Law" 
         source.table.3       <-     "tbl.6.13.Outlays.by.Public.Law" 

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(stringr)

# Superfunction -----------------------------------------------------------
clean.function <- function(
                         my.filename,      #<- changes
                         source.table,     #<- changes
                         budget.type,      #<- changes
                         folder = chapter.6.raw.data.folder,
                         intro.col.name = first.col.name,
                         valid.row.range = row.range,
                         valid.row.names = row.names) {

# Import -------------------------------------------------------------
          # construct filename
          import.file.name <- paste0(chapter.6.raw.data.folder, 
                                    my.filename)  
          #Import    
          df.1 <- read_excel(import.file.name, col_names=F)
    
# Reshaping ---------------------------------------------------------------

          #Basic Shaping: Only valid Row, cols
          df.1 <- df.1[valid.row.range,c(-2:-3)]
    
          # FY Colnames (based on size of dataset--brittle)
          FY.colnames <- str_c(1948:(1948+(ncol(df.1)-2) )) #<FY must be text (for tidy)
          
          colnames(df.1) <- c(intro.col.name,     #<First colname
                              FY.colnames) 
          
          # Rename left col (header rows)
          df.1[,1] <- valid.row.names

          # NAs to zeros
          df.1[is.na(df.1)] <- "0"

      #Add meta cols-#-#-#-#-#-#-#--#-#-#--#-#-#-
          
          #new identifying cols
          df.1$source.table       <-     source.table 
          df.1$budget.type        <-     budget.type 
          
          #Current/Constant
          # Divide nrows in two; tag first half as "current", and second "constant"    
              repeat.deflator.type.times <- nrow(df.1)/2
              repeat.deflator.type <- c( rep("current", repeat.deflator.type.times),
                                         rep("constant", repeat.deflator.type.times) )
              df.1$deflator.type <-repeat.deflator.type
          
        #Rearrange
         df.1 <- df.1 %>% 
           select(
                 source.table,
                 budget.type, 
                 deflator.type,
                 1,  #<- this might vary, so id by position
                 everything() )                    
                               
      #Tidy and formatting-#-#-#-#-#-#-#--#-#-#--#-#-#-
         
          #Gather
          df.2 <- df.1 %>% 
            gather(FY, amount, -1:-4)

          #adjust numbers
          df.2$amount <- as.numeric(df.2$amount)*1e6

          final.df <- df.2
          return(final.df)
}

# Export as .csv -----------------------------------------------------

combined <- bind_rows(
                            #tbl 6-2 TOA
                            clean.function(my.filename  = my.filename.1,
                                           source.table = source.table.1,
                                           budget.type  = budget.type.1),
                            #tbl 6-9 BA
                            clean.function(my.filename  = my.filename.2,
                                           source.table = source.table.2,
                                           budget.type  = budget.type.2), 
                            #tbl 6-12 Outlays
                            clean.function(my.filename  = my.filename.3,
                                           source.table = source.table.3,
                                           budget.type  = budget.type.3)    
                                      )

# Export

mylocation <- "./Data/Processed"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")
my.export.filename <- sprintf("%s/%s_%s.csv", mylocation, export.filename, mydate)          
          
write_csv(combined, my.export.filename)



