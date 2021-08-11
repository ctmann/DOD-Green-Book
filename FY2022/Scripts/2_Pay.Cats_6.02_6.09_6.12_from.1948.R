
# Info --------------------------------------------------------------------
#' Table 6.9 BA by Pay Category

#' # How to Update this File -------------------------------------------------

#' #-#-#-#-#-#-# THIS WILL NOT CHANGE #-#-#-#-#-#-#-#-#-#-#-#-#-#
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
        row.range <- c( 
                         7:9, #<--  Current Pay
                        11:14, #<-- Curent Non.Pay
                        #16,   #<--  Legacy: Current.OCO.placeholder
                        18:20, #<-- Constant Pay
                        22:25) #<-- Contant Non.Pay
                        #28)  #<-- Legacy: Constant.OCO.placeholder

        row.names <- rep(c("civilian.pay",
                            "military.pay",
                            "military.retired.pay.accrual",
                            "medicare.eligible.retired.health.fund.contributions",
                            "other.military.personnel",
                            "non.pay.operations",
                            "non.pay.investment"),
                                           2)

#'  Preferred Export filename
    export.filename <- "2_Pay.Category.since.1948_BA.Outlays.TOA"
      
                
#' #-#-#-#-#-#-# Indvidual File Info May Change #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#'       Update Name of specific files
         my.filename.1 <- "FY22 PB Green Book Table 6-2.xlsx" #<-- TOA by pay category
         my.filename.2 <- "FY22 PB Green Book Table 6-9.xlsx" #<-- BA by pay category
         my.filename.3 <- "FY22 PB Green Book Table 6-12.xlsx" #<-- Outlays by pay category

 #'       Update Name of specific files
         budget.type.1        <-     "TOA"
         budget.type.2        <-     "BA"
         budget.type.3        <-     "Outlays"

#'       Update Name of specific files
         source.table.1       <-     "tbl.6.02.TOA.by.Pay.Category" 
         source.table.2       <-     "tbl.6.09.BA.by.Pay.Category" 
         source.table.3       <-     "tbl.6.12.BA.by.Pay.Category" 
        
                
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
                         valid.row.range = row.range,
                         valid.row.names = row.names) {

# Import -------------------------------------------------------------
          import.file.name <- paste0(chapter.6.raw.data.folder, 
                                    my.filename)  
            #Import    
          df.1 <- read_excel(import.file.name, col_names=F)
    
# Reshaping ---------------------------------------------------------------

          #Basic Shaping: Only valid Row, cols
          df.1 <- df.1[valid.row.range,c(-2:-3)]
    
          # FY Colnames (based on size of dataset, incl. blank cols!!!--brittle)
          FY.colnames <- str_c(1948:(1948+(ncol(df.1)-2) )) #<FY must be text (for tidy)
          colnames(df.1) <- c("pay.category",      #<First col is public law title
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
          
          # Repeat for Pay/non.Pay    
              df.1$pay.type <-     rep(
                                       c(rep("pay", 3),
                                         rep("non.pay", 4)),
                                       2)

        #Rearrange
         df.1 <- df.1 %>% 
           select(
                 source.table,
                 budget.type, 
                 deflator.type,
                 pay.category,
                 pay.type,
                 everything() )                    
                               
      #Tidy and formatting-#-#-#-#-#-#-#--#-#-#--#-#-#-
         
          #Gather
          df.2 <- df.1 %>% 
            gather(FY, amount, -1:-5)

          #adjust numbers
          df.2$amount <- as.numeric(df.2$amount)*1e6

          final.df <- df.2
          return(final.df)
}

# Export as .csv -----------------------------------------------------

combined.pay.category <- bind_rows(
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
          
write_csv(combined.pay.category, my.export.filename)


