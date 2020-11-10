
# Info --------------------------------------------------------------------
#' Table 6.3 TOA by Category

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
          my.filename <- "FY21 PB Green Book Table 6-3.xlsx"

#'   5) Set current year
          current.FY <- 2021
          
#'   6) Identify rows for constant and current
        valid.rows <- c( 7:11, #<--  Current
                        14:18) #<-- Curent Non.Pay

        valid.names <- rep(c("army",
                             "navy",
                             "air.force",
                             "defense.wide",
                             "OCO.place.holder"),
                                           2)
            
#' 7) Identify Metacols
          budget.type        <-     "TOA"
#         spending.category  <-     "pay.or.non.paycategory"
          source.table       <-     "tbl.6.03.TOA.by.Military.Dept" 
        
          
#'  8) What's the name of the first column?
        first.col <- "military.dept"
         
                
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(stringr)

# Common Vars and Functions -----------------------------------------------

import.file.name <- paste0(chapter.6.raw.data.folder, 
                    my.filename)   

mylocation <- "./Data/Processed"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")
my.export.filename <- sprintf("%s/%s_%s.csv", mylocation, source.table, mydate)


# Methodology -------------------------------------------------------------
#' Cut up the datasets, combine, then clean all
         
          #Import    
          df.1 <- read_excel(import.file.name, na = "0", col_names=F, n_max=29)
          
          #Shaping: Only valid Rows (see "how to update this file/ valid.rows" above)
          df.1 <- df.1[valid.rows,]
          
          # Shaping: Remove unneeded  cols
          df.1 <- df.1[, c(-2:-3)]  
  
      # Reshaping-#-#-#-#-#-#-#--#-#-#--#-#-#-
            
          #Rename Cols
          FY.colnames <- str_c(1948:(1948+(ncol(df.1)-2) )) #<FY must be text (for tidy)
          colnames(df.1) <- c(first.col,      #<First col var
                                    FY.colnames) 
          
          # Rename (see above)
          df.1[,1] <- valid.names

          # NAs to zeros
          df.1[is.na(df.1)] <- 0

      #Add meta cols-#-#-#-#-#-#-#--#-#-#--#-#-#-
          
          #new identifying cols
          df.1$budget.type        <-     budget.type 
          #df.1$spending.category  <-     spending.category
          df.1$source.table       <-     source.table 
          
          #Add constant,current col 
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
                 #spending.category,
                 everything() )                    
                               
      #Tidy and formatting-#-#-#-#-#-#-#--#-#-#--#-#-#-
         
          #Tidy
          df.2 <- df.1 %>% 
            gather(FY, amount, -1:-4)

          #adjust numbers
          df.2$amount <- as.numeric(df.2$amount)*1e6

          final.df <- df.2
           

# Export as .csv -----------------------------------------------------

# Export
write_csv(final.df, my.export.filename)


