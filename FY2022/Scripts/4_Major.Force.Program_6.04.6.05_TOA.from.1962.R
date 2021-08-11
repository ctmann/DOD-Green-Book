
# Info --------------------------------------------------------------------
#' Table 6.4 and 6.5 TOA by Major Force Program (Current and Constant)

#' # How to Update this File -------------------------------------------------
#' 
#'   Set working Directory to current year:
          setwd("./DOD-Green-Book/FY2021")
#' 
#'   Download Comptroller data to Raw folder (manually).
#'   
#'   Update name of Chapter 6 raw data folder, files
          chapter.6.raw.data.folder <- "./Data/Raw/FY21 PB Green Book Chap 6/"
#' 
#'   Update Name of specific files
          my.filename.1 <- "FY21 PB Green Book Table 6-4.xlsx"
          my.filename.2 <- "FY21 PB Green Book Table 6-5.xlsx"
          
#'   Set current year
          current.FY <- 2021

#'First row of data (not colheader)
  first.row.of.data <- 6 
        
#' Identify Metacols
          budget.type       <-     "TOA"
          source.table.1    <-     "tbl.6.04_Current.TOA.by.Major.Force.Program"
          source.table.2    <-     "tbl.6.05_Constant.TOA.by.Major.Force.Program"
          source.table.all <-     "tbl.6.04.and.6.05_TOA.by.Major.Force.Program"

# names won't change annually?
        valid.names <- c(    "FY",
                             "strategic.forces",
                             "general.purpose.forces",
                             "C3.intel.and.space",
                             "mobility.forces",
                             "guard.and.reserve.forces",
                             "research.and.development",
                             "central.supply.and.maintenance",
                             "training.medical.and.other",
                             "administration.and.associated",
                             "support.of.other.nations",
                             "special.operations.forces",
                             "space",
                             "OCO.placeholder",
                             "other"
                             )  

#'  Preferred Export filename
    export.filename <- "4_Major.Force.Program.since.1962_TOA"
      
                                
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
  row.limit <- ( (current.FY + 4) - 1962) + first.row.of.data #<-begin in FY1962 
  my.range <- str_c("A",first.row.of.data, 
                    ":", 
                    "P", row.limit)

# export labelling
mylocation <- "./Data/Processed"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")
my.export.filename <- sprintf("%s/%s_%s.csv", mylocation, export.filename, mydate)


# Methodology -------------------------------------------------------------
#' Cut up the datasets, combine, then clean all
         
          #Import    
          df.1 <- bind_rows(
            read_excel(import.file.name.1,
                             range=my.range,
                       col_names=F),
            read_excel(import.file.name.2,
                             range=my.range,
                       col_names=F)
                            )
          
          # Shaping: Remove unneeded  cols
          df.1 <- df.1[, -2]  
  
      # Reshaping-#-#-#-#-#-#-#--#-#-#--#-#-#-
            
          #FY column
          df.1[,1] <- str_c(rep(str_c(1962:(current.FY + 4) ),2) )

          # Rename (see above)
          names(df.1) <- valid.names

          # NAs to zeros
          df.1[is.na(df.1)] <- 0

      #Add meta cols-#-#-#-#-#-#-#--#-#-#--#-#-#-
          
          #new identifying cols
          df.1$budget.type        <-    budget.type 
          df.1$deflator.type      <-    c( rep("current", nrow(df.1)/2),
                                           rep("constant",nrow(df.1)/2))
          
          df.1$source.table       <-    c( rep(source.table.1, nrow(df.1)/2),
                                           rep(source.table.2, nrow(df.1)/2))

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
            gather(major.force.program, amount, -1:-4)

          #adjust numbers
          df.2$amount <- as.numeric(df.2$amount)*1e6

          final.df <- df.2
           

# Export as .csv -----------------------------------------------------

# Export
write_csv(final.df, my.export.filename)


