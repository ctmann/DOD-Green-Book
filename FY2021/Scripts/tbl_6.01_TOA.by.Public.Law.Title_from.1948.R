
# Info --------------------------------------------------------------------
#' Table 6.1 TOA by Public Law Title

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
          tbl.6.1.TOA.by.Public.Law.Title <- "FY21 PB Green Book Table 6-1.xlsx"

#'   5) Set current year
          current.FY <- 2021
          
#'   6) Identify rows for constant and current
        valid.rows <- c( 7:16,  #<-- Current dataset
                        19:28) #<--  Constant dataset

        valid.names <-rep(c(  "MILPERS",
                              "Retired.Pay.Defense",
                              "O&M",
                              "Procurement",
                              "RDT.E",
                              "MILCON",
                              "family.housing",
                              "revolving.and.management",
                              "trust.receipts.and.other",
                              "OCO.placeholder"),
                                           2)
#' 7) Identify Metacols
          budget.type        <-     "TOA"
          spending.category  <-     "Public.Law.Title"
          source.table       <-     "tbl.6.01.TOA.by.Public.Law.Title" 
        
                
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(stringr)

# Common Vars and Functions -----------------------------------------------

import.file.name <- paste0(chapter.6.raw.data.folder, 
                    tbl.6.1.TOA.by.Public.Law.Title)   

mylocation <- "./Data/Processed"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")
my.file <- sprintf("%s/%s_%s.csv", mylocation, source.table, mydate)


# Methodology -------------------------------------------------------------
#' Cut up the datasets, combine, then clean all
         
          #Import    
          df.1 <- read_excel(import.file.name, na = "0", col_names=F, n_max=29)
          
          #Shaping: Only valid Rows (see "how to update this file/ valid.rows" above)
          df.1 <- df.1[valid.rows,]
          
          # Shaping: Remove unneeded spacer cols
          df.1 <- df.1[, c(-2:-3)]  
  
      # Reshaping-#-#-#-#-#-#-#--#-#-#--#-#-#-
            
          #Rename Cols
          FY.colnames <- str_c(1948:(1948+(ncol(df.1)-2) )) #<FY must be text (for tidy)
          colnames(df.1) <- c("public.law.title",      #<First col is public law title
                                    FY.colnames) 
          
          # Rename (see above)
          df.1$public.law.title <- valid.names

          # NAs to zeros
          df.1[is.na(df.1)] <- 0

      #Add meta cols-#-#-#-#-#-#-#--#-#-#--#-#-#-
          
          #new identifying cols
          df.1$budget.type        <-     budget.type 
          df.1$spending.category  <-     spending.category
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
                 budget.type, 
                 deflator.type,
                 spending.category,
                 source.table,
                 public.law.title,
                 everything() )                    
                               
      #Tidy and formatting-#-#-#-#-#-#-#--#-#-#--#-#-#-
         
          #Tidy
          df.2 <- df.1 %>% 
            gather(FY, amount, -1:-5)

          #adjust numbers
          df.2$amount <- as.numeric(df.2$amount)*1e6

          final.df <- df.2
           

# Export as .csv -----------------------------------------------------

# Export
write_csv(final.df, my.file)


