
# Info --------------------------------------------------------------------
#' Table 6.8 BA by Public Law Title
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
          my.filename <- "FY21 PB Green Book Table 6-8.xlsx"

#'   Set current year
          current.FY <- 2021

#'First row of data (excluding colheader)
  first.row.of.data <- 7 
  
# Range of data
  my.data.range <- c( 7:16,    #<- Current
                     19:28)    #<- Constant 
        
#' Identify Metacols
          budget.type       <-     "BA"
          source.table    <-       "tbl.6.08_BA.by.Public.Law.Title"

# names won't change annually?
        valid.names <- rep(c( "military.personnel",
                             "retired.pay.Defense",
                             "OM",
                             "procurement",
                             "RDTE",
                             "MILCON",
                             "family.housing",
                             "revolving.and.management.funds",
                             "trust.receipts.and.other",
                             "OCO.placeholder"
                             ), 2) 
                
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(stringr)

# Common Vars and Functions -----------------------------------------------

# filename        
import.file.name <- paste0(chapter.6.raw.data.folder, 
                    my.filename)

col.limit <-  (current.FY + 4) - 1948 + 4
all.excel.cols <- c(LETTERS,
                     paste0(rep(LETTERS, each = 26), LETTERS)
                     )
last.excel.col <- all.excel.cols[col.limit]

excel.col.range <-  str_c("A", my.data.range)



# export labelling
mylocation <- "./Data/Processed"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")
my.export.filename <- sprintf("%s/%s_%s.csv", mylocation, source.table, mydate)


  

# Import ----------------------------------------------------------

df.1 <- read_excel(paste0(import.file.name),
           col_names = FALSE)
        
# Shaping -------------------------------------------------------------

          #eliminate unneeded cols, rows
          df.1      <- df.1[my.data.range,c(-2:-3) ]
          
          #rename first col
          df.1$...1 <- valid.names
          #rename FY
            
          names(df.1) <- c("public.law.title", 
                           1948:(current.FY + 4))

          # NAs to zeros
          df.1[is.na(df.1)] <- 0

      #Add meta cols-#-#-#-#-#-#-#--#-#-#--#-#-#-
          
          #new identifying cols
          df.1$budget.type        <-    budget.type 
          df.1$deflator.type      <-    c( rep("current", nrow(df.1)/2),
                                           rep("constant",nrow(df.1)/2))
          
          df.1$source.table       <-    source.table

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


