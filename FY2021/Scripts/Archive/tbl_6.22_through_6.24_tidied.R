
# Info --------------------------------------------------------------------
#' Table 6.22-6.24 DOD OUTLAYS By by Service and Title

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
          tbl.6.22.Army <- "FY21 PB Green Book Table 6-22.xlsx"
          tbl.6.23.Navy <- "FY21 PB Green Book Table 6-23.xlsx"
          tbl.6.24.USAF <- "FY21 PB Green Book Table 6-24.xlsx"

#'   5) Set current year
          current.FY <- 2021
          
#'   6) Identify rows for constant and current
        valid.rows <- c( 7:14,  #<-- Current dataset
                        17:24) #<--  Constant dataset

        valid.names.public.law.titles <-rep(c("MILPERS",
                                              "O.M.",
                                              "Procurement",
                                              "RDT.E",
                                              "MILCON",
                                              "family.housing",
                                              "revolving.and.management",
                                              "trust.receipts.and.other"),
                                           2)
        
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(stringr)

# Common Vars and Functions -----------------------------------------------


# Methodology -------------------------------------------------------------
#' Cut up the datasets, combine, then clean all
          
nettle_clean <- function(file.name,
                         valid.rows,
                         valid.names.public.law.titles,
                         my.table.number, 
                         dod.service){
         
          #Import    
          outlays.1 <- read_excel(file.name, na = "0", col_names=F, n_max=34)
          
          #Shaping: Only valid Rows (see "how to update this file/ valid.rows" above)
          outlays.1 <- outlays.1[valid.rows,]
          
          # Shaping: Remove unneeded spacer cols
          outlays.1 <- outlays.1[, c(-2:-3)]  
  
      # Reshaping-#-#-#-#-#-#-#--#-#-#--#-#-#-
            
          #Rename Cols
          FY.colnames <- str_c(1948:(1948+(ncol(outlays.1)-2) )) #<FY must be text
          colnames(outlays.1) <- c("public.law.title",
                                    FY.colnames) 
          # NAs to zeros
          outlays.1[is.na(outlays.1)] <- 0
          #Numbers to unadjusted amounts
          
          # Rename (see above)
          outlays.1$public.law.title <- valid.names.public.law.titles

      #Add meta cols-#-#-#-#-#-#-#--#-#-#--#-#-#-
          
          #new identifying cols
          outlays.1$budget.type   <- "outlays"
          outlays.1$service       <- dod.service
          outlays.1$source.table  <- my.table.number
          
          #Add constant,current col 
          # Divide nrows in two; tag first half as "current", and second "constant"    
              repeat.deflator.type.times <- nrow(outlays.1)/2
              repeat.deflator.type <- c( rep("current", repeat.deflator.type.times),
                                        rep("constant", repeat.deflator.type.times) )
          outlays.1$deflator.type <-repeat.deflator.type
           
        #Rearrange
         outlays.1 <- outlays.1 %>% 
           select(
                 budget.type, 
                 service,
                 source.table,
                 deflator.type,
                 public.law.title,
                 everything() )                    
                               
      #Tidy and formatting-#-#-#-#-#-#-#--#-#-#--#-#-#-
         
          #Tidy
          outlays.2 <- outlays.1 %>% 
            gather(FY, amount, -1:-5)

          #adjust numbers
          outlays.2$amount <- as.numeric(outlays.2$amount)*1e6

           final.outlays <- outlays.2
           
  }
    

# Apply Function -----------------------------------------------------------

file.name <- paste0(chapter.6.raw.data.folder, tbl.6.22.Army)          
my.table.number <- "6.22"
dod.service <- "army"

army.df <- nettle_clean( file.name,
                         valid.rows,
                         valid.names.public.law.titles,
                         my.table.number, 
                         dod.service)


file.name <- paste0(chapter.6.raw.data.folder, tbl.6.23.Navy)          
my.table.number <- "6.23"
dod.service <- "navy"

navy.df <- nettle_clean( file.name,
                         valid.rows,
                         valid.names.public.law.titles,
                         my.table.number, 
                         dod.service)


file.name <- paste0(chapter.6.raw.data.folder, tbl.6.24.USAF)          
my.table.number <- "6.24"
dod.service <- "usaf"

usaf.df <- nettle_clean( file.name,
                         valid.rows,
                         valid.names.public.law.titles,
                         my.table.number, 
                         dod.service)

# Comine datasets
combined.outlays <- bind_rows(army.df, navy.df,usaf.df)

# Complete and Export as .csv -----------------------------------------------------

# Filename
mylocation <- "./Data/Processed"
myfilename <- "GBook_tbl.6.22-6.24_OUTLAYS.by.Service.and.Title"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")

my.file <- sprintf("%s/%s_%s.csv", mylocation, myfilename, mydate)

# Export
write_csv(combined.outlays, my.file)


