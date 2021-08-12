#' Original DoD Comptroller zip file downloaded here:
#' http://comptroller.defense.gov/BudgetMaterials.aspx
#' 
#' Table 2.1 Base Budget, War Funding and Supplementals by Military Department, by P.L. Title
#' (Discretionary Budget Authority)

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(zoo)

# How to Update this File -------------------------------------------------
#'   Set working Directory to current year
        setwd("./DOD-Green-Book/FY2022")
#'
#'   Manually download Comptroller data to Raw data folder 
#'
#'   Identify file
        my.filename <- "./Data/Raw/FY22 PB Green Book Chap 2.xlsx"
#'
#' Importing Values: Describe file datashape
#'   How many sheets are there?        
        my.sheets <- 18
#'   How many blank rows before header row?
        skip.rows <- 5
        
#' Identify table
   source.table <- "FY22 Green Book, tbl 2.1"
        
# Import ------------------------------------------------------------------


#Import  There are 18 sheet
t <-  lapply(excel_sheets(my.filename)[1:my.sheets], 
             read_excel, 
             path = my.filename, 
             col_names = F, 
             skip = skip.rows )

#' User Defined Cleaning Function ---------------------------------------------------

tidy_grnbook_supps_table <- function(t){
  
  # Create Missing Colnames
  t[1,1] <- "Military.Department"
  t[1,2] <- "FY"
  t[1,3] <- "Account"
  
  # Colnames using baseR
  colnames(t) <- t[1,]
  t <- t[-1,]
  
  # Make all colnames r-friendly
  colnames(t) <- make.names(colnames(t))
  
  # Select (don't omit!) Columns to use
  t <- t[, c(1:3, 5:11)]

  # Forward Fill for first two columns (Military Dept, and FY)
   t[1,2] <- t[2,2] 
  t[,c(1:2)] <- na.locf(t[,c('Military.Department', 'FY')])
  
  # DANGER! Remove rows that contain a NA in the Account column
  t <- t[complete.cases(t$Account),]
  
  # Remove non-alphabetic characters from Public.Law.Title column numbers and periods
  t$Account <- str_trim(gsub("[0-9.]+", "", t$Account))
  
  # Remove 'continued' from Military Department
  t$Military.Department <- str_trim(gsub("(Continued)", "", t$Military.Department, fixed = "True"))
  
  # Convert relevant cols to numeric
  t$FY <- as.numeric(gsub("FY ", "", t$FY))
  
  # Gather and Tidy
  t <- gather(t, Public.Law.Title, Amount, c(-1:-3))
  
  # Convert Amount to numeric
  t$Amount <- as.numeric(t$Amount)
  
  # There are NAs and 0 values mingled in Amount column. Change them all to 0
  t[is.na(t)] <- 0
  
  # This dataset was originally adjusted to be "in millions." Mulitply for unadjusted amount
  t$Amount <- t$Amount * 1e6
  
  # Remove  published "Totals" Column
    t <- t %>% 
    filter(!grepl("total", t$Military.Department, ignore.case = T))
  
  return(t)
}

# Tidy data new function -------------------------------------------------
  t <- lapply(X = t, FUN = tidy_grnbook_supps_table)
  t <- do.call(rbind, t)
  # Note: this could be done with purr's map, as well

  
# Add MetaData ------------------------------------------------------------
# Add Spending type column I've devised (base, OCO, emergency relief, other)
  
  # Create variables
  unique.public.laws <- unique(t$Account)
  
  base <- unique.public.laws[1]
  OCO.GWOT <- unique.public.laws[c(2,8)]
  emergency.relief <- unique.public.laws[c(4,5,6,9,10,12,14,15)]
  other <- unique.public.laws[c(3,7,11)]
  
  # Assign vars to column  
  t <- t %>% 
    mutate(
          account.categories = 
             case_when(
               str_detect(Account, "Base|Enduring") ~ "base.discretionary",
               str_detect(Account, "Hurricane|Earthquake|Flu|Ebola|Disaster") ~ "natural.disaster.or.viral",
               str_detect(Account, "War|Contingency") ~ "OCO.GWOT",
               TRUE ~ "other") )

  # Include Source Column
  t$Source_Data <- source.table

# Export ------------------------------------------------------------------
# Export as .csv
# Filename
  mylocation <- "./Data/Processed"
  myfilename <- "tbl.2.1_Base.and.Supps"
  mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")
  my.file <- sprintf("%s/%s_%s.csv", mylocation, myfilename, mydate)
  
# Export
# View(t)
 write_csv(t, my.file)
