#' Original DoD Comptroller zip file downloaded here:
#' http://comptroller.defense.gov/BudgetMaterials.aspx
#' 
#' Table 2.1 Base Budget, War Funding and Supplementals by Military Department, by P.L. Title
#' (Discretionary Budget Authority)
#' 
#' Updated: 05-01-2020 fo FY2021
#' 
# Libraries ---------------------------------------------------------------

library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(readr)
library(readxl)
library(zoo)


# Preliminaries -----------------------------------------------------------
# Set working dir manually

# Download new raw data?
  download.switch <- "download.switch.on"
  download.switch <- "download.switch.off"

# Import  Data from Website------------------------------------------------------------

x <- "https://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2021/FY_2021_Green_Book.zip"
y <- "FY21 PB Green Book Chap 2.xlsx" 

nettle_downzip <- function(zip.url, zip.file){
    my.temporary.zipped.file <- tempfile()   # Zip file will go in here
    my.temporarary.zipped.folder <- tempdir() # Unzipped file will go in here
    download.file(zip.url, dest = my.temporary.zipped.file) # Download Source Data to Temp file
    unzip(my.temporary.zipped.file, exdir = my.temporarary.zipped.folder) # Unzip to Temp directory
    location.of.unzipped.file <- paste0(my.temporarary.zipped.folder,"/", zip.file)
    return(location.of.unzipped.file )
    }

my.filename <- nettle_downzip(x,y)
#excel_sheets(my.filename)

# Reshape -----------------------------------------------------------------

#Import  There are 18 sheet
t <-  lapply(excel_sheets(my.filename)[1:18], 
             read_excel, 
             path = my.filename, 
             col_names = F, 
             skip = 5 )

#' User Defined Cleaning Function ---------------------------------------------------
#' Purpose:Create function that cleans and tidies
#' Note: Rather than removing cols, SELECT them by colname. Sometimes, one sheet will 
#' have extra blank cols not included on others that widens orlengthens the array.

tidy_grnbook_supps_table <- function(t){
  
  # Deprecated: Remove 5 rows at top of each sheet using read_excel 'skip' argument
  # t <- (t[-1:-5,])
  
#t <- t[[1]]
    
  # Create Missing Colnames
  t[1, 1:3] <- c("Military.Department", "FY", "Account")
  
  # Set all Colnames and remove extra row (there ought to be a function that does this)
  colnames(t) <- t[1,]
  t <- t[-1,]
  
  # Make all colnames r-friendly
  colnames(t) <- make.names(colnames(t))
  
  # Select (don't omit!) Columns to use
  t <- t[, c(1:3, 5:11)]

  # Forward Fill for first two columns (Military Dept, and FY)
   t[1,2] <- t[2,2] #< Modified in FY2020 ***
  t[,c(1:2)] <- na.locf(t[,c('Military.Department', 'FY')])
  
  # DANGER! Remove rows that contain a NA in the Public Law Title column
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
  
  ##### PreCalculated Totals ####
  #' The original dataset contains hard-coded totals which do not match real calculated totals
  #' should be removed but can be useful for checking the accuracy of this script:
  
  # Remove precalculated totals but save the subset for future use
  # Save Totals Subset - This doesn't seem to work outside of the function
  #precalculated_totals <- t %>% 
   # filter(grepl("total", t$Military.Department, ignore.case = T))
   
  # Remove  published "Totals" Column
  # Note: Published totals are consistently $1-2 million off from caluclated totals
  # For more info, see checksums script
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
  emergency.relief <- unique.public.laws[c(4,5,6,9,10,12)]
  other <- unique.public.laws[c(3,7,11)]
  
  # Assign vars to column  
  t <- t %>% 
    mutate(spending.type = ifelse(t$Account %in% base, "Base, Discretionary",
                           ifelse(t$Account %in% OCO.GWOT, "OCO.GWOT",
                           ifelse(t$Account %in% emergency.relief, "Emergency Relief",
                           ifelse(t$Account %in% other, "Other Discretionary", "uncategorized")))))   
  # Include Source Column
  t$Source_Data <- "FY20 Green Book, tbl 2.1"

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
