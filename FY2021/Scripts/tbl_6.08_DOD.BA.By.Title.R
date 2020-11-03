#' Original DoD Comptroller zip file downloaded here:
#' http://comptroller.defense.gov/BudgetMaterials.aspx
#' To View as pd:
#' http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2019/FY19_Green_Book.pdf
#' 
#' To download as zip
#' http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2019/FY_2019_Green_Book.zip
#' 
#' Table 6.8 DOD BA By Title
#' 
# Libraries ---------------------------------------------------------------

library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(readr)


#' # Import  Data ------------------------------------------------------------
x <- "http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2019/FY_2019_Green_Book.zip"
y <- "FY19 PB Green Book Chap 6/FY19 6-8_DoD BA by Title.xlsx"


nettle_downzip <- function(zip.url, zip.file){
    my.temporary.zipped.file <- tempfile()   # Zip file will go in here
    my.temporarary.zipped.folder <- tempdir() # Unzipped file will go in here
    download.file(zip.url, dest = my.temporary.zipped.file) # Download Source Data to Temp file
    unzip(my.temporary.zipped.file, exdir = my.temporarary.zipped.folder) # Unzip to Temp directory
    location.of.unzipped.file <- paste0(my.temporarary.zipped.folder,"/", zip.file)
    return(location.of.unzipped.file )
    }

my.filename <- nettle_downzip(x,y)


# Reshape -----------------------------------------------------------------

#excel_sheets(filename)
df.raw <- read_excel(my.filename, skip = 3) 


# Flatten -----------------------------------------------------------------
# Shape Subset for Current Dollars
current.df <- df.raw[2:10, -2:-3]
constant.df <- df.raw[15:23, -2:-3]


nettle_tidy <- function(df, type.of.dollars){
  x <- gather(df, Fiscal.Year, Amount, -1) 
  names(x)[1]<- 'Public Law Title'
  x$Amount <- x$Amount * 1e6
  x$`Public Law Title` <- str_trim(gsub("[0-9.]+", "", x$`Public Law Title`))
  x <- separate(x, Fiscal.Year, c('trash', 'FY'), convert = TRUE )
  x <- x[,-2]
  x$Deflator.Type <- type.of.dollars
  x$Source <- "Table 6.8 DOD BA By Title"
return(x)
}

current <- nettle_tidy(current.df, "Current")
constant <- nettle_tidy(constant.df, "Constant")
df.flat<- bind_rows(current, constant)


# Export ------------------------------------------------------------------
# Filename
mylocation <- "./Data/Processed"
myfilename <- "tbl.6.8_DOD.BA.By.Title"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")

my.file <- sprintf("%s/%s_%s.csv", mylocation, myfilename, mydate)
write_csv(df.flat, my.file)
















