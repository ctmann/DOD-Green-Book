#' Original DoD Comptroller zip file downloaded here:
#' http://comptroller.defense.gov/BudgetMaterials.aspx

#' Table 7.5 End Strength FTEs
#' 
#' Notes: Data stored locally due to firewalls and other probs
#' 
#' DataNotes
    #' #' 1.  Active duty military figures include the activation of 25,652 reservists and National Guard personnel in FY 1990, pursuant to section 673b, Title 10 U.S.C.; in FY 1991, 17,059 reservists and National Guard personnel; and in FY 1992, 954 reservists and National Guard personnel, pursuant to sections 672 and 673, Title 10 U.S.C., in support of Operations Desert Shield/Desert Storm.															
    #' 															
    #' "2.  Beginning in 1953, civilian work force figures include both U.S. and foreign national direct hires, and the foreign national indirect hire employees that support U.S. forces overseas.  Personnel funded from a
    #' non-DoD Foreign Military Sales trust fund are included as well.  Beginning in FY 1996, the federal civilian work force is measured in full time equivalents (FTE's)."															
    #' 															
    #' 3.  Navy reserve personnel on active duty for Training and Administration of Reserves (TARS) are included in the active Navy data prior to FY 1980, and in the Full-Time Guard and Reserve data thereafter.															
    #' 4.  Air Force civil service employment is included in the Army data prior to 1948, and identified separately thereafter.															
    #' 5.  Reflects enacted amounts.															
    #' NOTE:  For fiscal years 2020 and prior, war and supplemental data are included.															
#' 
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)

# How to Update this File -------------------------------------------------
# 1. Set working directory
    setwd("~/Documents/R Programming/Repositories/DOD-Green-Book/FY2021")

# 2. Change filename
my.filename <- "FY21 PB Green Book Chap 7.xlsx"
my.tab      <- "7-5"

# 3. Shape of Data table
# rows vary year by year, check "Reshape Dataset" in code below


# Common Vars and Functions -----------------------------------------------
my.filename.full <- str_c(getwd(), "/Data/Raw/", my.filename)

#' # Import  Data ------------------------------------------------------------
my.filename.full <- str_c(getwd(), "/Data/Raw/FY_2021_Green_Book/", my.filename)
end.strength <- read_excel(my.filename.full, my.tab)



# Tidy --------------------------------------------------------------------
# Reshape Dataset: This Varies from year to to year!
end.strength <- end.strength[c(5:86,  # Area - by row
                                    89),   # Area - by row (ignore base/oco, take total)
                                  c(1,3:8, # Area - by col (remove blank rows, totals)
                                    10:14)  # Area - by col
                                  ]

# Renames

names(end.strength) <- c(
  "FY",
  "Army,Active",
  "Navy,Active",
  "Marine Corps,Active",
  "Air Force,Active",
  "Space Force,Active",
  "Full Time Guard and Reserve,Active",
  "Army,Civilians",
  "Navy and Marine Corps,Civilians",
  "Air Force,Civilians",
  "Space Force,Civilians",
  "Defense Agencies,Civilians"
        )
end.strength <- end.strength[-1,]


# Tidy
end.strength <- end.strength %>% 
  pivot_longer(-FY, 
               names_to = "service",
               values_to = "personnel") #contains NAs

    
end.strength <- end.strength %>% 
  mutate(FY = FY %>% str_extract("[0-9]{4}"), #Remove dots
         personnel = replace_na(personnel, 0) %>% as.numeric()*1e3, ) %>%  #convert numbers
         separate(col = service, into = c("Service", "Active.or.Civilians"), ",")


# Export ------------------------------------------------------------------

mylocation <- "./Data/Processed"
myfilename <- "tbl.7.5_DOD.Manpower"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")

my.file <- sprintf("%s/%s_%s.csv", mylocation, myfilename, mydate)
write_csv(end.strength, my.file)


