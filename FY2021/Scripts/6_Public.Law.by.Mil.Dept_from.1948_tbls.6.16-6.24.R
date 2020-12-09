
# Info --------------------------------------------------------------------
#' Public Law Title by Military Dept from 1948
#' 
#' 6.16 TOA Army
#' 6.17 TOA Navy
#' 6.18 TOA USAF
#' -------------
#' 6.19 BA Army
#' 6.20 BA Navy
#' 6.21 BA USAF
#' -------------
#' 6.22 Outlays Army
#' 6.23 Outlays Navy
#' 6.24 Outlays USAF
#' 
#' Labeling/filenames are consistent
#' 
#' 
# # Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(stringr)

#' # How to Update this File -------------------------------------------------
#'
#'  #-#-#-#-#-#-#-#- Change this stuff #-#-#-#-#-#-#-#-#-#-#-#-
#'   -> Download Comptroller data to Raw folder (manually).
#'
#'   -> Change year, verify file prefix (names generated automatically in tibble)
        filename.prefix <- "./Data/Raw/FY21 PB Green Book Chap 6/FY21 PB Green Book Table 6-"
#'
#'   ->Set working Directory to current year:
        setwd("./DOD-Green-Book/FY2021")
        
#'   -> Verify Export name
        export.filename <- "Public.Law.by.Mil.Dept_since.1948_TOA.BA.Outlays"  
#'                
#'  #-#-#-#-#-#-#-#- This probably will NOT change #-#-#-#-#-#-#-#-#-#-#-#-
#' Includes table ranges and names
#' 
      #' Verify Tables #-#-#-#-#-#-#
      #' 
              #'  Tables 6.16, 6.17, 6.18 TOA
                      toa.rowrange <- c( 7:13,  #<-- Current dataset
                                        16:22) #<--  Constant dataset
              
                      toa.rownames <- rep(c("MILPERS",
                                            "OM",
                                            "Procurement",
                                            "RDTE",
                                            "MILCON",
                                            "family.housing",
                                            "revolving.and.management"),
                                           2)
      #'
      #'                                
              #' Tables 6.19, 6.20, 6.21 BA
                      ba.rowrange <- c( 7:14,  #<-- Current dataset
                                       17:24) #<--  Constant dataset
                      
                      ba.rownames <- rep(c("MILPERS",
                                            "OM",
                                            "Procurement",
                                            "RDTE",
                                            "MILCON",
                                            "family.housing",
                                            "revolving.and.management",
                                            "trust.receipts.and.other"), #<- New line!
                                                         2)
      #'
      #'                                
              #' Tables 6.22, 6.23, 6.24 Outlays
                      outlays.rowrange <- c( 7:14,  #<-- Current dataset
                                            17:24) #<--  Constant dataset
                      
                      outlays.rownames <- rep(c("MILPERS",
                                            "OM",
                                            "Procurement",
                                            "RDTE",
                                            "MILCON",
                                            "family.housing",
                                            "revolving.and.management",
                                            "trust.receipts.and.other"), #<- New line!
                                                         2) 
      #'
              #' Verify 
                    crs.service     <- c("army","navy","usaf")
      

# Common Vars -------------------------------------------------------------
#' Create tibble processing. This is updated automatically. 
#' It's possible that filenames will change in future years

        
        filename.repository <-
                        tibble(
                          table.names = paste0("tbl.", crs.service,".6.", 16:24),
                          crs.service = rep(crs.service,3),
                          budget.type = rep(c("TOA", "BA", "Outlays"), each=3) ,
                          # Filenames generated automatically
                          raw.file.location =  paste0(filename.prefix, 16:24, ".xlsx"),
                          #Row.names: filename.repository$rowname[[1]]
                          rowname  = rep(c(list(toa.rownames,
                                                ba.rownames,
                                                outlays.rownames) ),
                                          each = 3),
                          #Rowrange: filename.repository$rowrange[[1]]
                          rowrange  = rep(c(list(toa.rowrange,
                                             ba.rowrange,
                                             outlays.rowrange) ),
                                          each = 3)
                        )       
        
# Superfunction -----------------------------------------------------------
clean.function <- function(               
                    raw.filename,      #<-  raw.file.location
                    my.rowrange,       #<-  rowrange
                    my.rownames,       #<-  frowname
                    source.table,      #<-  table.names
                    table.budget.type)  #<- budget.type 
                                      {
  
# Import    
df.1 <- read_excel(raw.filename, col_names=F)
df.1

# Reshaping ---------------------------------------------------------------

#Basic Shaping: Only valid Row, cols
df.1 <- df.1[unlist(my.rowrange), c(-2:-3)]

    
# FY Colnames (based on size of dataset)
FY.colnames <- str_c(1948:(1948+(ncol(df.1)-2) )) #<FY must be text (for tidy)
colnames(df.1) <- c("public.law.title",            #<First col is public law title
                     FY.colnames) 
          
# Rename left col (header rows)
df.1[,1] <- unlist(my.rownames)

# NAs to zeros
df.1[is.na(df.1)] <- 0


#Add meta cols-#-#-#-#-#-#-#--#-#-#--#-#-#-

#Current/Constant
repeat.deflator.type.times <- nrow(df.1)/2
repeat.deflator.type <- c( rep("current", repeat.deflator.type.times),
                           rep("constant", repeat.deflator.type.times) )
df.1$deflator.type <-repeat.deflator.type

#new identifying cols
df.1$source.table       <-     source.table 
df.1$budget.type        <-     table.budget.type 
    
#Rearrange Cols-#-#-#-#-#-#-#--#-#-#--#-#-#-

#Rearrange
df.1 <- df.1 %>% 
           select(
                 source.table,
                 budget.type, 
                 deflator.type,
                 1,  #<- this might vary, so id by position
                 everything() )              

#Tidy-#-#-#-#-#-#-#--#-#-#--#-#-#-
#Gather
 df.2 <- df.1 %>% 
 gather(FY, amount, -1:-4)

#adjust numbers
df.2$amount <- as.numeric(df.2$amount)*1e6

final.df <- df.2
return(final.df)   
                          }

# Apply Function ----------------------------------------------------------

filename.repository <- filename.repository %>% 
  mutate(mydata = pmap(   list( raw.file.location, #<-vars must be in a list
                                rowrange,
                                rowname,
                                table.names,
                                budget.type),
                          .f = clean.function) ) %>%
          select(mydata) %>% 
          unnest()
    

# Export ------------------------------------------------------------------

                    
mylocation <- "./Data/Processed"
mydate <- paste('Updated', format(Sys.time(), format = "_%Y-%m-%d_%H%M") , sep = "")
my.export.filename <- sprintf("%s/%s_%s.csv", mylocation, export.filename, mydate)          
          
write_csv(filename.repository, my.export.filename)

       
                    