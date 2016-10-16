# DOD-Green-Book
R Tidied and Cleaned National Defense Budget Estimates 

## Summary of Included Documents
1. [National Defense Budget Estimates for FY 2017 (Green Book)](http://comptroller.defense.gove/Budget-Materials/)

## Description of Included Documents
1. Each year's Green Book can be found at the DOD Comptroller website in several formats; [as a pdf](http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2017/FY17_Green_Book.pdf) or a compressed set of .xls files, all of which are formatted differently and, of course, are non machine readable.

## Structure of this Repository
Raw data in DATA/RAW subfolder is cleaned and tidied from original online source and exported as .csv file to DATA/Processed subfolder.

## To Do
* Standardize account and service data and create data definitions table.

## Recent Changes
* Unzipped .xls tables were originally stored in the DATA/RAW folder as a matter of convenience. This made the source data vulnerable to user alterations. New script downloads and unzips source data to a temporary file each time,  ensuring better data integrity. 