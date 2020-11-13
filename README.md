# DOD-Green-Book
R Tidied and Cleaned National Defense Budget Estimates 
[National Defense Budget Estimates for FY XXXX (Green Book)](http://comptroller.defense.gove/Budget-Materials/)

## Description of Included Documents
* tbl_2.1 Base and Supps (war funding) since 2001
* tbl_5.1 and 5.6 Deflators from 1970
* tbl_7.5 End Strength from 1940

### 7.6 U.S. Labor Force 
|Total Title|Formula (calculated from +employment.category* variable)|
|------------|------------|
|Total DOD Employment |Active Duty military + DOD Civilian Direct Hires
|Total Federal Civilian Employment |DOD Civilian Direct Hires + Other Federal Civil Service
| Total Federal Employment |Active Duty military +  DOD Civilian Direct Hires +  Other Federal Civil Service + Coast Guard
|Total US Employment | Total Federal Employment (see above) +State and Local Gov't +  U.S. Private Sector Employment

### Chapter 6 Files
Dataset Number| Common Var | Begin FY| Table Numbers and Budget Type|
|------------|------------|---------|------------------------------|
|1|Public Law Title| 1948 | 6-1 (TOA), 6-8 (BA), 6-11 (Outlays)|
|2|Pay Category|1948|6-2 (TOA), 6-9 (BA), 6-12 (Outlays)|
|3|Military Dept.|1948|6-3 (TOA), 6-10 (BA), 6-13 (Outlays)|
|4|Major Force Program|1962|6-4 TOA (nominal), 6-5 (constant)
|5 TODO|DOD Civilian Pay|1970|6-14 (nominal), 6-15 (constant)
|6|By Service, By P.L. (no Defense-Wide)|1948|Army (TOA 6-16; BA 6-19; Outlays 6-22), Navy (TOA 6-17; BA 6-20; 6-23), Air Force (TOA 6-18; BA 6-21; Outlays 6-24)|