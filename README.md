# NB Project

## Data Notes

Data files in the `/data` folder have been named to reflect the following filters on the [CDC website](https://wonder.cdc.gov/ucd-icd10-expanded.html).

**/export/TX_2021_monthly.csv** joins data from the following files:  
* TX_2021_01.csv - COUNTY (County population and ALL deaths)
* TX_2021_02.csv - COUNTY (All Deaths EXCEPT at Decedents home / Other)
* TX_2021_03.csv - COUNTY > Month (All Deaths EXCEPT at Decedents home / Other)
* TX_2021_04.csv - COUNTY > Month (COVID Deaths EXCEPT at Decedents home / Other)

**/export/TX_2021_wkday.csv** joins data from the following files:  
* TX_2021_05.csv - COUNTY > Month > Weekday (All Deaths EXCEPT at Decedents home / Other)
* TX_2021_06.csv - COUNTY > Month > Weekday (COVID Deaths EXCEPT at Decedents home / Other)
