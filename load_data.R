library(googlesheets4)
library(tidyverse)

cols  <- c('id', 
           'month_series',
           'month',
           'study',	
           'table',	
           'cohort',
           'sex',	
           'measure',
           'treatment', 
           'z',
           'SE',	
           'SD'	,
           'SD_lower',	
           'SD_upper',
           'absolute',
           'unit',	
           'abs_SD',	
           'abs_SD_lower', 
           'abs_SD_upper',	
           'abs_SE',	
           'months_treated_gnrha',	
           'age',	
           'age_0',	
           'n',	
           'baseline',
           'baseline_SE',
           'baseline_SD',
            'abs_per',
            'abs_per_SE')

SHEET_ID  <- "1lwbN784ibPyx09uXo_o9B6XCGvHv_Zax00_vsLQdBvc"
sheet <- read_sheet(SHEET_ID, range="bone!A1:AC271", col_types="ciiciccccddddddcdddddddiddddd")
write_csv(sheet, './data.csv')



