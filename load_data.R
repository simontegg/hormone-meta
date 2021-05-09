library(googlesheets4)
library(tidyverse)

cols  <- c('id', 
           'month',
           'study',	
           'table',	
           'cohort',
           'sex',	
           'measure',
           'treatment', 
           'z',
           'SD'	,
           'SD_lower',	
           'SD_upper',
           'SE',	
           'absolute',
           'unit',	
           'abs_SD',	
           'abs_SD_lower', 
           'abs_SD_upper',	
           'abs_SE',	
           'months_treated',	
           'age',	
           'age_0',	
           'n',	
           'baseline')

SHEET_ID  <- "1lwbN784ibPyx09uXo_o9B6XCGvHv_Zax00_vsLQdBvc"
sheet <- read_sheet(SHEET_ID, range="bone!A1:X265", col_types="ciciccccddddddcddddiddid")
write_csv(sheet, './data.csv')



