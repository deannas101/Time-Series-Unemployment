# Data Importing from CSV -----------------------------------------------------
MIHOUG1URN <- read_csv("MIHOUG1URN.csv", 
                       col_types = cols(DATE = col_date(format = "%Y-%m-%d")))
