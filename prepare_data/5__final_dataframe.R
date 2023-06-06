###########################################################
# Create final dataframe ##################################
# Trude Slinger - g.slinger-2@umcutrecht.nl ###############
###########################################################

# packages 
library( "dplyr" )

# wd
setwd()

# outdir
outdir <- "out.final_dataframe"
dir.create( outdir, showWarnings = FALSE )

# pollution and SES data
df_pollution <- read.csv( "out.extract_pollution_data/patient_pollution_data_without_coordinates.csv", sep = ",", row.names = 1 )
df_ses <- read.csv( "out.extract_ses_data/patient_ses_data.csv", sep = ",", row.names = 1 )
df_ses <- df_ses[ , c( 1, 14:ncol( df_ses ) ) ]

# merge
df_complete <- merge( df_pollution, df_ses, by = "id", all.x = T )

# drop unclear epilepsy diagnosis (3) group
df_complete <- subset( df_complete, df_complete$diagnss != 3 ) 

# recode diagnosis (0 = no epilepsy, 1 = epilepsy)
df_complete$diagnss <- recode( df_complete$diagnss, "2" = 0, "1" = 1 )

# reformat column names
colnames( df_complete ) <- c(
  "id",
  "id_old",
  "postal_code_4_old",
  "postal_code_4",
  "postal_code_6",
  "presentation_date",
  "sex",
  "age",
  "diagnosis",
  "epilepsy_type",
  # 1 = focal
  # 2 = generalized
  # 3 = focal & generalized
  # 4 = unknown
  "epilepsy_etiology",
  # 1 = genetic
  # 2 = structural
  # 3 = metabolic
  # 4 = immunological
  # 5 = infectious
  # 6 = unknown
  "genetic_etiology_subtype",
  # 1 = established genetic
  # 2 = presumed genetic
  "sz_load",
  "no2_annual",
  "o3_annual",
  "pm10_annual",
  "pm25_annual",
  "year_ses_data",
  "part_score_welfare",
  "part_score_employment",
  "seswoa_lower",
  "seswoa_avg",
  "seswoa_upper"
)

# drop unnecessary columns
df_complete_final <- df_complete[ , c(
  "id",
  "postal_code_4",
  "postal_code_6",
  "presentation_date",
  "sex",
  "age",
  "diagnosis",
  "epilepsy_type",
  "epilepsy_etiology",
  "genetic_etiology_subtype",
  "no2_annual",
  "o3_annual",
  "pm10_annual",
  "pm25_annual",
  "year_ses_data",
  "part_score_welfare",
  "part_score_employment",
  "seswoa_lower",
  "seswoa_avg",
  "seswoa_upper"
) ]
 
# recode character variables needed for analysis to numbers
df_complete_final$seswoa_avg <- as.numeric( df_complete_final$seswoa_avg )
df_complete_final$age <- as.numeric( df_complete_final$age )

# save
write.csv( df_complete_final, paste0( outdir, "/dataframe_final.csv" ) )
