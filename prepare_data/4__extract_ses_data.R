###########################################################
# Extract SES data for all patients #######################
# Trude Slinger - g.slinger-2@umcutrecht.nl ###############
###########################################################

# packages
library( "sf" ) 
library( "stringr" )

# wd
setwd()

# outdir
outdir <- "out.extract_ses_data"
dir.create( outdir, showWarnings = FALSE )

# ses data
# https://www.cbs.nl/nl-nl/maatwerk/2022/40/ses-per-postcode-2014-2019-excl-studenten
df_ses <- read.csv( "data/ses/ses_2014_2019_without_students.csv", sep = ";" )

# patient data
df_patient <- read.csv( "data/clinical/diagnosis_data_including_postal_codes.csv", sep = ";" )
df_patient <- subset( df_patient, ! is.na( df_patient$pc4 ) )

# add columns
df_patient$year_ses_data <- NA
df_patient$part_score_welfare <- NA
df_patient$part_score_employment <- NA
df_patient$seswoa_lower <- NA
df_patient$seswoa_avg <- NA
df_patient$seswoa_upper <- NA

# for loop over patients
for( i in 1:nrow( df_patient ) ){
  
  # get postal code
  pc <- df_patient$pc4[ i ]
  
  # get presentation year
  year <- str_split_fixed( df_patient$presentation_date[ i ], "-", 3 )[ 3 ]
  
  # recode years < 2014 and > 2019
  if( year < 2014 ){ year <- 2014 }
  if( year > 2019 ){ year <- 2019 }

  # subset ses
  ses <- subset( df_ses, df_ses$Verslagjaar == year & df_ses$Viercijferige.postcode == pc )
  
  # add to dataframe
  df_patient$year_ses_data[ i ] <- year
  df_patient$part_score_welfare[ i ] <- ses$SES.WOA.deelscore.financiële.welvaart..gemiddelde.score
  df_patient$part_score_employment[ i ] <- ses$SES.WOA.deelscore.arbeidverleden..gemiddelde.score
  df_patient$seswoa_lower[ i ] <- ses$Ondergrens.95..interval.SES.WOA.totaalscore 
  df_patient$seswoa_avg[ i ] <- ses$Gemiddelde.SES.WOA.totaalscore
  df_patient$seswoa_upper[ i ] <- ses$Bovengrens.95..interval.SES.WOA.totaalscore
}

# save
write.csv( df_patient, paste0( outdir, "/patient_ses_data.csv" ) )