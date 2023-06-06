###########################################################
# Get buurt code for every postal code ####################
# Trude Slinger - g.slinger-2@umcutrecht.nl ###############
###########################################################

# set wd
setwd()

# output directory
outdir <- "out.merge_pc_and_buurt"
dir.create( outdir, showWarnings = F )

# load packages
library( "dplyr" )

# load data sets
# df_pc = UMCU patient data
# df_buurt = CBS data 2021 (https://www.cbs.nl/nl-nl/maatwerk/2021/36/
# buurt-wijk-en-gemeente-2021-voor-postcode-huisnummer)
df_pc <- read.csv( "data/clinical/participant_postal_codes.csv", sep = ",", row.names = 1 )
df_buurt <- read.csv( "data/geographical/gemeente_wijk_buurt_pc6.csv", sep = ";" )
colnames( df_buurt ) <- c( "pc6", "buurt", "wijk", "gemeente" )

# join data sets
# one postal code can have multiple buurt codes
df_pc_buurt <- inner_join( df_pc, df_buurt ) # 1,277 rows for 1,213 patients

# transform buurt code
df_pc_buurt$buurt_code <- sprintf( "%010d", as.numeric( df_pc_buurt$buurt ) )
df_pc_buurt$buurt_code <- df_pc_buurt$buurt_code %>% sub( "^00", "BU", .)
df_pc_buurt <- df_pc_buurt[ , c( 1:4, 7, 5:6 ) ]

# save
write.csv( df_pc_buurt, file = paste0( outdir, "/", "participant_pc_and_buurt.csv" ) )