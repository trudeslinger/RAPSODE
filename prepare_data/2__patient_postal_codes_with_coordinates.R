###########################################################
# Get coordinates for patient postal codes ################
# Trude Slinger - g.slinger-2@umcutrecht.nl ###############
###########################################################

# packages
library( "sf" ) 
library( "dplyr" )

# wd
setwd()

# outdir
outdir <- "out.patient_postal_codes_with_coordinates"
dir.create( outdir, showWarnings = FALSE )

# patient postal codes with buurt
pc_list <- read.csv( "out.merge_pc_and_buurt/participant_pc_and_buurt.csv", sep = ",", row.names = 1 )

# unique patient postal codes
pc_list_unique <- pc_list[ ! duplicated( pc_list$pc6 ) , ]
pc_list_unique <- pc_list_unique[ 3 ] # 1176 (less than #patients [some live @ same pc])

# shape file with postcodes per 'buurt'
infile  <- "out.transform_csb_buurten/buurt_etrs_2021.shp"

# open shapefile + remove rows without data
shp_etrs <- sf::read_sf( infile )
shp_etrs <- shp_etrs[ shp_etrs$POSTCODE != "-99999999" & !is.na( shp_etrs$POSTCODE ), colnames( shp_etrs ) %in% c( "POSTCODE", "geometry", "BU_CODE" ) ]

# center of 'buurten' as coordinates for the postal codes
sf_cent <- st_centroid( shp_etrs )

###############################
# ANALYSIS
###############################

# create container
df_final <- NULL

# indices
index_pc_buurt <- 0
index_pc <- 0
index_buurt <- 0

# for-loop to average coordinates 
# 1. if pc + buurt code(s) combination available in etrs file > average coordinates of buurt codes + postal code combination
# 2. if pc + buurt code(s) combination not available in etrs file > average coordinates of postal code
# 3. if pc + buurt code(s) combination AND postal code not available in etrs file > average coordinates of buurt code
for( i in 1:nrow( pc_list_unique ) ){
  
  # get postal code
  post <- pc_list_unique$pc6[ i ]

  # subset pc list
  df_pc_subset <- subset( pc_list, pc_list$pc6 == post )

  # subset coordinate df
  df_coord_subset <- subset( sf_cent, ( sf_cent$BU_CODE == df_pc_subset$buurt_code[ 1 ] | sf_cent$BU_CODE == df_pc_subset$buurt_code[ 2 ] | 
                                        sf_cent$BU_CODE == df_pc_subset$buurt_code[ 3 ] | sf_cent$BU_CODE == df_pc_subset$buurt_code[ 4 ] |
                                        sf_cent$BU_CODE == df_pc_subset$buurt_code[ 5 ] | sf_cent$BU_CODE == df_pc_subset$buurt_code[ 6 ] )
                             & sf_cent$POSTCODE == df_pc_subset$pc4 )
  
  # for pc + buurt codes combination existing in etrs file
  if( nrow( df_coord_subset ) > 0 ) { 
    
    # calculate coordinate means
    coord_x <- mean( st_coordinates( df_coord_subset$geometry )[ 1:nrow( df_coord_subset ) ] )
    coord_y <- mean( st_coordinates( df_coord_subset$geometry )[ ( nrow( df_coord_subset ) + 1 ):( nrow( df_coord_subset ) * 2 ) ] )
    
    # keep track of numbers
    index_pc_buurt <- index_pc_buurt + 1
    } 
  
  # for (1) only postal code or (2) only buurt code existing in etrs file
  else {
    
    # subset coordinate df
    df_coord_subset <- subset( sf_cent, sf_cent$POSTCODE == df_pc_subset$pc4 )  
    
    if( nrow( df_coord_subset ) > 0 ){
      
      # calculate coordinate means over postal code
      coord_x <- mean( st_coordinates( df_coord_subset$geometry )[ 1:nrow( df_coord_subset ) ] )
      coord_y <- mean( st_coordinates( df_coord_subset$geometry )[ ( nrow( df_coord_subset ) + 1 ):( nrow( df_coord_subset ) * 2 ) ] )
      
      # keep track of numbers
      index_pc <- index_pc + 1
      }
    else{
      
      # subset coordinate df
      df_coord_subset <- subset( sf_cent, sf_cent$BU_CODE == df_pc_subset$buurt_code )
      
      # calculate coordinate means over buurtcode
      coord_x <- mean( st_coordinates( df_coord_subset$geometry )[ 1:nrow( df_coord_subset ) ] )
      coord_y <- mean( st_coordinates( df_coord_subset$geometry )[ ( nrow( df_coord_subset ) + 1 ):( nrow( df_coord_subset ) * 2 ) ] )
      
      # keep track of numbers
      index_buurt <- index_buurt + 1
    }
  }
  
  # merge in df
  df <- as.data.frame( cbind( post, coord_x, coord_y ) )
 
  # convert to sf object
  df <- st_as_sf( x = df, coords = c( "coord_x", "coord_y" ), crs = st_crs( shp_etrs ) )
  
  # rbind sf objects
  df_final <- rbind( df_final, df )
}

# numbers (total = 1176)
index_pc_buurt # 1073
index_pc # 101 
index_buurt # 2

# modify + save sf with unique postal codes
colnames( df_final )[ 1 ] <- "pc6"
write_sf( df_final, paste0( outdir, "/", "coordinates_per_patient_pc.shp" ) )

# merge with patient data and remove duplicates
# existed because in pc_list 1 pc could have had multiple buurt codes
sf_pc <- inner_join( pc_list, df_final )
sf_pc <- subset( sf_pc, ! duplicated( sf_pc$id ) ) # 1212 (one missing: no Dutch postal code)

# calculate frequency of each postal code
pc_freq <- as.data.frame( table( sf_pc$pc6 ) )
colnames( pc_freq ) <- c( "pc6", "pc_freq" )
sf_pc <- inner_join( sf_pc, pc_freq )

# save sf with patient postal codes
write_sf( sf_pc, paste0( outdir, "/patient_pc_etrs.shp" ) )