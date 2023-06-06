###########################################################
# Extract pollution data ##################################
# Trude Slinger - g.slinger-2@umcutrecht.nl ###############
###########################################################

# packages 
library( "sf" ) 
library( "raster" )
library( "ggplot2" )
library( "stringr" )

# wd
setwd()

# outdir
outdir <- "out.extract_pollution_data"
dir.create( outdir, showWarnings = FALSE )

# load patient data
infile_pt <- "data/clinical/diagnosis_data_including_coordinates.shp"
shp_patient <- sf::read_sf( infile_pt )

# load pollution data
indir_poll <- "data/pollution/expanse"
poll_infiles <- dir( indir_poll )

######################################
# EXTRACT POLLUTION DATA
######################################

# containers
poll_container <- data.frame( matrix( nrow = 1, ncol = 4 ))
colnames( poll_container ) <- c( "NO2", "OZO", "P10", "P25" )
patient_container <- NULL

# extract pollution data
for( i in 1:nrow( shp_patient ) ) {
  
  # get year of presentation (2 numbers)
  year_presentation <- str_split_fixed( shp_patient$prsntt_[ i ], "-", 3 )[ 3 ]
  year_presentation <- str_sub( year_presentation, 3, 4 )
  
  # recode years 2020 and 2021 to 2019 (poll data available up to 2019)
  if( year_presentation == 20 | year_presentation == 21 ){ year_presentation <- 19 }

  # select infiles per patient
  poll_data <- poll_infiles[ grep( paste0( "_", year_presentation ), poll_infiles, fixed = T ) ]

  # extract pollution data
  for( n in 1:length ( poll_data ) ){
   
    # load pollution data
    poll_raster <- raster::raster( paste0( indir_poll, "/", poll_data[ n ] ) )
    
    # extract pollution data based on postal code
    # add to data frame
    coordinates <- st_as_sf( shp_patient$geometry[ i ] )
    poll_value <- raster::extract( poll_raster, coordinates )
    poll_container[ n ] <- poll_value
    
    # only if data is complete (i.e., 4 pollutants)
    if( n == 4 ){
    # add to data frame
    shp_patient_poll <- cbind( shp_patient[ i, ], poll_container )
    
    # add to final container
    patient_container <- rbind( patient_container, shp_patient_poll )
    }
  }
}

# save to file
write_sf( patient_container, paste0( outdir, "/patient_pollution_data_with_coordinates.shp" ) )
patient_container_no_coord <- st_drop_geometry( patient_container )
write.csv( patient_container_no_coord, paste0( outdir, "/patient_pollution_data_without_coordinates.csv" ) )
