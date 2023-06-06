###########################################################
# Load openCBS postcode shape and transform to etrs #######
# Trude Slinger - g.slinger-2@umcutrecht.nl ###############
###########################################################

# openCBS data: https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/
# geografische-data/wijk-en-buurtkaart-2021

# packages
library( "rgdal" )
library( "sf" ) 

# wd
setwd()

# outdir
outdir <- "out.transform_csb_buurten"
dir.create( outdir, showWarnings = FALSE )

# shape file with postcodes
infile <- "data/geographical/coordinates_buurt.shp"

################################################################################
# PROJECTION
################################################################################

# open shapefile
shp <- sf::read_sf( infile )

# check projection
sf::st_crs( shp )

# reproject to ETRS_1989_LAEA, CRS:3035 
shp_etrs <- sf::st_transform( shp, 3035 )

# check the new projection
sf::st_crs( shp_etrs )

# save the layer in the new projection 
write_sf( shp_etrs, paste0( outdir, "/buurt_etrs_2021.shp" ) )

# plot + save example (map of the Netherlands)
data <- shp_etrs[ shp_etrs$POSTCODE != "-99999999" & !is.na( shp_etrs$POSTCODE ), ]

png( file = paste0( outdir, "/sample_plot_Netherlands.png" ), width = 700, height = 850 )
plot( data, max.plot = 1, border = NA ) 
dev.off()
