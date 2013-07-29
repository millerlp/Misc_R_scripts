# filename: OSU_VGPM_load_function.R
# This script contains 4 functions for loading data from OSU's VGPM .xyz data
# files and plotting the data. See below for further information.
# Author: Luke Miller Dec 2, 2011
###############################################################################

# This script is an attempt to extract data from Oregon State University's 
# Ocean Productivity Standard Vertically Generalized Production Model (VGPM) 
# files that are accessible here:
# http://www.science.oregonstate.edu/ocean.productivity/index.php

# This script deals specifically with files obtained from this page:
# http://orca.science.oregonstate.edu/1080.by.2160.monthly.xyz.vgpm.m.chl.m.sst4.php
# or this page:
# http://orca.science.oregonstate.edu/2160.by.4320.monthly.xyz.vgpm.m.chl.m.sst4.php

# This script is meant to work with .xyz files rather than .hdf files. The
# .xyz files simply store the data as a set of 3 columns of data with lat/lon 
# and the productivity value, in a simple text file format. 
# The files will follow the naming format: vgpm.yyyyddd.all.xyz.gz

# Files are in an Equidistant Cylindrical projection, and the lat/lon value of 
# the center of each pixel is given. 
# For 1080x2160 data (2332800 rows of data in the file), the grid spacing is at 
# 1/6 of a degree. There are 180 degrees of latitude starting at the north pole,
# and 360 degrees of longitude starting at -180 degrees (i.e. in the north 
# Pacific ocean). For 2160x4320 data files (9331200 rows of data in the file), 
# the grid spacing is a 1/12°. All lat/lon locations given in the file are for
# the center of a grid cell.

# See here: http://orca.science.oregonstate.edu/faq01.php for more info.

# To cite the data used here, cite:
#  Behrenfeld, MJ, PG Falkowski
#  Limnology and Oceanography
#  1997a, Volume 42: 1-20
#  Photosynthetic rates derived from satellite-based chlorophyll concentration
#
# as well as citing the website:
# 	http://www.science.oregonstate.edu/ocean.productivity/index.php

# The plotting routines below make use of the fields package, which must be
# installed before running these functions. The fields package was produced by
#   Reinhard Furrer, Douglas Nychka and Stephen Sain (2011). fields:
#   Tools for spatial data. R package version 6.6.1.
#   http://CRAN.R-project.org/package=fields

################################################################################
################################################################################
# The function vgpm.load() opens a vgpm .xyz data file and extracts the 
# productivity data from the specified region of interest. 
# The return value is a matrix of productivity values with associated grid cell
# latitudes and longitudes listed in the row and column names
# The options to vgpm.load() are as follows:
# file = file name (or substitute file.choose() to pick file interactively)
# w.lon = western longitude limit for region of interest (-180 to +180)
# e.lon = eastern longitude limit for region of interest (-180 to +180)
# n.lat = northern latitude limit for region of interest (+90 to -90)
# s.lat = southern latitude limit for region of interest (+90 to -90)

vgpm.load = function(file, w.lon, e.lon, n.lat, s.lat){
	
# The value -9999 is used as a missing data marker, so all occurrences are 
# changed to NA during the initial file read.
	x = read.table(file, sep = ' ', skip = 1, na.strings = '-9999')
	
	names(x) = c('lon','lat','values') #Rename input columns
	
	#Establish which grid size is being used, 1080x2160 or 2160x4320
	if (nrow(x) == 2332800) { f.size = '1080'
	} else if (nrow(x) == 9331200) { f.size = '2160'
	} else {
		warning('Unknown file type\n', immediate. = TRUE)
	}
	
# The units for the column 'values' should be mg C / m^2 / Day
# The data are arranged so that the first value is at ~-180 longitude, 
# +89.9166N latitude. The values then increase in longitude (encircling the 
# globe at a fixed latitude) for 2160 rows total. After that, the data move to 
# the next latitude value south, and the next 2160 values are for each longitude
# at the new latitude. This continues until you reach the southern-most latitude
# value. The process is the same for the higher-resolution 2160x4320 files.
	if (f.size == '1080') {
		lons = x$lon[1:2160] #get set of longitude values
		lats = x$lat[seq(1,2332800,by = 2160)] #get latitude values
		values = matrix(x$values, nrow = 1080, ncol = 2160, byrow = TRUE)
	} else if (f.size == '2160') {
		lons = x$lon[1:4320] #get set of longitude values
		lats = x$lat[seq(1,9331200,by = 4320)] #get latitude values
		values = matrix(x$values, nrow = 2160, ncol = 4320, byrow = TRUE)
	}
	#Insert the lat/lon values as the 'values' matrix dimension names
	dimnames(values) = list(Latitude = lats, Longitude = lons)
	
# Specify the boundaries of your lat/lon of interest. Recall that
# longitude values run from -180E (international date line in the Pacific)
# to +180E, where Greenwich,England is at 0E. Latitude values range from
# +90N (north pole) to -90 (south pole). The first value for longitude must be
# the western-most edge of your region of interest, and the first value for the
# latitude must be the northern-most edge of the region of interest.
	lonlim = c(w.lon,e.lon) # c(western edge, eastern edge)
	latlim = c(n.lat,s.lat)	# c(northern edge, southern edge)
	
# Create vectors of lat/lon indices
	lonindx = 1:length(lons) #make vector of longitude cell indices
	latindx = 1:length(lats) #make vector of latitude cell indices
	
# Pull out 2 vectors that contain the indices of the lat/lon coordinates
# of interest. We search for longitudes that are greater than the 1st value
# in lonlim, and longitudes that are less than the 2nd value in lonlim, and
# then grab the corresponding indices from lonindx to store in goodlons. The
# same is done for the latitudes
	goodlons = lonindx[lons >= lonlim[1] & lons <= lonlim[2]]
	goodlats = latindx[lats >= latlim[2] & lats <= latlim[1]]
	
# Extract a subset of the matrix 'values', call it the Region of Interest (ROI) 
	ROI = values[goodlats[1]:goodlats[length(goodlats)],
			goodlons[1]:goodlons[length(goodlons)]]
# Add the latitudes and longitudes to the ROI matrix as dimension names
	dimnames(ROI) = list(Latitude = lats[goodlats], Longitude = lons[goodlons])
	
	ROI # return the ROI matrix at the conclusion of the function
} # end of vgpm.load function

################################################################################
################################################################################
# The plot.ROI function produces a plot of the productivity data stored in the
# output matrix from the vgpm.load() function above. The ROI matrix is 
# expected to have latitudes in rows (from North to South) and longitudes in 
# columns (from -180 to +180°). The associated latitudes and longitudes must
# be in the row/column names of the matrix, as produced by vgpm.load().

plot.ROI = function(ROI, log = TRUE, color = tim.colors(30)){
	
# For plotting with the image.plot() command, it is necessary to arrange ROI 
# so that longitudes are in rows, and latitudes are in columns, with the 
# latitudes order reversed so that they increase as you move across columns. 
	ROI.plot = t(ROI) #transpose the ROI matrix
	ROI.plot = ROI.plot[,rev(1:ncol(ROI.plot))] #reverse the latitudes
	lons = as.numeric(rownames(ROI.plot)) #extract longitudes
	lats = as.numeric(colnames(ROI.plot)) #extract latitudes
	
	require(fields)
	if (log){
		image.plot(x=lons, y=lats, log10(ROI.plot),
				col = color,
				legend.lab = expression(paste(log[10],"(mg C / ",m^2,"/ day)")),
				legend.mar = 4.1,
				xlab = "Longitude",
				ylab = "Latitude",
				main = 'Net Primary Production',
				las = 1)
	} else if (!log){
		image.plot(x=lons, y=lats, ROI.plot,
				col = color,
				legend.lab = expression(paste("mg C / ",m^2,"/ day")),
				legend.mar = 4.1,
				xlab = "Longitude",
				ylab = "Latitude",
				main = 'Net Primary Production',
				las = 1)
	}
} #end of plot.ROI function

################################################################################
################################################################################
# The function vgpm.plot() reads data from the desired region of interest and
# also optionally plots the data. 

# The options supplied to vgpm.plot() are as follows:
# file = file name (or substitute file.choose() to pick file interactively)
# w.lon = western longitude limit for region of interest (-180 to +180)
# e.lon = eastern longitude limit for region of interest (-180 to +180)
# n.lat = northern latitude limit for region of interest (+90 to -90)
# s.lat = southern latitude limit for region of interest (+90 to -90)
# plot.data = , plots data from region of interest
# log = ,  log10 transform productivity data before plotting
# color = , specify color set to plot productivity data
# Function returns a matrix of productivity values for the specified region of
# interest with lat/lon listed in the row and column names.
vgpm.plot = function(file, w.lon, e.lon, n.lat, s.lat, plot.data = TRUE,
		log = TRUE, color = tim.colors(30)){
	
	#Extract date from file title
	fname = basename(file)
	dots = gregexpr('\\.',fname)
	yrday = substr(fname,dots[[1]][1]+1,dots[[1]][2]-1)
	yr = substr(yrday,1,4)
	doy = substr(yrday,5,7)
	day1 = as.Date(paste(yr,doy,sep = '-'), format = '%Y-%j')
# The value -9999 is used as a missing data marker, so all occurrences are 
# changed to NA during the initial file read.
	x = read.table(file, sep = ' ', skip = 1, na.strings = '-9999')
	
	names(x) = c('lon','lat','values') #Rename input columns
	
	#Establish which grid size is being used, 1080x2160 or 2160x4320
	if (nrow(x) == 2332800) { f.size = '1080'
	} else if (nrow(x) == 9331200) { f.size = '2160'
	} else {
		warning('Unknown file type\n', immediate. = TRUE)
	}
	
# The units for the column 'values' should be mg C / m^2 / Day
# The data are arranged so that the first value is at ~-180 longitude, 
# +89.9166N latitude. The values then increase in longitude (encircling the 
# globe at a fixed latitude) for 2160 rows total. After that, the data move to 
# the next latitude value south, and the next 2160 values are for each longitude
# at the new latitude. This continues until you reach the southern-most latitude
# value. The process is the same for the higher-resolution 2160x4320 files.
	if (f.size == '1080') {
		lons = x$lon[1:2160] #get set of longitude values
		lats = x$lat[seq(1,2332800,by = 2160)] #get latitude values
		values = matrix(x$values, nrow = 1080, ncol = 2160, byrow = TRUE)
	} else if (f.size == '2160') {
		lons = x$lon[1:4320] #get set of longitude values
		lats = x$lat[seq(1,9331200,by = 4320)] #get latitude values
		values = matrix(x$values, nrow = 2160, ncol = 4320, byrow = TRUE)
	}
	#Insert the lat/lon values as the 'values' matrix dimension names
	dimnames(values) = list(Latitude = lats, Longitude = lons)
	
# Specify the boundaries of your lat/lon of interest. Recall that
# longitude values run from -180E (international date line in the Pacific)
# to +180E, where Greenwich,England is at 0E. Latitude values range from
# +90N (north pole) to -90 (south pole). The first value for longitude must be
# the western-most edge of your region of interest, and the first value for the
# latitude must be the northern-most edge of the region of interest.
	lonlim = c(w.lon,e.lon) # c(western edge, eastern edge)
	latlim = c(n.lat,s.lat)	# c(northern edge, southern edge)
	
# Create vectors of lat/lon indices
	lonindx = 1:length(lons) #make vector of longitude cell indices
	latindx = 1:length(lats) #make vector of latitude cell indices
	
# Pull out 2 vectors that contain the indices of the lat/lon coordinates
# of interest. We search for longitudes that are greater than the 1st value
# in lonlim, and longitudes that are less than the 2nd value in lonlim, and
# then grab the corresponding indices from lonindx to store in goodlons. The
# same is done for the latitudes
	goodlons = lonindx[lons >= lonlim[1] & lons <= lonlim[2]]
	goodlats = latindx[lats >= latlim[2] & lats <= latlim[1]]
	
# Extract a subset of the matrix 'values', call it the Region of Interest (ROI) 
	ROI = values[goodlats[1]:goodlats[length(goodlats)],
			goodlons[1]:goodlons[length(goodlons)]]
# Add the latitudes and longitudes to the ROI matrix as dimension names
	dimnames(ROI) = list(Latitude = lats[goodlats], Longitude = lons[goodlons])
	##########################
	##########################
#Plotting section
# For plotting with the image.plot() command, it is necessary to arrange ROI 
# so that longitudes are in rows, and latitudes are in columns, with the 
# latitudes order reversed so that they increase as you move across columns. 
	if (plot.data){
		ROI.plot = t(ROI) #transpose the ROI matrix
		ROI.plot = ROI.plot[,rev(1:ncol(ROI.plot))] #reverse the latitudes
		lons = as.numeric(rownames(ROI.plot)) #extract longitudes
		lats = as.numeric(colnames(ROI.plot)) #extract latitudes
		
		require(fields)
		if (log){
			image.plot(x=lons, y=lats, log10(ROI.plot),
					col = color,
					legend.lab = expression(paste(log[10],
									"(mg C / ",m^2,"/ day)")),
					legend.mar = 4.1,
					xlab = "Longitude",
					ylab = "Latitude",
					main = paste('Net Primary Production',
							strftime(day1,'%B %Y')),
					las = 1)
		} else if (!log){
			image.plot(x=lons, y=lats, ROI.plot,
					col = color,
					legend.lab = expression(paste("mg C / ",m^2,"/ day")),
					legend.mar = 4.1,
					xlab = "Longitude",
					ylab = "Latitude",
					main = paste('Net Primary Production', 
							strftime(day1,'%B %Y')),
					las = 1)
		}
	}
	
	ROI # return the ROI matrix at the conclusion of the function
} # end of vgpm.load function


################################################################################
################################################################################
# If you want to plot a very large area, such as the whole globe, plotting the 
# data using the regular image() or image.plot() functions produces very large
# file sizes due to the large number of vectors used to make up the figure. 
# Often the figure may not even display correctly on the screen due to the large 
# file size. For small areas this won't be as much of a problem.
# It is more space-efficient to plot large data sets as a raster image with the 
# useRaster = TRUE option of the image plotting tools. 
# To make use of the useRaster option for plotting, the lat/lon grid must be on
# a completely regular grid, but due to the limitations of precision by the 
# computer, the 1/6° (or 1/12°) steps stored as characters in the input file 
# aren't converted to exact 1/6° steps for latitude and longitude.
# We can make a set of approximate lat/lon values for plotting. For the 1/6° 
# the maximal error for the latitudes calculated here (~1/30000°) versus those 
# reported in the original input file is a few meters of latitude 
# near the south pole. 
# The error is smallest in the northwest corner of the plot and largest in the 
# southeast corner. Remember this only applies to the displayed figure, not the
# data and lat/lon values extracted above.

# The options supplied to vgpm.raster() are as follows:
# file = file name (or substitute file.choose() to pick file interactively)
# w.lon = western longitude limit for region of interest (-180 to +180)
# e.lon = eastern longitude limit for region of interest (-180 to +180)
# n.lat = northern latitude limit for region of interest (+90 to -90)
# s.lat = southern latitude limit for region of interest (+90 to -90)
# log = TRUE - log10 transform productivity data before plotting
# color = specify color set to plot productivity data
# Function returns a matrix of productivity values for the specified region of
# interest with lat/lon listed in the row and column names.
vgpm.raster = function(file, w.lon, e.lon, n.lat, s.lat, log = TRUE, 
		color = tim.colors(30)){
	
	#Extract date from file title
	fname = basename(file)
	dots = gregexpr('\\.',fname) #find locations of . in file name
	yrday = substr(fname,dots[[1]][1]+1,dots[[1]][2]-1) #extract yearday combo
	yr = substr(yrday,1,4) #extract year
	doy = substr(yrday,5,7) #extract day of year
	day1 = as.Date(paste(yr,doy,sep = '-'), format = '%Y-%j') #convert to Date
	
	#Read data from input file
	x = read.table(file, sep = ' ', skip = 1, na.strings = '-9999')
	
	names(x) = c('lon','lat','values') #Rename input columns
	
	if (nrow(x) == 2332800) { f.size = '1080'
	} else if (nrow(x) == 9331200) { f.size = '2160'
	} else {
		warning('Unknown file type\n', immediate. = TRUE)
	}
	
	if (f.size == '1080') {
		lons = x$lon[1:2160] #get set of longitude values
		lats = x$lat[seq(1,2332800,by = 2160)] #get latitude values
		values = matrix(x$values, nrow = 1080, ncol = 2160, byrow = TRUE)
	} else if (f.size == '2160') {
		lons = x$lon[1:4320] #get set of longitude values
		lats = x$lat[seq(1,9331200,by = 4320)] #get latitude values
		values = matrix(x$values, nrow = 2160, ncol = 4320, byrow = TRUE)
	}
#Insert the lat/lon values as the 'values' matrix dimension names
	dimnames(values) = list(Latitude = lats, Longitude = lons)
	
# Specify the boundaries of your lat/lon of interest. Recall that
# longitude values run from -180E (international date line in the Pacific)
# to +180E, where Greenwich,England is at 0E. Latitude values range from
# +90N (north pole) to -90 (south pole). The first value for longitude must be
# the western-most edge of your region of interest, and the first value for the
# latitude must be the northern-most edge of the region of interest.
	lonlim = c(w.lon,e.lon) # c(western edge, eastern edge)
	latlim = c(n.lat,s.lat)	# c(northern edge, southern edge)
	
# Create vectors of lat/lon indices
	lonindx = 1:length(lons) #make vector of longitude cell indices
	latindx = 1:length(lats) #make vector of latitude cell indices
	
# Pull out 2 vectors that contain the indices of the lat/lon coordinates
# of interest. We search for longitudes that are greater than the 1st value
# in lonlim, and longitudes that are less than the 2nd value in lonlim, and
# then grab the corresponding indices from lonindx to store in goodlons. The
# same is done for the latitudes
	goodlons = lonindx[lons >= lonlim[1] & lons <= lonlim[2]]
	goodlats = latindx[lats >= latlim[2] & lats <= latlim[1]]
	
# Extract a subset of the matrix 'values', call it the Region of Interest (ROI) 
	ROI = values[goodlats[1]:goodlats[length(goodlats)],
			goodlons[1]:goodlons[length(goodlons)]]
# Add the latitudes and longitudes to the ROI matrix as dimension names
	dimnames(ROI) = list(Latitude = lats[goodlats], Longitude = lons[goodlons])
	n.lats = as.numeric(rownames(ROI))
	n.lons = as.numeric(colnames(ROI))
	
	# Generate a new set of lats and longs on a regular grid spacing for plot.
	if (f.size == '1080') {
		lats2 = seq(n.lats[1],(n.lats[length(n.lats)]-0.1666667),by=-0.1666667)
		lons2 = seq(n.lons[1],(n.lons[length(n.lons)]+0.1666667),by=0.1666667)
	} else if (f.size == '2160') {
		lats2 = seq(n.lats[1],(n.lats[length(n.lats)]-0.0833333),by=-0.0833333)
		lons2 = seq(n.lons[1],(n.lons[length(n.lons)]+0.0833333),by=0.0833333)
	}
	if(length(lats2) > length(n.lats)) lats2 = lats2[1:length(n.lats)]
	if(length(lons2) > length(n.lons)) lons2 = lons2[1:length(n.lons)]
	ROI.plot = t(ROI) # swap longs and lats in 'ROI', so lats are in columns
	ROI.plot = ROI.plot[,rev(1:length(lats2))] # reverse latitudes so that 
	 # southern lats are listed first
	if (log) {
		image.plot(lons2, rev(lats2), log10(ROI.plot), useRaster = TRUE, 
				col = color,
				xlab = 'Longitude', ylab = 'Latitude', 
				main = paste('Net Primary Production', strftime(day1,'%B %Y')), 
				legend.lab = expression(paste(log[10],'(mg C /', m^2,'/ day)')),
				legend.mar = 4.1)
	} else if (!log){
		image.plot(lons2, rev(lats2), ROI.plot, useRaster = TRUE, 
				col = color,
				xlab = 'Longitude', ylab = 'Latitude', 
				main = paste('Net Primary Production', strftime(day1,'%B %Y')), 
				legend.lab = expression(paste('mg C /', m^2,'/ day')),
				legend.mar = 4.3)
	}
	ROI # return region of interest data to workspace
}  # end of vgpm.raster() function
