# NOAA_OISST_function.R
# A function to extract mean sea surface temperature data from NOAA's Optimum 
# Interpolated Sea Surface Temperature (OISST) v2 High Resolution datasets.
# Daily data are available on a 1/4 degree global grid. 
# Author: Luke Miller Jan 13, 2014
###############################################################################

extractOISST = function(fname,lsmask,lon1,lon2,lat1,lat2, date1, date2){
	# This function takes 1-year NetCDF files from the 
	# ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2.highres/ directory
	# where filenames for daily mean SST files are named with the scheme
	# sst.day.mean.YEAR.v2.nc 
	# _________________________________________________________________
	require(ncdf)
	# Inputs
	# fname: full path to NetCDF data file
	# lsmask: full path to land-sea mask NetCDF file
	# lon1: western-most longitude of search area, must be smaller than lon2
	# lon2: eastern-most longitude of search area, must be larger than lon1
	# lat1: southern-most latitude of search area, must be smaller than lat2
	# lat2: northern-most latitude of search area, must be larger than lat1
	# date1: first date in file to extract, must be Date class
	# date2: last date in file to extract, must be Date class
	# lon2, lat2, date2 are optional. 
	
	# Output
	# A 3-dimensional array with latitudes in rows, longitudes in columns, and
	# dates along the 3rd dimension. The value [1,1,1] is the northernmost, 
	# westernmost lat/long location on the 1st date. The value [1,1,2] is the
	# 2nd date at the same lat/long location (if more than 1 date is requested).
	# To extract lat/lon/date values from the output array, use the 
	# dimnames() function:
	# lats = as.numeric(dimnames(sst2)$Lat)
	# longs = as.numeric(dimnames(sst2)$Long)
	# dates = as.Date(dimnames(sst2)$Date)
	
	# ________________________________________________________
	# NetCDF files should be downloaded from the links on:
	# http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html
	# In addition to the temperature data files, also download a copy of the 
	# landmask file lsmask.oisst.v2.nc from the same page. 
	
	# Inside the NetCDF files, data are available on a 
	# 0.25 degree latitude x 0.25 degree longitude global grid (1440x720 cells)
	# From -89.875N to 89.875N, 0.125E to 359.875E.
	# Locations are at the CENTER of a grid cell. 
	# Southern Hemisphere latitudes must be given as NEGATIVE degrees NORTH.
	# For example, the Tropic of Capricorn is at roughly -23.43 degrees north.
	# All longitudes must be given as positive degrees EAST of the prime 
	# meridian. For example, Los Angeles is at roughly 241.77 degrees east. 
	
	# Generate set of grid cell latitudes (center of cell) from south to north
	lats = seq(-89.875,89.875,0.25)
	# Generate set of grid cell longitudes (center of cell)
	lons = seq(0.125,359.875,0.25) 
	# Create connection to NetCDF data file
	nc = open.ncdf(fname)
	
	lon1indx = which.min(abs(lon1 - lons)) #get index of nearest longitude value
	if (missing(lon2)){
		# If lon2 isn't specified, reused lon1
		lon2 = lon1
		lon2indx = lon1indx
		cat("Only 1 longitude specified\n")
	} else {
		# Get index of nearest longitude value to lon2
		lon2indx = which.min(abs(lon2 - lons)) 	
	}
	lat1indx = which.min(abs(lat1 - lats)) #get index of nearest latitude value
	if (missing(lat2)){
		# If lat2 is not specified, reuse lat1
		lat2 = lat1
		lat2indx = lat1indx
		cat("Only 1 latitude specified\n")
	} else {
		# Get index of nearest latitude value to lat2
		lat2indx = which.min(abs(lat2 - lats)) 	
	}
	
	# The lon/lat indx values should now correspond to indices in the NetCDF 
	# file for the desired grid cell. 
	nlon = (lon2indx - lon1indx) + 1 # get number of longitudes to extract
	nlat = (lat2indx - lat1indx) + 1 # get number of latitudes to extract
	
	# Extract available dates from netCDF file
	ncdates = nc$dim$time$vals
	ncdates = as.Date(ncdates,origin = '1800-1-1') #available time points in nc
	
	if (class(date1) == 'Date'){
		# Get index of nearest time point
		date1indx = which.min(abs(date1 - ncdates)) 	
	} else if (class(date1) == 'character'){
		# Convert to a Date object first
		date1 = as.Date(date1)
		date1indx = which.min(abs(date1 - ncdates)) 
	}
	if (missing(date2)) {
		# If date2 isn't specified, reuse date1
		date2indx = which.min(abs(date1 - ncdates)) 
		cat('Only 1 date specified\n')
	} else {
		if (class(date2) == 'Date'){
			# If date2 exists, get index of nearest time point to date2
			date2indx = which.min(abs(date2 - ncdates)) 		
		} else if (class(date1) == 'character'){
			date2 = as.Date(date2)
			date2indx = which.min(abs(date2 - ncdates))
		}
	}
	
	ndates = (date2indx - date1indx) + 1 #get number of time steps to extract
	
	# Define the output array
	sstout = array(data = NA, dim = c(nlon,nlat,ndates))
	# Extract the data from the NetCDF file
	sstout[,,] = get.var.ncdf(nc, varid = 'sst', 
			start = c(lon1indx,lat1indx,date1indx),
			count = c(nlon,nlat,ndates))
	# The output array sstout will be arranged with longitudes in rows, 
	# increasing in an easterly direction as you move down a row (larger 
	# longitude values), and latitudes in columns, increasing in latitude (more 
	# northerly) as you move across columns. The 3rd dimension represents 
	# different dates. This arrangement stems from how the data are set up in 
	# the NetCDF file, so my apologies if it's counterintuitive. 

	# If there are missing data in the NetCDF, they should appear as 32767. 
	# Replace that value with NA if it occurs anywhere.
	sstout = ifelse(sstout == 32767, NA, sstout)

	# The NOAA OISST files contain sea surface temperatures for the entire 
	# globe, including on the continents. This clearly isn't right, so they also
	# supply a land-sea mask file in netCDF format. We use the values (0 or 1) 
	# stored in the mask file to turn all of the continent areas into NA's. 
	# Open the land-sea mask
	nc2 = open.ncdf(lsmask) 
	# Create array to hold land-sea mask
	mask = array(data = NA, dim = c(nlon,nlat,1))
	# Get land-sea mask values (0 or 1)
	mask[,,] = get.var.ncdf(nc2, varid = "lsmask", 
			start = c(lon1indx,lat1indx,1), count = c(nlon,nlat,1)) 
	# Replace 0's with NA's
	mask = ifelse(mask == 0,NA,1) 
	
	# Get dimensions of sstout array
	dims = dim(sstout) 
	for (i in 1:dims[3]){
		sstout[,,i] = sstout[,,i] * mask[,,1] # All masked values become NA
		# Add dimension names
		attr(sstout,'dimnames') = list(Long = seq(lons[lon1indx],lons[lon2indx],
						by = 0.25),
				Lat = seq(lats[lat1indx],lats[lat2indx], 
						by = 0.25),
				Date = as.character(seq(ncdates[date1indx],
								ncdates[date2indx],by = 1)))
	}
	# sstout now has dimension names that show the longitude and latitude of
	# each point in the array, as well as the date (3rd dimension of the array).
	############################################################################
	# Rearrange the output matrix or array so that latitudes run from north to
	# south down the rows, and longitudes run from west to east across columns.
	dims = dim(sstout) # get size of array
	# Make new output array to hold rearranged data. The dimension names will
	# match the newly rearranged latitude and longitude values
	sst2 = array(data = NA, dim = c(dims[2],dims[1],dims[3]),
			dimnames = list(Lat = rev(seq(lats[lat1indx],lats[lat2indx], 
									by = 0.25)),
					Long = seq(lons[lon1indx],lons[lon2indx],by = 0.25),
					Date = as.character(seq(ncdates[date1indx],
									ncdates[date2indx],by = 1))))
	# Step through each page of array and rearrange lat/lon values
	for (i in 1:dims[3]){
		# Extract one day's worth of lat/lon pairs
		temp = as.matrix(sstout[,,i])
		temp = t(temp) # transpose lon/lat to lat/lon
		temp = temp[nrow(temp):1,] # reverse row order to reverse latitudes
		sst2[,,i] = temp # write data to sst2 array
	}	
	
	##########################
	sst2 # return sst2 array
	##########################
} # end of function 


extractOISST1day = function(fname,lsmask,lon1,lon2,lat1,lat2){
	require(ncdf)
	# The 1-day OISST v2 high resolution files come as gz-compressed NetCDF 
	# files. You must unzip those files before trying to use this function. 
	# ______________________________________________________________________
	# Inputs
	# fname: full path to NetCDF data file
	# lsmask: full path to land-sea mask NetCDF file
	# lon1: western-most longitude of search area, must be smaller than lon2
	# lon2: eastern-most longitude of search area, must be larger than lon1
	# lat1: southern-most latitude of search area, must be smaller than lat2
	# lat2: northern-most latitude of search area, must be larger than lat1
	# lon2, lat2 are optional. 
	
	# Output
	# A 2-dimensional matrix with latitudes in rows and longitudes in columns.
	# The value [1,1] is the northernmost, 
	# westernmost lat/long location on the 1st date.
	# To extract lat/lon values from the output array, use the 
	# dimnames() function:
	# lats = as.numeric(dimnames(sst2)$Lat)
	# longs = as.numeric(dimnames(sst2)$Long)
	
	# ________________________________________________________
	# NetCDF files should be downloaded from the links on:
	# ftp://eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/
	# 
	# In addition to the temperature data files, also download a copy of the 
	# landmask file lsmask.oisst.v2.nc from:  
	# http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html
	
	# Inside the NetCDF files, data are available on a 
	# 0.25 degree latitude x 0.25 degree longitude global grid (1440x720 cells)
	# From -89.875N to 89.875N, 0.125E to 359.875E.
	# Locations are at the CENTER of a grid cell. 
	# Southern Hemisphere latitudes must be given as NEGATIVE degrees NORTH.
	# For example, the Tropic of Capricorn is at roughly -23.43 degrees north.
	# All longitudes must be given as positive degrees EAST of the prime 
	# meridian. For example, Los Angeles is at roughly 241.77 degrees east. 
	
	# Generate set of grid cell latitudes (center of cell) from south to north
	lats = seq(-89.875,89.875,0.25)
	# Generate set of grid cell longitudes (center of cell)
	lons = seq(0.125,359.875,0.25) 
	# Create connection to NetCDF data file (must be unzipped manually already)
	nc = open.ncdf(fname)
	
	lon1indx = which.min(abs(lon1 - lons)) #get index of nearest longitude value
	if (missing(lon2)){
		# If lon2 isn't specified, reused lon1
		lon2 = lon1
		lon2indx = lon1indx
		cat("Only 1 longitude specified\n")
	} else {
		# Get index of nearest longitude value to lon2
		lon2indx = which.min(abs(lon2 - lons)) 	
	}
	lat1indx = which.min(abs(lat1 - lats)) #get index of nearest latitude value
	if (missing(lat2)){
		# If lat2 is not specified, reuse lat1
		lat2 = lat1
		lat2indx = lat1indx
		cat("Only 1 latitude specified\n")
	} else {
		# Get index of nearest latitude value to lat2
		lat2indx = which.min(abs(lat2 - lats)) 	
	}
	
	# The lon/lat indx values should now correspond to indices in the NetCDF 
	# file for the desired grid cell. 
	nlon = (lon2indx - lon1indx) + 1 # get number of longitudes to extract
	nlat = (lat2indx - lat1indx) + 1 # get number of latitudes to extract
	# Extract the date from the file
	dateref = nc$dim$time$units
	dateref = sub('days since ','',dateref,ignore.case=TRUE)
	date1 = as.Date(nc$dim$time$vals[1],origin = dateref)
	# Although this extracts the date, I do not currently include it in the
	# output, as I assume you are already getting the date from the input 
	# filename before running this function.

	# Define the output array
	sstout = matrix(data = NA, nrow = nlon, ncol = nlat)
	# Extract the data from the NetCDF file
	# There are extra zlev and date dimensions that aren't applicable in the
	# single-day mean SST extraction
	sstout[,] = get.var.ncdf(nc, varid = 'sst', 
			start = c(lon1indx,lat1indx,1,1),
			count = c(nlon,nlat,1,1))
	# The output array sstout will be arranged with longitudes in rows, 
	# increasing in an easterly direction as you move down a row (larger 
	# longitude values), and latitudes in columns, increasing in latitude (more 
	# northerly) as you move across columns. 
	# This arrangement stems from how the data are set up in 
	# the NetCDF file, so my apologies if it's counterintuitive. 
	
	# If there are missing data in the NetCDF, they should appear as 32767. 
	# Replace that value with NA if it occurs anywhere.
	sstout = ifelse(sstout == 32767, NA, sstout)
	
	# The NOAA OISST files contain sea surface temperatures for the entire 
	# globe, including on the continents. This clearly isn't right, so they also
	# supply a land-sea mask file in netCDF format. We use the values (0 or 1) 
	# stored in the mask file to turn all of the continent areas into NA's. 
	# Open the land-sea mask
	nc2 = open.ncdf(lsmask) 
	# Create array to hold land-sea mask
	mask = array(data = NA, dim = c(nlon,nlat,1))
	# Get land-sea mask values (0 or 1)
	mask[,,] = get.var.ncdf(nc2, varid = "lsmask", 
			start = c(lon1indx,lat1indx,1), count = c(nlon,nlat,1)) 
	# Replace 0's with NA's
	mask = ifelse(mask == 0,NA,1) 
	

	sstout[,] = sstout[,] * as.matrix(mask[,,1]) # All masked values become NA
		# Add dimension names
	attr(sstout,'dimnames') = list(Long = seq(lons[lon1indx],lons[lon2indx],
					by = 0.25), Lat = seq(lats[lat1indx],lats[lat2indx], 
					by = 0.25))
	
	# sstout now has dimension names that show the longitude and latitude of
	# each point in the array.
	############################################################################
	# Rearrange the output matrix or array so that latitudes run from north to
	# south down the rows, and longitudes run from west to east across columns.
	sstout = t(sstout)
	sstout = sstout[nrow(sstout):1,]
	
	#############################
	sstout # return sstout matrix
	#############################
} # end of function 


plotOISST = function(sst2){
	# For plotting a simple image of the data from a multi-day OISST file
	# When using image() with the rearranged data in sst2, it is necessary to 
	# reverse the order of the latitudes so they increase, and 
	# transpose and reverse the order in which the latitudes in sst2 are 
	# plotted. 
	image(x = as.numeric(dimnames(sst2)$Lon),
			y = rev(as.numeric(dimnames(sst2)$Lat)),
			if (length(dim(sst2))>2) {
				as.matrix(t(sst2[nrow(sst2):1,,1]))
			} else {
				t(sst2[nrow(sst2):1,])
			},
			las = 1,
			ylim = range(mylats), 
			col = rainbow(64, start = 4/6, end = 6/6),
			ylab = "Latitude (degrees north)",
			xlab = "Longitude (degrees east)",
			add = FALSE)
}

