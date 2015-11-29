# NOAA_OISST_ncdf4.R
# Functions to extract mean sea surface temperature data from NOAA's Optimum 
# Interpolated Sea Surface Temperature (OISST) v2 High Resolution daily or
# weekly datasets.
# Daily data are available on a 1/4° global grid. 
# Weekly data are available on a 1° global grid.
# Windows users, note that you'll need to manually download the ncdf4 package
# from http://cirrus.ucsd.edu/~pierce/ncdf/, since it is not available 
# directly from CRAN. David Pierce is the author of the ncdf4 package. 

# See http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html
# for information about the year-long OISST v2 daily files.

# See http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html
# for more information about the weekly OISST v2 files

# See http://www.ncdc.noaa.gov/sst/griddata.php 
# for more information about the single-day AVHRR OISST files

# Author: Luke Miller Nov 25, 2014
###############################################################################
require(ncdf4)	# install.packages('ncdf4') if you don't already have it.
# NOTE: If you are on Windows, a pre-compiled package is not available directly 
# from CRAN repositories. You must go to http://cirrus.ucsd.edu/~pierce/ncdf/ 
# and download the appropriate zip file for your version of Windows and R.
# Once that file is downloaded to your computer, open a version of Rgui.exe
# and go to the menu item "Packages>Install package(s) from local zip file"
# to install the ncdf4 package. This step should only be necessary once, until
# you upgrade to a new version of R. Mac and Linux versions of ncdf4 should be
# available directly from CRAN.

require(fields) # install.packages('fields') if you don't already have it.

extractOISSTdaily = function(fname,lsmask,lonW,lonE,latS,latN, date1, date2){
	# This function takes 1-year-long NetCDF files of daily SST from the 
	# ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2.highres/ directory
	# where filenames for daily mean SST files are named with the scheme
	# sst.day.mean.YEAR.v2.nc 
	# _________________________________________________________________
	
	# Inputs
	# fname: full path to NetCDF data file
	# lsmask: full path to land-sea mask NetCDF file
	# lonW: western-most longitude of search area, must be smaller than lonE
	# lonE: eastern-most longitude of search area, must be larger than lon1
	# latS: southern-most latitude of search area, must be smaller than latN
	# latN: northern-most latitude of search area, must be larger than latS
	# date1: first date in file to extract, must be Date class
	# date2: last date in file to extract, must be Date class
	# lonE, latN, date2 are optional. 
	
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
	# 0.25 degree latitude x 0.25 degree longitude global grid (720x1440 cells)
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
	nc = nc_open(fname)
	
	lonWindx = which.min(abs(lonW - lons)) #get index of nearest longitude value
	if (missing(lonE)){
		# If lonE isn't specified, reused lon1
		lonE = lonW
		lonEindx = lon1indx
		cat("Only 1 longitude specified\n")
	} else {
		# Get index of nearest longitude value to lonE
		lonEindx = which.min(abs(lonE - lons)) 	
	}
	latSindx = which.min(abs(latS - lats)) #get index of nearest latitude value
	if (missing(latN)){
		# If latN is not specified, reuse latS
		latN = latS
		latNindx = latSindx
		cat("Only 1 latitude specified\n")
	} else {
		# Get index of nearest latitude value to latN
		latNindx = which.min(abs(latN - lats)) 	
	}
	
	# The lon/lat indx values should now correspond to indices in the NetCDF 
	# file for the desired grid cell. 
	nlon = (lonEindx - lonWindx) + 1 # get number of longitudes to extract
	nlat = (latNindx - latSindx) + 1 # get number of latitudes to extract
	
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
		} else if (class(date2) == 'character'){
			date2 = as.Date(date2)
			date2indx = which.min(abs(date2 - ncdates))
		}
	}
	
	ndates = (date2indx - date1indx) + 1 #get number of time steps to extract
	
	# Define the output array
	sstout = array(data = NA, dim = c(nlon,nlat,ndates))
	# Extract the data from the NetCDF file
	sstout[,,] = ncvar_get(nc, varid = 'sst', 
			start = c(lonWindx,latSindx,date1indx),
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
	nc2 = nc_open(lsmask) 
	# Create array to hold land-sea mask
	mask = array(data = NA, dim = c(nlon,nlat,1))
	# Get land-sea mask values (0 or 1)
	mask[,,] = ncvar_get(nc2, varid = "lsmask", 
			start = c(lonWindx,latSindx,1), count = c(nlon,nlat,1)) 
	# Replace 0's with NA's
	mask = ifelse(mask == 0,NA,1) 
	
	# Get dimensions of sstout array
	dims = dim(sstout) 
	for (i in 1:dims[3]){
		sstout[,,i] = sstout[,,i] * mask[,,1] # All masked values become NA
		# Add dimension names
		attr(sstout,'dimnames') = list(Long = seq(lons[lonWindx],lons[lonEindx],
						by = 0.25),
				Lat = seq(lats[latSindx],lats[latNindx], 
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
			dimnames = list(Lat = rev(seq(lats[latSindx],lats[latNindx], 
									by = 0.25)),
					Long = seq(lons[lonWindx],lons[lonEindx],by = 0.25),
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

extractOISSTweekly = function(fname,lsmask,lonW,lonE,latS,latN, date1, date2){
	# This function uses the 1x1° lat/long gridded weekly SST NetCDF files from 
	# http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html
	# either: sst.wkmean.1981-1989.nc
	# or: sst.wkmean.1990-present.nc
	# and the land-sea mask called lsmask.nc available from that page.
	# This function cannot use the land-sea mask file from the high-resolution
	# daily files, which are on a 0.25 x 0.25° grid
	# _________________________________________________________________
	
	# Inputs
	# fname: full path to NetCDF data file
	# lsmask: full path to land-sea mask NetCDF file
	# lonW: western-most longitude of search area, must be smaller than lon2
	# lonE: eastern-most longitude of search area, must be larger than lon1
	# latS: southern-most latitude of search area, must be smaller than lat2
	# latN: northern-most latitude of search area, must be larger than lat1
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
	# 1 degree latitude x 1 degree longitude global grid (180x360 cells)
	# From -89.5N to 89.5N, 0.5E to 359.5E.
	# Locations are at the CENTER of a grid cell. 
	# Southern Hemisphere latitudes must be given as NEGATIVE degrees NORTH.
	# For example, the Tropic of Capricorn is at roughly -23.43 degrees north.
	# All longitudes must be given as positive degrees EAST of the prime 
	# meridian. For example, Los Angeles is at roughly 241.77 degrees east. 
	
	# Generate set of grid cell latitudes (center of cell) from north to south
	# These are arranged in the reverse of the daily gridded files
	lats = seq(89.5,-89.5,-1)
	# Generate set of grid cell longitudes (center of cell)
	lons = seq(0.5,359.5,1) 
	# Create connection to NetCDF data file
	nc = nc_open(fname)
	
	lonWindx = which.min(abs(lonW - lons)) #get index of nearest longitude value
	if (missing(lonE)){
		# If lon2 isn't specified, reuse lon1
		lonE = lonW
		lonEindx = lonWindx
		cat("Only 1 longitude specified\n")
	} else {
		# Get index of nearest longitude value to lon2
		lonEindx = which.min(abs(lonE - lons)) 	
	}
	
	latNindx = which.min(abs(latN - lats)) #get index of nearest latitude value
	if (missing(latS)){
		# If lat2 is not specified, reuse lat1
		latS = latN
		latSindx = latNindx
		cat("Only 1 latitude specified\n")
	} else {
		# Get index of nearest latitude value to lat2
		latSindx = which.min(abs(latS - lats)) 	
	}
	
	# The lon/lat indx values should now correspond to indices in the NetCDF 
	# file for the desired grid cell. 
	nlon = (lonEindx - lonWindx) + 1 # get number of longitudes to extract
	nlat = (latSindx - latNindx) + 1 # get number of latitudes to extract
	
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
		} else if (class(date2) == 'character'){
			date2 = as.Date(date2)
			date2indx = which.min(abs(date2 - ncdates))
		}
	}
	
	ndates = (date2indx - date1indx) + 1 #get number of time steps to extract
	
	datesout = ncdates[date1indx:date2indx] # Get vector of actual dates
	
	# Define the output array
	sstout = array(data = NA, dim = c(nlon,nlat,ndates))
	# Extract the data from the NetCDF file
	sstout[,,] = ncvar_get(nc, varid = 'sst', 
			start = c(lonWindx,latNindx,date1indx),
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
	nc2 = nc_open(lsmask) 
	# Create array to hold land-sea mask
	mask = array(data = NA, dim = c(nlon,nlat,1))
	# Get land-sea mask values (0 or 1)
	mask[,,] = ncvar_get(nc2, varid = "mask", 
			start = c(lonWindx,latNindx,1), count = c(nlon,nlat,1)) 
	# Replace 0's with NA's
	mask = ifelse(mask == 0,NA,1) 
	
	# Get dimensions of sstout array
	dims = dim(sstout) 
	for (i in 1:dims[3]){
		sstout[,,i] = sstout[,,i] * mask[,,1] # All masked values become NA
		# Add dimension names
		attr(sstout,'dimnames') = list(Long = seq(lons[lonWindx],lons[lonEindx],
						by = 1),
				Lat = seq(lats[latNindx],lats[latSindx], 
						by = -1),
				Date = as.character(datesout))
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
			dimnames = list(Lat = rev(seq(lats[latSindx],lats[latNindx], 
									by = 1)),
					Long = seq(lons[lonWindx],lons[lonEindx],by = 1),
					Date = as.character(datesout)))
	# Step through each page of array and rearrange lat/lon values
	for (i in 1:dims[3]){
		# Extract one day's worth of lat/lon pairs
		temp = as.matrix(sstout[,,i])
		temp = t(temp) # transpose lon/lat to lat/lon
#		temp = temp[nrow(temp):1,] # reverse row order to reverse latitudes
		sst2[,,i] = temp # write data to sst2 array
	}	
	
	##########################
	sst2 # return sst2 array
	##########################
} # end of function 



extractOISST1day = function(fname,lsmask,lonW,lonE,latS,latN){
	# The 1-day OISST v2 high resolution files come as gz-compressed NetCDF 
	# files. You MUST unzip those files before trying to use this function.
	# Obtain the files from http://www.ncdc.noaa.gov/sst/griddata.php 
	# or directly from ftp://eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/
	
	# These are essentially just smaller single-day versions of the much
	# larger one-year files that contain daily data that are available at
	# ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2.highres/
	# ______________________________________________________________________
	# Inputs
	# fname: full path to unzipped NetCDF data file
	# lsmask: full path to land-sea mask NetCDF file
	# lonW: western-most longitude of search area, must be smaller than lonE
	# lonE: eastern-most longitude of search area, must be larger than lonW
	# latS: southern-most latitude of search area, must be smaller than latN
	# latN: northern-most latitude of search area, must be larger than latS
	# lonE, latN are optional. 
	
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
	# 0.25° latitude x 0.25° longitude global grid (720x1440 cells)
	# From -89.875N to 89.875N, 0.125E to 359.875E.
	# Locations are at the CENTER of a grid cell. 
	# Southern Hemisphere latitudes must be given as NEGATIVE degrees NORTH.
	# For example, the Tropic of Capricorn is at roughly -23.43° north.
	# All longitudes must be given as positive degrees EAST of the prime 
	# meridian. For example, Los Angeles is at roughly 241.77° east. 
	
	# Generate set of grid cell latitudes (center of cell) from south to north
	lats = seq(-89.875,89.875,0.25)
	# Generate set of grid cell longitudes (center of cell)
	lons = seq(0.125,359.875,0.25) 
	# Create connection to NetCDF data file (must be unzipped manually already)
	nc = nc_open(fname)
	
	lonWindx = which.min(abs(lonW - lons)) #get index of nearest longitude value
	if (missing(lonE)){
		# If lonE isn't specified, reused lonW
		lonE = lonW
		lonEindx = lonWindx
		cat("Only 1 longitude specified\n")
	} else {
		# Get index of nearest longitude value to lonE
		lonEindx = which.min(abs(lonE - lons)) 	
	}
	latSindx = which.min(abs(latS - lats)) #get index of nearest latitude value
	if (missing(latN)){
		# If latN is not specified, reuse latS
		latN = latS
		latNindx = latSindx
		cat("Only 1 latitude specified\n")
	} else {
		# Get index of nearest latitude value to latN
		latNindx = which.min(abs(latN - lats)) 	
	}
	
	# The lon/lat indx values should now correspond to indices in the NetCDF 
	# file for the desired grid cell. 
	nlon = (lonEindx - lonWindx) + 1 # get number of longitudes to extract
	nlat = (latNindx - latSindx) + 1 # get number of latitudes to extract
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
	sstout[,] = ncvar_get(nc, varid = 'sst', 
			start = c(lonWindx,latSindx,1,1),
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
	nc2 = nc_open(lsmask) 
	# Create array to hold land-sea mask
	mask = array(data = NA, dim = c(nlon,nlat,1))
	# Get land-sea mask values (0 or 1)
	mask[,,] = ncvar_get(nc2, varid = "lsmask", 
			start = c(lonWindx,latSindx,1), count = c(nlon,nlat,1)) 
	# Replace 0's with NA's
	mask = ifelse(mask == 0,NA,1) 
	

	sstout[,] = sstout[,] * as.matrix(mask[,,1]) # All masked values become NA
		# Add dimension names
	attr(sstout,'dimnames') = list(Long = seq(lons[lonWindx],lons[lonEindx],
					by = 0.25), Lat = seq(lats[latSindx],lats[latNindx], 
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


plotOISST = function(sst2, day = 1) {
	# For plotting a simple image of the data from a multi-day OISST file
	# When using image() with the rearranged data in sst2, it is necessary to 
	# reverse the order of the latitudes so they increase, and 
	# transpose and reverse the order in which the latitudes in sst2 are 
	# plotted. 
	# If there is more than one time point, use the day argument to specify
	# which page of the array you want to plot (integer value <= dim(sst2)[3])
	lats = as.numeric(dimnames(sst2)$Lat) # extract latitudes
	lons = as.numeric(dimnames(sst2)$Long) # extract longitudes
	par(mar = c(4.5,4.5,3,6)) # widen margins to fit colorbar
	image(x = lons, y = rev(lats),	
			if (length(dim(sst2))>2){
						as.matrix(t(sst2[nrow(sst2):1, ,day]))
					} else {
						t(sst2[nrow(sst2):1,])
					},
			las = 1,
			ylim = range(lats),
			col = tim.colors(32),
			yaxt = "n", xaxt = "n",
			ylab = "Latitude (degrees north)",
			xlab = "Longitude (degrees east)",
			main = if(length(dim(sst2)>2)){
				dimnames(sst2)$Date[day] # extract date from array dimnames
			})
	axis(1,at = pretty(lons),
			labels = pretty(lons))
	axis(2,at = pretty(lats),
			labels = pretty(lats), las = 1)
	# image.plot from the fields package inserts a color bar
	
	image.plot(zlim = range(if(length(dim(sst2))>2){
						sst2[,,day]
					} else {
						sst2[,]
					}, na.rm=TRUE),
					nlevel = 32,
			legend.only = TRUE, horizontal = FALSE, col = tim.colors(32),
			legend.args = list(text = expression(Temperature*","~degree*C), 
					cex = 1.2,
					side = 4, line = 2))
}

