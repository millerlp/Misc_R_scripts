# xtide_interface.R
# 
# Author: Luke Miller May 17, 2013
###############################################################################
# Site names: http://www.flaterco.com/xtide/locations.html 
# List of command line flags: http://www.flaterco.com/xtide/settings.html

#Obviously Xtide, or a port like tide.exe (for Windows) must be downloaded from 
# the XTide site and installed. It is a standalone program, separate from R.
###############################################################################

# Sys.setenv(TZ="UTC") 	# Useful if you're getting data for sites in multiple 
						# time zones and need to standardize on one time zone.


# Start and end times, supplied as character strings
startchar = '2013-05-16 16:00'
endchar = '2013-05-20 16:00'
# Site name, taken from http://www.flaterco.com/xtide/locations.html
sitename = 'Bodega Harbor entrance, California'

# If we need to change directories to get access to Xtide, this set of steps
# will save your current working directory and switch to the Xtide directory.
old.dir = getwd()
tidedir = "W:/xtide" # Enter the path to your Xtide directory here.
setwd(tidedir)

# -l	Give the site name after this switch
# -b	Beginning time, in the format YYYY-MM-DD HH:MM
# -e 	Ending time, in the format YYYY-MM-DD HH:MM
# -f 	output format, c = csv
# -u 	units, i.e. m or ft
# -em	event mask, suppress output of sunrise, sunset, moon phase etc
# -z 	UTC (zulu) time zone
# -ml	mark level, only returns info in plain mode when tide crosses this level
# -m	output information: r = raw, m = medium rare, p = plain
#		raw mode returns the site, unix time code, and height
#		medium rare mode returns site, date, time, and height
#		plain mode returns site, date, time, height, and event
# 			i.e. High Tide, Low Tide, mark level crossing, sunrise, sunset etc


################################################################################
# Create query to return high and low tide times only, with units in feet and
# using the current system time zone. 
tidecommand = paste('tide.exe -l "',sitename,'" -b "',
		startchar, '" -e "', endchar,
		'" -f c -em pSsMm -m p -u ft ', sep = '')

ss = system(tidecommand, intern = TRUE) #invoke tide.exe and return results

# Parse the high/low tide events into a data frame. 
hilo = data.frame(Site = character(0), Time = numeric(0), TideHt = numeric(0),
		Event = character(0))
for (i in 1:length(ss)) {
	commas = gregexpr(',',ss[i]) #get locations of commas in line
	Site = substr(ss[i],1,commas[[1]][1]-1) # Get site
	yr = substr(ss[i], commas[[1]][1]+1,commas[[1]][2]-1) #get year
	hr = substr(ss[i], commas[[1]][2]+1,commas[[1]][3]-1) #get date
	Time = as.POSIXlt(paste(yr,hr), format = "%Y-%m-%d %I:%M %p")
	TideHt = substr(ss[i],commas[[1]][3]+1,commas[[1]][4]-1) # get tide height
	space = gregexpr(' ',TideHt) # get location of whitespace
	TideHt = as.numeric(substr(TideHt,1,space[[1]][1]-1)) # convert to numeric
	Event = substr(ss[i],commas[[1]][4]+1,nchar(ss[i])) # get event
	hilo = rbind(hilo,data.frame(Site=Site,Time=Time,TideHt=TideHt,Event=Event)) 
}
rm(TideHt,commas,Site,yr,hr,Time,Event) # cleanup
head(hilo)
################################################################################
# Create query to return high and low tide times, along with any time when 
# the tide crosses the +2.0ft level. (High/low tides are always included).
# Time values will be in the system time zone. Be careful of daylight savings
# transitions.
tidecommand = paste('tide.exe -l "',sitename,'" -b "',
		startchar, '" -e "', endchar,
		'" -f c -em pSsMm -m p -ml 2.0ft -u ft ', sep = '')

ss = system(tidecommand, intern = TRUE) #invoke tide.exe and return results
ss2 = grep('Mark Falling', ss) #find lines with 'Mark Falling' 
# Convert the character strings in 'ss' into a data frame
events = read.table(textConnection(ss[ss2]), sep = ',', 
		colClasses = 'character')
# Add column names to the data frame
names(events) = c('Site','Date','Hour','TideHt','Event')
# Combine the Date & Hour columns into a POSIX time stamp
events$Time = as.POSIXlt(paste(events$Date,events$Hour), 
		format = "%Y-%m-%d %I:%M %p")
# Strip off the height units and convert tide height to numeric values
events$TideHt = as.numeric(gsub('[ [:alpha:]]',' ',events$TideHt))

head(events)
# Calculate time between each event.
diff(events$Time)


################################################################################

# Create query for returning times of high, low tide, plus Sunrise and Sunset,
# all in UTC timezone, for your site. Height units are meters.
tidecommand = paste('tide.exe -l "',sitename,'" -b "',
		startchar, '" -e "', endchar, 
		'" -f c -em pMm -m p -u m -z', sep = '')

ss = system(tidecommand, intern = TRUE) #invoke Xtide and return results

# Convert the character strings in 'ss' into a data frame
hiloSun = read.table(textConnection(ss), sep = ',', colClasses = 'character')
# Add column names to the data frame
names(hiloSun) = c('Site','Date','Hour','TideHt','Event')
# Combine the Date & Hour columns into a POSIX time stamp
hiloSun$Time = as.POSIXlt(paste(hiloSun$Date,hiloSun$Hour), 
		format = "%Y-%m-%d %I:%M %p", tz = "UTC")
# Strip off the height units and convert tide height to numeric values
hiloSun$TideHt = as.numeric(gsub('[ [:alpha:]]',' ',hiloSun$TideHt))

head(hiloSun)
# Convert Time values to current R session time zone, overwriting old timestamps
hiloSun$LocalTime = c(hiloSun$Time)
head(hiloSun)
################################################################################
# Create query to return tide height at fixed time intervals (10 minutes here),
# in UTC time zone, units of meters.
tidecommand = paste('tide.exe -l "',sitename,'" -b "',
		startchar, '" -e "', endchar,
		'" -f c -m m -s 00:10 -u m -z', sep = '')

ss = system(tidecommand, intern = TRUE) #invoke tide.exe and return results
# Convert the character strings in 'ss' into a data frame
tides = read.table(textConnection(ss), sep = ',', colClasses = 'character')
# Add column names to the data frame
names(tides) = c('Site','Date','Hour','TideHt')
# Combine the Date & Hour columns into a POSIX time stamp
tides$Time = as.POSIXlt(paste(tides$Date,tides$Hour), 
		format = "%Y-%m-%d %I:%M %p", tz = "UTC")
# Strip off the height units and convert tide height to numeric values
tides$TideHt = as.numeric(gsub('[ [:alpha:]]',' ',tides$TideHt))
tides$LocalTime = c(tides$Time)
head(tides)

##################################
# Create query to return times of high/low tide, sunrise and sunset, in the 
# local time zone. 
tidecommand = paste('tide.exe -l "',sitename,'" -b "',
		startchar, '" -e "', endchar,
		'" -f c -em pMm -m p -u ft ', sep = '')

ss = system(tidecommand, intern = TRUE) #invoke tide.exe and return results

sunsets = character(0)
for (i in 1:length(ss2)) {
	commas = gregexpr(',',ss[ss2[i]]) #get locations of commas in line
	yr = substr(ss[ss2[i]], commas[[1]][1]+1,commas[[1]][2]-1) #get year
	hr = substr(ss[ss2[i]], commas[[1]][2]+1,commas[[1]][3]-1) #get date
	sunsets= c(sunsets,paste(yr,hr)) #append new time stamp 
}
ss2 = grep('Sunset',ss) #find lines with Sunset
# Convert the character strings in 'ss' into a data frame
sunsets = read.table(textConnection(ss[ss2]), sep = ',', 
		colClasses = 'character')
# Add column names to the data frame
names(sunsets) = c('Site','Date','Hour','TideHt','Event')
# Combine the Date & Hour columns into a POSIX time stamp
sunsets$Time = as.POSIXlt(paste(sunsets$Date,sunsets$Hour), 
		format = "%Y-%m-%d %I:%M %p")
head(sunsets)

########
# Return to original working directory
setwd(old.dir) 
