# xtide_interface.R
# An example R script to show how to build XTide queries, submit them to the 
# system command line, and then parse the results into a data frame. There are
# several different examples outlined below. 
# Author: Luke Miller May 17, 2013
###############################################################################
# Site names: http://www.flaterco.com/xtide/locations.html 
# List of command line flags: http://www.flaterco.com/xtide/settings.html

# Obviously XTide, or a port like tide.exe (for Windows) must be downloaded from 
# the XTide site and installed. It is a standalone program, separate from R.

# In Windows, the XTide directory that contains the tide.exe program needs to be
# added to the system search PATH. It's easiest if you also put the 
# harmonics.tcd file in the same directory as tide.exe.
###############################################################################

# Sys.setenv(TZ="UTC") 	# Useful if you're getting data for sites in multiple 
						# time zones and need to standardize on one time zone.

# Start and end times, supplied as character strings
startchar = '2013-05-16 16:00'
endchar = '2013-05-20 16:00'
# Site name, taken from http://www.flaterco.com/xtide/locations.html
sitename = 'Bodega Harbor entrance, California'

# Example switches for Xtide command line program
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
# Example 1
# Create query to return high and low tide times only, with units in feet and
# using the current system time zone. 
tidecommand = paste('tide -l "',sitename,'" -b "',
		startchar, '" -e "', endchar,
		'" -f c -em pSsMm -m p -u ft ', sep = '')

ss = system(tidecommand, intern = TRUE) #invoke tide.exe and return results

# Convert the character strings in 'ss' into a data frame
hilow = read.table(textConnection(ss), sep = ',', colClasses = 'character')
# Add column names to the data frame
names(hilow) = c('Site','Date','Hour','TideHt','Event')
# Combine the Date & Hour columns into a POSIX time stamp
hilow$Time = as.POSIXlt(paste(hilow$Date,hilow$Hour), 
		format = "%Y-%m-%d %I:%M %p")
# Strip off the height units and convert tide height to numeric values
hilow$TideHt = as.numeric(gsub('[ [:alpha:]]',' ',hilow$TideHt))

head(hilow)

################################################################################
# Example 2
# Create query to return high and low tide times, along with any time when 
# the tide crosses the +2.0ft level. (High/low tides are always included).
# Time values will be in the system time zone. Be careful of daylight savings
# transitions.
tidecommand = paste('tide -l "',sitename,'" -b "',
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
# Example 3
# Create query for returning times of high, low tide, plus Sunrise and Sunset,
# all in UTC timezone, for your site. Height units are meters.
tidecommand = paste('tide -l "',sitename,'" -b "',
		startchar, '" -e "', endchar, 
		'" -f c -em pMm -m p -u m -z', sep = '')

ss = system(tidecommand, intern = TRUE) #invoke Xtide and return results

# Convert the character strings in 'ss' into a data frame
hilowSun = read.table(textConnection(ss), sep = ',', colClasses = 'character')
# Add column names to the data frame
names(hilowSun) = c('Site','Date','Hour','TideHt','Event')
# Combine the Date & Hour columns into a POSIX time stamp
hilowSun$Time = as.POSIXlt(paste(hilowSun$Date,hilowSun$Hour), 
		format = "%Y-%m-%d %I:%M %p", tz = "UTC")
# Strip off the height units and convert tide height to numeric values
hilowSun$TideHt = as.numeric(gsub('[ [:alpha:]]',' ',hilowSun$TideHt))

# Convert Time values to current R session time zone, overwriting old timestamps
hilowSun$LocalTime = c(hilowSun$Time)
head(hilowSun)

################################################################################
# Example 4
# Create query to return tide height at fixed time intervals (10 minutes here),
# in UTC time zone, units of meters.
tidecommand = paste('tide -l "',sitename,'" -b "',
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
# Create a column of time stamps in the current R session time zone
tides$LocalTime = c(tides$Time)
head(tides)

################################################################################
# Example 5
# Create query to return times of high/low tide, sunrise and sunset, in the 
# local time zone. Extract the time for the Sunsets and place in a data frame
tidecommand = paste('tide -l "',sitename,'" -b "',
		startchar, '" -e "', endchar,
		'" -f c -em pMm -m p -u ft ', sep = '')

ss = system(tidecommand, intern = TRUE) #invoke tide.exe and return results

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

