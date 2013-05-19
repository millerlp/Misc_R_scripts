# Filename: tide_prediction.R

# Generate tide height predictions for NOAA *reference* tide stations. This 
# requires a previously-produced file of harmonics for the sites, stored as a 
# Rdata file.
# The available reference tide stations are listed here:
# http://www.flaterco.com/xtide/locations.html
# Note that only stations listed as type 'Ref' on that page can be used 
# with this script.
# Currently the script assumes that all input times (and corresponding tide 
# time outputs) are in UTC/GMT time zone, not your local time zone. Check the 
# output against NOAA's tide predictions at 
# http://tidesandcurrents.noaa.gov/tide_predictions.shtml

# Author: Luke Miller  May 19, 2013
###############################################################################
# Sys.setenv(TZ='UTC')	# Change R session time zone to UTC (optional)

# User-friendly version for loading harmonics Rdata file:
cat('Please load the harmonics Rdata file.\n')
load(file.choose())

# Hard-coded version, if your harmonics file is always in the same place:
#load('D:/R/Weather_data/Harmonics_20120302.Rdata') 

# Specify tide station, Site names: http://www.flaterco.com/xtide/locations.html
# Only the stations labeled 'Ref' will work with this script.
cat('Please enter the station name: \n')
stationID = scan(file = '', character(), quiet = TRUE, n = 1)
# Hard-coded version:
#stationID = 'Monterey Harbor'
#stationID = 'Boston, Boston Harbor'
#stationID = 'San Diego, San Diego Bay'
#stationID = 'Boston'


# Find row index of the desired station in the harms list that was loaded from
# the harmonics Rdata file.
stInd = grep(stationID, harms$station)

# If no stations are found, or multiple possible stations are found, notify the
# user.
if (length(stInd) == 0){
	cat('No station found.\n')
} else if (length(stInd) > 1) {
	cat('Multiple stations found, please choose one (enter number): \n')
	for (i in 1:length(stInd)) {
		cat('[', i, ']', harms$station[stInd[i]],'\n')
	}
	choice = scan(file = '', what = numeric(), n = 1, quiet = TRUE)
	stInd = stInd[choice]
}

# Show the name of the station being used
cat('Using station: ', harms$station[stInd],'\n')

# Specify number of minutes between tide predictions
cat('Enter desired frequency of predictions in minutes (1-60): \n')
freq = scan(file = '', numeric(), quiet = TRUE, n = 1)
#freq = 6 # units of minutes, hard coded version

freq = freq * 60 # convert minutes to seconds

# Specify start and end times for the tide predictions
# If you cross a year boundary, the predictions must use the
# correct equilibrium and nodefactors for each year. 
cat('Enter starting time (YYYY-MM-DD HH:MM) in UTC/GMT time zone: \n')
t1 = scan(file = '', character(), quiet = TRUE, n = 1)
cat('Enter ending time (YYYY-MM-DD HH:MM) in UTC/GMT time zone: \n')
t2 = scan(file = '', character(), quiet = TRUE, n = 1)
# Hard coded version:
#t1 = '2012-12-31 23:00'
#t2 = '2013-01-01 09:00'

# Convert to POSIXct time format, with UTC time zone
t1 = as.POSIXct(t1, tz = 'UTC')
t2 = as.POSIXct(t2, tz = 'UTC')

# Create data frame with sequence of times at desired frequency
times = data.frame(POStime = seq(t1,t2,freq))
# Extract the year for each of those time points
times$yr = as.POSIXlt(times$POStime)$year + 1900
# Use the year to make a character value representing the start of the year
times$yrstart = paste(times$yr, '01', '01', sep = '-')
# Calculate hours since the start of the year for each time point
times$hours = as.numeric(difftime(times$POStime, 
				as.POSIXct(times$yrstart, tz = 'UTC'), units = 'hours'))
# Make a year index to pull correct starting values from harms for the year
times$yrindx = times$yr - harms$startyear + 1

########################################
# Tide height calculation
# Input times behave as if they are given in UTC, so output tides correspond to
# the UTC times. 
pred = vector('numeric',nrow(times))
pred[1:nrow(times)] = harms$datum[stInd]
for (j in 1:nrow(times)) {
	for (i in 1:length(harms$name)) {
		pred[j] = pred[j] + ((harms$nodefactor[i,times$yrindx[j]] * 
						t(harms$A[stInd,i]) * 
						(cos((harms$speed[i] * 
									times$hours[j] + 
										harms$equilarg[i, times$yrindx[j]] - 
												t(harms$kappa[stInd,i])) * 
											pi/180))))		
	}
}
# Finished tide height calculations, units of feet (sorry, that's the way it is)
####################################

timesout = as.POSIXct(times$hours * 3600, origin = times$yrstart, tz = 'UTC')

# Make an output data frame
results = data.frame(TimeUTC = timesout, TideHt.ft = pred)

# This will produce a column of times in the current R session's time zone, 
# which isn't necessarily the time zone of your tide station. 
results$TimeLocal = c(results$TimeUTC)

cat('Finished. See "results" data frame for output.\n')
# Optionally print the results
plot(results$TimeUTC,results$TideHt.ft, type = 'l', col = 'blue', las = 1,
		xlab = 'Time, UTC time zone', ylab = 'Tide Height, ft')
