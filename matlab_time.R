# Filename: matlab_time.R
# Convert between MATLAB datenum values and R POSIXt time values.
# 
# Author: Luke Miller   Feb 20, 2011
###############################################################################

#Convert a numeric  MATLAB datenum (days since 0000-1-1 00:00) to seconds in 
#the Unix epoch (seconds since 1970-1-1 00:00). Specify a time zone if the 
#input datenum is anything other than the GMT/UTC time zone. 
matlab2POS = function(x, timez = "UTC") {
	days = x - 719529 	# 719529 = days from 1-1-0000 to 1-1-1970
	secs = days * 86400 # 86400 seconds in a day
	# This next string of functions is a complete disaster, but it works.
	# It tries to outsmart R by converting the secs value to a POSIXct value
	# in the UTC time zone, then converts that to a time/date string that 
	# should lose the time zone, and then it performs a second as.POSIXct()
	# conversion on the time/date string to get a POSIXct value in the user's 
	# specified timezone. Time zones are a goddamned nightmare.
	return(as.POSIXct(strftime(as.POSIXct(secs, origin = '1970-1-1', 
									tz = 'UTC'), format = '%Y-%m-%d %H:%M', 
							tz = 'UTC', usetz = FALSE), tz = timez))
}


###############################################################################
#Convert POSIXct, POSIXlt or 'seconds since 1970-1-1' to MATLAB datenum value.
#The conversion drops any time zone associated with the POSIXt value. It is the
#user's responsibility to keep track of time zones in MATLAB datenums.
#The constant 719529 in the function is the days from 0000-1-1 to 1970-1-1.
POSIXt2matlab = function(x) {
	if (class(x)[1] == "POSIXlt"){
		days = as.numeric(as.Date(x)) #extract days since 1970-1-1
		frac.day = (((x$hour)*3600) + ((x$min)*60) + x$sec)/86400
		datenum = 719529 + days + frac.day 
		datenum = 719529 + days + frac.day		
	} else if (class(x)[1] == "POSIXct"){
		x = as.POSIXlt(x) #convert to POSIXlt class
		days = as.numeric(as.Date(x)) #extract days since 1970-1-1
		frac.day = (((x$hour)*3600) + ((x$min)*60) + x$sec)/86400
		datenum = 719529 + days + frac.day
	} else if (class(x)[1] == "numeric"){
		days = x / 86400 #convert seconds to days
		datenum = days + 719529 
	} else {
		stop("Input cannot be coerced to POSIXlt or numeric value")
	}
	return(datenum)
}
#The output is a numeric vector of 'days since 0000-1-1 00:00'. 


###############################################################################
#Convert POSIXct or POSIXlt objects to MATLAB datenum, in UTC time zone. 
#All time stamps with non-GMT/UTC time zones will be first converted to the 
#GMT/UTC time zone, then converted to MATLAB datenum value. 
POSIXt2matlabUTC = function(x) {
	if (class(x)[1] == "POSIXct") {
		x = as.POSIXlt(x, tz = "UTC") #convert to UTC time zone
		days = as.numeric(x) / 86400 #convert to days
		datenum = days + 719529 #convert to MATLAB datenum
	} else if (class(x)[1] == "POSIXlt") {
		x = as.POSIXlt(x, tz = "UTC") #convert to UTC time zone
		days = as.numeric(x) / 86400  #convert to days
		datenum = days + 719529 #convert to MATLAB datenum
	} else {stop("POSIXct or POSIXlt object required for input")}
	return(datenum)
}
#The output is a numeric vector of 'days since 0000-1-1 00:00', adjusted to UTC
