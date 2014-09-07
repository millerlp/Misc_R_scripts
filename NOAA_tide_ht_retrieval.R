# coops_tide_ht_retrieval.R
# Author: Luke Miller Feb 2011
################################################################################
# Run this script as-is. It will prompt you to enter the appropriate
# station and date values when needed. You can only request less than 1 month of 
# data at a time, so if you get an error returned, try asking for slightly less
# data.

# This script will download a set of verified tide height data from a NOAA 
# CO-OPS DODS/OPeNDAP server and parse it into a data frame to be saved to disk 
#
# Station list: http://opendap.co-ops.nos.noaa.gov/stations/index.jsp
# Scroll through that list to find your station ID. 

# OPeNDAP server gateway: http://opendap.co-ops.nos.noaa.gov/dods/
# The gateway has links to several other data types available including 
# water temperature, air temperature, wind etc. 
# See http://docs.opendap.org/index.php/UserGuideOPeNDAPMessages for info on
# structuring OPeNDAP queries.
#
# Six-minute water level data can only be retrieved 1 month at a time. Other
# fields such as water temperature could return up to 1 year at a time. This 
# script only deals with 6-minute tide height data, but could serve as a guide 
# for accessing other data types from NOAA CO-OPS. 
# The tide height is reported in meters and time in UTC (Greenwich).

###############################################################################
require(RCurl)

noquote(print("Enter NOAA tide station number (i.e. Monterey = 9413450): "))
station = scan("",what = character(),nlines = 1)
noquote(print("Enter start date (format: 20080123 = Jan 23, 2008): "))
startdate = scan("",what = character(),nlines = 1) #get one line of values
noquote(print("Enter end date (format: 20081231 = Dec 31, 2008): "))
enddate = scan("",what = character(),nlines = 1)

#OPeNDAP query for 6-minute verified water level looks like this (on 1 line):
#http://opendap.co-ops.nos.noaa.gov/dods/IOOS/
#SixMin_Verified_Water_Level.ascii?
#WATERLEVEL_6MIN_VFD_PX._STATION_ID,
#WATERLEVEL_6MIN_VFD_PX._DATUM,
#WATERLEVEL_6MIN_VFD_PX.DATE_TIME,
#WATERLEVEL_6MIN_VFD_PX.WL_VALUE,
#WATERLEVEL_6MIN_VFD_PX.I,
#WATERLEVEL_6MIN_VFD_PX.F,
#WATERLEVEL_6MIN_VFD_PX.R,
#WATERLEVEL_6MIN_VFD_PX.T
#&WATERLEVEL_6MIN_VFD_PX._STATION_ID=%229449880%22
#&WATERLEVEL_6MIN_VFD_PX._DATUM=%22MLLW%22
#&WATERLEVEL_6MIN_VFD_PX._BEGIN_DATE=%2220080801%22
#&WATERLEVEL_6MIN_VFD_PX._END_DATE=%2220080808%22

#####################################################
######## DON'T CHANGE ANYTHING BELOW THIS LINE ####################
#The parts of the url to be assembled:
url1 = "http://opendap.co-ops.nos.noaa.gov/dods/IOOS/"
url2 = "SixMin_Verified_Water_Level.ascii?"
url3 = "WATERLEVEL_6MIN_VFD_PX._STATION_ID," #return stationId
url4 = "WATERLEVEL_6MIN_VFD_PX._DATUM," #return datum
url5 = "WATERLEVEL_6MIN_VFD_PX.DATE_TIME," #return record date-time
url6 = "WATERLEVEL_6MIN_VFD_PX.WL_VALUE," #return water level value
url7 = "WATERLEVEL_6MIN_VFD_PX.I," #return quality flag
url8 = "WATERLEVEL_6MIN_VFD_PX.F," #return quality flag
url9 = "WATERLEVEL_6MIN_VFD_PX.R," #return quality flag
url10 = "WATERLEVEL_6MIN_VFD_PX.T" #return quality flag
#The remaining parts of the url specify how to filter the data on the server 
#to only retrieve the desired station and date range. Values must be enclosed
#in ascii double-quotes, which are represented by the code %22
# Do not change any values here, do not insert your own values here.
url11 = "&WATERLEVEL_6MIN_VFD_PX._STATION_ID=%22" # station ID goes here
url12 = "%22"
url13 = "&WATERLEVEL_6MIN_VFD_PX._DATUM=%22MLLW%22"#we want MLLW as the datum
url14 = "&WATERLEVEL_6MIN_VFD_PX._BEGIN_DATE=%22" # start date gets put in here
url15 = "%22"
url16 = "&WATERLEVEL_6MIN_VFD_PX._END_DATE=%22" # end date gets put in here
url17 = "%22"
####### DON'T CHANGE ANYTHING ABOVE THIS LINE ############
##############################################

#Assemble the URL
urltotal = paste(url1,url2,url3,url4,url5,url6,url7,url8,url9,url10,url11,
		station,url12,url13,url14,startdate,url15,url16,enddate,url17,sep ="")

#Download the data
cat("Contacting server...\n"); flush.console()
dat = getURL(urltotal) #use RCurl to retrieve text into a vector 'dat'
cat("Data returned...\n"); flush.console()
Sys.sleep(2) #If you access data in a loop, be courteous and give the server
#a short break between requests
#cleanup
rm(url1,url2,url3,url4,url5,url6,url7,url8,url9,url10,url11,url12,url13,url14)
rm(url15,url16,url17)

con = textConnection(dat) #create text Connection to dat vector
all.lines = readLines(con) #read lines of text into separate slots in a vector
close(con) #close connection to dat vector

if (length(grep('^Error',all.lines))>0) { #check for error in retrieval
	cat("There was an error...\n")
	cat(dat,"\n") #print contents of dat to show error
	flush.console()
} else { #retrieval was successful, parse the text
	
	#The column headers are typically preceded by a line of dashes
	headerlines = grep("^--------",all.lines) #find index of headers (-1)
	
	#read column header names into a vector
	con = textConnection(dat)
	headers = scan(con, skip = headerlines, nlines = 1, sep = ",",
			what = "character", strip.white = TRUE)
	close(con)
	
	#read rest of the data into a data frame 'df'
	con = textConnection(dat)
	df = read.table(con, skip = headerlines+1, sep = ",", header = FALSE,
			quote = "\"", col.names = headers, strip.white = TRUE,
			stringsAsFactors = FALSE)
	close(con)
	
	###########################################################################
	#The following operations will need to be altered if you change the 
	#fields or data type being returned by the OPeNDAP server
	
	#Convert the time column to POSIX time (seconds since 1970-01-01 00:00:00)
	df[,3] = as.POSIXct(strptime(df[,3],format = "%b %d %Y %I:%M%p",
					tz = "GMT"))
	
	#Give the columns shorter names
	names(df) = c("stationId","datum","TimeUTC","TideHT","Flag.Inferred",
			"Flag.Flat.Tol","Flag.Rate.Tol","Flag.Temp.Tol")
	
	#Uncomment this if you want to plot the data
#	plot(df$TimeUTC, df$TideHT, type = "l",
#			xlab = "Date",ylab = "Tide Height, meters")
	
	#Save data automatically to a .csv file. 
	filename = paste("Station_",station,"_",startdate,"-",enddate,
			".csv",sep = "") #make file name
	write.csv(df,filename,row.names = FALSE, quote = FALSE) #write file to disk
	cat("Saved to ",getwd(),"/",filename,"\n",sep = "")
	flush.console()
	
	#Alternate file save method lets user specify file name at run time
	#Uncomment this if you wish to use it instead of the automated file 
	#output above
#	write.csv(df,file.choose(),row.names = FALSE, quote = FALSE)
	
	#cleanup
	rm(dat,con,all.lines,startdate,enddate,filename,headerlines, headers)
	
} #end of if-else statement
