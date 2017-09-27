# coops_tide_ht_retrieval_yearly.R
# Author: Luke Miller July 2011
# Updated 2017-09-26 to handle newer ERDDAP format
################################################################################
# This script will download a set of verified tide height data from a NOAA 
# CO-OPS DODS/OPeNDAP server and parse it into a data frame to be saved to disk 
#
# Station list: http://opendap.co-ops.nos.noaa.gov/stations/index.jsp
# OPeNDAP server gateway: https://opendap.co-ops.nos.noaa.gov/erddap/index.html
# The gateway has links to several other data types available including 
# water temperature, air temperature, wind etc. 
#
# Six-minute water level data can only be retrieved 1 month at a time. Other
# fields such as water temperature can return up to 1 year at a time.
# The tide height is reported in meters.

###############################################################################
require(RCurl) #If you don't have this, type install.packages("RCurl") at the 
			   #R command line before running this script.
require(chron) #for the leap.year function

################################################################################
## ENTER YOUR STATION ID AND YEARS HERE ##
station = 9413450 #Enter your desired station ID here
year = 2012  #Enter the first year of data to get here
year2 = 2012 #Enter the last year of data to get here (can be same as 1st year)
################################################################################

# Example station ID's. 
# See http://opendap.co-ops.nos.noaa.gov/stations/index.jsp for a list of all
# stations
#Monterey, CA 				9413450
#La Jolla, CA				9410230
#Los Angeles				9410660 (water) 9410647(air/wind)
#San Francisco				9414290
#Point Arena, CA			9416841
#Crescent City, CA			9419750
#Port Orford, OR			9431647
#Charleston, OR				9432780
#Yaquina River, Newport OR	9435380
#Toke Point, WA				9440910
#Neah Bay, WA				9443090


for (yr in year:year2) {
	leap = leap.year(yr) #test if desired year is a leap year
	
	for (mo in 1:12) { #start of mo for-loop
		
		#create text string for month value
		if (mo < 10) {month = paste("0",as.character(mo),sep="")} else {
			month = as.character(mo)
		}
		#figure out number of days in month
		if ((mo == 4) | (mo == 6) | (mo == 9) | (mo == 11)) {nday = 30} else {
			if (mo == 2 & leap == TRUE) {nday = 29} else {
				if (mo == 2 & leap == FALSE) {nday = 28} else nday = 31 }
		} 
		
		startdate = paste(yr,month,"01",sep = "")
		enddate = paste(yr,month,nday,sep = "")
		

#OPeNDAP query for 6-minute verified water level looks like this (on 1 line):
# https://opendap.co-ops.nos.noaa.gov/erddap/tabledap/
# IOOS_SixMin_Verified_Water_Level.asc?STATION_ID
# %2CDATUM%2CBEGIN_DATE%2CEND_DATE%2Ctime%2CWL_VALUE%2CSIGMA%2CI%2CF%2CR%2CT&
# STATION_ID=%229413450%22&
# DATUM=%22MLLW%22&
# BEGIN_DATE=%2220080123%22&
# END_DATE=%2220080130%22

########################################################
###### DON'T CHANGE ANY OF THE CODE BELOW THIS LINE ####
#The parts of the url
		url1 = "https://opendap.co-ops.nos.noaa.gov/erddap/tabledap/"
		url2 = "IOOS_SixMin_Verified_Water_Level.asc?"
		url3 = "STATION_ID%2C" #return stationId
		url4 = "DATUM%2C" #return datum
		url5 = "time%2C" #return record date-time
		url6 = "WL_VALUE%2C" #return water level value
		url7 = "I%2C" #return quality flag
		url8 = "F%2C" #return quality flag
		url9 = "R%2C" #return quality flag
		url10 = "T" #return quality flag
#The remaining parts of the url specify how to filter the data on the server 
#to only retrieve the desired station and date range. Values must be enclosed
#in ascii double-quotes, which are represented by the code %22
		url11 = "&STATION_ID=%22" # station ID goes here
		url12 = "%22"
		url13 = "&DATUM=%22MLLW%22" # we want MLLW as the datum
		url14 = "&BEGIN_DATE=%22" # start date gets put in here
		url15 = "%22"
		url16 = "&END_DATE=%22" # end date gets put in here
		url17 = "%22"
		##### DON'T CHANGE ANY CODE ABOVE THIS LINE ###########
		########################################################################
		urltotal = paste(url1,url2,url3,url4,url5,url6,url7,url8,url9,url10,url11,
				station,url12,url13,url14,startdate,url15,url16,enddate,url17,sep ="")
		cat("Contacting server...\n"); flush.console()
		dat = getURL(urltotal) #use RCurl to retrieve text into a vector 'dat'
		cat("Data returned...\n"); flush.console()
		Sys.sleep(2) #pause for a few seconds to avoid overloading server
		
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
		} else {
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
			#plot(df$TimeUTC, df$TideHT, type = "l",
			#		xlab = "Date",ylab = "Tide Height, meters")
			
			#Save data automatically to a .csv file. 
			filename = paste("Station_",station,"_tide_ht_",startdate,"-",enddate,
					".csv",sep = "")
			write.csv(df,filename,row.names = FALSE, quote = FALSE)
			cat("Saved to ",filename,"\n")
			flush.console()
			
			#Alternate file save method lets user specify file name at run time
			#write.csv(df,file.choose(),row.names = FALSE, quote = FALSE)
			
			#cleanup
			rm(dat,con,all.lines,startdate,enddate,filename,headerlines, headers,df,
					urltotal)
			
		} #end of if-else statement
		
	} #end of mo for-loop

} #end of yr for-loop

###############################################################################
###############################################################################

cat("Finished\n\a")