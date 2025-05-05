# coops_water_level_retrieval_dataseries_v4.R #Could rename
# Author: Luke Miller July 2011 - updated 2021 - Updated by Nick Cook 2025

################################################################################

# This script will download a set of verified water_level (or parameter of your choice) data from a NOAA
# CO-OPS DataRetriever API and parse it into a data frame to be saved to disk
#
# Station list: https://tidesandcurrents.noaa.gov/stations.html?type=Historic+Water+Levels
# Current server link
# https://api.tidesandcurrents.noaa.gov/api/prod/datagetter
# The gateway has links to several other data types available including
# water temperature, air temperature, wind etc.
#
# Six-minute water level data can only be retrieved 1 month at a time. Other
# fields such as water temperature (or hourly water level data) can return up to 1 year at a time.
# The tide height is reported in feet.



###############################################################################
require(curl) # If you don't have this, type install.packages("curl") at the
# R command line before running this script.
require(chron) # for the leap.year function

################################################################################
## ENTER YOUR STATION ID AND OTHER SAMPLE PARAMETERS HERE ##

station = '9413450' #Enter your desired station ID here
year = 2023  # Enter the first year of data to get here
year2 = 2024 # Enter the last year of data to get here (can be same as 1st year)
datum = 'MLLW' # Check if this is available and adjust if required, common options: MLLW or NAVD
timezone =  'gmt' # For data series you don't want daylight savings messing things up
product = 'water_level' # Almost always want this, but can explore other
units = 'metric' # Options are 'english' (results in feet) or 'metric' (results in meters)
application = 'DataAPI_Sample' # Standard do not change
format = 'csv' # must use csv
################################################################################

counter = 0

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
		
# Example station ID's.		
# See https://tidesandcurrents.noaa.gov/stations.html?type=Historic+Water+Levels for a list of all
# stations		
#Monterey, CA                  9413450		
#La Jolla, CA                  9410230		
#Los Angeles                   9410660 (water) 9410647(air/wind)		
#San Francisco                 9414290		
#Point Arena, CA               9416841		
#Crescent City, CA             9419750		
#Port Orford, OR               9431647		
#Charleston, OR                9432780		
#Yaquina River, Newport OR     9435380		
#Toke Point, WA                9440910		
#Neah Bay, WA                  9443090		
#Sitka, AK                     9451600
		
#API query for 6-minute verified water level looks like this (on 1 line):
#https://api.tidesandcurrents.noaa.gov/api/prod/datagetter
#https://api.tidesandcurrents.noaa.gov/api/prod/#station see data fields on this webpage	
#begin_date=		
#end_date=		
#station=		
#product=		
#datum=		
#time_zone=		
#units=		
#application=DataAPI_Sample	
#format=csv #can choose xml as well
		
########################################################		
###### DON'T CHANGE ANY OF THE CODE BELOW THIS LINE ####		
#The parts of the url		
		url1 = "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?"		
		#The remaining parts of the url specify how to filter the data on the server
		url2 = "begin_date=" #begin date of requested data		
		url3 = "&end_date="  #end date of requested data		
		url4 = "&station="#station number		
		url5 = "&product=" #usually want water_level, look up		
		url6 = "&datum="		
		url7 = "&time_zone="		
		url8 = "&units="		
		url9 = "&application=DataAPI_Sample"		
		url10 = "&format="
##### DON'T CHANGE ANY CODE ABOVE THIS LINE ###########
########################################################################
		
		urltotal = paste(url1,url2,startdate,url3,enddate,url4,station,url5,product,url6,datum,url7,timezone,url8,units, url9,url10,format,sep ="")
		cat("Contacting server...\n"); flush.console()		
		con = curl(urltotal) # Open a connection		
		dat = readLines(con) # Read the returned data		
		close(con)
		
		cat("Data returned...\n"); flush.console()		
		Sys.sleep(2) #pause for a few seconds to avoid overloading server
		
		#cleanup		
		rm(url1,url2,url3,url4,url5,url6,url7,url8,url9,url10)
		
		con = textConnection(dat) #create text Connection to dat vector		
		all.lines = readLines(con) #read lines of text into separate slots in a vector		
		close(con) #close connection to dat vector
		
		if (length(grep('^Error',all.lines))>0) { #check for error in retrieval			
			cat("There was an error...\n")
			cat(dat,"\n") #print contents of dat to show error			
			flush.console()			
		} else {			
			#The column headers are typically preceded by a line of dashes # 2025: not anymore, commented out			
			#headerlines = grep("^--------",all.lines) #find index of headers (-1)
			
			#read column header names into a vector	
			con = textConnection(dat)			
			headers = scan(con, skip = 0, nlines = 1, sep = ",",					
					what = "character", strip.white = TRUE)			
			close(con)
			
			#read rest of the data into a data frame 'df'			
			con = textConnection(dat)			
			df = read.table(con, skip = 0, sep = ",", header = TRUE,					
					quote = "\"", col.names = headers, strip.white = TRUE,					
					stringsAsFactors = FALSE)			
			close(con)
			
			#Save first data frame and append all others into the same data frame                                             			
			if (counter == 0) {				
				df_total = df				
			} else {				
				df_total = rbind(df_total, df)				
			}			
			counter = counter + 1
			
		}
	}
}

# When finished, the data frame df_total is the final output frame that should 
# contain the entire data set. 

