# coops_water_temp_retrieval_monthly_v3.R
# This script will download a set of water temperature data from a NOAA 
# CO-OPS DODS/OPeNDAP server and parse it into a data frame to be saved to disk 
# By default this will retrieve 6-minute interval data
#
# Station list: http://opendap.co-ops.nos.noaa.gov/stations/index.jsp
# OPeNDAP server gateway: http://opendap.co-ops.nos.noaa.gov/dods/
# The gateway has links to several other data types available including 
# water temperature, air temperature, wind etc. 
#
# I wrote this to retrieve water temp data in 1-month chunks, and save each 
# month's data to a separate output file in the current working directory.
# Author: Luke Miller Feb 2020
###############################################################################
require(RCurl)
require(chron) #for leap year function
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
#Friday Harbor, WA 			9449880
#Boston, MA 				8443970
#Sitka, AK					9451600

year1 = 2018
year2 = 2019  # Can be the same as year1
station = 9451600  # NOAA station number
leap = leap.year(year1) #test if desired year is a leap year


for (yr in year1:year2){
	leap = leap.year(yr) # test if desired year is a leap year
	
	for (mo in 1:12) { # start of mo for-loop
		
		# Create text string for month value
		if (mo < 10) {month = paste("0",as.character(mo),sep="")} else {
			month = as.character(mo)
		}
		# Figure out number of days in month
		if ((mo == 4) | (mo == 6) | (mo == 9) | (mo == 11)) {nday = 30} else {
			if (mo == 2 & leap == TRUE) {nday = 29} else {
				if (mo == 2 & leap == FALSE) {nday = 28} else nday = 31 }
		} 
		
		startdate = paste(yr,month,"01",sep = "")
		enddate = paste(yr,month,nday,sep = "")
		### Assemble url parts. All times will be requested in GMT time zone
		### and all units will be metric 
		url1 = "https://tidesandcurrents.noaa.gov/api/datagetter?"
		url2 = "begin_date=" # insert startdate after this
		url3 = "&end_date="  # insert enddate after this
		url4 = "&station="  # insert station number after this
		url5 = "&product=water_temperature&units=metric&time_zone=gmt"
		url6 = "&format=csv"  # returns a csv-formatted file

		urltotal = paste(url1,url2,startdate,url3,enddate,url4,station,url5,url6,
				sep ="")
		cat("Contacting server...\n"); flush.console()
		dat = getURL(urltotal) #use RCurl to retrieve text into a vector 'dat'
		cat("Data returned...\n"); flush.console()
		Sys.sleep(2) #pause for a few seconds to avoid overloading server
		
		#cleanup
		rm(url1,url2,url3,url4,url5,url6)
		
		con = textConnection(dat) #create text Connection to dat vector
		all.lines = readLines(con) #read lines of text into separate slots in a vector
		close(con) #close connection to dat vector
		
		while (length(grep('504 Gateway Time-out',all.lines))>0) {
			cat("Timeout, retrying...\n"); flush.console()
			dat = getURL(urltotal) #use RCurl to retrieve text into a vector 'dat'
			cat("Data returned...\n"); flush.console()
			con = textConnection(dat) #create text Connection to dat vector
			all.lines = readLines(con) #read lines of text into separate slots in a vector
			close(con) #close connection to dat vector
		}
		
		
		if (length(grep('^Error',all.lines))>0) { #check for error in retrieval
			cat("There was an error...\n")
			cat(dat,"\n") #print contents of dat to show error
			flush.console()
		} else {
			# The column headers are typically the first line when csv is the 
			# return format
			
			# read column header names into a vector
			con = textConnection(dat)
			headers = scan(con, nlines = 1, sep = ",",
					what = "character", strip.white = TRUE)
			close(con)
			
			# read rest of the data into a data frame 'df'
			con = textConnection(dat)
			df = read.table(con, skip = 1, sep = ",", header = FALSE,
					quote = "\"", col.names = headers, strip.white = TRUE,
					stringsAsFactors = FALSE)
			close(con)
			
			###########################################################################
			#The following operations will need to be altered if you change the 
			#fields or data type being returned by the OPeNDAP server
			
			# Convert the time column to POSIX time (seconds since 1970-01-01 00:00:00)
			df[,1] = as.POSIXct(df[,1],format = "%Y-%m-%d %H:%M", tz = "UTC")
			
			#Give the columns shorter names
			names(df) = c("DateTimeUTC","WaterTempC","Flag.MaxTemp",
					"Flag.MinTemp","Flag.Rate.Tol")
			
			#Uncomment this if you want to plot the data
			#plot(df$TimeUTC, df$TideHT, type = "l",
			#		xlab = "Date",ylab = "Tide Height, meters")
			
			#Save data automatically to a .csv file. 
			filename = paste("Station_",station,"_water_temp_",startdate,"-",enddate,
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
	
}

cat("Finished\n\a")