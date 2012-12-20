# polylims.R
# Source file for a function to create x-y data for plotting polygons, handles
# the case where some y-data are missing in an otherwise continuous series of
# data.
# Author: Luke Miller Dec 19, 2012
###############################################################################


polylims = function(xdata, ydata, ydata2) {
	# A function to find the beginings and endings of each run of real numbers
	# in a set of data, in order to create polygons for plotting. The assumption
	# is that ydata, ydata2, and xdata vectors are the same length, and that 
	# the ydata and ydata2 vectors contain NA values for missing data. The 
	# same values in ydata and ydata2 must be missing. The output will be 
	# a list of data frames of x and y values suitable for plotting polygons.
	
	# Find all non-NA indices in ydata.
	yna = which(!is.na(ydata))
	# Calc difference between each consecutive index, but first tack on a zero
	# at the start, which will determine if ydata started off with NA values
	yna2 = diff(c(0,yna))
	# Find index of any difference greater than 1 (indicates a larger step)
	yna3 = which(yna2 > 1)
	# Return the indices in ydata that mark the start of a contiguous
	# set of data
	starts = yna[yna3]
	# If the first value in ydata is a real number, we need to add its index to 
	# the 'starts' vector
	if (!is.na(ydata[1])) starts = c(1,starts)
	# Now work on finding the ending indices for each run of real numbers in
	# the ydata vector. 
	# Calc difference between each index, but first tack on a value equal to the
	# length of ydata at the end, which will determine if ydata ended with NA
	# values
	yna5 = diff(c(yna,length(ydata)))
	# Find the indices of any values in yna5 greater than 1, which indicates a
	# large step in the yna vector
	yna6 = which(yna5 > 1)
	# Extract the indices of the end of each run of real numbers in ydata from
	# the indices stored in yna
	ends = yna[yna6]
	# If the last value in ydata is a real number, add that index onto 'ends'
	if(!is.na(ydata[length(ydata)])){ 
		ends = c(ends,length(ydata))
	} else {
		# Or else if the last value in ydata is NA, add the index of the last 
		# real number on to the end of the 'ends' vector
		ends = c(ends,yna[length(yna)])
	}
	# At this point the lengths of the vectors 'starts' and 'ends' should be
	# equal, and each pair of values represents the starting and ending indices
	# of a continuous set of data in the ydata vector.
	
	# Next separate out each set of continuous ydata, and the associated xdata,
	# and format them for plotting as a polygon.
	polylist = list()
	for (i in 1:length(starts)){
		temp = data.frame(x = c(xdata[starts[i]],xdata[starts[i]:ends[i]],
						rev(xdata[starts[i]:ends[i]])),
				y = c(ydata[starts[i]],ydata2[starts[i]:ends[i]],
						rev(ydata[starts[i]:ends[i]])))
		polylist[[i]] = temp	
	}
	polylist
	# You can iterate through the items in polylist and plot them as 
	# polygons on your plot. Use code similar to the following:
	#	for (i in 1:length(polylist)){
	#			polygon(polylist[[i]]$x, polylist[[i]]$y, 
	#				col = rgb(0.5,0.5,0.5,0.5), border = NA)
	#	}
}
