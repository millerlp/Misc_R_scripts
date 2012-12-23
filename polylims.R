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
	
	# Use rle function to find contiguous real numbers
	rl = rle(is.na(ydata))
	starts = vector()
	ends = vector()
	indx = 1
	for (i in 1:length(rl$lengths)){
		if (rl$values[i]){
			# Value was NA, advance index without saving the numeric values
			indx = indx + rl$lengths[i]
		} else {
			# Value was a real number, extract and save those values
			starts = c(starts,indx)
			ends = c(ends, (indx + rl$lengths[i] - 1))
			indx = indx + rl$lengths[i]
		}	
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

