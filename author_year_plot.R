# author_year_plot.R
# 
# Author: Luke Miller 2015-04-22
###############################################################################


###############################################################################
# Export a text file from Endnote that only lists Year and Authors, all 
# separated by commas. To do this, create an Output Style
# that lists the year followed by a comma and then each author separated by
# a comma. Select all references, then go to File>Export. In the window that
# opens, you'll see a menu for output style, choose your author-only version 
# there and save the output file as text file. 
f1 = 'authors_list_20150422.txt'
#
## Scan input file, divide each line into a separate entry in a character vector
authors = scan(file = f1, what = character(), sep = '\n') 
#
yr = character()
# Extract year from each record. 
for (i in 1:length(authors)){
	yr[i] = substr(authors[i],regexpr('[1-2]',authors[i])[[1]],
			regexpr(',',authors[i])[[1]] - 1)
}
yr = as.numeric(yr) # Convert to numbers
# Entries with missing or ambiguous years (anything with multiple years listed
# like 1997-2013) will end up as NA's in the yr vector, and will generate a 
# warning.

cnt = numeric(length(yr)) # Create empty vector
# To count the number of authors on a paper, simply count the number of 
# commas in each line of the authors vector. There is always one comma after 
# the year, denoting at least one author, and every additional comma means there
# is another author. 
for (i in 1:length(authors)){
	cnt[i] = length(gregexpr(',',authors[i])[[1]])
}
# Pick out rows that don't have a useful year value
bad.entries = which(is.na(yr))
# Remove the offending rows from the yr and cnt vectors
yr = yr[-(bad.entries)]
cnt = cnt[-(bad.entries)]
# Make a data frame out of the yr and cnt vectors
df = data.frame(Year = yr, Count = cnt)

# Make a new dataframe that holds each combination of Year and Count
newdf = expand.grid(Years = unique(yr), Count = unique(cnt))
# Make a new column to hold a tally of the number of papers for each Year and
# author Count combination. 
newdf$TotalPapers = NA

# Go through the combinations of years and counts to tally the number of papers
# that match that combo in the 'df' dataframe
for (i in 1:nrow(newdf)){
	# Put the tally of number of papers matching each Year & Count combo in the
	# TotalPapers column
	newdf$TotalPapers[i] = nrow(df[df$Year == newdf$Year[i] & 
							df$Count == newdf$Count[i],])
}

# Drop any combinations where the TotalPapers was 0
newdf = newdf[-(which(newdf$TotalPapers == 0)),]

#########################################################
#########################################################
# Create a function to plot a color scale bar on the existing plot using the
# vector of colors that will be generated later by the colorRampPalette function
color.bar <- function(lut, min, max=-min, nticks=11, 
		x1 = 1, x2 = 2, y1 = 1, y2 = 2, 
		ticks=seq(min,max, length=nticks), round = TRUE, title = '',
		cex.title = 1, text.col = 'black', horiz = FALSE){
	# lut = a vector of color values, in hex format
	# min = minimum value represented by the first color
	# max = maximum value represented by the last color
	# nticks = number of tick marks on the colorbar
	# x1 = location of left edge of colorbar, in plot's x-units
	# x2 = location of right edge of colorbar, in plot's x-units
	# y1 = location of bottom edge of color bar, in plot's y-units
	# y2 = location of top edge of color bar, in plot's y-units
	# ticks = a sequence of tick mark value to be added to colorbar
	# round = TRUE or FALSE, round off tick values to 0 decimal place.
	# title = Title for colorbar
	# cex.title = size for title
	# text.col = color of tick marks, title, and border of colorbar
	# horiz = TRUE or FALSE, lay out color bar vertically or horizontally
	
	# Calculate a scaling factor based on the number of entries in the 
	# look-up-table and the absolute distance between y2 and y1 on the plot
	if (horiz == FALSE){
		scale = (length(lut)-1)/(y2-y1)	
	} else if (horiz == TRUE){
		# For horizontal bars, use the distance between x2 and x1 instead
		scale = (length(lut)-1)/(x2-x1)
	}
	# Round off the tick marks if desired
	if (round) { ticks = round(ticks,0) }
	# Draw little thin rectangles for each color in the look up table. The
	# rectangles will span the distance between x1 and x2 on the plot's 
	# coordinates, and have a y-axis height scaled to fit all of the colors
	# between y1 and y2 on the plot's coordinates. Each color will only be a
	# small fraction of that overall height, using the scale factor. For a 
	# horizontal-oriented bar the thin rectangles will run between y1 and y2,
	# scaled to fit all of the colors between x1 and x2. 
	for (i in 1:(length(lut)-1)) {
		if (horiz == FALSE) {
			# Calculate myy, the lower y-location of a rectangle
			myy = (i-1)/scale + y1
			# Calculate the upper y value as y+(1/scale), and draw the rectangle
			rect(x1,myy,x2,myy+(1/scale), col=lut[i], border=NA)
		} else if (horiz == TRUE) {
			# Calculate x, the left x-location of a rectangle
			myx = (i-1)/scale + x1
			# Calculate the right x value as x+(1/scale), and draw the rectangle
			rect(myx,y1,myx+(1/scale),y2, col=lut[i], border=NA)
		}
	}
	# Draw a border around the color bar
	rect(x1,y1,x2,y2, col = NULL, border = text.col)
	# Draw tick marks and tick labels
	for (i in 1:length(ticks)){
		if (horiz == FALSE) {
			myy = (ticks[i]-1)/scale + y1
			# This is an attempt to set the tick mark and labels just off to the 
			# right side of the color bar without having them take up too much 
			# of the plot area. The x locations are calculated as x2 plus a 
			# fraction of the width of the rectangle.
			myx2 = x2 + ((x2-x1)*0.1)
			myx3 = x2 + ((x2-x1)*0.13)
			# Draw little tick marks
			lines(x = c(x2,myx2), y = c(myy,myy), col = text.col)
			# Draw tick labels
			text(x = myx3, y = myy, labels = ticks[i], adj = c(0,0.3), 
					col = text.col)
		} else if (horiz == TRUE) {
			# For a horizontal scale bar
			myx = (ticks[i]-1)/scale + x1
			
			# This is an attempt to set the tick mark and labels just below the 
			# bottom of the color bar without having them take up too much of
			# the plot area. The y locations are calculated as y1 minus a 
			# fraction of the height of the rectangle
			myy2 = y1 - ((y2-y1)*0.1)
			myy3 = y1 - ((y2-y1)*0.13)
			# Draw little tick marks
			lines(x = c(myx,myx), y = c(y1,myy2), col = text.col)
			# Draw tick labels
			text(x = myx, y = myy3, labels = ticks[i], adj = c(0.5,1), 
					col = text.col)
		}
	}
	# Draw a title for the color bar
	text(x = ((x1+x2)/2), y = y2, labels = title, adj = c(0.5,-0.35),
			cex = cex.title, col = text.col)
}
####################################################
####################################################
# Define a color ramp function from white to blue

# From ColorBrewer 9-class Blues (single-hue). ColorBrewer recommends the 
# following set of 9 color values, expressed in hex format. I reverse them so
# that the highest value will be the lightest color. 
colfun = colorRampPalette(rev(c("#f7fbff","#deebf7","#c6dbef","#9ecae1",
						"#6baed6","#4292c6","#2171b5","#08519c","#08306b")),
		space = 'Lab')

# Define a set of colors from blue to white using that function, covering the
# entire range of possible values for newdf$TotalPapers
cols = colfun(max(newdf$TotalPapers))
# Assign a color to each entry in the newdf data frame based on its TotalPapers
# value. 
newdf$col = ""
for (i in 1:nrow(newdf)){
	newdf$col[i] = cols[newdf$TotalPapers[i]]
}

##############################
# Create an output file in svg format
svg(filename = "author-year-count.svg", width = 9, height = 4.8)
par(mar =c(5,6,1,2)) # Change the figure margins slightly
plot(Count~Years, data = newdf, type = 'n', 
		ylim = c(0,45), las = 1,
		cex.lab = 1.6,
		cex.axis = 1.3,
		ylab = 'Number of coauthors',
		xlab = 'Publication Year',
		yaxt = 'n')
# Color the background of the plot using a rectangle, and determine its 
# dimensions on the fly by calling the par()$usr function to get the coordinates
# of the plot edges.
rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4], col = "#BBBBBB")
# Draw some grid lines at useful locations
abline(h = c(1,2,3,4,5,10,15,20,25,30,35,40), col = "#CCCCCC")
abline(v = seq(1875,2015, by = 5), col = "#CCCCCC")
# Redraw the plot's bounding box to cover where the horizontal lines overwrite
# it. 
box()
# Redraw the point data over the newly drawn background and horizontal lines
points(Count~Years, data = newdf, col = newdf$col, pch = 20, cex = 0.9)
# Call the color.bar function created earlier to create a color scale.
color.bar(lut = cols, nticks = 8, horiz = TRUE,
		min = 1, max = max(newdf$TotalPapers),
		x1 = 1880, x2 = 1920, y1 = 42, y2 = 44, 
		title = 'Number of papers', cex.title = 1.1, text.col = 'black')
# Draw the y-axis labels at the appropriate spots
axis(2, at = c(1,2,3,4,5,10,15,20,25,30,35,40), 
		labels = c('1','','3','','5','10','15','20','25','30','35','40'), 
		las = 1, cex.axis = 1.1)
dev.off()




