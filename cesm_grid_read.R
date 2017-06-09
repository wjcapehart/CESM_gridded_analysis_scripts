# load libraries

library(ncdf4)
library(maps)
library(mapdata)


# Let's start by identifying specific points, days and ensembles from which to pull data.
#   it's nice to put this data up top to make changing locatons easier.

target_lon  <- -104.
target_lat  <-   44.
target_ens  <-    1
target_date <- as.Date("2050-07-01") # you need to wrap the date in the 
                                     # as.Date() function




# get URL root location (comment out if in windows)
URL_Root_Directory <- "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/testAll/"

# get URL root location (comment out if in unix)
# URL_Root_Directory <- "C:/Users/7446253/Documents/"


# files can be found at :"http://kyrill.ias.sdsmt.edu:8080/thredds/catalog.html"


# Here is the variable we want to extract
#  these should match the files names since the variable in the file and file name match
target_variable = "TSA"
variable_name   = "Monthly Mean 2-m Air Temperature"
variable_units  = "deg F"


# get URL files
RCP_85_File <- paste(target_variable, 
                     "_RCP85_CONUS_1920-2100.nc4", 
                     sep="")

RCP_45_File <- paste(target_variable, 
                     "_RCP45_CONUS_1920-2080.nc4", 
                     sep="")

# make the final URLs for extracting with the "paste" function

URL_85 <- paste(URL_Root_Directory,  # string 1 to concatenate
                RCP_85_File,         # string 2 to concatenate
                sep=""               # separation character ("" = none)
               )

URL_45 <- paste(URL_Root_Directory,  # string 1 to concatenate
                RCP_45_File,         # string 2 to concatenate
                sep=""               # separation character ("" = none)
               )

# we now open the files...
#   the output of these functions are "handles" by which we can reference the file

nc.85 <- nc_open(filename = URL_85)
nc.45 <- nc_open(filename = URL_45)

# to view the inventory (or "metadata") of a netCDF file you can "print()" the metadata
#   let's do this with the RCP 8.5 file

print(nc.85)


# Time Control
#    the RCP 8.5 runs go from 1920-2100 and have 2172 monthly time steps
#    the RCP 4.5 runs go from 1920-2080 and have 1932 monthly time steps

time_months.85 <- seq(as.Date("1920-01-01"),  # start value
                      as.Date("2080-12-01"),  # end value
                      "months"                # increment 
                     )



time_months.45 <- seq(as.Date("1920-01-01"),  # start value
                      as.Date("2080-12-01"),  # end value
                      "months"                # increment
                     )


# Days per Working Month
#   Working have different numbers of days.  
#   So here we record the total number of days per month with the variable "time_bounds"
#   this is a 2 x NT array

time_bounds.85 <- ncvar_get(nc    = nc.85,          # netcdf file ID
                            varid = "time_bounds",  # variable name from file
                            start = c(1,   1),      # starting coordinates
                            count = c(2,1932)       # end coordinates (pulling data only to 2080)
                           )

time_bounds.45 <- ncvar_get(nc    = nc.45,         # netcdf file ID
                            varid = "time_bounds"  # variable name from file
                           )

# we can quickly get the number of days per month

days_per_month.85 <- time_bounds.85[2, ] - time_bounds.85[1, ]
days_per_month.45 <- time_bounds.45[2, ] - time_bounds.45[1, ]

# import spatial coordinates

lon <- ncvar_get(nc    = nc.85,         # netcdf file ID
                 varid = "lon"  # variable name from file
                )

lat <- ncvar_get(nc    = nc.85,         # netcdf file ID
                 varid = "lat"  # variable name from file
                )

# location information

lon <- lon - 360.0  # converting to +/- deg east for easier plotting

# Getting Map extents

minlon <- min(lon) 
maxlon <- max(lon) 
minlat <- min(lat)
maxlat <- max(lat)

#  with time, latitude and longitude we can now extract the specific points
#   in our files to extract for x, y and t
 
target_x <- which.min( abs(            lon -  target_lon) ) 
target_y <- which.min( abs(            lat -  target_lat) ) 
target_t <- which.min( abs( time_months.85 - target_date) ) 
 
# import data fields (first RCP 4.5)

var.45 <- ncvar_get(nc      = nc.45,            # netcdf file ID
                    varid   = target_variable,  # variable name from file
                    verbose =  FALSE            # print diagnostic data
                    )

# import data fields (secondly RCP 4.5)


var.85 <- ncvar_get(nc      = nc.85,              # netcdf file ID
                    varid   = target_variable,    # variable name from file
                    verbose = TRUE,               # print diagnostic data
                    start   = c( 1, 1, 1,   1),   # starting coordinates
                    count   = c(62,34,30,1932)    # end coordinates (we're only going to 2080)
                    )

# converting units for civilian friendly values (e.g., for temperature)

if ( (target_variable == "TSA") && (variable_units == "deg C") ) 
  {
     var.45 = var.45 - 273.17     # K -> deg C
     var.85 = var.85 - 273.17     # K -> deg C
  }

if ( (target_variable == "TSA") && (variable_units == "deg F") ) 
  {
    var.45 = 1.8 * var.45 - 459.67  # K -> deg F
    var.85 = 1.8 * var.85 - 459.67  # K -> deg F
  }

# making a map  There are a lot of ways to do this.

# this method uses a set of axes with mapping data attachd

# extract specific ensemble and time
plot_var <- var.45[,, target_ens, target_t]

max_plotting_range = max(c(var.45[,, target_ens, target_t],
                           var.85[,, target_ens, target_t]),
                         na.rm=TRUE)  # na.rm=True : don't count missing values

min_plotting_range = min(c(var.45[,, target_ens, target_t],
                           var.85[,, target_ens, target_t]),
                         na.rm=TRUE)  # na.rm=True : don't count missing values


# create contour map
filled.contour(x = lon,                                         # x coordinate
               y = lat,                                         # y coordinate
               z = plot_var,                                    # z value to contour
               zlim = c(min_plotting_range,                     # contour value range
                        max_plotting_range),
               color.palette = colorRampPalette(c("violet",     # color table
                                                  "blue",
                                                  "lightblue",
                                                  "yellow",
                                                  "orange",
                                                  "red")
                                               ),
               xlab = "Longitude",                              # x axis label
               ylab = "Latitude",                               # y axis label
               main = paste("RCP 4.5 ",                         # title string
                            variable_name,
                            " for ",
                            time_months.45[target_t], 
                            " in ",
                            variable_units,
                            sep=""),
               plot.axes = {axis(1); axis(2);                   # mapping info begins
                            map(database = "worldHires",  # map border dataset
                                xlim     = c(minlon, maxlon),   # x axis range
                                ylim     = c(minlat, maxlat),   # y axis range
                                add      = TRUE,                # add to current plot
                                col      = "black")             # map color
                           }
              )

plot_var =  var.85[,,target_ens, target_t]

# create contour map
filled.contour(x = lon,                                         # x coordinate
               y = lat,                                         # y coordinate
               z = plot_var,                                    # z value to contour
               zlim = c(min_plotting_range,                     # contour value range
                        max_plotting_range),
               color.palette = colorRampPalette(c("violet",     # color table
                                                  "blue",
                                                  "lightblue",
                                                  "yellow",
                                                  "orange",
                                                  "red")
                                                ),
               xlab = "Longitude",                              # x axis label
               ylab = "Latitude",                               # y axis label
               main = paste("RCP 8.5 ",                         # title string
                            variable_name,
                            " for ",
                            time_months.45[target_t], 
                            " in ",
                            variable_units,
                            sep=""),
               plot.axes = {axis(1); axis(2);                   # mapping info begins
                 map(database = "worldHires",                   # map border dataset
                     xlim     = c(minlon, maxlon),              # x axis range
                     ylim     = c(minlat, maxlat),              # y axis range
                     add      = TRUE,                           # add to current plot
                     col      = "black") }                      # map color
               )



# line plot


plot(x    = time_months.45,                        # x value
     y    = var.45[target_x,target_y,target_ens,], # y value
     type = "l",                                   # plot type
     col  = "blue",                                # plot color
     main = paste("RCP 4.5 ",                      # main title
                  variable_name,
                  " for Ens ",
                  target_ens,
                  " @ ",
                  target_lon, "E ",
                  target_lat, "N ",
                  " in ",
                  variable_units,
                  sep=""),
     xlab = "Time",                                # x axis
     ylab = variable_name,                         # y axis
     ylim = c(min(c(var.45[target_x, target_y, target_ens, ],   # y axis range
                    var.85[target_x, target_y, target_ens, ]),
                  na.rm=TRUE),
              max(c(var.45[target_x, target_y, target_ens, ],
                    var.85[target_x, target_y, target_ens, ]),
                  na.rm=TRUE)
              )
     )


plot(x    = time_months.85,                        # x value
     y    = var.85[target_x,target_y,target_ens,], # y value
     type = "l",                                   # plot type
     col  = "red",                                 # plot color
     main = paste("RCP 8.5 ",                      # main title
                  variable_name,
                  " for Ens ",
                  target_ens,
                  " @ ",
                  target_lon, "E ",
                  target_lat, "N ",
                  " in ",
                  variable_units,
                  sep=""),
     xlab = "Time",                                # x axis
     ylab = variable_name,                         # y axis
     ylim = c(min(c(var.45[target_x, target_y, target_ens, ],   # y axis range
                    var.85[target_x, target_y, target_ens, ]),
                  na.rm=TRUE),
              max(c(var.45[target_x, target_y, target_ens, ],
                    var.85[target_x, target_y, target_ens, ]),
                  na.rm=TRUE)
              )
     )

