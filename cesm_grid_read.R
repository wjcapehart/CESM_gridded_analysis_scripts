# load libraries

library(ncdf4)
library(maps)


# get URL root location (comment out if in windows)
URL_Root_Directory <- "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/testAll/"

# get URL root location (comment out if in unix)
# URL_Root_Directory <- "C:/Users/7446253/Documents/"


# files can be found at :"http://kyrill.ias.sdsmt.edu:8080/thredds/catalog.html"

# get URL files
RCP_85_File <- "TSA_RCP85_CONUS_1920-2100.nc4"
RCP_45_File <- "TSA_RCP45_CONUS_1920-2080.nc4"

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

nc.85 <- nc_open(filename=URL_85)
nc.45 <- nc_open(filename=URL_45)

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
                            count = c(2,1932)       # end coordinates
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

lon <- lon - 360.0

minlon <- min(lon) 
maxlon <- max(lon) 
minlat <- min(lat)
maxlat <- max(lat)

# import data coordinates

t_2m.45 <- ncvar_get(nc      = nc.45,  # netcdf file ID
                     varid   = "TSA",  # variable name from file
                     verbose = TRUE
                    )


t_2m.85 <- ncvar_get(nc      = nc.85,  # netcdf file ID
                     varid   = "TSA",  # variable name from file
                     verbose =  TRUE,
                     start = c( 1, 1, 1,   1), # starting coordinates
                     count = c(62,34,30,1932) # end coordinates
                    )


# map

ens   <-  1
month <- 30

plot_var =  t_2m.45[,,ens, month] - 273.17

filled.contour(x = lon, 
               y = lat,
               z = plot_var, 
               color.palette = colorRampPalette(c("violet",
                                                  "blue",
                                                  "lightblue",
                                                  "yellow",
                                                  "orange",
                                                  "red"
                                                 )
                                               ),
               xlab = "Longitude", 
               ylab = "Latitude",
               plot.axes = {axis(1); axis(2);
                            map('state',
                            xlim= c(minlon, maxlon),
                            ylim= c(minlat, maxlat),
                            add = TRUE,
                            col = "black")
                           },
               main = paste("RCP 4.5 Air temp for ",
                            time_months.45[month], "in deg C",
                            sep="")
              )



# line plot

target_lon <- -104.
target_lat <-   44.

ens        <-    1

target_x = which.min(abs(lon - target_lon)) 
target_y = which.min(abs(lat - target_lat)) 

plot(x = time_months.45,
     y = t_2m.45[target_x,target_y,ens,],
     main = paste("RCP 4.5 Air temp for Ens ",
                  ens,
                  " @ ",
                  target_lon, "E ",
                  target_lat, "N ",
                  "in deg C"),
     type = "l",
     xlab = "Time",
     ylab = "Temperature"
    )
