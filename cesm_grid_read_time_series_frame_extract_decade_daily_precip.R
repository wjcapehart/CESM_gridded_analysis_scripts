# load libraries

library(ncdf4)
library(reshape2)
library(ggplot2)
library(lubridate)
library(vcd) 

# Let's start by identifying specific points, days and ensembles from which to pull data.
#   it's nice to put this data up top to make changing locatons easier.

target_lon  <- -104.
target_lat  <-   44.


control_period_start = as.Date("2000-01-01")
control_period_end   = as.Date("2009-12-31")

test_period_start =  as.Date("2050-01-01")
test_period_end   =  as.Date("2059-12-31")


# CESM files can be found at :"http://kyrill.ias.sdsmt.edu:8080/thredds/catalog.html"


# use OPeNDAP root location (comment out if in windows)
URL_Root_Directory <- "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/CESM_SODAK/"


# use Gettinger's local location
# URL_Root_Directory <- "C:/Users/7446253/Documents/"

# use Unix IAS File System location
# URL_Root_Directory <- "/projects/ECEP/CESM_Ensembles/CONUS_Analyses/Monthly/"

# Here is the variable we want to extract
#  these should match the files names since the variable in the file and file name match
target_variable = "PRECT"
variable_name   = "Total Daily Precip"
variable_units  = "mm" # post processing (starts as m/s)


# get URL files
RCP_85_File <- paste(target_variable,
                     "_RCP85_SODAK_DAILY_1920-2100.nc4",
                     sep="")

RCP_45_File <- paste(target_variable,
                     "_RCP45_SODAK_DAILY_1920-2080.nc4",
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


nt = 58766
y1 = 1920
y2 = 2080
nm = 12
nd = c(31,28,31,30,31,30,31,31,30,31,30,31)

time_days.85 <- seq(from     = as.Date("1920-01-01"),  # start value
                      length = nt,  # end value
                      by      = "days")



tt = 1
for(yy in y1:y2) {
  for(mm in 1:nm) {
    for (dd in 1:nd[mm]) {
      time_days.85[tt] <- as.Date(paste(toString(yy),"-",
                                        toString(mm),"-",
                                        toString(dd),
                                        sep=""))
      tt <- 1 + tt
    }
  }
  print( c(toString(tt),toString(nt),toString(as.Date(time_days.85[tt-1]))) )
}

# for this script the RCP 8.5 is truncated with respect to ensemble members and dates to match the RCP 4.5
nt = 58765
y1 = 1920
y2 = 2080
nm = 12
nd = c(31,28,31,30,31,30,31,31,30,31,30,31)

time_days.85 <- seq(from     = as.Date("1920-01-01"),  # start value
                    length = nt,  # end value
                    by      = "days")



tt = 1
for(yy in y1:y2) {
  for(mm in 1:nm) {
    for (dd in 1:nd[mm]) {
      time_days.85[tt] <- as.Date(paste(toString(yy),"-",
                                        toString(mm),"-",
                                        toString(dd),
                                        sep=""))
      tt <- 1 + tt
    }
  }
  print( c(toString(tt-1),toString(nt),toString(as.Date(time_days.85[tt-1]))) )
}

time_days.45 <- time_days.85
time_days    <- time_days.85

remove(y1,y2,yy,mm,dd,nd)

# create ensemble dimension

ensemble = seq(from =  1,  # start value
               to   = 15,  # end value
               by   =  1   # increment
               )

# import spatial coordinates

lon <- ncvar_get(nc    = nc.85,         # netcdf file ID
                 varid = "lon"  # variable name from file
                )

lat <- ncvar_get(nc    = nc.85,         # netcdf file ID
                 varid = "lat"  # variable name from file
                )

days_from_start <- ncvar_get(nc    = nc.85,         # netcdf file ID
                             varid = "lat" )   


# location information

lon <- lon - 360.0  # converting to +/- deg east for easier plotting

# Getting Map extents

minlon <- min(lon)
maxlon <- max(lon)
minlat <- min(lat)
maxlat <- max(lat)

#  with time, latitude and longitude we can now extract the specific points
#   in our files to extract for x, y and t

target_x <- which.min( abs(         lon -  target_lon) )
target_y <- which.min( abs(         lat -  target_lat) )

# import data fields (first RCP 4.5)

var2d.45 <- ncvar_get(nc      = nc.45,                 # netcdf file ID
                      varid   = target_variable,       # variable name from file
                      verbose =  TRUE,                 # print diagnostic data
                      start   = c( 1,  1,  1,    1),   # starting coordinates
                      count   = c(10,  7, 15,    nt)    # end coordinates (we're only going to 2080 and using 15 ensembles)
                     ) * 86400. * 1000.0 
# import data fields (secondly RCP 8.5)


var2d.85 <- ncvar_get(nc      = nc.85,                 # netcdf file ID
                    varid   = target_variable,       # variable name from file
                    verbose = TRUE,                  # print diagnostic data
                    start   = c( 1,  1,  1,    1),   # starting coordinates
                    count   = c(10,  7, 15,   nt)    # end coordinates (we're only going to 2080 and using 15 ensembles)
                    ) * 86400. * 1000.0 



# assign dimension names to arrays.  these are strings including things that shoudl be numbers.

dimnames(var2d.45) <- list(lon,lat,ensemble,time_days)
dimnames(var2d.85) <- list(lon,lat,ensemble,time_days)

# tidy things up

remove(time_bounds.45, 
       time_days.45, 
       time_days.85,
       RCP_45_File,
       RCP_85_File,
       URL_45,
       URL_85,
       URL_Root_Directory,
       nc.45, 
       nc.85)




# also extract single point time series, these we can convert these into a table.

var1d.45 = t(var2d.45[target_x, target_y, , ])  # t() == transpose
var1d.85 = t(var2d.85[target_x, target_y, , ])   # t() == transpose

# add corrdinates
dimnames(var1d.45) <- list(as.Date(time_days), ensemble)
dimnames(var1d.85) <- list(as.Date(time_days), ensemble)

# "flatten" the 2-D time/ensemble aray to a single list with the melt command
var1d      = melt(data        = var1d.45,              # your array
                  na.rm       = TRUE,                  # don't use missing values
                  varnames    = c("Time","Ensemble"),  # names of your two dimensions
                  value.name  = "RCP45")               # the final name of your aray value


# repeat for 8.5 (we'll add those values to previous data frame)
var1d.85   = melt(data        = var1d.85,              # your array
                  na.rm       = TRUE,                  # don't use missing values
                  varnames    = c("Time","Ensemble"),  # names of your two dimensions
                  value.name  = "RCP85")               # the final name of your aray value\


var1d$Time = as.Date(var1d$Time, origin="1970-01-01")

var1d$RCP85 = var1d.85$RCP85

# clean more things up.
remove(var1d.85,
       var1d.45,
       var2d.45,
       var2d.85)

# floor our precip data to a non-trace being equal to 0.005 in (0.127 mm) or greater.


# break down by decade block
var1d$month   = month(var1d$Time)
var1d$year   = year(var1d$Time)
var1d$decade = trunc( var1d$year/10. )*10

var1d$RCP45[ var1d$RCP45 < 0.127 ] <- NA
var1d$RCP85[ var1d$RCP85 < 0.127 ] <- NA

# now here is where things get messy because there has to be an easier way to do this
#  (see if Dr Caudle has any ideas!)

control_decade <- 1980

test1_decade   <- 2000

test2_decade   <- 2050

control_decade_values <- var1d$RCP45[var1d$decade == control_decade]

test1_rcp85_decade_values <- var1d$RCP85[var1d$decade == test1_decade]
test2_rcp85_decade_values <- var1d$RCP85[var1d$decade == test2_decade]

test1_rcp45_decade_values <- var1d$RCP45[var1d$decade == test1_decade]
test2_rcp45_decade_values <- var1d$RCP45[var1d$decade == test2_decade]

mean_control = mean(control_decade_values)

mean_test1_rcp45 = mean(test1_rcp45_decade_values)
mean_test2_rcp45 = mean(test2_rcp45_decade_values)

mean_test1_rcp85 = mean(test1_rcp85_decade_values)
mean_test2_rcp85 = mean(test2_rcp85_decade_values)


delta_test1_control_rcp45 = mean_test1_rcp45 - mean_control
delta_test2_control_rcp45 = mean_test2_rcp45 - mean_control

delta_test1_control_rcp85 = mean_test1_rcp85 - mean_control
delta_test2_control_rcp85 = mean_test2_rcp85 - mean_control




boxplot(formula = RCP45 ~ decade,
        data    = var1d,
        main    = "RCP 8.5 (red) & RCP 4.5 (orange) Scenario",
        xlab    = "Decade",
        ylab    = paste(variable_name," (",variable_units,")",sep=""),
        ylim    = c(0,
                    10),
        boxwex=3, # width of box
        col="orange",
        boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1), # make invisible (alpha = 1)
        at = seq(1920,2080,10))


ylim    = c(min( c(var1d$RCP45, var1d$RCP85), na.rm=TRUE ),
            max( c(var1d$RCP45, var1d$RCP85), na.rm=TRUE )
)

boxplot(formula = RCP45 ~ decade,
        data    = var1d,
        add     = TRUE,  #overlay this on the previous one
        boxwex=3, 
        xaxt = "n", #don't plot the values on the x axis
        col="orange",
        at = seq(1920,2080,10) - 2) #shift position of x axis plot location



boxplot(formula = RCP85 ~ decade,
        data    = var1d,
        add     = TRUE,
        boxwex=3, 
        xaxt = "n", #don't plot the values on the x axis
        col="red",
        at = seq(1920,2080,10) +2) #shift position of x axis plot location


