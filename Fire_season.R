library(ncdf4)     # processing netCDF data
library(lubridate) # processing dates and time
library(reshape2)  # manipulating data frames
library(extRemes)  # extreme data analysis
library(beanplot)  # trying to use the beanplot routine since box whiskers gave me some heartburn






location_name = "Rapid City"

target_lon  = -104. # degrees east
target_lat  =   44. # degrees north


location_name = "Brookings"

target_lon  = -96.7984 # degrees east
target_lat  =   44.3114 # degrees north




location_name = "Phillip"#

target_lon  = -101.6651 # degrees east
target_lat  =  44.0394 # degrees north



n_ensembles = 15



period_span     = 30.
base_start      = 1976              # start year
base_end        = base_start + period_span-1 # end year  (just before our simulations diverge)

per1_start      = 2020             # start
per1_end        = per1_start + period_span-1 # end year



URL_Root_Directory <- "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/CESM_CONUS/"

# Here is the variable we want to extract
#  these should match the files names since the variable in the file and file name match

target_variable = "TSA"
variable_name   = "Mean Air Temperature"
variable_units  = "deg C" # after post processing (starts as m/s)
threshold_value = 90.

# get the extreme function to use

extreme_func = "GP"
# http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/CESM_CONUS/FIRESEASONL_RCP85_CONUS_1920-2100.nc4

# get URL files
RCP_85_File  = paste(target_variable,"_RCP85_CONUS_1920-2100.nc4",
                     sep="")

RCP_45_File  = paste(target_variable,"_RCP45_CONUS_1920-2080.nc4",
                     sep="")

# make the final URLs for extracting with the "paste" function

URL_85  = paste(URL_Root_Directory,  # string 1 to concatenate
                RCP_85_File,         # string 2 to concatenate
                sep=""               # separation character ("" = none)
)

URL_45  = paste(URL_Root_Directory,  # string 1 to concatenate
                RCP_45_File,         # string 2 to concatenate
                sep=""               # separation character ("" = none)
)







nc.45 <- nc_open(filename = URL_45)
nc.85 <- nc_open(filename = URL_85)





# First get the specifics ranges that we'll need.
nt = 58765  # Total time steps we'll use
y1 =  1920  # starting calendar year
y2 =  2080  # ending calendar year
nm =    12  # number of months in a year
nd = c(31,28,31,30,31,30,31,31,30,31,30,31) # days in a month.

# we now create our straw time arrays (which are in standard calendar form)




time_months = seq(from   = as.Date("1920-01-01"),  # start value
                  to     = as.Date("2080-12-01"),  # end value
                  by     = "month")
 
nt = length(time_months)


# create ensemble dimension

ensemble = seq(from =           1,  # start value
               to   = n_ensembles,  # end value
               by   =           1   # increment
)

# import spatial coordinates

lon = ncvar_get(nc    = nc.45, # netcdf file ID
                varid = "lon"  # variable name from file
)

lon = lon - 360.0  # converting to +/- deg east for easier plotting

lat = ncvar_get(nc    = nc.45,  # netcdf file ID
                varid = "lat"  # variable name from file
)



# we now get the target x and y locations
target_x <- which.min( abs(         lon -  target_lon) )
target_y <- which.min( abs(         lat -  target_lat) )


# RCP 8.5


var.45 = ncvar_get(nc      = nc.45,                      # netcdf file ID
                            varid   = target_variable,            # variable name from file
                            verbose = FALSE,                      # print diagnostic data
                            start   = c(target_x,  target_y,           1,  1), # starting coordinates
                            count   = c(       1,         1, n_ensembles, nt)  # end coordinates (we're only going to 2080 and using 15 ensembles)
                            )




var.85 = ncvar_get(nc      = nc.85,                      # netcdf file ID
                            varid   = target_variable,            # variable name from file
                            verbose = FALSE,                      # print diagnostic data
                            start   = c(target_x,  target_y,           1,  1), # starting coordinates
                            count   = c(       1,         1, n_ensembles, nt)  # end coordinates (we're only going to 2080 and using 15 ensembles)
                            )


var.85 =  9. / 5.* (var.85 - 273.) + 32.
var.45 =  9. / 5.* (var.45 - 273.) + 32.


# Add Dimensions



var.45 =  t(var.45[ , ]) # t() == transpose
var.85 =  t(var.85[ , ]) # t() == transpose


dimnames(var.45) <- list(as.Date(time_months, origin="1970-01-01"),ensemble)
dimnames(var.85) <- list(as.Date(time_months, origin="1970-01-01"),ensemble)


remove(RCP_45_File,
       RCP_85_File,
       URL_45,
       URL_85,
       URL_Root_Directory,
       nc.45,
       nc.85)

var.45 = melt(data       = var.45,
              na.rm      = TRUE,
              varnames   = c("Time","Ensemble"),
              value.name = "RCP45")

var.45$Time = as.Date(var.45$Time, origin="1970-01-01")

temp =    melt(data       =  var.85,
               na.rm      = TRUE,
               varnames   = c("Time","Ensemble"),
               value.name = "RCP85")

var.45$RCP85 = temp$RCP85


remove(temp, 
       var.85)




var           = melt(data       = var.45,
                     na.rm      = TRUE,
                     varnames   = c("RCP45","RCP85"),
                     id.vars    = c("Time","Ensemble"),
                     value.name = "Variable")

colnames(var) = c("Time","Ensemble","Scenario","Variable")

remove(var.45)

var$Year   = year(var$Time)
var$Decade = trunc( var$Year/10. ) * 10
var$Month  = month(var$Time)

var_monthly = var

remove(var)



####

var_monthly_by_decade = aggregate(formula = Variable ~
                                            Decade +
                                            Month +
                                            Ensemble + 
                                            Scenario,
                       data    = var_monthly,
                       FUN     = mean)

var_decade_by_month = aggregate(formula = Variable ~
                                  Month +
                                  Decade +
                                    Ensemble + 
                                    Scenario,
                                  data    = var_monthly,
                                  FUN     = mean)


#####

var_yearly = aggregate(formula = Variable ~
                                 Year +
                                 Ensemble +
                                 Scenario,
                       data    = var_monthly,
                       FUN     = mean)

var_yearly$Decade = trunc( var_yearly$Year/10. ) * 10


var_yearly$Decade_Scenario = paste(sprintf("%04d", var_yearly$Decade),
                                   var_yearly$Scenario,
                                   sep = " ")



# colour tables.
# I am making my own colours

darkcyan = rgb(red   = 0.00,
               green = 0.50,
               blue  = 0.50,
               alpha = 0.75)  # 1 = opaque; 0 = fully clear

cyan     = rgb(red   = 0.00,
               green = 1.00,
               blue  = 1.00,
               alpha = 0.50)

darkblue = rgb(red   = 0.00,
               green = 0.00,
               blue  = 0.50,
               alpha = 0.75)

blue     = rgb(red   = 0.00,
               green = 0.00,
               blue  = 1.00,
               alpha = 0.50)


darkmag  = rgb(red   = 0.50,
               green = 0.00,
               blue  = 0.50,
               alpha = 0.75)

magenta  = rgb(red   = 1.00,
               green = 0.00,
               blue  = 1.00,
               alpha = 0.50)


darkred  = rgb(red   = 0.50,
               green = 0.00,
               blue  = 0.00,
               alpha = 0.75)

red      = rgb(red   = 1.00,
               green = 0.00,
               blue  = 0.00,
               alpha = 0.50)


# subsetting for Caution


workspace = subset(var_yearly,
                   (Year >= 1990) & (Year <2050)  )


beanplot(formula     = Variable~Decade_Scenario,  # formula selection for y axis
         data        = workspace,                                     # data frame to use
         col         = list(c(blue, darkblue,darkblue,darkblue),  # same order as earlier for each series
                            c(red,  darkred,  darkred, darkred)),
         border      = c(darkblue,darkred),
         overallline = "median",                              # can't get rid of this dang thing
         beanlines   = "median",                              # use median for the "central value"
         side        = "both",
         main        = paste(location_name,
                             variable_name,
                             sep = " "),         # title caption
         xlab        = "Decade",                 # xaxis title
         ylab        = paste(variable_name,                   # yaxis title
                             " (",
                             variable_units,
                             ")",
                             sep=""),
         log         = "",
         na.omit = TRUE, cutmin=TRUE,bw="nrd0"
)


legend("topleft",
       fill = c("blue", "red"),
       legend = c("RCP 4.5","RCP 8.5")  )

