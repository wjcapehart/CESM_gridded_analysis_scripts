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



URL_Root_Directory <- "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/CESM_SODAK/"

# Here is the variable we want to extract
#  these should match the files names since the variable in the file and file name match

target_variable = "NWS HI"
variable_name   = "Heat Index Duration"
variable_units  = "hr" # after post processing (starts as m/s)

threshold_value = 90. 

# get the extreme function to use

extreme_func = "GP"


# get URL files
RCP_85_File  = paste("ANIMAL_INDICIES_RCP85_SODAK_DAILY_1920-2100.nc4",
                     sep="")

RCP_45_File  = paste("ANIMAL_INDICIES_RCP45_SODAK_DAILY_1920-2080.nc4",
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

ntm = 1932

# we now create our straw time arrays (which are in standard calendar form)


time_days.45 = seq(from   = as.Date("1920-01-01"),  # start value
                   length = nt,  # end value
                   by     = "days")

time_months = seq(from = as.Date("1920-01-01"),
                  to   = as.Date("2080-12-01"),
                  by   = "months")

# we now do a hard-rewrite of our time vector one day at a time.
tt1 = 1
for(yy in y1:y2) {
  for(mm in 1:nm) {
    tt2 = tt1+nd[mm]-1
    time_days.45[tt1:tt2] = seq.Date(from = as.Date(paste(toString(yy),"-",
                                                          toString(mm),"-",
                                                          "01",
                                                          sep="")),
                                     to   = as.Date(paste(toString(yy),    "-",
                                                          toString(mm),    "-",
                                                          toString(nd[mm]),
                                                          sep="")),
                                     by   = "day")
    tt1 = tt2+1
  }
  # print( c(toString(tt1-1),
  #          toString(nt),
  #          toString(as.Date(time_days.45[tt1-1]))
  #          )
  #      )
}
print(paste("final day",time_days.45[nt]))

# mirror the RCP45 time to match the RCP85
time_days.45 = time_days.45
time_days    = time_days.45


# and tidy things up
remove(y1,y2,yy,mm,nd,nm,tt1,tt2)


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


Milk_loss_dairy_cow_daily.45 = ncvar_get(nc      = nc.45,                      # netcdf file ID
                            varid   = "Milk_loss_dairy_cow_daily",            # variable name from file
                            verbose = FALSE,                      # print diagnostic data
                            start   = c(target_x,  target_y,           1,  1), # starting coordinates
                            count   = c(       1,         1, n_ensembles, nt)  # end coordinates (we're only going to 2080 and using 15 ensembles)
                            )




Delta_heatstress_Death_Rate_dairy_cow_monthly.45 = ncvar_get(nc      = nc.45,                      # netcdf file ID
                            varid   = "Delta_heatstress_Death_Rate_dairy_cow_monthly",            # variable name from file
                            verbose = FALSE,                      # print diagnostic data
                            start   = c(target_x,  target_y,           1,   1), # starting coordinates
                            count   = c(       1,         1, n_ensembles, ntm)  # end coordinates (we're only going to 2080 and using 15 ensembles)
                            )



# RCP 8.5

Milk_loss_dairy_cow_daily.85 = ncvar_get(nc      = nc.85,                      # netcdf file ID
                                         varid   = "Milk_loss_dairy_cow_daily",            # variable name from file
                                         verbose = FALSE,                      # print diagnostic data
                                         start   = c(target_x,  target_y,           1,  1), # starting coordinates
                                         count   = c(       1,         1, n_ensembles, nt)  # end coordinates (we're only going to 2080 and using 15 ensembles)
)



Delta_heatstress_Death_Rate_dairy_cow_monthly.85 = ncvar_get(nc      = nc.85,                      # netcdf file ID
                            varid   = "Delta_heatstress_Death_Rate_dairy_cow_monthly",            # variable name from file
                            verbose = FALSE,                      # print diagnostic data
                            start   = c(target_x,  target_y,           1,   1), # starting coordinates
                            count   = c(       1,         1, n_ensembles,  ntm)  # end coordinates (we're only going to 2080 and using 15 ensembles)
)


dim(Milk_loss_dairy_cow_daily.85)
dim(Milk_loss_dairy_cow_daily.45)
length(time_days)

# Add Dimensions



Milk_loss_dairy_cow_daily.45 =  t(Milk_loss_dairy_cow_daily.45[ , ]) # t() == transpose
Milk_loss_dairy_cow_daily.85 =  t(Milk_loss_dairy_cow_daily.85[ , ]) # t() == transpose

Delta_heatstress_Death_Rate_dairy_cow_monthly.45 =  t(Delta_heatstress_Death_Rate_dairy_cow_monthly.45[ , ]) # t() == transpose
Delta_heatstress_Death_Rate_dairy_cow_monthly.85 =  t(Delta_heatstress_Death_Rate_dairy_cow_monthly.85[ , ]) # t() == transpose

dimnames(Milk_loss_dairy_cow_daily.45) <- list(as.Date(time_days, origin="1970-01-01"),ensemble)
dimnames(Milk_loss_dairy_cow_daily.85) <- list(as.Date(time_days, origin="1970-01-01"),ensemble)

dimnames(Delta_heatstress_Death_Rate_dairy_cow_monthly.45) <- list(as.Date(time_months, origin="1970-01-01"),ensemble)
dimnames(Delta_heatstress_Death_Rate_dairy_cow_monthly.85) <- list(as.Date(time_months, origin="1970-01-01"),ensemble)





remove(time_days.45,
       RCP_45_File,
       RCP_85_File,
       URL_45,
       URL_85,
       URL_Root_Directory,
       nc.45, 
       nc.85)

# Milk_loss_dairy_cow_daily
melt1 = melt(data       = Milk_loss_dairy_cow_daily.45,
                   na.rm      = FALSE,
                   varnames   = c("Time","Ensemble"),
                   value.name = "RCP_45")


temp =    melt(data       = Milk_loss_dairy_cow_daily.85,
               na.rm      = FALSE,
               varnames   = c("Time","Ensemble"),
               value.name = "RCP_85")

melt1$RCP_85 = temp$RCP_85

Milk_loss_dairy_cow_daily = melt(data       = melt1,
     na.rm      = TRUE,
     varnames   = c("RCP_45","RCP_85"),
     id.vars    = c("Time","Ensemble"),
     value.name = "Milk_loss_dairy_cow_daily")
colnames(Milk_loss_dairy_cow_daily) <- c("Time","Ensemble","Scenario","Milk_loss_dairy_cow_daily")
Milk_loss_dairy_cow_daily$Time = as.Date(Milk_loss_dairy_cow_daily$Time, origin="1970-01-01")


Milk_loss_dairy_cow_daily$Year = year(Milk_loss_dairy_cow_daily$Time)

Milk_loss_dairy_cow_yearly = aggregate(formula = Milk_loss_dairy_cow_daily ~
                                                        Year +
                                                        Ensemble +
                                                        Scenario,
                                       data = Milk_loss_dairy_cow_daily, 
                                       FUN=sum)

colnames(Milk_loss_dairy_cow_yearly) <- c("Year","Ensemble","Scenario","Milk_loss_dairy_cow_yearly")

remove(Milk_loss_dairy_cow_daily.45)
remove(Milk_loss_dairy_cow_daily.85)
remove(temp, melt1)


# Delta_heatstress_Death_Rate_dairy_cow_monthly
melt1 = melt(data       = Delta_heatstress_Death_Rate_dairy_cow_monthly.45,
             na.rm      = FALSE,
             varnames   = c("Time","Ensemble"),
             value.name = "RCP_45")


temp =    melt(data       = Delta_heatstress_Death_Rate_dairy_cow_monthly.85,
               na.rm      = FALSE,
               varnames   = c("Time","Ensemble"),
               value.name = "RCP_85")

melt1$RCP_85 = temp$RCP_85

Delta_heatstress_Death_Rate_dairy_cow_monthly = melt(data       = melt1,
                                 na.rm      = TRUE,
                                 varnames   = c("RCP_45","RCP_85"),
                                 id.vars    = c("Time","Ensemble"),
                                 value.name = "Delta_heatstress_Death_Rate_dairy_cow_monthly")
colnames(Delta_heatstress_Death_Rate_dairy_cow_monthly) <- c("Time","Ensemble","Scenario","Delta_heatstress_Death_Rate_dairy_cow_monthly")
Delta_heatstress_Death_Rate_dairy_cow_monthly$Time = as.Date(Delta_heatstress_Death_Rate_dairy_cow_monthly$Time, origin="1970-01-01")


Delta_heatstress_Death_Rate_dairy_cow_monthly$Year = year(Delta_heatstress_Death_Rate_dairy_cow_monthly$Time)

Delta_heatstress_Death_Rate_dairy_cow_yearly = aggregate(formula = Delta_heatstress_Death_Rate_dairy_cow_monthly ~
                                         Year +
                                         Ensemble +
                                         Scenario,
                                       data = Delta_heatstress_Death_Rate_dairy_cow_monthly, 
                                       FUN=mean)

colnames(Delta_heatstress_Death_Rate_dairy_cow_yearly) <- c("Year","Ensemble","Scenario","Delta_heatstress_Death_Rate_dairy_cow_yearly")

remove(Delta_heatstress_Death_Rate_dairy_cow_monthly.45)
remove(Delta_heatstress_Death_Rate_dairy_cow_monthly.85)
remove(temp, melt1)



# Add decade and scenario category

decade =  trunc( Delta_heatstress_Death_Rate_dairy_cow_yearly$Year/10. )*10
scen   = Delta_heatstress_Death_Rate_dairy_cow_yearly$Scenario


Delta_heatstress_Death_Rate_dairy_cow_yearly$decade_Scenario = paste(sprintf("%04d",decade),
                                                                     scen,
                                                                      sep = " ")

Milk_loss_dairy_cow_yearly$decade_Scenario = paste(sprintf("%04d",decade),
                                                   scen,
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


workspace = subset(Milk_loss_dairy_cow_yearly,  
                (Year >= 1990) & (Year <2050)  )    


beanplot(formula     = Milk_loss_dairy_cow_yearly~decade_Scenario,  # formula selection for y axis
         data        = workspace,                                     # data frame to use
         col         = list(c(blue, darkblue,darkblue,darkblue),  # same order as earlier for each series
                            c(red,  darkred,  darkred, darkred)), 
         border      = c(darkblue,darkred),                              
         overallline = "median",                              # can't get rid of this dang thing
         beanlines   = "median",                              # use median for the "central value"
         side        = "both",
         main        = paste(location_name,
                             "Dairly Cow Milk Loss from High Temperatures",
                             sep = " "),         # title caption
         xlab        = "Decade",                 # xaxis title
         ylab        = paste("Dairly Cow Milk Loss (l yr-1 cow-1)"),
         log         = "",
         na.omit = TRUE, cutmin=TRUE,bw="nrd0"
)


legend("topleft", 
       fill = c("blue", "red"), 
       legend = c("RCP 4.5","RCP 8.5")  )





workspace = subset(Delta_heatstress_Death_Rate_dairy_cow_yearly,  
                   (Year >= 1990) & (Year <2050)  )    



beanplot(formula     = (Delta_heatstress_Death_Rate_dairy_cow_yearly*10000)~decade_Scenario,  # formula selection for y axis
         data        = workspace,                                     # data frame to use
         col         = list(c(blue, darkblue,darkblue,darkblue),  # same order as earlier for each series
                            c(red,  darkred,  darkred, darkred)), 
         border      = c(darkblue,darkred),                              
         overallline = "median",                              # can't get rid of this dang thing
         beanlines   = "median",                              # use median for the "central value"
         side        = "both",
         main        = paste(location_name,
                             "Increase of Dairy Cow Deaths from Heat",
                             sep = " "),         # title caption
         xlab        = "Decade",                 # xaxis title
         ylab        = "Death Rate Change (%rate [100 cows-1] yr-1)",
         log         = "",
         na.omit = TRUE, cutmin=TRUE,bw="nrd0"
)


legend("topleft", 
       fill = c("blue", "red"), 
       legend = c("RCP 4.5","RCP 8.5")  )





