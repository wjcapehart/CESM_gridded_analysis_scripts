library(ncdf4)     # processing netCDF data
library(lubridate) # processing dates and time
library(reshape2)  # manipulating data frames
library(extRemes)  # extreme data analysis
library(beanplot)  # trying to use the beanplot routine since box whiskers gave me some heartburn






location_name = "Rapid City"

target_lon  = -104. # degrees east
target_lat  =   44. # degrees north

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
RCP_85_File  = paste("NWSHI_DURATIONS_RCP85_SODAK_DAILY_1920-2100.nc4",
                     sep="")

RCP_45_File  = paste("NWSHI_DURATIONS_RCP45_SODAK_DAILY_1920-2080.nc4",
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


time_days.45 = seq(from   = as.Date("1920-01-01"),  # start value
                   length = nt,  # end value
                   by     = "days")

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


HI_Hours_27C.45 = ncvar_get(nc      = nc.45,                      # netcdf file ID
                            varid   = "HI_Hours_27C",            # variable name from file
                            verbose = FALSE,                      # print diagnostic data
                            start   = c(target_x,  target_y,           1,  1), # starting coordinates
                            count   = c(       1,         1, n_ensembles, nt)  # end coordinates (we're only going to 2080 and using 15 ensembles)
                            )




HI_Hours_32C.45 = ncvar_get(nc      = nc.45,                      # netcdf file ID
                            varid   = "HI_Hours_32C",            # variable name from file
                            verbose = FALSE,                      # print diagnostic data
                            start   = c(target_x,  target_y,           1,  1), # starting coordinates
                            count   = c(       1,         1, n_ensembles, nt)  # end coordinates (we're only going to 2080 and using 15 ensembles)
                            )



HI_Hours_41C.45 = ncvar_get(nc      = nc.45,                      # netcdf file ID
                            varid   = "HI_Hours_41C",            # variable name from file
                            verbose = FALSE,                      # print diagnostic data
                            start   = c(target_x,  target_y,           1,  1), # starting coordinates
                            count   = c(       1,         1, n_ensembles, nt)  # end coordinates (we're only going to 2080 and using 15 ensembles)
                            )


HI_Hours_54C.45 = ncvar_get(nc      = nc.45,                      # netcdf file ID
                            varid   = "HI_Hours_54C",            # variable name from file
                            verbose = FALSE,                      # print diagnostic data
                            start   = c(target_x,  target_y,           1,  1), # starting coordinates
                            count   = c(       1,         1, n_ensembles, nt)  # end coordinates (we're only going to 2080 and using 15 ensembles)
                            )


# RCP 8.5

HI_Hours_27C.85 = ncvar_get(nc      = nc.85,                      # netcdf file ID
                            varid   = "HI_Hours_27C",            # variable name from file
                            verbose = FALSE,                      # print diagnostic data
                            start   = c(target_x,  target_y,           1,  1), # starting coordinates
                            count   = c(       1,         1, n_ensembles, nt)  # end coordinates (we're only going to 2080 and using 15 ensembles)
)


HI_Hours_32C.85 = ncvar_get(nc      = nc.85,                      # netcdf file ID
                            varid   = "HI_Hours_32C",            # variable name from file
                            verbose = FALSE,                      # print diagnostic data
                            start   = c(target_x,  target_y,           1,  1), # starting coordinates
                            count   = c(       1,         1, n_ensembles, nt)  # end coordinates (we're only going to 2080 and using 15 ensembles)
)




HI_Hours_41C.85 = ncvar_get(nc      = nc.85,                      # netcdf file ID
                            varid   = "HI_Hours_41C",            # variable name from file
                            verbose = FALSE,                      # print diagnostic data
                            start   = c(target_x,  target_y,           1,  1), # starting coordinates
                            count   = c(       1,         1, n_ensembles, nt)  # end coordinates (we're only going to 2080 and using 15 ensembles)
)


HI_Hours_54C.85 = ncvar_get(nc      = nc.85,                      # netcdf file ID
                            varid   = "HI_Hours_54C",            # variable name from file
                            verbose = FALSE,                      # print diagnostic data
                            start   = c(target_x,  target_y,           1,  1), # starting coordinates
                            count   = c(       1,         1, n_ensembles, nt)  # end coordinates (we're only going to 2080 and using 15 ensembles)
)




# Add Dimensions



HI_Hours_27C.45 =  t(HI_Hours_27C.45[ , ]) # t() == transpose
HI_Hours_32C.45 =  t(HI_Hours_32C.45[ , ]) # t() == transpose
HI_Hours_41C.45 =  t(HI_Hours_41C.45[ , ]) # t() == transpose
HI_Hours_54C.45 =  t(HI_Hours_54C.45[ , ]) # t() == transpose


HI_Hours_27C.85 =  t(HI_Hours_27C.85[ , ]) # t() == transpose
HI_Hours_32C.85 =  t(HI_Hours_32C.85[ , ]) # t() == transpose
HI_Hours_41C.85 =  t(HI_Hours_41C.85[ , ]) # t() == transpose
HI_Hours_54C.85 =  t(HI_Hours_54C.85[ , ]) # t() == transpose


dimnames(HI_Hours_27C.45) <- list(as.Date(time_days, origin="1970-01-01"),ensemble)
dimnames(HI_Hours_32C.45) <- list(as.Date(time_days, origin="1970-01-01"),ensemble)
dimnames(HI_Hours_41C.45) <- list(as.Date(time_days, origin="1970-01-01"),ensemble)
dimnames(HI_Hours_54C.45) <- list(as.Date(time_days, origin="1970-01-01"),ensemble)

dimnames(HI_Hours_27C.85) <- list(as.Date(time_days, origin="1970-01-01"),ensemble)
dimnames(HI_Hours_32C.85) <- list(as.Date(time_days, origin="1970-01-01"),ensemble)
dimnames(HI_Hours_41C.85) <- list(as.Date(time_days, origin="1970-01-01"),ensemble)
dimnames(HI_Hours_54C.85) <- list(as.Date(time_days, origin="1970-01-01"),ensemble)


remove(time_days.45,
       time_days.85,
       RCP_45_File,
       RCP_85_File,
       URL_45,
       URL_85,
       URL_Root_Directory,
       nc.45, 
       nc.85)

# RPC 4.5
HI_Hours.45 = melt(data       = HI_Hours_27C.45,
                   na.rm      = TRUE,
                   varnames   = c("Time","Ensemble"),
                   value.name = "Caution")

HI_Hours.45$Time = as.Date(HI_Hours.45$Time, origin="1970-01-01")

temp =    melt(data       = HI_Hours_32C.45,
               na.rm      = TRUE,
               varnames   = c("Time","Ensemble"),
               value.name = "Extreme_Caution")

HI_Hours.45$Extreme_Caution = temp$Extreme_Caution


temp =    melt(data       = HI_Hours_41C.45,
               na.rm      = TRUE,
               varnames   = c("Time","Ensemble"),
               value.name = "Danger")

HI_Hours.45$Danger = temp$Danger


temp =    melt(data       = HI_Hours_54C.45,
               na.rm      = TRUE,
               varnames   = c("Time","Ensemble"),
               value.name = "Extreme_Danger")

HI_Hours.45$Extreme_Danger = temp$Extreme_Danger





# RCP 8.5

HI_Hours.85 = melt(data       = HI_Hours_27C.85,
                   na.rm      = TRUE,
                   varnames   = c("Time","Ensemble"),
                   value.name = "Caution")

HI_Hours.85$Time = as.Date(HI_Hours.85$Time, origin="1970-01-01")

temp =    melt(data       = HI_Hours_32C.85,
               na.rm      = TRUE,
               varnames   = c("Time","Ensemble"),
               value.name = "Extreme_Caution")

HI_Hours.85$Extreme_Caution = temp$Extreme_Caution


temp =    melt(data       = HI_Hours_41C.85,
               na.rm      = TRUE,
               varnames   = c("Time","Ensemble"),
               value.name = "Danger")

HI_Hours.85$Danger = temp$Danger


temp =    melt(data       = HI_Hours_54C.85,
               na.rm      = TRUE,
               varnames   = c("Time","Ensemble"),
               value.name = "Extreme_Danger")

HI_Hours.85$Extreme_Danger = temp$Extreme_Danger




HI_Hours    = melt(data       = HI_Hours.45,
                   na.rm      = TRUE,
                   varnames   = c("Caution","Extreme_Caution","Danger","Extreme_Danger"),
                   id.vars    = c("Time","Ensemble"),
                   value.name = "RCP_45")
colnames(HI_Hours) <- c("Time","Ensemble","Risk_Category","RCP_45")


temp = melt(data       = HI_Hours.85,
            na.rm      = TRUE,
            varnames   = c("Caution","Extreme_Caution","Danger","Extreme_Danger"),
            id.vars    = c("Time","Ensemble"),
            value.name = "RCP_85")
colnames(temp) <- c("Time","Ensemble","Risk_Category","RCP_85")

HI_Hours$RCP_8.5 = temp$RCP_85

remove(HI_Hours_27C.85, HI_Hours_32C.85, HI_Hours_41C.85, HI_Hours_54C.85)
remove(HI_Hours_27C.45, HI_Hours_32C.45, HI_Hours_41C.45, HI_Hours_54C.45)
remove(HI_Hours.85,HI_Hours.45,temp)




HI_Duration    = melt(data       = HI_Hours,
                   na.rm      = TRUE,
                   varnames   = c("RCP_45","RCP_85"),
                   id.vars    = c("Time","Ensemble","Risk_Category"),
                   value.name = "Stress_Duration")
colnames(HI_Duration) <- c("Time","Ensemble","Risk_Category","Scenario","Stress_Duration")
HI_Duration$Year = year(HI_Duration$Time)



HI_By_Year = aggregate(formula = Stress_Duration ~
                           Year +
                           Ensemble +
                           Risk_Category +
                           Scenario, 
                 data = HI_Duration, 
                 FUN=sum)

HI_By_Year$decade = trunc( HI_By_Year$Year/10. )*10


HI_By_Year$decade_Scenario = paste(sprintf("%04d", HI_By_Year$decade),
                                   HI_By_Year$Scenario,
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


workspace = subset(HI_By_Year,  
                (Risk_Category=="Caution") & (Year >= 1990) & (Year <2050)  )    


beanplot(formula     = Stress_Duration~decade_Scenario,  # formula selection for y axis
         data        = workspace,                                     # data frame to use
         col         = list(c(blue, darkblue,darkblue,darkblue),  # same order as earlier for each series
                            c(red,  darkred,  darkred, darkred)), 
         border      = c(darkblue,darkred),                              
         overallline = "median",                              # can't get rid of this dang thing
         beanlines   = "median",                              # use median for the "central value"
         side        = "both",
         main        = paste(location_name,
                             "NWS Heat Index",
                             sep = " "),         # title caption
         xlab        = "Decade",                 # xaxis title
         ylab        = paste("Hrs/Yr of HI at/over Caution Level"),
         log         = ""
)


legend("topleft", 
       fill = c("blue", "red"), 
       legend = c("RCP 4.5","RCP 8.5")  )






workspace = subset(HI_By_Year,  
                   (Risk_Category=="Extreme_Caution") & (Year >= 1990) & (Year <2050)  )    

workspace&Stress_Duration = workspace&Stress_Duration / 10.0

beanplot(formula     = Stress_Duration~decade_Scenario,  # formula selection for y axis
         data        = workspace,                                     # data frame to use
         col         = list(c(blue, darkblue,darkblue,darkblue),  # same order as earlier for each series
                            c(red,  darkred,  darkred, darkred)), 
         border      = c(darkblue,darkred),                              
         overallline = "median",                              # can't get rid of this dang thing
         beanlines   = "median",                              # use median for the "central value"
         side        = "both",
         main        = paste(location_name,
                             "NWS Heat Index",
                             sep = " "),         # title caption
         xlab        = "Decade",                 # xaxis title
         ylab        = paste("Hours/Year of HI at/over Extrm. Caution Level"),
         log         = ""
)


legend("topleft", 
       fill = c("blue", "red"), 
       legend = c("RCP 4.5","RCP 8.5")  )





workspace = subset(HI_By_Year,  
                   (Risk_Category=="Danger") & (Year >= 1990) & (Year <2050)  )    


beanplot(formula     = Stress_Duration~decade_Scenario,  # formula selection for y axis
         data        = workspace,                                     # data frame to use
         col         = list(c(blue, darkblue,darkblue,darkblue),  # same order as earlier for each series
                            c(red,  darkred,  darkred, darkred)), 
         border      = c(darkblue,darkred),                              
         overallline = "median",                              # can't get rid of this dang thing
         beanlines   = "median",                              # use median for the "central value"
         side        = "both",
         main        = paste(location_name,
                             "NWS Heat Index",
                             sep = " "),         # title caption
         xlab        = "Decade",                 # xaxis title
         ylab        = paste("Hours/Year of HI at/over Danger Level"),
         log         = ""
)


legend("topleft", 
       fill = c("blue", "red"), 
       legend = c("RCP 4.5","RCP 8.5")  )

