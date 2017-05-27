
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"





begin

 ;   setfileoption("nc", "FileStructure", "Advanced")
 ;   setfileoption("nc", "Format",  "NetCDF4")


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;
   ;  User Modification Area
   ;
   ;   User selects Repositories, Years and File Locations
   ;

   ;
   ; Repository, and Date Runs
   ;


   ;
   ; Clipbox Latitude and Longitudes
   ;


   far_south =   23.00 ; degrees_north
   far_north =   55.00 ; degrees_north

   far_west  = -129.00 + 360.0; degrees_east
   far_east  =  -52.00 + 360.0; degrees_east

   n_ens = 15

   rcp   = "RCP45"

   parameters = (/ "TSA", \
                   "ANN_FAREA_BURNED", \
                   "FIRESEASONL", \
                   "FLDS", \
                   "FSDS", \
                   "Q2M", \
                   "QCHANR", \
                   "RAIN", \
                   "SNOW", \
                   "SNOWLIQ", \
                   "SOILLIQ", \
                   "SURFDATA", \
                   "U10", \
                   "WIND", \
                   "Z500", \
                   "ZWT" /)

   tossed_params = (/ "BSW", "DZSOI", "HKSAT", "SUCSAT", "ZSOI", "WATSAT", \
                      "area", "topo", "landfrac", "landmask", "pftmask",   \
                      "time_bounds","time_written","nstep","mscur", \
                      "mdcur","mcsec","mcdate","levlak","levgrnd"  /)

   root_dir    = "/projects/C3WE/CESM_RUNS/"
   scratch_dir = "/projects/C3WE/CESM_RUNS/scratch/"
   final_dir   = "/projects/C3WE/CESM_RUNS/CONUS/"


   ensemble_member                 = ispan(1,n_ens,1)
   ensemble_member!0               = "ensemble_member"
   ensemble_member&ensemble_member = ispan(1,n_ens,1)

   part1 = (/ "b.e11.B20TRC5CNBDRD.f09_g16.",  \
              "b.e11.BRCP45C5CNBDRD.f09_g16." /)

   part2 = sprinti("%0.3i", ispan(1,n_ens,1))
   part3 = ".clm2.h0."
   part5 = (/ ".192001-200512.nc",  \
              ".200601-208012.nc"  /)

   n_timefiles  = dimsizes(part5)
   n_parameters = dimsizes(parameters)

   nt00 = (/    0,       1032 /)
   nt01 = (/ 1032, 1032 + 900 /) -1
   nt   = (/ 1032,        900 /)

   tf = 0
   ef = 1
   vf = 0

   time_full               = new( 1032+900, double)
   time_full@units         = "days since 1920-01-01 00:00:00"
   time_full@calendar      = "noleap"
   time_full@bounds        = "time_bounds"
   time_full!0             = "time_full"

   nco_spatial_extraction_string = " -d lat,"                  + \
                                    sprintf("%5.2f",far_south) + \
                                    ","                        + \
                                    sprintf("%5.2f",far_north) + \
                                    " -d lon,"                 + \
                                    sprintf("%6.2f",far_west)  + \
                                    ","                        + \
                                    sprintf("%6.2f",far_east)  + \
                                    " "

   ;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ens1_date = 0
      ens1_date@units    = "days since 1850-01-01 00:00:00"
      ens1_date@calendar = "noleap"
   ensx_date    = 0
      ensx_date@calendar = "noleap"
      ensx_date@units    = "days since 1920-01-01 00:00:00"

   ens1_ens1_date_diff = cd_convert(ens1_date,ensx_date@units)

   delta_t_ens001_vs_ens00x =   -25550

   last_month = 66065; RCP8.5


   do vf = 0, n_parameters-1

      system("rm -v deleteme45.nc")

      do ef = 0, n_ens-1

         system("rm -v deleteme45.nc")

         do tf = 0, n_timefiles-1

            system("rm -v deleteme45.nc")

            string_year = part5(tf)
            longone = False

            if ((part2(ef) .eq. "001").and.(tf .eq. 0)) then
               string_year = ".185001-200512.nc"
               longone = True
            else
               string_year = part5(tf)
               longone = False
            end if


            cesm_filename = root_dir       + "/" + \
                            parameters(vf) + "/" + \
                            part1(tf)      +       \
                            part2(ef)      +       \
                            ".clm2.h0."    +       \
                            parameters(vf) +       \
                            string_year

            temp_file =     scratch_dir                 + \
                            parameters(vf) + "_"        + \
                            rcp            + "_"        + \
                            sprinti("e%0.3i", (ef + 1)) + \
                            "_"                         + \
                            sprinti("t%0.3i", (tf + 1)) + \
                            ".nc"

            nco_command = "ncea "               + \
                          nco_spatial_extraction_string + \
                          cesm_filename         + \
                          " deleteme45.nc"

            print( " "+nco_command)
            system(" "+nco_command)

            nco_command = "ncks -v "     + \
                          parameters(vf)  + \
                          " deleteme45.nc " + \
                          temp_file



            print( " "+nco_command)
            system(" "+nco_command)
            print( "rm -v deleteme45.nc")
            system("rm -v deleteme45.nc")
         end do

         temp_files =    scratch_dir                 + \
                         parameters(vf) + "_"        + \
                         rcp            + "_"        + \
                         sprinti("e%0.3i", (ef + 1)) + \
                         "_t*.nc"


         ens_file =      scratch_dir                 + \
                         parameters(vf) + "_"        + \
                         rcp            + "_"        + \
                         sprinti("e%0.3i", (ef + 1)) + \
                         ".nc"


         nco_command = "ncrcat -h " + temp_files + " " + ens_file
         print( " " + nco_command)
         system(" " + nco_command)

         print( " rm -frv " + temp_files)
         system(" rm -frv " + temp_files)


      end do
      ;; ncl command is
      ;;
      ;;  ncap -s "time_bounds=time_bounds-25550" -s  "time=time-25550"  ANN_FAREA_BURNED_RCP85_e001.nc temp.nc
      ;;
      ;;  ncatted -O -a long_name,time,a,c,"time" temp.nc
      ;;  ncatted -O -a units,time,a,c,"days since 1920-01-01 00:00:00" temp.nc
      ;;  ncatted -O -a calendar,time,a,c,"noleap" temp.nc
      ;;  ncatted -O -a bounds,time,a,c,"time_bounds" temp.nc
      ;;
      ;;  ncatted -O -a long_name,time_bounds,a,c,"history time interval endpoints" temp.nc
      ;;
      ;;  ncea  -d time,31.,27375. temp.nc  ANN_FAREA_BURNED_RCP85_e001.nc
      ;;
      ;;  rm -v temp.nc
      ;;
      ;;  ncecat ANN_FAREA_BURNED_RCP85_e00*.nc ANN_FAREA_BURNED_RCP85_CONUS.nc
      ;;
      ;;  ncrename -O -d record,ensemble  ANN_FAREA_BURNED_RCP85_CONUS.nc

      ens_001_file = scratch_dir          + \
                     parameters(vf) + "_" + \
                     rcp            + "_" + \
                     "e001"               + \
                     ".nc"

      ens_0xx_files = scratch_dir          + \
                      parameters(vf) + "_" + \
                      rcp            + "_" + \
                      "e*"                 + \
                      ".nc"

      ens_all_file = scratch_dir             + \
                     parameters(vf)    + "_" + \
                     rcp               + "_" + \
                     "CONUS_1920-2080"       + \
                     ".nc"


      ;; fix the tme bounds for the first ensemnble member

         nco_command = "ncap -s 'time_bounds=time_bounds-25550' -s 'time=time-25550' "+ ens_001_file + " deleteme45.nc"
         print( " " + nco_command)
         system(" " + nco_command)

         nco_command = "ncatted -O -a long_name,time,a,c,'time' deleteme45.nc"
         print( " " + nco_command)
         system(" " + nco_command)

         nco_command = "ncatted -O -a units,time,a,c,'days since 1920-01-01 00:00:00' deleteme45.nc"
         print( " " + nco_command)
         system(" " + nco_command)

         nco_command = "ncatted -O -a calendar,time,a,c,'noleap' deleteme45.nc"
         print( " " + nco_command)
         system(" " + nco_command)

         nco_command = "ncatted -O -a bounds,time,a,c,'time_bounds' deleteme45.nc"
         print( " " + nco_command)
         system(" " + nco_command)

         nco_command = "ncatted -O -a long_name,time_bounds,a,c,'history time interval endpoints' deleteme45.nc"
         print( " " + nco_command)
         system(" " + nco_command)

         system("rm -v "+ ens_001_file)

         nco_command = "ncea  -d time,31.,27375. deleteme45.nc  " + ens_001_file
         print( " " + nco_command)
         system(" " + nco_command)

         system("rm -v deleteme45.nc")

      ;; concatenate by ensemble member

         nco_command = "ncecat "+ ens_0xx_files + " " + ens_all_file
         print( " " + nco_command)
         system(" " + nco_command)

         nco_command = "ncatted -O -a long_name,time,a,c,'time' " + ens_all_file
         print( " " + nco_command)
         system(" " + nco_command)

         nco_command = "ncrename -O -d record,ensemble " + ens_all_file
         print( " " + nco_command)
         system(" " + nco_command)

         system("rm -v "+ ens_0xx_files)



         print("  /usr/local/netcdf/bin/nccopy -u -k 3 -d 5 " + ens_all_file + " " + ens_all_file+"4")
         system(" /usr/local/netcdf/bin/nccopy -u -k 3 -d 5 " + ens_all_file + " " + ens_all_file+"4")


         if (parameters(vf) .eq. "SOILLIQ")

            nco_command = "ncpdq -a time,ensemble,levgrnd,lat,lon " + ens_all_file + " deleteme45.nc"
            print( " " + nco_command)
            system(" " + nco_command)

         else

            nco_command = "ncpdq -a time,ensemble,lat,lon " + ens_all_file + " deleteme45.nc"
            print( " " + nco_command)
            system(" " + nco_command)

         end if



      ;; done!
      print( "ncdump -h " + ens_all_file)
      system("ncdump -h " + ens_all_file)



   end do


end
