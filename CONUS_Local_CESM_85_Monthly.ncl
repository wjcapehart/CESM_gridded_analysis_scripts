
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

   rcploop = (/ "RCP45","RCP85" /)

do rcpl = 0, 1

   rcp   = rcploop(rcpl)

   ;
   ; Clipbox Latitude and Longitudes
   ;


   far_south =   23.00 ; degrees_north
   far_north =   55.00 ; degrees_north

   far_west  = -129.00 + 360.0; degrees_east
   far_east  =  -52.00 + 360.0; degrees_east

   parameters = (/ "QCHANR", \
                   "SOILLIQ", \
                   "TSA", \
                   "ANN_FAREA_BURNED", \
                   "FIRESEASONL", \
                   "FLDS", \
                   "FSDS", \
                   "FSR", \
                   "Q2M", \
                   "RAIN", \
                   "SNOW", \
                   "SNOWLIQ", \
                   "U10", \
                   "WIND", \
                   "ZWT", \
                   "VOLR", \
                   "TREFHT", \
                   "TREFHTMN", \
                   "TREFHTMX", \
                   "PSL" /)

   parameters := (/ \
                    "PSL",       \
                    "TREFHT",   \
                    "TREFHTMN", \
                    "TREFHTMX" \
                    /)

   root_dir    = "/projects/ECEP/CESM_Ensembles/CESM_LES_MES_ORIGINAL_OUTPUT/"
   scratch_dir = "/projects/ECEP/CESM_Ensembles/CONUS_Analyses/scratch/"
   final_dir   = "/projects/ECEP/CESM_Ensembles/CONUS_Analyses/"


   if (rcp  .eq.  "RCP85") then

      time_full               = new( 1032+900+240, double)
      time_full@units         = "days since 1920-01-01 00:00:00"
      time_full@calendar      = "noleap"
      time_full@bounds        = "time_bounds"
      time_full!0             = "time_full"

      n_ens = 30

      nt00 = (/    0,       1032,     900+1032 /)
      nt01 = (/ 1032, 1032 + 900, 1032+900+240 /) - 1
      nt   = (/ 1032,        900 ,         240 /)

      part1 = (/ "b.e11.B20TRC5CNBDRD.f09_g16.",  \
                 "b.e11.BRCP85C5CNBDRD.f09_g16.", \
                 "b.e11.BRCP85C5CNBDRD.f09_g16." /)

      part2 = sprinti("%0.3i", ispan(1,n_ens,1))

      part3 = ".clm2.h0."

      part5 = (/ ".192001-200512.nc",  \
                 ".200601-208012.nc",  \
                 ".208101-210012.nc"  /)

      full_record = "CONUS_1920-2100"


      time_of_last_month = "66065"; RCP8.5

      scratchfile = "./deleteme85.nc"


   else

      time_full               = new( 1032+900, double)
      time_full@units         = "days since 1920-01-01 00:00:00"
      time_full@calendar      = "noleap"
      time_full@bounds        = "time_bounds"
      time_full!0             = "time_full"

      n_ens = 15

      nt00 = (/    0,        1032 /)
      nt01 = (/ 1032, 1032 +  900 /) - 1
      nt   = (/ 1032,         900 /)

      part1 = (/ "b.e11.B20TRC5CNBDRD.f09_g16.",  \
                 "b.e11.BRCP45C5CNBDRD.f09_g16." /)

      part2 = sprinti("%0.3i", ispan(1,n_ens,1))

      part3 = ".clm2.h0."

      part5 = (/ ".192001-200512.nc",  \
                 ".200601-208012.nc"  /)

      full_record = "CONUS_1920-2080"

      time_of_last_month = "58765"; RCP4.5

      scratchfile = "./deleteme45.nc"

   end if

   ensemble_member                 = ispan(1,n_ens,1)
   ensemble_member!0               = "ensemble_member"
   ensemble_member&ensemble_member = ispan(1,n_ens,1)

   n_timefiles  = dimsizes(part5)
   n_parameters = dimsizes(parameters)

   tf = 0
   ef = 1
   vf = 0



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

   delta_t_ens001_vs_ens00x =   -25550; <- this will be the dif between ens 1 and the other ensembles

   do vf = 0, n_parameters-1

      print(parameters(vf))
      if ((parameters(vf) .eq. "QCHANR").or.(parameters(vf) .eq. "VOLR"))  then
         nco_spatial_extraction_string = " -d lat,"                  + \
                                          sprintf("%5.2f",far_south) + \
                                          ","                        + \
                                          sprintf("%5.2f",far_north) + \
                                          " -d lon,"                 + \
                                          sprintf("%6.2f",far_west-360.)  + \
                                          ","                        + \
                                          sprintf("%6.2f",far_east-360.)  + \
                                          " "
      else
         nco_spatial_extraction_string = " -d lat,"                  + \
                                          sprintf("%5.2f",far_south) + \
                                          ","                        + \
                                          sprintf("%5.2f",far_north) + \
                                          " -d lon,"                 + \
                                          sprintf("%6.2f",far_west)  + \
                                          ","                        + \
                                          sprintf("%6.2f",far_east)  + \
                                          " "
      end if

      if ((parameters(vf) .eq. "QCHANR").or.(parameters(vf) .eq. "VOLR")) then
         part3 = ".rtm.h0."
      else
         if ((parameters(vf) .eq. "PSL"     ).or. \
             (parameters(vf) .eq. "TREFHT"  ).or. \
             (parameters(vf) .eq. "TREFHTMN").or. \
             (parameters(vf) .eq. "TREFHTMX")) then
            part3 = ".cam.h0."
         else
            part3 = ".clm2.h0."
         end if
      end if

      system("rm -v " + scratchfile)

      do ef = 0, n_ens-1

         system("rm -v " + scratchfile)

         do tf = 0, n_timefiles-1

            system("rm -v " + scratchfile)

            string_year = part5(tf)
            longone     = False

            if ((ef .eq. 0).and.(tf .eq. 0)) then
               string_year = ".185001-200512.nc"
               longone     = True
            else
               string_year = part5(tf)
               longone     = False
            end if

            cesm_filename = root_dir       + "/" + \
                            parameters(vf) + "/" + \
                            part1(tf)      +       \
                            part2(ef)      +       \
                            part3          +       \
                            parameters(vf) +       \
                            string_year

            temp_file =     scratch_dir                 + \
                            parameters(vf)        + "_" + \
                            rcp                   + "_" + \
                            sprinti("e%0.3i", (ef + 1)) + \
                            "_"                         + \
                            sprinti("t%0.3i", (tf + 1)) + \
                            ".nc"

            ; extract spatial clipbox orig file -> scratch

            nco_command = "ncea "                       + \
                          nco_spatial_extraction_string + \
                          cesm_filename                 + \
                          " "                           + \
                          scratchfile

            print( " " + nco_command)
            system(" " + nco_command)


            ; extract variable from clipped file scratch -> temp_000

               nco_command = "ncks -v "        + \
                              parameters(vf)    + \
                              " "               + \
                              scratchfile       + \
                              " "               + \
                              temp_file

               print( " " + nco_command)
               system(" " + nco_command)

               print( "rm -v " + scratchfile)
               system("rm -v " + scratchfile)


         end do

         temp_files =    scratch_dir                 + \
                         parameters(vf)        + "_" + \
                         rcp                   + "_" + \
                         sprinti("e%0.3i", (ef + 1)) + \
                         "_t*.nc"

         ens_file =      scratch_dir                 + \
                         parameters(vf)        + "_" + \
                         rcp                   + "_" + \
                         sprinti("e%0.3i", (ef + 1)) + \
                         ".nc"

         ; concatemnate ensembles temp_000 -> ensemble_file.nc

         nco_command = "ncrcat -h " + temp_files + " " + ens_file
         print( " " + nco_command)
         system(" " + nco_command)

         ; remove the temp_000

         print( " rm -frv " + temp_files)
         system(" rm -frv " + temp_files)

      end do
      ;;
      ;;  we now have to reset the fist ensemble to start at 1920 rather than 1850
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
      ;;  ncea  -d time,31.,66065. temp.nc  ANN_FAREA_BURNED_RCP85_e001.nc
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
                     full_record             + \
                     ".nc"

      ;; fix the tme bounds for the first ensemnble member

         ; move zero date from 1850 to 1920 for the first ensemble ens_001 -> scratch


         if (part3 .ne. ".cam.h0.") then

            nco_command = "ncap2  -s 'time_bounds=time_bounds-25550' -s 'time=time-25550' "+ ens_001_file + " " + scratchfile
            print( " " + nco_command)
            system(" " + nco_command)

            nco_command = "ncatted -O -a bounds,time,m,c,'time_bounds'  " + scratchfile
            print( " " + nco_command)
            system(" " + nco_command)

            ; fix longname for time_bounds scratch -> scratch

            nco_command = "ncatted -O -a long_name,time_bounds,m,c,'history time interval endpoints'  " + scratchfile
            print( " " + nco_command)
            system(" " + nco_command)


         else
            nco_command = "ncap2 -s 'time=time-25550' "+ ens_001_file + " " + scratchfile
            print( " " + nco_command)
            system(" " + nco_command)
         end if

         ; wipe out ens_001_file

         system("rm -v "+ ens_001_file)

         ; fix long name for time scratch -> scratch

         nco_command = "ncatted -O -a long_name,time,m,c,'time'  " + scratchfile
         print( " " + nco_command)
         system(" " + nco_command)

         ; fix units for time scratch -> scratch

         nco_command = "ncatted -O -a units,time,m,c,'days since 1920-01-01 00:00:00'  " + scratchfile
         print( " " + nco_command)
         system(" " + nco_command)

         ; fix calendar for time scratch -> scratch

         nco_command = "ncatted -O -a calendar,time,m,c,'noleap'  " + scratchfile
         print( " " + nco_command)
         system(" " + nco_command)


         ; add coordinate attribute


         if (parameters(vf) .eq. "SOILLIQ") then
            nco_command = "ncatted -O -a coordinate,"+parameters(vf)+",m,c,'time levgrnd latitude longitude'  " + scratchfile
            print( " " + nco_command)
            system(" " + nco_command)
         else
            nco_command = "ncatted -O -a coordinate,"+parameters(vf)+",m,c,'time latitude longitude'  " + scratchfile
            print( " " + nco_command)
            system(" " + nco_command)
         end if

         ; fix timebound coordinate for time scratch -> scratch
         ; clip the time series for ensemble 1 to start at 1920 scratch->ens_001_file

         nco_command = "ncea  -d time,31.,"+time_of_last_month+".  " + scratchfile +  " " + ens_001_file
         print( " " + nco_command)
         system(" " + nco_command)

         ; remove series scratchfile

         print( " rm -frv " + scratchfile)
         system(" rm -frv " + scratchfile)

         ; concatenate by all ensemble member into a single file ens_0xx_files->complete_ensemble

         nco_command = "ncecat "+ ens_0xx_files + " " + ens_all_file
         print( " " + nco_command)
         system(" " + nco_command)

         ; add longname attribute to the time attribute complete_ensemble->complete_ensemble

         nco_command = "ncatted -O -a long_name,time,m,c,'time' " + ens_all_file
         print( " " + nco_command)
         system(" " + nco_command)

         ; change the name of the record dimension from 'record' to 'ensemble' complete_ensemble->complete_ensemble

         nco_command = "ncrename -O -d record,ensemble " + ens_all_file
         print( " " + nco_command)
         system(" " + nco_command)

         ; swap the ensemble and time dimensions complete_ensemble file -> scratch

         nco_command = "ncpdq  -a time,ensemble " + ens_all_file + "  " + scratchfile
         print( " " + nco_command)
         system(" " + nco_command)

         ; clean scratch files

         print( " rm -v " + ens_0xx_files)
         system(" rm -v " + ens_0xx_files)

         ; replace scratch->complete_ensemble

         print( " mv -v " + scratchfile + " " + ens_all_file)
         system(" mv -v " + scratchfile + " " + ens_all_file)

         ; nccopy classic netcdf v3 to netcdf v4

         print( " $NETCDF/bin/nccopy -u -4 -d 5 " + ens_all_file + " " + ens_all_file+"4")
         system(" $NETCDF/bin/nccopy -u -4 -d 5 " + ens_all_file + " " + ens_all_file+"4")
         print( " rm -v " + ens_all_file)
         system(" rm -v " + ens_all_file)


      ;; done!
      print( " $NETCDF/bin/ncdump -h " + ens_all_file+"4")
      system(" $NETCDF/bin/ncdump -h " + ens_all_file+"4")

   end do
   delete([/ time_full, nt00,nt01,nt,part1,part2,part3,part5, ensemble_member /])

end do

end
