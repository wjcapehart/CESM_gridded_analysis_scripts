
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "./wjc_runlength.ncl"





begin



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

   nens = 30
   ensemble_member                 = ispan(1,nens,1)
   ensemble_member!0               = "ensemble_member"
   ensemble_member&ensemble_member = ispan(1,nens,1)

   part1 = (/ "b.e11.B20TRC5CNBDRD.f09_g16.",  \
              "b.e11.BRCP85C5CNBDRD.f09_g16.", \
              "b.e11.BRCP85C5CNBDRD.f09_g16." /)

   part2 = sprinti("%0.3i", ispan(1,nens,1))
   part3 = ".clm2.h0."
   part5 = (/ ".192001-200512.nc",  \
              ".200601-208012.nc",  \
              ".208101-210012.nc"  /)

   n_timefiles = dimsizes(part5)
   n_ens       = dimsizes(part2)
   nt00 = (/    0,       1032,     900+1032 /)
   nt01 = (/ 1032, 1032 + 900, 1032+900+240 /) -1
   nt   = (/ 1032,        900 ,         240 /)

   tf = 0
   ef = 1
   vf = 0

   time_full               = new( 1032+900+240, double)
   time_full@units         = "days since 1920-01-01 00:00:00"
   time_full@calendar      = "noleap"
   time_full@bounds        = "time_bounds"
   time_full!0             = "time_full"

   root_dir   = "/projects/biomimicry/CESM/CESM_RUNS/"


   ;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;
      ;  Load Static Geospatial Fields and creating requested subset
      ;     Lat and Lon
      ;     Terrain Elevation and Orography Detail
      ;     Land Cover, Soil Type, Land Mask
      ;

      cesm_model_file = root_dir         + \
                        "TSA/" + \
                        "b.e11.BRCP85C5CNBDRD.f09_g16.021.clm2.h0.TSA.200601-208012.nc"

      wf = addfile(cesm_model_file, "r")

      lon0 = (/ wf->lon(:) /)
         lon0!0           = "lon0"
         lon0&lon0         =  lon0
         lon0@description = "longitude"


      lat0 = (/  wf->lat(:) /)
         lat0!0           = "lat0"
         lat0&lat0         =  lat0
         lat0@description = "latitude"

      lon = lon0({far_west:far_east})
         lon!0             = "lon"
         lon@description   = "longitude"
         lon@long_name     = "longitude"
         lon@standard_name = "longitude"
         lon@units         = "degrees_east"

      lat = lat0({far_south:far_north})
         lat!0             = "lat"
         lat@description   = "latitude"
         lat@long_name      = "latitude"
         lat@standard_name = "latitude"
         lat@units         = "degrees_north"

      levgrnd = (/ wf->levgrnd /)
         levgrnd!0           = "levgrnd"
         levgrnd@description = "center of soil layer"
         levgrnd@long_name   = levgrnd@description
         levgrnd@units       = "m"
         levgrnd@standard_name = "depth"
         levgrnd@positive = "down"



      levlak = (/ wf->levlak /)
         levlak!0           = "levlak"
         levlak@description = "coordinate lake level"
         levlak@long_name   = levgrnd@description
         levlak@units       = "m"
         levlak@standard_name = "depth"
         levlak@positive      = "down"

      grid_call_area = wf->area({far_south:far_north},{far_west:far_east})
         grid_call_area!0             = "lat"
         grid_call_area!1             = "lon"
         grid_call_area@description   = "grid cell areas"
         grid_call_area@long_name      = "grid cell areas"
         grid_call_area@standard_name = "cell_area"
         grid_call_area@units         = "km2"

      terrain_height = wf->topo({far_south:far_north},{far_west:far_east})
         terrain_height!0             = "lat"
         terrain_height!1             = "lon"
         terrain_height@description   = "grid cell topography"
         terrain_height@long_name     = "grid cell topography"
         terrain_height@standard_name = "surface_altitude"
         terrain_height@units         = "m"

         print(max(terrain_height))

      landfrac = wf->landfrac({far_south:far_north},{far_west:far_east})
         landfrac!0             = "lat"
         landfrac!1             = "lon"
         landfrac@description   = "land fraction"
         landfrac@long_name     = "land fraction"
         landfrac@standard_name = "land_area_fraction"

      landmask = wf->landmask({far_south:far_north},{far_west:far_east})
         landmask!0             = "lat"
         landmask!1             = "lon"
         landmask@description   = "land/ocean mask (0.=ocean and 1.=land)"
         landmask@long_name     = "land/ocean mask (0.=ocean and 1.=land)"
         landmask@standard_name = "land_binary_mask"


      pftmask = wf->pftmask({far_south:far_north},{far_west:far_east})
         pftmask!0             = "lat"
         pftmask!1             = "lon"
         pftmask@description   = "pft real/fake mask (0.=fake and 1.=real)"
         pftmask@long_name     = "pft real/fake mask (0.=fake and 1.=real)"


      soil_depth = wf->ZSOI(:,{far_south:far_north},{far_west:far_east})
         soil_depth!0             = "levgrnd"
         soil_depth!1             = "lat"
         soil_depth!2             = "lon"
         soil_depth@description   = "soil depth"
         soil_depth@long_name     = "soil depth"
         soil_depth@standard_name = "depth"
         soil_depth@positive      = "down"

      soil_thickness = wf->DZSOI(:,{far_south:far_north},{far_west:far_east})
         soil_thickness!0             = "levgrnd"
         soil_thickness!1             = "lat"
         soil_thickness!2             = "lon"
         soil_thickness@description   = "soil thickness"
         soil_thickness@long_name     = "soil thickness"


      soil_porosity = wf->WATSAT(:,{far_south:far_north},{far_west:far_east})
         soil_porosity!0             = "levgrnd"
         soil_porosity!1             = "lat"
         soil_porosity!2             = "lon"
         soil_porosity@description   = "saturated soil water content (porosity)"
         soil_porosity@long_name     = "saturated soil water content (porosity)"
         soil_porosity@standard_name  = "soil_porosity"
         soil_porosity@units = "m3 m-3"

      soil_saturation_matric_potential = wf->SUCSAT(:,{far_south:far_north},{far_west:far_east})
         soil_saturation_matric_potential!0             = "levgrnd"
         soil_saturation_matric_potential!1             = "lat"
         soil_saturation_matric_potential!2             = "lon"
         soil_saturation_matric_potential@description   = "saturated soil matric potential"
         soil_saturation_matric_potential@long_name     = "saturated soil matric potential"
         soil_saturation_matric_potential@standard_name  = "soil_suction_at_saturation"
         soil_saturation_matric_potential@units = "mm"


      soil_saturation_hydraulic_conductivity = wf->HKSAT(:,{far_south:far_north},{far_west:far_east})
         soil_saturation_hydraulic_conductivity!0             = "levgrnd"
         soil_saturation_hydraulic_conductivity!1             = "lat"
         soil_saturation_hydraulic_conductivity!2             = "lon"
         soil_saturation_hydraulic_conductivity@description   = "saturated hydraulic conductivity"
         soil_saturation_hydraulic_conductivity@long_name     = "saturated hydraulic conductivity"
         soil_saturation_hydraulic_conductivity@standard_name  = "soil_hydraulic_conductivity_at_saturation"
         soil_saturation_hydraulic_conductivity@units = "m s-1"


      soil_retention_curve_slope = wf->BSW(:,{far_south:far_north},{far_west:far_east})
         soil_retention_curve_slope!0             = "levgrnd"
         soil_retention_curve_slope!1             = "lat"
         soil_retention_curve_slope!2             = "lon"
         soil_retention_curve_slope@description   = "slope of soil water retention curve"
         soil_retention_curve_slope@long_name     = "slope of soil water retention curve"


      ny     = dimsizes(lat)
      nx     = dimsizes(lon)
      n_soil = dimsizes(levgrnd)
      n_lake = dimsizes(levlak)


      delete(wf)



      system("rm -v ./CESM_Static_Metadata_85.nc")

      f_metadata = addfile("./CESM_Static_Metadata_85.nc", "c" )

      f_metadata->lon = lon
      f_metadata->lat = lat
      f_metadata->levgrnd = levgrnd
      f_metadata->levlak = levlak
      f_metadata->grid_call_area = grid_call_area
      f_metadata->terrain_height = terrain_height
      f_metadata->landfrac = landfrac
      f_metadata->landmask = landmask
      f_metadata->pftmask = pftmask
      f_metadata->soil_depth = soil_depth
      f_metadata->soil_thickness = soil_thickness
      f_metadata->soil_porosity = soil_porosity
      f_metadata->soil_saturation_matric_potential = soil_saturation_matric_potential
      f_metadata->soil_saturation_hydraulic_conductivity = soil_saturation_hydraulic_conductivity
      f_metadata->soil_retention_curve_slope = soil_retention_curve_slope

      delete(f_metadata)

      system("nccopy -4 -d 9 -s -u ./CESM_Static_Metadata_85.nc ./CESM_Static_Metadata_85.nc4" )
      system("rm -v ./CESM_Static_Metadata_85.nc")
;

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



end
