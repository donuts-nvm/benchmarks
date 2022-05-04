SUBROUTINE interp_domain_em_part1 ( grid, ngrid, config_flags ,  &
!
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_dummy_args.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
lu_index,u_1,u_2,ru,v_1,v_2,rv,w_1,w_2,ww,rw,ph_1,ph_2,phb,phb_fine,ph0,php,t_1,t_2,t_init,tp_1,tp_2,mu_1,mu_2,mub,mub_fine,mu0, &
mudf,tke_1,tke_2,p,al,alt,alb,zx,zy,rdz,rdzw,pb,fnm,fnp,rdnw,rdn,dnw,dn,znu,znw,t_base,z,q2,t2,th2,psfc,u10,v10,imask,moist_1, &
moist_2,chem_1,chem_2,u_b,u_bt,v_b,v_bt,w_b,w_bt,ph_b,ph_bt,t_b,t_bt,mu_b,mu_bt,rqv_b,rqv_bt,rqc_b,rqc_bt,rqr_b,rqr_bt,rqi_b, &
rqi_bt,rqs_b,rqs_bt,rqg_b,rqg_bt,fcx,gcx,sm000010,sm010040,sm040100,sm100200,sm010200,soilm000,soilm005,soilm020,soilm040, &
soilm160,soilm300,sw000010,sw010040,sw040100,sw100200,sw010200,soilw000,soilw005,soilw020,soilw040,soilw160,soilw300,st000010, &
st010040,st040100,st100200,st010200,soilt000,soilt005,soilt020,soilt040,soilt160,soilt300,landmask,topostdv,toposlpx,toposlpy, &
shdmax,shdmin,snoalb,slopecat,toposoil,landusef,soilctop,soilcbot,soilcat,vegcat,tslb,zs,dzs,smois,sh2o,xice,smstav,smstot, &
sfcrunoff,udrunoff,ivgtyp,isltyp,vegfra,sfcevp,grdflx,sfcexc,acsnow,acsnom,snow,snowh,canwat,sst,smfr3d,keepfr3dflag,potevp, &
snopcx,soiltb,tke_myj,ct,thz0,z0,qz0,uz0,vz0,qsfc,akhs,akms,kpbl,htop,hbot,cuppt,totswdn,totlwdn,rswtoa,rlwtoa,czmean,cfracl, &
cfracm,cfrach,acfrst,ncfrst,acfrcv,ncfrcv,f_ice_phy,f_rain_phy,f_rimef_phy,h_diabatic,msft,msfu,msfv,f,e,sina,cosa,ht,ht_fine, &
ht_int,ht_input,tsk,u_base,v_base,qv_base,z_base,rthcuten,rqvcuten,rqrcuten,rqccuten,rqscuten,rqicuten,w0avg,rainc,rainnc, &
raincv,rainncv,rainbl,nca,lowlyr,mass_flux,apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma,apr_capme,apr_capmi,xf_ens,pr_ens, &
rthften,rqvften,rthraten,rthratenlw,rthratensw,cldfra,swdown,gsw,glw,xlat,xlong,albedo,albbck,emiss,cldefi,rublten,rvblten, &
rthblten,rqvblten,rqcblten,rqiblten,tmn,xland,znt,ust,mol,pblh,capg,thc,hfx,qfx,lh,flhc,flqc,qsg,qvg,qcg,soilt1,tsnav,snowc, &
mavail,tkesfcf,taucldi,taucldc,defor11,defor22,defor12,defor33,defor13,defor23,xkmv,xkmh,xkmhd,xkhv,xkhh,div,bn2 &
!ENDOFREGISTRYGENERATEDINCLUDE
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid , ngrid
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_dummy_decl.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
real                                     :: cfn
real                                     :: cfn1
real                                     :: epsts
integer                                  :: step_number
real                                     :: rdx
real                                     :: rdy
real                                     :: dts
real                                     :: dtseps
real                                     :: resm
real                                     :: zetatop
real                                     :: cf1
real                                     :: cf2
real                                     :: cf3
integer                                  :: number_at_same_level
integer                                  :: itimestep
integer                                  :: oid
integer                                  :: auxhist1_oid
integer                                  :: auxhist2_oid
integer                                  :: auxhist3_oid
integer                                  :: auxhist4_oid
integer                                  :: auxhist5_oid
integer                                  :: auxinput1_oid
integer                                  :: auxinput2_oid
integer                                  :: auxinput3_oid
integer                                  :: auxinput4_oid
integer                                  :: auxinput5_oid
integer                                  :: nframes
integer                                  :: lbc_fid
logical                                  :: tiled
logical                                  :: patched
logical                                  :: write_metadata
real                                     :: dtbc
integer                                  :: ifndsnowh
integer                                  :: ifndsoilw
real                                     :: u_frame
real                                     :: v_frame
real                                     :: p_top
integer                                  :: stepcu
integer                                  :: stepra
integer                                  :: stepbl
logical                                  :: warm_rain
integer                                  :: run_days
integer                                  :: run_hours
integer                                  :: run_minutes
integer                                  :: run_seconds
integer                                  :: start_year
integer                                  :: start_month
integer                                  :: start_day
integer                                  :: start_hour
integer                                  :: start_minute
integer                                  :: start_second
integer                                  :: end_year
integer                                  :: end_month
integer                                  :: end_day
integer                                  :: end_hour
integer                                  :: end_minute
integer                                  :: end_second
integer                                  :: interval_seconds
logical                                  :: input_from_file
integer                                  :: history_interval
integer                                  :: frames_per_outfile
logical                                  :: restart
integer                                  :: restart_interval
integer                                  :: io_form_input
integer                                  :: io_form_history
integer                                  :: io_form_restart
integer                                  :: io_form_boundary
integer                                  :: debug_level
character*256                               :: history_outname
character*256                               :: auxhist1_outname
character*256                               :: auxhist2_outname
character*256                               :: auxhist3_outname
character*256                               :: auxhist4_outname
character*256                               :: auxhist5_outname
character*256                               :: history_inname
character*256                               :: auxhist1_inname
character*256                               :: auxhist2_inname
character*256                               :: auxhist3_inname
character*256                               :: auxhist4_inname
character*256                               :: auxhist5_inname
integer                                  :: history_interval_mo
integer                                  :: history_interval_d
integer                                  :: history_interval_h
integer                                  :: history_interval_m
integer                                  :: history_interval_s
integer                                  :: inputout_interval_mo
integer                                  :: inputout_interval_d
integer                                  :: inputout_interval_h
integer                                  :: inputout_interval_m
integer                                  :: inputout_interval_s
integer                                  :: inputout_interval
integer                                  :: auxhist1_interval_mo
integer                                  :: auxhist1_interval_d
integer                                  :: auxhist1_interval_h
integer                                  :: auxhist1_interval_m
integer                                  :: auxhist1_interval_s
integer                                  :: auxhist1_interval
integer                                  :: auxhist2_interval_mo
integer                                  :: auxhist2_interval_d
integer                                  :: auxhist2_interval_h
integer                                  :: auxhist2_interval_m
integer                                  :: auxhist2_interval_s
integer                                  :: auxhist2_interval
integer                                  :: auxhist3_interval_mo
integer                                  :: auxhist3_interval_d
integer                                  :: auxhist3_interval_h
integer                                  :: auxhist3_interval_m
integer                                  :: auxhist3_interval_s
integer                                  :: auxhist3_interval
integer                                  :: auxhist4_interval_mo
integer                                  :: auxhist4_interval_d
integer                                  :: auxhist4_interval_h
integer                                  :: auxhist4_interval_m
integer                                  :: auxhist4_interval_s
integer                                  :: auxhist4_interval
integer                                  :: auxhist5_interval_mo
integer                                  :: auxhist5_interval_d
integer                                  :: auxhist5_interval_h
integer                                  :: auxhist5_interval_m
integer                                  :: auxhist5_interval_s
integer                                  :: auxhist5_interval
integer                                  :: auxinput1_interval_mo
integer                                  :: auxinput1_interval_d
integer                                  :: auxinput1_interval_h
integer                                  :: auxinput1_interval_m
integer                                  :: auxinput1_interval_s
integer                                  :: auxinput1_interval
integer                                  :: auxinput2_interval_mo
integer                                  :: auxinput2_interval_d
integer                                  :: auxinput2_interval_h
integer                                  :: auxinput2_interval_m
integer                                  :: auxinput2_interval_s
integer                                  :: auxinput2_interval
integer                                  :: auxinput3_interval_mo
integer                                  :: auxinput3_interval_d
integer                                  :: auxinput3_interval_h
integer                                  :: auxinput3_interval_m
integer                                  :: auxinput3_interval_s
integer                                  :: auxinput3_interval
integer                                  :: auxinput4_interval_mo
integer                                  :: auxinput4_interval_d
integer                                  :: auxinput4_interval_h
integer                                  :: auxinput4_interval_m
integer                                  :: auxinput4_interval_s
integer                                  :: auxinput4_interval
integer                                  :: auxinput5_interval_mo
integer                                  :: auxinput5_interval_d
integer                                  :: auxinput5_interval_h
integer                                  :: auxinput5_interval_m
integer                                  :: auxinput5_interval_s
integer                                  :: auxinput5_interval
integer                                  :: restart_interval_mo
integer                                  :: restart_interval_d
integer                                  :: restart_interval_h
integer                                  :: restart_interval_m
integer                                  :: restart_interval_s
integer                                  :: history_begin_y
integer                                  :: history_begin_mo
integer                                  :: history_begin_d
integer                                  :: history_begin_h
integer                                  :: history_begin_m
integer                                  :: history_begin_s
integer                                  :: inputout_begin_y
integer                                  :: inputout_begin_mo
integer                                  :: inputout_begin_d
integer                                  :: inputout_begin_h
integer                                  :: inputout_begin_m
integer                                  :: inputout_begin_s
integer                                  :: auxhist1_begin_y
integer                                  :: auxhist1_begin_mo
integer                                  :: auxhist1_begin_d
integer                                  :: auxhist1_begin_h
integer                                  :: auxhist1_begin_m
integer                                  :: auxhist1_begin_s
integer                                  :: auxhist2_begin_y
integer                                  :: auxhist2_begin_mo
integer                                  :: auxhist2_begin_d
integer                                  :: auxhist2_begin_h
integer                                  :: auxhist2_begin_m
integer                                  :: auxhist2_begin_s
integer                                  :: auxhist3_begin_y
integer                                  :: auxhist3_begin_mo
integer                                  :: auxhist3_begin_d
integer                                  :: auxhist3_begin_h
integer                                  :: auxhist3_begin_m
integer                                  :: auxhist3_begin_s
integer                                  :: auxhist4_begin_y
integer                                  :: auxhist4_begin_mo
integer                                  :: auxhist4_begin_d
integer                                  :: auxhist4_begin_h
integer                                  :: auxhist4_begin_m
integer                                  :: auxhist4_begin_s
integer                                  :: auxhist5_begin_y
integer                                  :: auxhist5_begin_mo
integer                                  :: auxhist5_begin_d
integer                                  :: auxhist5_begin_h
integer                                  :: auxhist5_begin_m
integer                                  :: auxhist5_begin_s
integer                                  :: auxinput1_begin_y
integer                                  :: auxinput1_begin_mo
integer                                  :: auxinput1_begin_d
integer                                  :: auxinput1_begin_h
integer                                  :: auxinput1_begin_m
integer                                  :: auxinput1_begin_s
integer                                  :: auxinput2_begin_y
integer                                  :: auxinput2_begin_mo
integer                                  :: auxinput2_begin_d
integer                                  :: auxinput2_begin_h
integer                                  :: auxinput2_begin_m
integer                                  :: auxinput2_begin_s
integer                                  :: auxinput3_begin_y
integer                                  :: auxinput3_begin_mo
integer                                  :: auxinput3_begin_d
integer                                  :: auxinput3_begin_h
integer                                  :: auxinput3_begin_m
integer                                  :: auxinput3_begin_s
integer                                  :: auxinput4_begin_y
integer                                  :: auxinput4_begin_mo
integer                                  :: auxinput4_begin_d
integer                                  :: auxinput4_begin_h
integer                                  :: auxinput4_begin_m
integer                                  :: auxinput4_begin_s
integer                                  :: auxinput5_begin_y
integer                                  :: auxinput5_begin_mo
integer                                  :: auxinput5_begin_d
integer                                  :: auxinput5_begin_h
integer                                  :: auxinput5_begin_m
integer                                  :: auxinput5_begin_s
integer                                  :: restart_begin_y
integer                                  :: restart_begin_mo
integer                                  :: restart_begin_d
integer                                  :: restart_begin_h
integer                                  :: restart_begin_m
integer                                  :: restart_begin_s
integer                                  :: history_end_y
integer                                  :: history_end_mo
integer                                  :: history_end_d
integer                                  :: history_end_h
integer                                  :: history_end_m
integer                                  :: history_end_s
integer                                  :: inputout_end_y
integer                                  :: inputout_end_mo
integer                                  :: inputout_end_d
integer                                  :: inputout_end_h
integer                                  :: inputout_end_m
integer                                  :: inputout_end_s
integer                                  :: auxhist1_end_y
integer                                  :: auxhist1_end_mo
integer                                  :: auxhist1_end_d
integer                                  :: auxhist1_end_h
integer                                  :: auxhist1_end_m
integer                                  :: auxhist1_end_s
integer                                  :: auxhist2_end_y
integer                                  :: auxhist2_end_mo
integer                                  :: auxhist2_end_d
integer                                  :: auxhist2_end_h
integer                                  :: auxhist2_end_m
integer                                  :: auxhist2_end_s
integer                                  :: auxhist3_end_y
integer                                  :: auxhist3_end_mo
integer                                  :: auxhist3_end_d
integer                                  :: auxhist3_end_h
integer                                  :: auxhist3_end_m
integer                                  :: auxhist3_end_s
integer                                  :: auxhist4_end_y
integer                                  :: auxhist4_end_mo
integer                                  :: auxhist4_end_d
integer                                  :: auxhist4_end_h
integer                                  :: auxhist4_end_m
integer                                  :: auxhist4_end_s
integer                                  :: auxhist5_end_y
integer                                  :: auxhist5_end_mo
integer                                  :: auxhist5_end_d
integer                                  :: auxhist5_end_h
integer                                  :: auxhist5_end_m
integer                                  :: auxhist5_end_s
integer                                  :: auxinput1_end_y
integer                                  :: auxinput1_end_mo
integer                                  :: auxinput1_end_d
integer                                  :: auxinput1_end_h
integer                                  :: auxinput1_end_m
integer                                  :: auxinput1_end_s
integer                                  :: auxinput2_end_y
integer                                  :: auxinput2_end_mo
integer                                  :: auxinput2_end_d
integer                                  :: auxinput2_end_h
integer                                  :: auxinput2_end_m
integer                                  :: auxinput2_end_s
integer                                  :: auxinput3_end_y
integer                                  :: auxinput3_end_mo
integer                                  :: auxinput3_end_d
integer                                  :: auxinput3_end_h
integer                                  :: auxinput3_end_m
integer                                  :: auxinput3_end_s
integer                                  :: auxinput4_end_y
integer                                  :: auxinput4_end_mo
integer                                  :: auxinput4_end_d
integer                                  :: auxinput4_end_h
integer                                  :: auxinput4_end_m
integer                                  :: auxinput4_end_s
integer                                  :: auxinput5_end_y
integer                                  :: auxinput5_end_mo
integer                                  :: auxinput5_end_d
integer                                  :: auxinput5_end_h
integer                                  :: auxinput5_end_m
integer                                  :: auxinput5_end_s
integer                                  :: io_form_auxinput1
integer                                  :: io_form_auxinput2
integer                                  :: io_form_auxinput3
integer                                  :: io_form_auxinput4
integer                                  :: io_form_auxinput5
integer                                  :: io_form_auxhist1
integer                                  :: io_form_auxhist2
integer                                  :: io_form_auxhist3
integer                                  :: io_form_auxhist4
integer                                  :: io_form_auxhist5
integer                                  :: julyr
integer                                  :: julday
real                                     :: gmt
character*256                               :: input_inname
character*256                               :: input_outname
character*256                               :: bdy_inname
character*256                               :: bdy_outname
character*256                               :: rst_inname
character*256                               :: rst_outname
logical                                  :: write_input
logical                                  :: write_restart_at_0h
integer                                  :: time_step
integer                                  :: time_step_fract_num
integer                                  :: time_step_fract_den
integer                                  :: max_dom
integer                                  :: s_we
integer                                  :: e_we
integer                                  :: s_sn
integer                                  :: e_sn
integer                                  :: s_vert
integer                                  :: e_vert
real                                     :: dx
real                                     :: dy
integer                                  :: grid_id
integer                                  :: parent_id
integer                                  :: level
integer                                  :: i_parent_start
integer                                  :: j_parent_start
integer                                  :: parent_grid_ratio
integer                                  :: parent_time_step_ratio
integer                                  :: feedback
integer                                  :: smooth_option
real                                     :: ztop
integer                                  :: moad_grid_ratio
integer                                  :: moad_time_step_ratio
integer                                  :: shw
integer                                  :: tile_sz_x
integer                                  :: tile_sz_y
integer                                  :: numtiles
integer                                  :: nproc_x
integer                                  :: nproc_y
integer                                  :: irand
real                                     :: dt
integer                                  :: mp_physics
integer                                  :: ra_lw_physics
integer                                  :: ra_sw_physics
real                                     :: radt
integer                                  :: sf_sfclay_physics
integer                                  :: sf_surface_physics
integer                                  :: bl_pbl_physics
real                                     :: bldt
integer                                  :: cu_physics
real                                     :: cudt
real                                     :: gsmdt
integer                                  :: isfflx
integer                                  :: ifsnow
integer                                  :: icloud
integer                                  :: surface_input_source
integer                                  :: num_soil_layers
integer                                  :: maxiens
integer                                  :: maxens
integer                                  :: maxens2
integer                                  :: maxens3
integer                                  :: ensdim
integer                                  :: chem_opt
integer                                  :: num_land_cat
integer                                  :: num_soil_cat
integer                                  :: dyn_opt
integer                                  :: rk_ord
integer                                  :: w_damping
integer                                  :: diff_opt
integer                                  :: km_opt
integer                                  :: damp_opt
real                                     :: zdamp
real                                     :: dampcoef
real                                     :: khdif
real                                     :: kvdif
real                                     :: smdiv
real                                     :: emdiv
real                                     :: epssm
logical                                  :: non_hydrostatic
integer                                  :: time_step_sound
integer                                  :: h_mom_adv_order
integer                                  :: v_mom_adv_order
integer                                  :: h_sca_adv_order
integer                                  :: v_sca_adv_order
logical                                  :: top_radiation
real                                     :: mix_cr_len
real                                     :: tke_upper_bound
real                                     :: kh_tke_upper_bound
real                                     :: kv_tke_upper_bound
real                                     :: tke_drag_coefficient
real                                     :: tke_heat_flux
logical                                  :: pert_coriolis
integer                                  :: spec_bdy_width
integer                                  :: spec_zone
integer                                  :: relax_zone
logical                                  :: specified
logical                                  :: periodic_x
logical                                  :: symmetric_xs
logical                                  :: symmetric_xe
logical                                  :: open_xs
logical                                  :: open_xe
logical                                  :: periodic_y
logical                                  :: symmetric_ys
logical                                  :: symmetric_ye
logical                                  :: open_ys
logical                                  :: open_ye
logical                                  :: nested
integer                                  :: real_data_init_type
real                                     :: cen_lat
real                                     :: cen_lon
real                                     :: truelat1
real                                     :: truelat2
real                                     :: moad_cen_lat
real                                     :: stand_lon
real                                     :: bdyfrq
integer                                  :: iswater
integer                                  :: isice
integer                                  :: isurban
integer                                  :: isoilwater
integer                                  :: map_proj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: lu_index
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: u_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: u_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ru
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: v_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: v_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ww
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: phb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: phb_fine
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: php
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_init
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tp_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tp_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mub
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mub_fine
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mudf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: p
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: al
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: alt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: alb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: zx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: zy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rdz
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rdzw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pb
real      ,DIMENSION(grid%sm32:grid%em32)           :: fnm
real      ,DIMENSION(grid%sm32:grid%em32)           :: fnp
real      ,DIMENSION(grid%sm32:grid%em32)           :: rdnw
real      ,DIMENSION(grid%sm32:grid%em32)           :: rdn
real      ,DIMENSION(grid%sm32:grid%em32)           :: dnw
real      ,DIMENSION(grid%sm32:grid%em32)           :: dn
real      ,DIMENSION(grid%sm32:grid%em32)           :: znu
real      ,DIMENSION(grid%sm32:grid%em32)           :: znw
real      ,DIMENSION(grid%sm32:grid%em32)           :: t_base
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: q2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: t2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: th2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psfc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: u10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: v10
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: imask
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_2
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: u_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: u_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: v_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: v_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: w_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: w_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: ph_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: ph_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: t_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: t_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: mu_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: mu_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqv_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqv_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqc_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqc_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqr_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqr_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqi_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqi_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqs_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqs_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqg_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqg_bt
real      ,DIMENSION(grid%spec_bdy_width)           :: fcx
real      ,DIMENSION(grid%spec_bdy_width)           :: gcx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: landmask
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: topostdv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: toposlpx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: toposlpy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: shdmax
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: shdmin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: snoalb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: slopecat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: toposoil
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_land_cat,grid%sm33:grid%em33)           :: landusef
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_cat,grid%sm33:grid%em33)           :: soilctop
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_cat,grid%sm33:grid%em33)           :: soilcbot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilcat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: vegcat
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: tslb
real      ,DIMENSION(grid%num_soil_layers)           :: zs
real      ,DIMENSION(grid%num_soil_layers)           :: dzs
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: smois
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: sh2o
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: xice
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: smstav
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: smstot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sfcrunoff
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: udrunoff
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ivgtyp
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: isltyp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: vegfra
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sfcevp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: grdflx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sfcexc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: acsnow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: acsnom
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: snow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: snowh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: canwat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sst
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: smfr3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: keepfr3dflag
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: potevp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: snopcx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soiltb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_myj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: thz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: z0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: uz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: vz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qsfc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: akhs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: akms
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: kpbl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: htop
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hbot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cuppt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: totswdn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: totlwdn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rswtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rlwtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: czmean
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cfracl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cfracm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cfrach
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: acfrst
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ncfrst
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: acfrcv
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ncfrcv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: f_ice_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: f_rain_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: f_rimef_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: h_diabatic
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: msft
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: msfu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: msfv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: f
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: e
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sina
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cosa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ht
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ht_fine
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ht_int
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ht_input
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tsk
real      ,DIMENSION(grid%sm32:grid%em32)           :: u_base
real      ,DIMENSION(grid%sm32:grid%em32)           :: v_base
real      ,DIMENSION(grid%sm32:grid%em32)           :: qv_base
real      ,DIMENSION(grid%sm32:grid%em32)           :: z_base
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rthcuten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqvcuten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqrcuten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqccuten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqscuten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqicuten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w0avg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rainc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rainnc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: raincv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rainncv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rainbl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: nca
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: lowlyr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mass_flux
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_gr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_mc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_st
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_as
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_capma
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_capme
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_capmi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%ensdim)           :: xf_ens
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%ensdim)           :: pr_ens
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rthften
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqvften
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rthraten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rthratenlw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rthratensw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cldfra
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: swdown
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: gsw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: glw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: xlat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: xlong
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: albedo
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: albbck
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: emiss
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cldefi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rublten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rvblten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rthblten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqvblten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqcblten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqiblten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tmn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: xland
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: znt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ust
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: pblh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: capg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: thc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hfx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qfx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: lh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: flhc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: flqc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qsg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qvg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qcg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tsnav
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: snowc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mavail
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tkesfcf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: taucldi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: taucldc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: defor11
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: defor22
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: defor12
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: defor33
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: defor13
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: defor23
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: xkmv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: xkmh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: xkmhd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: xkhv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: xkhh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: div
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: bn2
!ENDOFREGISTRYGENERATEDINCLUDE

      INTEGER nlev
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,k
      TYPE (grid_config_rec_type)            :: config_flags
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_scalar_derefs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! BEGIN SCALAR DEREFS
 cfn = grid%cfn
 cfn1 = grid%cfn1
 epsts = grid%epsts
 step_number = grid%step_number
 rdx = grid%rdx
 rdy = grid%rdy
 dts = grid%dts
 dtseps = grid%dtseps
 resm = grid%resm
 zetatop = grid%zetatop
 cf1 = grid%cf1
 cf2 = grid%cf2
 cf3 = grid%cf3
 number_at_same_level = grid%number_at_same_level
 itimestep = grid%itimestep
 oid = grid%oid
 auxhist1_oid = grid%auxhist1_oid
 auxhist2_oid = grid%auxhist2_oid
 auxhist3_oid = grid%auxhist3_oid
 auxhist4_oid = grid%auxhist4_oid
 auxhist5_oid = grid%auxhist5_oid
 auxinput1_oid = grid%auxinput1_oid
 auxinput2_oid = grid%auxinput2_oid
 auxinput3_oid = grid%auxinput3_oid
 auxinput4_oid = grid%auxinput4_oid
 auxinput5_oid = grid%auxinput5_oid
 nframes = grid%nframes
 lbc_fid = grid%lbc_fid
 tiled = grid%tiled
 patched = grid%patched
 write_metadata = grid%write_metadata
 dtbc = grid%dtbc
 ifndsnowh = grid%ifndsnowh
 ifndsoilw = grid%ifndsoilw
 u_frame = grid%u_frame
 v_frame = grid%v_frame
 p_top = grid%p_top
 stepcu = grid%stepcu
 stepra = grid%stepra
 stepbl = grid%stepbl
 warm_rain = grid%warm_rain
 run_days = grid%run_days
 run_hours = grid%run_hours
 run_minutes = grid%run_minutes
 run_seconds = grid%run_seconds
 start_year = grid%start_year
 start_month = grid%start_month
 start_day = grid%start_day
 start_hour = grid%start_hour
 start_minute = grid%start_minute
 start_second = grid%start_second
 end_year = grid%end_year
 end_month = grid%end_month
 end_day = grid%end_day
 end_hour = grid%end_hour
 end_minute = grid%end_minute
 end_second = grid%end_second
 interval_seconds = grid%interval_seconds
 input_from_file = grid%input_from_file
 history_interval = grid%history_interval
 frames_per_outfile = grid%frames_per_outfile
 restart = grid%restart
 restart_interval = grid%restart_interval
 io_form_input = grid%io_form_input
 io_form_history = grid%io_form_history
 io_form_restart = grid%io_form_restart
 io_form_boundary = grid%io_form_boundary
 debug_level = grid%debug_level
 history_outname = grid%history_outname
 auxhist1_outname = grid%auxhist1_outname
 auxhist2_outname = grid%auxhist2_outname
 auxhist3_outname = grid%auxhist3_outname
 auxhist4_outname = grid%auxhist4_outname
 auxhist5_outname = grid%auxhist5_outname
 history_inname = grid%history_inname
 auxhist1_inname = grid%auxhist1_inname
 auxhist2_inname = grid%auxhist2_inname
 auxhist3_inname = grid%auxhist3_inname
 auxhist4_inname = grid%auxhist4_inname
 auxhist5_inname = grid%auxhist5_inname
 history_interval_mo = grid%history_interval_mo
 history_interval_d = grid%history_interval_d
 history_interval_h = grid%history_interval_h
 history_interval_m = grid%history_interval_m
 history_interval_s = grid%history_interval_s
 inputout_interval_mo = grid%inputout_interval_mo
 inputout_interval_d = grid%inputout_interval_d
 inputout_interval_h = grid%inputout_interval_h
 inputout_interval_m = grid%inputout_interval_m
 inputout_interval_s = grid%inputout_interval_s
 inputout_interval = grid%inputout_interval
 auxhist1_interval_mo = grid%auxhist1_interval_mo
 auxhist1_interval_d = grid%auxhist1_interval_d
 auxhist1_interval_h = grid%auxhist1_interval_h
 auxhist1_interval_m = grid%auxhist1_interval_m
 auxhist1_interval_s = grid%auxhist1_interval_s
 auxhist1_interval = grid%auxhist1_interval
 auxhist2_interval_mo = grid%auxhist2_interval_mo
 auxhist2_interval_d = grid%auxhist2_interval_d
 auxhist2_interval_h = grid%auxhist2_interval_h
 auxhist2_interval_m = grid%auxhist2_interval_m
 auxhist2_interval_s = grid%auxhist2_interval_s
 auxhist2_interval = grid%auxhist2_interval
 auxhist3_interval_mo = grid%auxhist3_interval_mo
 auxhist3_interval_d = grid%auxhist3_interval_d
 auxhist3_interval_h = grid%auxhist3_interval_h
 auxhist3_interval_m = grid%auxhist3_interval_m
 auxhist3_interval_s = grid%auxhist3_interval_s
 auxhist3_interval = grid%auxhist3_interval
 auxhist4_interval_mo = grid%auxhist4_interval_mo
 auxhist4_interval_d = grid%auxhist4_interval_d
 auxhist4_interval_h = grid%auxhist4_interval_h
 auxhist4_interval_m = grid%auxhist4_interval_m
 auxhist4_interval_s = grid%auxhist4_interval_s
 auxhist4_interval = grid%auxhist4_interval
 auxhist5_interval_mo = grid%auxhist5_interval_mo
 auxhist5_interval_d = grid%auxhist5_interval_d
 auxhist5_interval_h = grid%auxhist5_interval_h
 auxhist5_interval_m = grid%auxhist5_interval_m
 auxhist5_interval_s = grid%auxhist5_interval_s
 auxhist5_interval = grid%auxhist5_interval
 auxinput1_interval_mo = grid%auxinput1_interval_mo
 auxinput1_interval_d = grid%auxinput1_interval_d
 auxinput1_interval_h = grid%auxinput1_interval_h
 auxinput1_interval_m = grid%auxinput1_interval_m
 auxinput1_interval_s = grid%auxinput1_interval_s
 auxinput1_interval = grid%auxinput1_interval
 auxinput2_interval_mo = grid%auxinput2_interval_mo
 auxinput2_interval_d = grid%auxinput2_interval_d
 auxinput2_interval_h = grid%auxinput2_interval_h
 auxinput2_interval_m = grid%auxinput2_interval_m
 auxinput2_interval_s = grid%auxinput2_interval_s
 auxinput2_interval = grid%auxinput2_interval
 auxinput3_interval_mo = grid%auxinput3_interval_mo
 auxinput3_interval_d = grid%auxinput3_interval_d
 auxinput3_interval_h = grid%auxinput3_interval_h
 auxinput3_interval_m = grid%auxinput3_interval_m
 auxinput3_interval_s = grid%auxinput3_interval_s
 auxinput3_interval = grid%auxinput3_interval
 auxinput4_interval_mo = grid%auxinput4_interval_mo
 auxinput4_interval_d = grid%auxinput4_interval_d
 auxinput4_interval_h = grid%auxinput4_interval_h
 auxinput4_interval_m = grid%auxinput4_interval_m
 auxinput4_interval_s = grid%auxinput4_interval_s
 auxinput4_interval = grid%auxinput4_interval
 auxinput5_interval_mo = grid%auxinput5_interval_mo
 auxinput5_interval_d = grid%auxinput5_interval_d
 auxinput5_interval_h = grid%auxinput5_interval_h
 auxinput5_interval_m = grid%auxinput5_interval_m
 auxinput5_interval_s = grid%auxinput5_interval_s
 auxinput5_interval = grid%auxinput5_interval
 restart_interval_mo = grid%restart_interval_mo
 restart_interval_d = grid%restart_interval_d
 restart_interval_h = grid%restart_interval_h
 restart_interval_m = grid%restart_interval_m
 restart_interval_s = grid%restart_interval_s
 history_begin_y = grid%history_begin_y
 history_begin_mo = grid%history_begin_mo
 history_begin_d = grid%history_begin_d
 history_begin_h = grid%history_begin_h
 history_begin_m = grid%history_begin_m
 history_begin_s = grid%history_begin_s
 inputout_begin_y = grid%inputout_begin_y
 inputout_begin_mo = grid%inputout_begin_mo
 inputout_begin_d = grid%inputout_begin_d
 inputout_begin_h = grid%inputout_begin_h
 inputout_begin_m = grid%inputout_begin_m
 inputout_begin_s = grid%inputout_begin_s
 auxhist1_begin_y = grid%auxhist1_begin_y
 auxhist1_begin_mo = grid%auxhist1_begin_mo
 auxhist1_begin_d = grid%auxhist1_begin_d
 auxhist1_begin_h = grid%auxhist1_begin_h
 auxhist1_begin_m = grid%auxhist1_begin_m
 auxhist1_begin_s = grid%auxhist1_begin_s
 auxhist2_begin_y = grid%auxhist2_begin_y
 auxhist2_begin_mo = grid%auxhist2_begin_mo
 auxhist2_begin_d = grid%auxhist2_begin_d
 auxhist2_begin_h = grid%auxhist2_begin_h
 auxhist2_begin_m = grid%auxhist2_begin_m
 auxhist2_begin_s = grid%auxhist2_begin_s
 auxhist3_begin_y = grid%auxhist3_begin_y
 auxhist3_begin_mo = grid%auxhist3_begin_mo
 auxhist3_begin_d = grid%auxhist3_begin_d
 auxhist3_begin_h = grid%auxhist3_begin_h
 auxhist3_begin_m = grid%auxhist3_begin_m
 auxhist3_begin_s = grid%auxhist3_begin_s
 auxhist4_begin_y = grid%auxhist4_begin_y
 auxhist4_begin_mo = grid%auxhist4_begin_mo
 auxhist4_begin_d = grid%auxhist4_begin_d
 auxhist4_begin_h = grid%auxhist4_begin_h
 auxhist4_begin_m = grid%auxhist4_begin_m
 auxhist4_begin_s = grid%auxhist4_begin_s
 auxhist5_begin_y = grid%auxhist5_begin_y
 auxhist5_begin_mo = grid%auxhist5_begin_mo
 auxhist5_begin_d = grid%auxhist5_begin_d
 auxhist5_begin_h = grid%auxhist5_begin_h
 auxhist5_begin_m = grid%auxhist5_begin_m
 auxhist5_begin_s = grid%auxhist5_begin_s
 auxinput1_begin_y = grid%auxinput1_begin_y
 auxinput1_begin_mo = grid%auxinput1_begin_mo
 auxinput1_begin_d = grid%auxinput1_begin_d
 auxinput1_begin_h = grid%auxinput1_begin_h
 auxinput1_begin_m = grid%auxinput1_begin_m
 auxinput1_begin_s = grid%auxinput1_begin_s
 auxinput2_begin_y = grid%auxinput2_begin_y
 auxinput2_begin_mo = grid%auxinput2_begin_mo
 auxinput2_begin_d = grid%auxinput2_begin_d
 auxinput2_begin_h = grid%auxinput2_begin_h
 auxinput2_begin_m = grid%auxinput2_begin_m
 auxinput2_begin_s = grid%auxinput2_begin_s
 auxinput3_begin_y = grid%auxinput3_begin_y
 auxinput3_begin_mo = grid%auxinput3_begin_mo
 auxinput3_begin_d = grid%auxinput3_begin_d
 auxinput3_begin_h = grid%auxinput3_begin_h
 auxinput3_begin_m = grid%auxinput3_begin_m
 auxinput3_begin_s = grid%auxinput3_begin_s
 auxinput4_begin_y = grid%auxinput4_begin_y
 auxinput4_begin_mo = grid%auxinput4_begin_mo
 auxinput4_begin_d = grid%auxinput4_begin_d
 auxinput4_begin_h = grid%auxinput4_begin_h
 auxinput4_begin_m = grid%auxinput4_begin_m
 auxinput4_begin_s = grid%auxinput4_begin_s
 auxinput5_begin_y = grid%auxinput5_begin_y
 auxinput5_begin_mo = grid%auxinput5_begin_mo
 auxinput5_begin_d = grid%auxinput5_begin_d
 auxinput5_begin_h = grid%auxinput5_begin_h
 auxinput5_begin_m = grid%auxinput5_begin_m
 auxinput5_begin_s = grid%auxinput5_begin_s
 restart_begin_y = grid%restart_begin_y
 restart_begin_mo = grid%restart_begin_mo
 restart_begin_d = grid%restart_begin_d
 restart_begin_h = grid%restart_begin_h
 restart_begin_m = grid%restart_begin_m
 restart_begin_s = grid%restart_begin_s
 history_end_y = grid%history_end_y
 history_end_mo = grid%history_end_mo
 history_end_d = grid%history_end_d
 history_end_h = grid%history_end_h
 history_end_m = grid%history_end_m
 history_end_s = grid%history_end_s
 inputout_end_y = grid%inputout_end_y
 inputout_end_mo = grid%inputout_end_mo
 inputout_end_d = grid%inputout_end_d
 inputout_end_h = grid%inputout_end_h
 inputout_end_m = grid%inputout_end_m
 inputout_end_s = grid%inputout_end_s
 auxhist1_end_y = grid%auxhist1_end_y
 auxhist1_end_mo = grid%auxhist1_end_mo
 auxhist1_end_d = grid%auxhist1_end_d
 auxhist1_end_h = grid%auxhist1_end_h
 auxhist1_end_m = grid%auxhist1_end_m
 auxhist1_end_s = grid%auxhist1_end_s
 auxhist2_end_y = grid%auxhist2_end_y
 auxhist2_end_mo = grid%auxhist2_end_mo
 auxhist2_end_d = grid%auxhist2_end_d
 auxhist2_end_h = grid%auxhist2_end_h
 auxhist2_end_m = grid%auxhist2_end_m
 auxhist2_end_s = grid%auxhist2_end_s
 auxhist3_end_y = grid%auxhist3_end_y
 auxhist3_end_mo = grid%auxhist3_end_mo
 auxhist3_end_d = grid%auxhist3_end_d
 auxhist3_end_h = grid%auxhist3_end_h
 auxhist3_end_m = grid%auxhist3_end_m
 auxhist3_end_s = grid%auxhist3_end_s
 auxhist4_end_y = grid%auxhist4_end_y
 auxhist4_end_mo = grid%auxhist4_end_mo
 auxhist4_end_d = grid%auxhist4_end_d
 auxhist4_end_h = grid%auxhist4_end_h
 auxhist4_end_m = grid%auxhist4_end_m
 auxhist4_end_s = grid%auxhist4_end_s
 auxhist5_end_y = grid%auxhist5_end_y
 auxhist5_end_mo = grid%auxhist5_end_mo
 auxhist5_end_d = grid%auxhist5_end_d
 auxhist5_end_h = grid%auxhist5_end_h
 auxhist5_end_m = grid%auxhist5_end_m
 auxhist5_end_s = grid%auxhist5_end_s
 auxinput1_end_y = grid%auxinput1_end_y
 auxinput1_end_mo = grid%auxinput1_end_mo
 auxinput1_end_d = grid%auxinput1_end_d
 auxinput1_end_h = grid%auxinput1_end_h
 auxinput1_end_m = grid%auxinput1_end_m
 auxinput1_end_s = grid%auxinput1_end_s
 auxinput2_end_y = grid%auxinput2_end_y
 auxinput2_end_mo = grid%auxinput2_end_mo
 auxinput2_end_d = grid%auxinput2_end_d
 auxinput2_end_h = grid%auxinput2_end_h
 auxinput2_end_m = grid%auxinput2_end_m
 auxinput2_end_s = grid%auxinput2_end_s
 auxinput3_end_y = grid%auxinput3_end_y
 auxinput3_end_mo = grid%auxinput3_end_mo
 auxinput3_end_d = grid%auxinput3_end_d
 auxinput3_end_h = grid%auxinput3_end_h
 auxinput3_end_m = grid%auxinput3_end_m
 auxinput3_end_s = grid%auxinput3_end_s
 auxinput4_end_y = grid%auxinput4_end_y
 auxinput4_end_mo = grid%auxinput4_end_mo
 auxinput4_end_d = grid%auxinput4_end_d
 auxinput4_end_h = grid%auxinput4_end_h
 auxinput4_end_m = grid%auxinput4_end_m
 auxinput4_end_s = grid%auxinput4_end_s
 auxinput5_end_y = grid%auxinput5_end_y
 auxinput5_end_mo = grid%auxinput5_end_mo
 auxinput5_end_d = grid%auxinput5_end_d
 auxinput5_end_h = grid%auxinput5_end_h
 auxinput5_end_m = grid%auxinput5_end_m
 auxinput5_end_s = grid%auxinput5_end_s
 io_form_auxinput1 = grid%io_form_auxinput1
 io_form_auxinput2 = grid%io_form_auxinput2
 io_form_auxinput3 = grid%io_form_auxinput3
 io_form_auxinput4 = grid%io_form_auxinput4
 io_form_auxinput5 = grid%io_form_auxinput5
 io_form_auxhist1 = grid%io_form_auxhist1
 io_form_auxhist2 = grid%io_form_auxhist2
 io_form_auxhist3 = grid%io_form_auxhist3
 io_form_auxhist4 = grid%io_form_auxhist4
 io_form_auxhist5 = grid%io_form_auxhist5
 julyr = grid%julyr
 julday = grid%julday
 gmt = grid%gmt
 input_inname = grid%input_inname
 input_outname = grid%input_outname
 bdy_inname = grid%bdy_inname
 bdy_outname = grid%bdy_outname
 rst_inname = grid%rst_inname
 rst_outname = grid%rst_outname
 write_input = grid%write_input
 write_restart_at_0h = grid%write_restart_at_0h
 time_step = grid%time_step
 time_step_fract_num = grid%time_step_fract_num
 time_step_fract_den = grid%time_step_fract_den
 max_dom = grid%max_dom
 s_we = grid%s_we
 e_we = grid%e_we
 s_sn = grid%s_sn
 e_sn = grid%e_sn
 s_vert = grid%s_vert
 e_vert = grid%e_vert
 dx = grid%dx
 dy = grid%dy
 grid_id = grid%grid_id
 parent_id = grid%parent_id
 level = grid%level
 i_parent_start = grid%i_parent_start
 j_parent_start = grid%j_parent_start
 parent_grid_ratio = grid%parent_grid_ratio
 parent_time_step_ratio = grid%parent_time_step_ratio
 feedback = grid%feedback
 smooth_option = grid%smooth_option
 ztop = grid%ztop
 moad_grid_ratio = grid%moad_grid_ratio
 moad_time_step_ratio = grid%moad_time_step_ratio
 shw = grid%shw
 tile_sz_x = grid%tile_sz_x
 tile_sz_y = grid%tile_sz_y
 numtiles = grid%numtiles
 nproc_x = grid%nproc_x
 nproc_y = grid%nproc_y
 irand = grid%irand
 dt = grid%dt
 mp_physics = grid%mp_physics
 ra_lw_physics = grid%ra_lw_physics
 ra_sw_physics = grid%ra_sw_physics
 radt = grid%radt
 sf_sfclay_physics = grid%sf_sfclay_physics
 sf_surface_physics = grid%sf_surface_physics
 bl_pbl_physics = grid%bl_pbl_physics
 bldt = grid%bldt
 cu_physics = grid%cu_physics
 cudt = grid%cudt
 gsmdt = grid%gsmdt
 isfflx = grid%isfflx
 ifsnow = grid%ifsnow
 icloud = grid%icloud
 surface_input_source = grid%surface_input_source
 num_soil_layers = grid%num_soil_layers
 maxiens = grid%maxiens
 maxens = grid%maxens
 maxens2 = grid%maxens2
 maxens3 = grid%maxens3
 ensdim = grid%ensdim
 chem_opt = grid%chem_opt
 num_land_cat = grid%num_land_cat
 num_soil_cat = grid%num_soil_cat
 dyn_opt = grid%dyn_opt
 rk_ord = grid%rk_ord
 w_damping = grid%w_damping
 diff_opt = grid%diff_opt
 km_opt = grid%km_opt
 damp_opt = grid%damp_opt
 zdamp = grid%zdamp
 dampcoef = grid%dampcoef
 khdif = grid%khdif
 kvdif = grid%kvdif
 smdiv = grid%smdiv
 emdiv = grid%emdiv
 epssm = grid%epssm
 non_hydrostatic = grid%non_hydrostatic
 time_step_sound = grid%time_step_sound
 h_mom_adv_order = grid%h_mom_adv_order
 v_mom_adv_order = grid%v_mom_adv_order
 h_sca_adv_order = grid%h_sca_adv_order
 v_sca_adv_order = grid%v_sca_adv_order
 top_radiation = grid%top_radiation
 mix_cr_len = grid%mix_cr_len
 tke_upper_bound = grid%tke_upper_bound
 kh_tke_upper_bound = grid%kh_tke_upper_bound
 kv_tke_upper_bound = grid%kv_tke_upper_bound
 tke_drag_coefficient = grid%tke_drag_coefficient
 tke_heat_flux = grid%tke_heat_flux
 pert_coriolis = grid%pert_coriolis
 spec_bdy_width = grid%spec_bdy_width
 spec_zone = grid%spec_zone
 relax_zone = grid%relax_zone
 specified = grid%specified
 periodic_x = grid%periodic_x
 symmetric_xs = grid%symmetric_xs
 symmetric_xe = grid%symmetric_xe
 open_xs = grid%open_xs
 open_xe = grid%open_xe
 periodic_y = grid%periodic_y
 symmetric_ys = grid%symmetric_ys
 symmetric_ye = grid%symmetric_ye
 open_ys = grid%open_ys
 open_ye = grid%open_ye
 nested = grid%nested
 real_data_init_type = grid%real_data_init_type
 cen_lat = grid%cen_lat
 cen_lon = grid%cen_lon
 truelat1 = grid%truelat1
 truelat2 = grid%truelat2
 moad_cen_lat = grid%moad_cen_lat
 stand_lon = grid%stand_lon
 bdyfrq = grid%bdyfrq
 iswater = grid%iswater
 isice = grid%isice
 isurban = grid%isurban
 isoilwater = grid%isoilwater
 map_proj = grid%map_proj
! END SCALAR DEREFS
!ENDOFREGISTRYGENERATEDINCLUDE

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

      ! code here to interpolate the data into the nested domain
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_nest_interpdown_interp.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL interp_fcnm (                                                               &         
                  lu_index,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%lu_index,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  u_2,                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%em_u_2,                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .TRUE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  v_2,                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%em_v_2,                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .TRUE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  w_2,                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%em_w_2,                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  ph_2,                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%em_ph_2,                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  phb,                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%em_phb,                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  t_2,                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%em_t_2,                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  mu_2,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%em_mu_2,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  mub,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%em_mub,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  mu0,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%em_mu0,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  alb,                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%em_alb,                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  pb,                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%em_pb,                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
IF ( P_qv .GE. PARAM_FIRST_SCALAR ) THEN
CALL interp_fcn (                                                               &         
                  moist_2(grid%sm31,grid%sm32,grid%sm33,P_qv),                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%moist_2(ngrid%sm31,ngrid%sm32,ngrid%sm33,P_qv),                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( P_qc .GE. PARAM_FIRST_SCALAR ) THEN
CALL interp_fcn (                                                               &         
                  moist_2(grid%sm31,grid%sm32,grid%sm33,P_qc),                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%moist_2(ngrid%sm31,ngrid%sm32,ngrid%sm33,P_qc),                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( P_qr .GE. PARAM_FIRST_SCALAR ) THEN
CALL interp_fcn (                                                               &         
                  moist_2(grid%sm31,grid%sm32,grid%sm33,P_qr),                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%moist_2(ngrid%sm31,ngrid%sm32,ngrid%sm33,P_qr),                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( P_qi .GE. PARAM_FIRST_SCALAR ) THEN
CALL interp_fcn (                                                               &         
                  moist_2(grid%sm31,grid%sm32,grid%sm33,P_qi),                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%moist_2(ngrid%sm31,ngrid%sm32,ngrid%sm33,P_qi),                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( P_qs .GE. PARAM_FIRST_SCALAR ) THEN
CALL interp_fcn (                                                               &         
                  moist_2(grid%sm31,grid%sm32,grid%sm33,P_qs),                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%moist_2(ngrid%sm31,ngrid%sm32,ngrid%sm33,P_qs),                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( P_qg .GE. PARAM_FIRST_SCALAR ) THEN
CALL interp_fcn (                                                               &         
                  moist_2(grid%sm31,grid%sm32,grid%sm33,P_qg),                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%moist_2(ngrid%sm31,ngrid%sm32,ngrid%sm33,P_qg),                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
CALL interp_fcnm (                                                               &         
                  landmask,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%landmask,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  tslb,                                                           &         ! CD field
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%tslb,                                                        &   ! ND field
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  smois,                                                           &         ! CD field
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%smois,                                                        &   ! ND field
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  sh2o,                                                           &         ! CD field
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%sh2o,                                                        &   ! ND field
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_water_field (                                                               &         
                  xice,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xice,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  smstav,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%smstav,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  sfcrunoff,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%sfcrunoff,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  udrunoff,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%udrunoff,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_fcni (                                                               &         
                  ivgtyp,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ivgtyp,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcni (                                                               &         
                  isltyp,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%isltyp,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  vegfra,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%vegfra,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  acsnow,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%acsnow,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  acsnom,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%acsnom,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  snow,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%snow,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  snowh,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%snowh,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  canwat,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%canwat,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_water_field (                                                               &         
                  sst,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%sst,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_fcn (                                                               &         
                  f_ice_phy,                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%f_ice_phy,                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  f_rain_phy,                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%f_rain_phy,                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  f_rimef_phy,                                                           &         ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, ckpe, cjps, cjpe,   &         ! CD dims
                  ngrid%f_rimef_phy,                                                        &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, nkpe, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  msft,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%msft,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  msfu,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%msfu,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .TRUE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  msfv,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%msfv,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .TRUE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  f,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%f,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  e,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%e,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  sina,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%sina,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  cosa,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%cosa,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  ht,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ht,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  tsk,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tsk,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  xlat,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xlat,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_fcn (                                                               &         
                  xlong,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xlong,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  tmn,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tmn,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  xland,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xland,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
CALL interp_mask_land_field (                                                               &         
                  snowc,                                                           &         ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%snowc,                                                        &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  shw, ngrid%imask,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,lu_index,ngrid%lu_index  &
                  ) 
!ENDOFREGISTRYGENERATEDINCLUDE

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_scalar_derefs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! BEGIN SCALAR DEREFS
 grid%cfn    = cfn
 grid%cfn1    = cfn1
 grid%epsts    = epsts
 grid%step_number    = step_number
 grid%rdx    = rdx
 grid%rdy    = rdy
 grid%dts    = dts
 grid%dtseps    = dtseps
 grid%resm    = resm
 grid%zetatop    = zetatop
 grid%cf1    = cf1
 grid%cf2    = cf2
 grid%cf3    = cf3
 grid%number_at_same_level    = number_at_same_level
 grid%itimestep    = itimestep
 grid%oid    = oid
 grid%auxhist1_oid    = auxhist1_oid
 grid%auxhist2_oid    = auxhist2_oid
 grid%auxhist3_oid    = auxhist3_oid
 grid%auxhist4_oid    = auxhist4_oid
 grid%auxhist5_oid    = auxhist5_oid
 grid%auxinput1_oid    = auxinput1_oid
 grid%auxinput2_oid    = auxinput2_oid
 grid%auxinput3_oid    = auxinput3_oid
 grid%auxinput4_oid    = auxinput4_oid
 grid%auxinput5_oid    = auxinput5_oid
 grid%nframes    = nframes
 grid%lbc_fid    = lbc_fid
 grid%tiled    = tiled
 grid%patched    = patched
 grid%write_metadata    = write_metadata
 grid%dtbc    = dtbc
 grid%ifndsnowh    = ifndsnowh
 grid%ifndsoilw    = ifndsoilw
 grid%u_frame    = u_frame
 grid%v_frame    = v_frame
 grid%p_top    = p_top
 grid%stepcu    = stepcu
 grid%stepra    = stepra
 grid%stepbl    = stepbl
 grid%warm_rain    = warm_rain
 grid%run_days    = run_days
 grid%run_hours    = run_hours
 grid%run_minutes    = run_minutes
 grid%run_seconds    = run_seconds
 grid%start_year    = start_year
 grid%start_month    = start_month
 grid%start_day    = start_day
 grid%start_hour    = start_hour
 grid%start_minute    = start_minute
 grid%start_second    = start_second
 grid%end_year    = end_year
 grid%end_month    = end_month
 grid%end_day    = end_day
 grid%end_hour    = end_hour
 grid%end_minute    = end_minute
 grid%end_second    = end_second
 grid%interval_seconds    = interval_seconds
 grid%input_from_file    = input_from_file
 grid%history_interval    = history_interval
 grid%frames_per_outfile    = frames_per_outfile
 grid%restart    = restart
 grid%restart_interval    = restart_interval
 grid%io_form_input    = io_form_input
 grid%io_form_history    = io_form_history
 grid%io_form_restart    = io_form_restart
 grid%io_form_boundary    = io_form_boundary
 grid%debug_level    = debug_level
 grid%history_outname    = history_outname
 grid%auxhist1_outname    = auxhist1_outname
 grid%auxhist2_outname    = auxhist2_outname
 grid%auxhist3_outname    = auxhist3_outname
 grid%auxhist4_outname    = auxhist4_outname
 grid%auxhist5_outname    = auxhist5_outname
 grid%history_inname    = history_inname
 grid%auxhist1_inname    = auxhist1_inname
 grid%auxhist2_inname    = auxhist2_inname
 grid%auxhist3_inname    = auxhist3_inname
 grid%auxhist4_inname    = auxhist4_inname
 grid%auxhist5_inname    = auxhist5_inname
 grid%history_interval_mo    = history_interval_mo
 grid%history_interval_d    = history_interval_d
 grid%history_interval_h    = history_interval_h
 grid%history_interval_m    = history_interval_m
 grid%history_interval_s    = history_interval_s
 grid%inputout_interval_mo    = inputout_interval_mo
 grid%inputout_interval_d    = inputout_interval_d
 grid%inputout_interval_h    = inputout_interval_h
 grid%inputout_interval_m    = inputout_interval_m
 grid%inputout_interval_s    = inputout_interval_s
 grid%inputout_interval    = inputout_interval
 grid%auxhist1_interval_mo    = auxhist1_interval_mo
 grid%auxhist1_interval_d    = auxhist1_interval_d
 grid%auxhist1_interval_h    = auxhist1_interval_h
 grid%auxhist1_interval_m    = auxhist1_interval_m
 grid%auxhist1_interval_s    = auxhist1_interval_s
 grid%auxhist1_interval    = auxhist1_interval
 grid%auxhist2_interval_mo    = auxhist2_interval_mo
 grid%auxhist2_interval_d    = auxhist2_interval_d
 grid%auxhist2_interval_h    = auxhist2_interval_h
 grid%auxhist2_interval_m    = auxhist2_interval_m
 grid%auxhist2_interval_s    = auxhist2_interval_s
 grid%auxhist2_interval    = auxhist2_interval
 grid%auxhist3_interval_mo    = auxhist3_interval_mo
 grid%auxhist3_interval_d    = auxhist3_interval_d
 grid%auxhist3_interval_h    = auxhist3_interval_h
 grid%auxhist3_interval_m    = auxhist3_interval_m
 grid%auxhist3_interval_s    = auxhist3_interval_s
 grid%auxhist3_interval    = auxhist3_interval
 grid%auxhist4_interval_mo    = auxhist4_interval_mo
 grid%auxhist4_interval_d    = auxhist4_interval_d
 grid%auxhist4_interval_h    = auxhist4_interval_h
 grid%auxhist4_interval_m    = auxhist4_interval_m
 grid%auxhist4_interval_s    = auxhist4_interval_s
 grid%auxhist4_interval    = auxhist4_interval
 grid%auxhist5_interval_mo    = auxhist5_interval_mo
 grid%auxhist5_interval_d    = auxhist5_interval_d
 grid%auxhist5_interval_h    = auxhist5_interval_h
 grid%auxhist5_interval_m    = auxhist5_interval_m
 grid%auxhist5_interval_s    = auxhist5_interval_s
 grid%auxhist5_interval    = auxhist5_interval
 grid%auxinput1_interval_mo    = auxinput1_interval_mo
 grid%auxinput1_interval_d    = auxinput1_interval_d
 grid%auxinput1_interval_h    = auxinput1_interval_h
 grid%auxinput1_interval_m    = auxinput1_interval_m
 grid%auxinput1_interval_s    = auxinput1_interval_s
 grid%auxinput1_interval    = auxinput1_interval
 grid%auxinput2_interval_mo    = auxinput2_interval_mo
 grid%auxinput2_interval_d    = auxinput2_interval_d
 grid%auxinput2_interval_h    = auxinput2_interval_h
 grid%auxinput2_interval_m    = auxinput2_interval_m
 grid%auxinput2_interval_s    = auxinput2_interval_s
 grid%auxinput2_interval    = auxinput2_interval
 grid%auxinput3_interval_mo    = auxinput3_interval_mo
 grid%auxinput3_interval_d    = auxinput3_interval_d
 grid%auxinput3_interval_h    = auxinput3_interval_h
 grid%auxinput3_interval_m    = auxinput3_interval_m
 grid%auxinput3_interval_s    = auxinput3_interval_s
 grid%auxinput3_interval    = auxinput3_interval
 grid%auxinput4_interval_mo    = auxinput4_interval_mo
 grid%auxinput4_interval_d    = auxinput4_interval_d
 grid%auxinput4_interval_h    = auxinput4_interval_h
 grid%auxinput4_interval_m    = auxinput4_interval_m
 grid%auxinput4_interval_s    = auxinput4_interval_s
 grid%auxinput4_interval    = auxinput4_interval
 grid%auxinput5_interval_mo    = auxinput5_interval_mo
 grid%auxinput5_interval_d    = auxinput5_interval_d
 grid%auxinput5_interval_h    = auxinput5_interval_h
 grid%auxinput5_interval_m    = auxinput5_interval_m
 grid%auxinput5_interval_s    = auxinput5_interval_s
 grid%auxinput5_interval    = auxinput5_interval
 grid%restart_interval_mo    = restart_interval_mo
 grid%restart_interval_d    = restart_interval_d
 grid%restart_interval_h    = restart_interval_h
 grid%restart_interval_m    = restart_interval_m
 grid%restart_interval_s    = restart_interval_s
 grid%history_begin_y    = history_begin_y
 grid%history_begin_mo    = history_begin_mo
 grid%history_begin_d    = history_begin_d
 grid%history_begin_h    = history_begin_h
 grid%history_begin_m    = history_begin_m
 grid%history_begin_s    = history_begin_s
 grid%inputout_begin_y    = inputout_begin_y
 grid%inputout_begin_mo    = inputout_begin_mo
 grid%inputout_begin_d    = inputout_begin_d
 grid%inputout_begin_h    = inputout_begin_h
 grid%inputout_begin_m    = inputout_begin_m
 grid%inputout_begin_s    = inputout_begin_s
 grid%auxhist1_begin_y    = auxhist1_begin_y
 grid%auxhist1_begin_mo    = auxhist1_begin_mo
 grid%auxhist1_begin_d    = auxhist1_begin_d
 grid%auxhist1_begin_h    = auxhist1_begin_h
 grid%auxhist1_begin_m    = auxhist1_begin_m
 grid%auxhist1_begin_s    = auxhist1_begin_s
 grid%auxhist2_begin_y    = auxhist2_begin_y
 grid%auxhist2_begin_mo    = auxhist2_begin_mo
 grid%auxhist2_begin_d    = auxhist2_begin_d
 grid%auxhist2_begin_h    = auxhist2_begin_h
 grid%auxhist2_begin_m    = auxhist2_begin_m
 grid%auxhist2_begin_s    = auxhist2_begin_s
 grid%auxhist3_begin_y    = auxhist3_begin_y
 grid%auxhist3_begin_mo    = auxhist3_begin_mo
 grid%auxhist3_begin_d    = auxhist3_begin_d
 grid%auxhist3_begin_h    = auxhist3_begin_h
 grid%auxhist3_begin_m    = auxhist3_begin_m
 grid%auxhist3_begin_s    = auxhist3_begin_s
 grid%auxhist4_begin_y    = auxhist4_begin_y
 grid%auxhist4_begin_mo    = auxhist4_begin_mo
 grid%auxhist4_begin_d    = auxhist4_begin_d
 grid%auxhist4_begin_h    = auxhist4_begin_h
 grid%auxhist4_begin_m    = auxhist4_begin_m
 grid%auxhist4_begin_s    = auxhist4_begin_s
 grid%auxhist5_begin_y    = auxhist5_begin_y
 grid%auxhist5_begin_mo    = auxhist5_begin_mo
 grid%auxhist5_begin_d    = auxhist5_begin_d
 grid%auxhist5_begin_h    = auxhist5_begin_h
 grid%auxhist5_begin_m    = auxhist5_begin_m
 grid%auxhist5_begin_s    = auxhist5_begin_s
 grid%auxinput1_begin_y    = auxinput1_begin_y
 grid%auxinput1_begin_mo    = auxinput1_begin_mo
 grid%auxinput1_begin_d    = auxinput1_begin_d
 grid%auxinput1_begin_h    = auxinput1_begin_h
 grid%auxinput1_begin_m    = auxinput1_begin_m
 grid%auxinput1_begin_s    = auxinput1_begin_s
 grid%auxinput2_begin_y    = auxinput2_begin_y
 grid%auxinput2_begin_mo    = auxinput2_begin_mo
 grid%auxinput2_begin_d    = auxinput2_begin_d
 grid%auxinput2_begin_h    = auxinput2_begin_h
 grid%auxinput2_begin_m    = auxinput2_begin_m
 grid%auxinput2_begin_s    = auxinput2_begin_s
 grid%auxinput3_begin_y    = auxinput3_begin_y
 grid%auxinput3_begin_mo    = auxinput3_begin_mo
 grid%auxinput3_begin_d    = auxinput3_begin_d
 grid%auxinput3_begin_h    = auxinput3_begin_h
 grid%auxinput3_begin_m    = auxinput3_begin_m
 grid%auxinput3_begin_s    = auxinput3_begin_s
 grid%auxinput4_begin_y    = auxinput4_begin_y
 grid%auxinput4_begin_mo    = auxinput4_begin_mo
 grid%auxinput4_begin_d    = auxinput4_begin_d
 grid%auxinput4_begin_h    = auxinput4_begin_h
 grid%auxinput4_begin_m    = auxinput4_begin_m
 grid%auxinput4_begin_s    = auxinput4_begin_s
 grid%auxinput5_begin_y    = auxinput5_begin_y
 grid%auxinput5_begin_mo    = auxinput5_begin_mo
 grid%auxinput5_begin_d    = auxinput5_begin_d
 grid%auxinput5_begin_h    = auxinput5_begin_h
 grid%auxinput5_begin_m    = auxinput5_begin_m
 grid%auxinput5_begin_s    = auxinput5_begin_s
 grid%restart_begin_y    = restart_begin_y
 grid%restart_begin_mo    = restart_begin_mo
 grid%restart_begin_d    = restart_begin_d
 grid%restart_begin_h    = restart_begin_h
 grid%restart_begin_m    = restart_begin_m
 grid%restart_begin_s    = restart_begin_s
 grid%history_end_y    = history_end_y
 grid%history_end_mo    = history_end_mo
 grid%history_end_d    = history_end_d
 grid%history_end_h    = history_end_h
 grid%history_end_m    = history_end_m
 grid%history_end_s    = history_end_s
 grid%inputout_end_y    = inputout_end_y
 grid%inputout_end_mo    = inputout_end_mo
 grid%inputout_end_d    = inputout_end_d
 grid%inputout_end_h    = inputout_end_h
 grid%inputout_end_m    = inputout_end_m
 grid%inputout_end_s    = inputout_end_s
 grid%auxhist1_end_y    = auxhist1_end_y
 grid%auxhist1_end_mo    = auxhist1_end_mo
 grid%auxhist1_end_d    = auxhist1_end_d
 grid%auxhist1_end_h    = auxhist1_end_h
 grid%auxhist1_end_m    = auxhist1_end_m
 grid%auxhist1_end_s    = auxhist1_end_s
 grid%auxhist2_end_y    = auxhist2_end_y
 grid%auxhist2_end_mo    = auxhist2_end_mo
 grid%auxhist2_end_d    = auxhist2_end_d
 grid%auxhist2_end_h    = auxhist2_end_h
 grid%auxhist2_end_m    = auxhist2_end_m
 grid%auxhist2_end_s    = auxhist2_end_s
 grid%auxhist3_end_y    = auxhist3_end_y
 grid%auxhist3_end_mo    = auxhist3_end_mo
 grid%auxhist3_end_d    = auxhist3_end_d
 grid%auxhist3_end_h    = auxhist3_end_h
 grid%auxhist3_end_m    = auxhist3_end_m
 grid%auxhist3_end_s    = auxhist3_end_s
 grid%auxhist4_end_y    = auxhist4_end_y
 grid%auxhist4_end_mo    = auxhist4_end_mo
 grid%auxhist4_end_d    = auxhist4_end_d
 grid%auxhist4_end_h    = auxhist4_end_h
 grid%auxhist4_end_m    = auxhist4_end_m
 grid%auxhist4_end_s    = auxhist4_end_s
 grid%auxhist5_end_y    = auxhist5_end_y
 grid%auxhist5_end_mo    = auxhist5_end_mo
 grid%auxhist5_end_d    = auxhist5_end_d
 grid%auxhist5_end_h    = auxhist5_end_h
 grid%auxhist5_end_m    = auxhist5_end_m
 grid%auxhist5_end_s    = auxhist5_end_s
 grid%auxinput1_end_y    = auxinput1_end_y
 grid%auxinput1_end_mo    = auxinput1_end_mo
 grid%auxinput1_end_d    = auxinput1_end_d
 grid%auxinput1_end_h    = auxinput1_end_h
 grid%auxinput1_end_m    = auxinput1_end_m
 grid%auxinput1_end_s    = auxinput1_end_s
 grid%auxinput2_end_y    = auxinput2_end_y
 grid%auxinput2_end_mo    = auxinput2_end_mo
 grid%auxinput2_end_d    = auxinput2_end_d
 grid%auxinput2_end_h    = auxinput2_end_h
 grid%auxinput2_end_m    = auxinput2_end_m
 grid%auxinput2_end_s    = auxinput2_end_s
 grid%auxinput3_end_y    = auxinput3_end_y
 grid%auxinput3_end_mo    = auxinput3_end_mo
 grid%auxinput3_end_d    = auxinput3_end_d
 grid%auxinput3_end_h    = auxinput3_end_h
 grid%auxinput3_end_m    = auxinput3_end_m
 grid%auxinput3_end_s    = auxinput3_end_s
 grid%auxinput4_end_y    = auxinput4_end_y
 grid%auxinput4_end_mo    = auxinput4_end_mo
 grid%auxinput4_end_d    = auxinput4_end_d
 grid%auxinput4_end_h    = auxinput4_end_h
 grid%auxinput4_end_m    = auxinput4_end_m
 grid%auxinput4_end_s    = auxinput4_end_s
 grid%auxinput5_end_y    = auxinput5_end_y
 grid%auxinput5_end_mo    = auxinput5_end_mo
 grid%auxinput5_end_d    = auxinput5_end_d
 grid%auxinput5_end_h    = auxinput5_end_h
 grid%auxinput5_end_m    = auxinput5_end_m
 grid%auxinput5_end_s    = auxinput5_end_s
 grid%io_form_auxinput1    = io_form_auxinput1
 grid%io_form_auxinput2    = io_form_auxinput2
 grid%io_form_auxinput3    = io_form_auxinput3
 grid%io_form_auxinput4    = io_form_auxinput4
 grid%io_form_auxinput5    = io_form_auxinput5
 grid%io_form_auxhist1    = io_form_auxhist1
 grid%io_form_auxhist2    = io_form_auxhist2
 grid%io_form_auxhist3    = io_form_auxhist3
 grid%io_form_auxhist4    = io_form_auxhist4
 grid%io_form_auxhist5    = io_form_auxhist5
 grid%julyr    = julyr
 grid%julday    = julday
 grid%gmt    = gmt
 grid%input_inname    = input_inname
 grid%input_outname    = input_outname
 grid%bdy_inname    = bdy_inname
 grid%bdy_outname    = bdy_outname
 grid%rst_inname    = rst_inname
 grid%rst_outname    = rst_outname
 grid%write_input    = write_input
 grid%write_restart_at_0h    = write_restart_at_0h
 grid%time_step    = time_step
 grid%time_step_fract_num    = time_step_fract_num
 grid%time_step_fract_den    = time_step_fract_den
 grid%max_dom    = max_dom
 grid%s_we    = s_we
 grid%e_we    = e_we
 grid%s_sn    = s_sn
 grid%e_sn    = e_sn
 grid%s_vert    = s_vert
 grid%e_vert    = e_vert
 grid%dx    = dx
 grid%dy    = dy
 grid%grid_id    = grid_id
 grid%parent_id    = parent_id
 grid%level    = level
 grid%i_parent_start    = i_parent_start
 grid%j_parent_start    = j_parent_start
 grid%parent_grid_ratio    = parent_grid_ratio
 grid%parent_time_step_ratio    = parent_time_step_ratio
 grid%feedback    = feedback
 grid%smooth_option    = smooth_option
 grid%ztop    = ztop
 grid%moad_grid_ratio    = moad_grid_ratio
 grid%moad_time_step_ratio    = moad_time_step_ratio
 grid%shw    = shw
 grid%tile_sz_x    = tile_sz_x
 grid%tile_sz_y    = tile_sz_y
 grid%numtiles    = numtiles
 grid%nproc_x    = nproc_x
 grid%nproc_y    = nproc_y
 grid%irand    = irand
 grid%dt    = dt
 grid%mp_physics    = mp_physics
 grid%ra_lw_physics    = ra_lw_physics
 grid%ra_sw_physics    = ra_sw_physics
 grid%radt    = radt
 grid%sf_sfclay_physics    = sf_sfclay_physics
 grid%sf_surface_physics    = sf_surface_physics
 grid%bl_pbl_physics    = bl_pbl_physics
 grid%bldt    = bldt
 grid%cu_physics    = cu_physics
 grid%cudt    = cudt
 grid%gsmdt    = gsmdt
 grid%isfflx    = isfflx
 grid%ifsnow    = ifsnow
 grid%icloud    = icloud
 grid%surface_input_source    = surface_input_source
 grid%num_soil_layers    = num_soil_layers
 grid%maxiens    = maxiens
 grid%maxens    = maxens
 grid%maxens2    = maxens2
 grid%maxens3    = maxens3
 grid%ensdim    = ensdim
 grid%chem_opt    = chem_opt
 grid%num_land_cat    = num_land_cat
 grid%num_soil_cat    = num_soil_cat
 grid%dyn_opt    = dyn_opt
 grid%rk_ord    = rk_ord
 grid%w_damping    = w_damping
 grid%diff_opt    = diff_opt
 grid%km_opt    = km_opt
 grid%damp_opt    = damp_opt
 grid%zdamp    = zdamp
 grid%dampcoef    = dampcoef
 grid%khdif    = khdif
 grid%kvdif    = kvdif
 grid%smdiv    = smdiv
 grid%emdiv    = emdiv
 grid%epssm    = epssm
 grid%non_hydrostatic    = non_hydrostatic
 grid%time_step_sound    = time_step_sound
 grid%h_mom_adv_order    = h_mom_adv_order
 grid%v_mom_adv_order    = v_mom_adv_order
 grid%h_sca_adv_order    = h_sca_adv_order
 grid%v_sca_adv_order    = v_sca_adv_order
 grid%top_radiation    = top_radiation
 grid%mix_cr_len    = mix_cr_len
 grid%tke_upper_bound    = tke_upper_bound
 grid%kh_tke_upper_bound    = kh_tke_upper_bound
 grid%kv_tke_upper_bound    = kv_tke_upper_bound
 grid%tke_drag_coefficient    = tke_drag_coefficient
 grid%tke_heat_flux    = tke_heat_flux
 grid%pert_coriolis    = pert_coriolis
 grid%spec_bdy_width    = spec_bdy_width
 grid%spec_zone    = spec_zone
 grid%relax_zone    = relax_zone
 grid%specified    = specified
 grid%periodic_x    = periodic_x
 grid%symmetric_xs    = symmetric_xs
 grid%symmetric_xe    = symmetric_xe
 grid%open_xs    = open_xs
 grid%open_xe    = open_xe
 grid%periodic_y    = periodic_y
 grid%symmetric_ys    = symmetric_ys
 grid%symmetric_ye    = symmetric_ye
 grid%open_ys    = open_ys
 grid%open_ye    = open_ye
 grid%nested    = nested
 grid%real_data_init_type    = real_data_init_type
 grid%cen_lat    = cen_lat
 grid%cen_lon    = cen_lon
 grid%truelat1    = truelat1
 grid%truelat2    = truelat2
 grid%moad_cen_lat    = moad_cen_lat
 grid%stand_lon    = stand_lon
 grid%bdyfrq    = bdyfrq
 grid%iswater    = iswater
 grid%isice    = isice
 grid%isurban    = isurban
 grid%isoilwater    = isoilwater
 grid%map_proj    = map_proj
! END SCALAR DEREFS
!ENDOFREGISTRYGENERATEDINCLUDE
      RETURN

END SUBROUTINE interp_domain_em_part1

! Stub ... not used in serial code
SUBROUTINE interp_domain_em_part2
END SUBROUTINE interp_domain_em_part2



