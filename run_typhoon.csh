#!/bin/csh

# set directories and filename examples
### NOTE: The script assumes that we can find (will create) these files at the following location
###   .dat - $dat_dir/$dat_example
###   .ctl - $ctl_dir/$typhoon/$ctl_example
###   .nc  - $nc_dir/$nc_example
###   .txt - $out_dir/$out_example
### If all .ctl files are in one directory, adjust the patterns below (search for 'adjustment #1' in this run script)
setenv ROOT_DIR "/Users/pchengnirvana/Modeling/for_MMY" # ROOT directory of this package
setenv src_dir "$ROOT_DIR/src"     # directory of source codes and executables
setenv dat_dir "$ROOT_DIR/data"    # directory of raw .dat files
setenv ctl_dir "$ROOT_DIR/data"    # directory of .ctl files, can be different from $dat_dir
setenv nc_dir  "$ROOT_DIR/nc"      # directory of converted .nc files
setenv out_dir "$ROOT_DIR/outputs" # directory of output files
set dat_example = "1_d03_uvp.dat"  # set .dat   filename example
set ctl_example = "1_d03.ctl"      # set .ctl   filename example
set nc_example  = "1_d03.nc"       # set .nc    filename example
set out_example = "1_03_size.txt"  # set output filename example, can change to any suffix, but need domain number

# main dimension settings
# uncomment to overwrite dimension info in .ctl files
setenv file_recl 4
#setenv nx 450
#setenv ny 450
setenv nz 1
#setenv nt 31
setenv nr 170

# set typhoon number list
set typhoon_list = (54)

# main executable filename under $src_dir directory
setenv executable "typhoon.exe" 

# debug option, True or False; False by default
setenv debug 'TRUE'

# do we want to use .nc file; False by default
# under development
setenv use_netcdf 'FALSE'

# check directories
if (! -e $ROOT_DIR) then
  echo "Error: Working directory $ROOT_DIR does not exist"
  exit
endif
if (! -e $dat_dir) then
  echo "Error: Data directory $dat_dir does not exist"
  exit
endif
if (! -e $ctl_dir) then
  echo "Error: Data directory $ctl_dir does not exist"
  exit
endif
#if (! -e $nc_dir) then
#  echo "Creating output directory: $nc_dir"
#  mkdir -p $nc_dir
#endif
if (! -e $out_dir) then
  echo "Creating output directory: $out_dir"
  mkdir -p $out_dir
endif

# set domain number
setenv domain `echo $dat_example | cut -d\. -f1 | cut -d\_ -f2`
set domain_ctl = `echo $ctl_example | cut -d\. -f1 | cut -d\_ -f2`
set domain_nc  = `echo $nc_example  | cut -d\. -f1 | cut -d\_ -f2`
set domain_out = `echo $out_example | cut -d\. -f1 | cut -d\_ -f2`
if ( $domain != $domain_ctl || $domain != $domain_nc ) then
  echo "ERROR: Domain number set in filename examples do not match"
  exit
endif
set example_typhoon = `echo $dat_example | cut -d\_ -f1`

# loop over each typhoon in the id list
foreach typhoon ( $typhoon_list )
  echo "Working on typhoon #$typhoon"

  # now set input and output filenames 
  setenv dat_file `echo $dat_example | sed "s/$example_typhoon/$typhoon/"`
  setenv ctl_file `echo $ctl_example | sed "s/$example_typhoon/$typhoon/"`
  setenv nc_file  `echo $nc_example  | sed "s/$example_typhoon/$typhoon/"`
  setenv out_file `echo $out_example | sed "s/$example_typhoon/$typhoon/"`

  # now grep typhoon dimensions from .ctl file if not set in environment
  ### adjust directory if nessary (adjustment #1)
  if ( ! $?nx ) then
    setenv nx `grep xdef $ctl_dir/$typhoon/$ctl_file | awk '{print $2}'`
  else
    if ( "$nx" == "") then
      setenv nx `grep xdef $ctl_dir/$typhoon/$ctl_file | awk '{print $2}'`
    endif
  endif
  if ( ! $?ny ) then
    setenv ny `grep ydef $ctl_dir/$typhoon/$ctl_file | awk '{print $2}'`
  else
    if ( "$ny" == "") then
      setenv ny `grep ydef $ctl_dir/$typhoon/$ctl_file | awk '{print $2}'`
    endif
  endif
  # zmax will be retrieved no matter if it is asked for
  setenv zmax `grep zdef $ctl_dir/$typhoon/$ctl_file | awk '{print $2}'`
  if ( ! $?nz ) then
    setenv nz $zmax
  else
    if ( "$nz" == "") then
      setenv nz $zmax
    endif
  endif
  # tmax will be retrieved no matter if it is asked for
  setenv tmax `grep tdef $ctl_dir/$typhoon/$ctl_file | awk '{print $2}'`
  if ( ! $?nt ) then
    setenv nt $tmax
  else
    if ( "$nt" == "") then
      setenv nt $tmax
    endif
  endif
  # grep undefined value
  setenv nan_val `grep undef $ctl_dir/$typhoon/$ctl_file | awk '{print $2}'`

  # run executables
  cd $src_dir
  ./$executable

end
