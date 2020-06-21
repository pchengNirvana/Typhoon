#!/bin/csh

# set directories
setenv ROOT_DIR "/Users/pchengnirvana/Modeling/for_MMY" # ROOT directory of this package
setenv src_dir "$ROOT_DIR/src"     # directory of source codes and executables
setenv dat_dir "$ROOT_DIR/data"    # directory of raw .dat files
setenv ctl_dir "$ROOT_DIR/data"    # directory of .ctl files, can be different from $dat_dir
setenv nc_dir  "$ROOT_DIR/nc"      # directory of converted .nc files
setenv out_dir "$ROOT_DIR/outputs" # directory of output files

# set typhoon number list
set typhoon_list = (54)

# set wrfout settings
setenv domain 3
setenv nx 450
setenv ny 450
setenv nz 1
setenv nr 170
setenv file_recl 4

# set input dat file suffix
setenv in_file_suffix "_uvp.dat"

# set output file suffix
setenv out_file_suffix "_size.txt"

# set executable file
setenv executable "typhoon.exe"

# set grep search pattern
set pattern = 'tdef'

# debug option, T or F
setenv debug 'T'

# check directories
if (! -e $ROOT_DIR) then
  echo "Error: Working directory $ROOT_DIR does not exist"
  exit
endif
if (! -e $dat_dir) then
  echo "Error: Data directory $dat_dir does not exist"
  exit
endif
if (! -e $out_dir) then
  echo "Creating output directory: $out_dir"
  mkdir -p $out_dir
endif

# loop over each typhoon in the id list
foreach typhoon ( $typhoon_list )
  echo "Working on typhoon #$typhoon"
  setenv typhoon_id $typhoon

  # now grep typhoon tdef value from .ctl file
  #setenv nt `grep $pattern $dat_dir/$typhoon_id/${typhoon_id}*.ctl |  awk '{print $2}'`
  setenv nt `grep $pattern $ctl_dir/$typhoon_id/${typhoon_id}*.ctl |  awk '{print $2}'`

  # run executables
  cd $src_dir
  ./$executable
  
end
