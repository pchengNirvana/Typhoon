#!/bin/csh

# set directories
setenv ROOT_DIR "/Users/pchengnirvana/Modeling/for_MMY"
setenv src_dir "$ROOT_DIR/src"
setenv data_dir "$ROOT_DIR/data"
setenv out_dir "$ROOT_DIR/outputs"

# set typhoon number list
set typhoon_list = (103)

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
if (! -e $data_dir) then
  echo "Error: Data directory $data_dir does not exist"
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
  setenv nt `grep $pattern $data_dir/$typhoon_id/${typhoon_id}*.ctl |  awk '{print $2}'`
  echo "nt value set to $nt"

  # run executables
  cd $src_dir
  ./$executable
  
end
