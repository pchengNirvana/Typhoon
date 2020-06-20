
.SUFFIXES: .mod .o .f90 .exe

# Set compilers and paths
ROOT = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SRCDIR = $(PWD)/src

FC     = mpif90
LIBS    = -L$(NETCDF)/lib -lnetcdff
INCLUDE = -I$(NETCDF)/include

ifeq ($(FC),lf95)
  FFLAGS = --g
else
  FFLAGS = -g
endif

ifeq ($(FC),pgf90)
  FFLAGS += -Mnosave -Ktrap=fp -Kieee
endif

ifeq ($(FC),mpif90)
#  FFLAGS += -Mnosave -Ktrap=fp -Kieee
  MOSSCO_FFLAGS += -DNO_ISO_FORTRAN_ENV
endif

ifeq ($(FC),gfortran)
  FFLAGS += -ffree-line-length-none
endif

ifeq ($(FC),lf95)
  FFLAGS += --nap --nchk --pca --nsav --trace --trap --wide
endif

ifeq ($(FC),ifort)
  FFLAGS += -fpe0 -ftrapuv
endif

fDEF = \
 module_def_vars.f90

fOBJ = \
 module_messages.f90 \
 module_procedures.f90 \
 module_io_main.f90

fSRC = \
 typhoon.f90

extraOBJ = \
 a.o \
 b.o

oDEF = $(fDEF:.f90=.o)

oOBJ = $(fOBJ:.f90=.o)

oSRC = $(fSRC:.f90=.o)

fMOD = $(fDEF) $(fOBJ)

MODULES = $(fMOD:.f90=.mod)

EXEC = $(fSRC:.f90=.exe)

#
#  ------------------------------  Build:  ----------------------------------
#
default:
	@make build

build: 
	@make -C $(SRCDIR) -f $(PWD)/Makefile $(EXEC)

%.o : %.mod

.f90.o:
	$(FC) $(FFLAGS) $(INCLUDE) -c $<

$(oOBJ): $(oDEF)

$(EXEC): $(oDEF) $(oOBJ) $(oSRC)
	$(FC) -o $@ $(@:.exe=.o) $(extraOBJ) $(oDEF) $(oOBJ) $(LIBS) $(MOSSCO_FFLAGS)

#
#  ------------------------------  Clean:  ----------------------------------
#
SUBDIRS = $(SRCDIR)
SUBDIRSCLEAN=$(addsuffix clean,$(SUBDIRS))

clean: $(SUBDIRSCLEAN)

clean_subdirs:
	rm -f $(MODULES) $(EXEC) $(oDEF) $(oOBJ) $(oSRC)

%clean: %
	$(MAKE) -C $< -f $(PWD)/Makefile clean_subdirs

#
#  ---------------------------  Other Rules:  -------------------------------
#
rebuild: clean build


#
#  ---------------------------  Backup Rules:  ------------------------------
#
#$(EXEC): $(oMOD)
#	$(FC) -o $@ $(@:.exe=.o) $(oMOD) $(LIBS) $(MOSSCO_FFLAGS)
