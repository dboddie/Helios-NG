#-----------------------------------------------------------------------------
# Helios generic make system - GENERIC COMPONENT MAKEFILE -
#-----------------------------------------------------------------------------
# Helios generic component make rules for: filesys/ufs/fsck
#
# File: /HSRC/filesys/ufs/fsck/makefile
#
# These are the host/processor independent rules and variables for this
# component of Helios. It is included by the processor specific makefile
# That may add to some of its functionality. You should only alter this
# makefile by using "ifneq ($(HPROC),YYY)" clauses for a specific processor.
#
#		(C) Copyright 1990 Perihelion Software
#
# WARNING: assumes you are using GNUmake.
#-----------------------------------------------------------------------------

ifndef InProcMake
# This part of the makefile is run in the components generic source directory

# The variables HPROC, HHOST, HSRC, and HPROD should already be set by 
# your make script or environment, if not then get local defaults from
# makeinc/DFLT.mak. Note that the following pathname cannot use $(HSRC).
ifndef HPROC
include ../../makeinc/DFLT.mak
endif

.PHONY: notarget
notarget:
	@$(MAKE) -C $(HPROC)

.DEFAULT:
	@$(MAKE) -C $(HPROC) $@

else
#-----------------------------------------------------------------------------
# This part of the makefile is always run in a processor specific subdirectory.

# Manditory variables for a component:
# name of this dir relative to $(HSRC)
COMPONENT = filesys/ufs/fsck

# Manditory includes
include $(HSRC)/makeinc/$(HHOST).mak	# Default host system variables
include $(HSRC)/makeinc/$(HPROC).mak	# Default processor rules and vars.

# Manditory phoney targets
.PHONY: default clean Xdefault Xclean
# "default"    - should make this component in its proc subdir
# "install"    - copy the new component to $(HPROD) production directory
# "installdef" - If component is a library, make .def file and install in LIB
# "clean"      - remove all intermediate and executable files from proc subdir
# Phoney targets prefixed by X are for adding processor specific targets.
#
# Processor specific sources are held in the processor specific subdir.,
# generic ones in the directory above it.
HVPATH := $(HSRC)/$(COMPONENT)

ifdef RSRC
HVPATH := $(RSRC)/$(COMPONENT)/$(HPROC):$(HSRC)/$(COMPONENT):$(RSRC)/$(COMPONENT)
endif

vpath %.h $(HVPATH)
vpath %.a $(HVPATH)
vpath %.c $(HVPATH)

#-----------------------------------------------------------------------------
# Generic component make rules and variables added below here

# Note all final generic targets here
TARGETS = fsck

default : $(TARGETS) Xdefault

install	: $(TARGETS) Xinstall
	$(CP) $(TARGETS) $(HPROD)/bin
	$(TOUCH) install

clean	: Xclean
	-$(RM)	$(OBJECTS) $(TARGETS) $(XOBJECTS) $(XTARGETS) mkout \
		core ../mkout install installdef Xinstall


#-----------------------------------------------------------------------------
# for archive, backup and cleaning purposes,
# note all generic SOURCE and OBJECT files:

# Note all sources
SOURCES		= $(SOURCES.c) $(SOURCES.a) \
		  $(HEADERS.c) $(HEADERS.m) \
		  $(SOURCES.odd)

#Full set of library object files
#OBJECTS		= $(SOURCES.c:.c=.p) $(SOURCES.a:.a=.p)
# Or if you are not a library:
OBJECTS	= $(SOURCES.c:.c=.o) $(SOURCES.a:.a=.o)

# C source files
SOURCES.c	= dir.c inode.c main.c pass1.c pass1b.c pass2.c pass3.c \
			pass4.c pass5.c preen.c setup.c utilities.c

# AMPP source files
SOURCES.a	=

# C header files
HEADERS.c	= fsck.h

# AMPP macro files
HEADERS.m	=

# Any other source files
SOURCES.odd	= makefile


#-----------------------------------------------------------------------------
# Other generic component variables:
UFS := $(HSRC)/filesys/ufs
INCLUDES := $(UFS)/include/sys/,$(UFS)/include/,$(UFS)/,$(INCLUDE)/,$(INCLUDE)/sys/,../

# Set any flags required
ifeq ($(HPROC),ARM)
  NCFLAGS := $(NCFLAGS) -I$(INCLUDES)
else
  NCFLAGS := $(NCFLAGS) $(DEFINES) -i$(INCLUDES) -j$(INCLUDES) -D_BSD -Dvax -DSTANDALONE
endif

# Note any library def files to be linked with
LIBDEFS	:=

LINKFLAGS := $(LINKFLAGS)

#-----------------------------------------------------------------------------
# Generic targets and their dependencies:

# uncomment if you have compiler or makedepend generated dependencies
#include depends

fsck: $(OBJECTS) $(UFS)/cmds/$(HPROC)/fstab.o $(UFS)/$(HPROC)/ufs_tables.o \
      $(UFS)/cmds/$(HPROC)/privfns.o $(UFS)/cmds/$(HPROC)/myworld.o
	$(LINK) $(LINKFLAGS) -o $@ $(LIB)/c0.o $^ \
	 -l$(LIB)/helios.lib -l$(LIB)/c.lib -l$(LIB)/bsd.lib -s12000 -h4000


#-----------------------------------------------------------------------------
endif #InProcMake	# This line must be at EOF

