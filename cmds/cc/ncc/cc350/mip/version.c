/* mip/version.c
 * Copyright (C) Acorn Computers Ltd., 1988.
 * Defines the version string and banner for the Codemist compiler.
 */

/*
 * RCS $Revision: 1.1 $
 * Checkin $Date: 1993/11/23 16:41:52 $
 * Revising $Author: nickc $
 */

#ifdef __STDC__
#  include <string.h>
#else
#  include <strings.h>
#endif
#include "globals.h"                    /* for TARGET_MACHINE */
#include "version.h"
#include "mcdep.h"                      /* for dbg_name */
#include "fevsn.h"
#include "mipvsn.h"
#include "mcvsn.h"


#ifdef TARGET_HAS_DEBUGGER
#  define DEBUGGER dbg_name
#else
#  define DEBUGGER ""                   /* change mcdep.h to tidy! */
#endif

#ifndef __DATE__
#define __DATE__ "<no-date>"
#endif

#ifndef __TIME__
#define __TIME__ "<no-time>"
#endif

#ifdef __STDC__
#define STATIC_BANNER 1         /* no string concatenation.             */
#endif

#ifdef NON_RELEASE_VSN          /* doesn't disable debugging options.   */
#  define RELEASE_VSN  NON_RELEASE_VSN
#endif

#ifndef RELEASE_VSN
#  ifdef TARGET_HAS_DEBUGGER
#     undef STATIC_BANNER       /* debugger name is link-time known.    */
#  endif
#endif

/* Note the comment in mip/version.h re the 4 nulls at end of string.   */

#ifdef STATIC_BANNER

#  ifndef RELEASE_VSN
static char cc_banner[] =  "Norcroft " \
                           TARGET_SYSTEM " " TARGET_MACHINE " " LANGUAGE " "\
                           FE_VERSION "/" MIP_VERSION "/" MC_VERSION " [" \
                           __DATE__ ", " __TIME__ "]\0\0\0";
#  else
static char cc_banner[] =  "Norcroft " \
                           TARGET_SYSTEM " " TARGET_MACHINE " " LANGUAGE \
                           " vsn " RELEASE_VSN " [" __DATE__ "]\0\0\0";
#  endif

char *version_banner(void)
{
  return cc_banner;
}

#else  /* STATIC_BANNER */

/*
 * Can't build the banner using ANSI string concatenation,
 * so build it dynamically instead.
 */

static char cc_banner[128] = "";        /* expression instead of 128?   */

char *version_banner(void)
{   if (cc_banner[0]=='\0')
      {
#  ifndef RELEASE_VSN
        sprintf(cc_banner, "Norcroft %s %s %s %s/%s/%s%s%s [%s, %s]\0\0\0",
                           TARGET_SYSTEM, TARGET_MACHINE, LANGUAGE,
                           FE_VERSION, MIP_VERSION, MC_VERSION,
                           (*DEBUGGER ? "/":""), DEBUGGER,
                           __DATE__, __TIME__);
#  else
        sprintf(cc_banner, "Norcroft %s %s %s vsn %s [%s]\0\0\0",
                           TARGET_SYSTEM, TARGET_MACHINE, LANGUAGE,
                           RELEASE_VSN, __DATE__);
#  endif
      }
    return(cc_banner);
}

#endif /* STATIC_BANNER */

/* end of mip/version.c */
