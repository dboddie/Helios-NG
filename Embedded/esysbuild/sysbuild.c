/*{{{  Header */

/*
 * Helios system builder
 *
 * Takes as input a set of image files generated by the linker and
 * combines them into a single image suitable for booting into a processor.
 * The file consists of an initial size word (in little-endian order), followed
 * by a sequence of self-relative pointers to the start of each image, followed
 * by the images themselves.
 *
 * Author: NHG   Changes Feb 88: TJK
 *
 * RCS Id: $Id: sysbuild.c,v 1.1 1994/05/16 10:00:04 nickc Exp $
 */

/*}}}*/
/*{{{  Includes */

#include <stdio.h>
#include <module.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/*}}}*/
/*{{{  Types */

#ifndef WORD
typedef long 		WORD;
typedef char *		STRING;
typedef unsigned char	UBYTE;
#endif

/*}}}*/
/*{{{  Constants */

#if defined(IBMPC) || defined(__HELIOS)
# define MEMSIZE 500000L	/* 500k max nucleus size */
#else
# define MEMSIZE 1000000L	/* 1Mb max nucleus */
#endif

/*}}}*/
/*{{{  Variables */

UBYTE *		 image;
UBYTE *		 iptr;
WORD		 isize;
WORD		 ptabsize;
FILE *		 infd;
FILE *		 outfd;

/*}}}*/

/*{{{  Code */

#if !defined(LINUX)
/*{{{  swap() */

WORD
swap(WORD x )
{
#ifdef BYTE_SEX_ODD
  WORD r = 0;

  r |= ((x >> 24) & 0xff) <<  0;
  r |= ((x >> 16) & 0xff) <<  8;
  r |= ((x >>  8) & 0xff) << 16;
  r |= ((x >>  0) & 0xff) << 24;

  return r;
#else
 return x;
#endif
}
#endif

/*}}}*/
/*{{{  main() */

int
sysbuild(
	 int 		argc,
	 STRING *	argv )
{
  WORD 		p;
  WORD *	iwd;
  WORD		sz;

  
  argc -= 2;
  argv++;
  infd  = outfd = NULL;
  image = NULL;
  
  ptabsize = argc;
  
  image = (UBYTE *)malloc( (int) MEMSIZE );
  
  if (image == 0)
    sysbuild_error( "Cannot allocate memory" );
  
  printf( "Helios System Builder V2.0 16/5/94\n" );
  printf( "(C) 1994 Perihelion Distributed Software" );
  printf( " All Rights Reserved.\n" );
  
  iptr = image + (8 + ptabsize*4);
  iwd  = (WORD *)image;
  isize = iptr - image;
  
  p = 0;
  
  while (p++ != argc)
    {
      WORD 	hdr[3];
      WORD	fsize;
      int	s;
      char *	nptr = argv[p];

      
      infd = fopen(nptr,"rb");

      if (infd == NULL) 
	sysbuild_error( "Cannot open %s for input", nptr );
      
      if ((s = fread( hdr, 1, 12, infd )) != 12)
	sysbuild_error( "Cannot read image file header: %s %ld", nptr, s );
      
      /* Extract final part of name */
      nptr = nptr + strlen(nptr);
      
      while (*nptr != '/' && *nptr != '\\' && nptr >= argv[p])
	nptr--;
      
      nptr++;
      
      fsize = swap(hdr[2]);
      
      if (((isize + fsize + 3) & ~3) > MEMSIZE)
	sysbuild_error("Image size too large\n");
      
      if ((sz = fread(iptr,1,(int)fsize,infd)) != fsize)
	sysbuild_error( "Bad header in file %s %ld %ld", argv[ p ], sz, fsize );

      printf( "%20s: offset %#8lx size %#8lx\n", nptr, isize, fsize );
      
      /* align to next word boundary */
      fsize = (fsize+3) & (~3);
      
      isize += fsize;
      
      if (isize > MEMSIZE )
	sysbuild_error("Run out of memory\n");
      
	{
	  WORD ip = iptr - image - 4*p;
	  iptr += fsize;

	  iwd[p] = swap(ip);
	}
      
      fclose(infd); infd = 0;
    }
  
  iwd[0] = swap(isize);
  iwd[p] = 0L;
  
  outfd = fopen(argv[0],"wb");

  if (outfd == NULL) 
    sysbuild_error("Cannot open %s for output",argv[0]);
  
  if (fwrite( image, 1, (int)isize, outfd ) != isize)
    printf("Write failed\n");
  
  fclose( outfd );
  outfd = NULL;
  
  printf( "System size = %#lx (%ld)\n", isize, isize );
  
  tidyup();

  return 0;
}

/*}}}*/

/*}}}*/
