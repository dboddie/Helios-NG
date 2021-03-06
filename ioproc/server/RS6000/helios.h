/* @(#)77	1.45  com/inc/stdio.h, bos, bos320 9/19/91 14:08:01 */
/*
 * COMPONENT_NAME: (INCSTD) Standard Include Files
 *
 * FUNCTIONS: 
 *
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1990
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */
#ifndef _H_STDIO
#define _H_STDIO
#include <standards.h>

/*
 *
 *      The ANSI and POSIX standards require that certain values be in stdio.h.
 *      It also requires that if _ANSI_C_SOURCE or _POSIX_SOURCE is defined
 *	then ONLY those values are present. This header includes all the ANSI 
 *	and POSIX required entries.
 *      In addition other entries for the AIX system are included.
 *
 */

#ifdef _ANSI_C_SOURCE

/*
 * 	The following definitions are included in <sys/types.h>.  They
 *	are also included here to comply with ANSI standards.
 */

#ifndef NULL
#define NULL	((void *)0)
#endif

#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned long	size_t;
#endif

#ifndef _FPOS_T
#define	_FPOS_T
typedef long	fpos_t;
#endif

/*
 *	The definition of TMP_MAX is included in <sys/limits.h>.  It is
 *	also defined here to comply with ANSI standards.
 */

#ifndef TMP_MAX
#define	TMP_MAX		16384
#endif


#define FOPEN_MAX 	2000
#define FILENAME_MAX 	255
#define BUFSIZ		4096
#define _P_tmpdir       "/tmp/"
#define L_tmpnam	(sizeof(_P_tmpdir) + 15)

/*
 * _IOLBF means that a file's output will be buffered line by line
 * In addition to being flags, _IONBF, _IOLBF and _IOFBF are possible
 * values for "type" in setvbuf.
 */
#define _IOFBF		0000
#define _IOLBF		0100
#define _IONBF		0004


#ifndef EOF
#define EOF		(-1)
#endif

#ifndef WEOF
#define WEOF		(-1)
#endif

#define SEEK_SET	0
#define SEEK_CUR	1
#define SEEK_END	2

typedef struct {
	unsigned char	*_ptr;
	int	_cnt;
	unsigned char	*_base;
	unsigned char   *_bufendp;
	short	_flag;
	short	_file;
	int	__stdioid;
	char	*__newbase;
	long	_unused[1];
} FILE;

#define _IOEOF		0020
#define _IOERR		0040
#define _NIOBRW		20
extern FILE	_iob[_NIOBRW];

#define stdin		(&_iob[0])
#define stdout		(&_iob[1])
#define stderr		(&_iob[2])

#ifdef	_NONSTD_TYPES
extern int 	fread();
extern int	fwrite();
#elif	defined	_NO_PROTO
extern size_t 	fread();
extern size_t	fwrite();
#else	/* _NONSTD_TYPES, _NO_PROTO */
extern size_t 	fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
extern size_t	fwrite(const void *ptr, size_t size, size_t nmemb,FILE *stream);
#endif	/* _NONSTD_TYPES, _NO_PROTO */

#ifdef _NO_PROTO
extern int	filbuf();
extern int	flsbuf();
extern int	ferror();
extern int	feof();
extern void	clearerr();
extern int	putchar();
extern int	getchar();
extern int	putc();
extern int	getc();
extern int	remove();
extern int	rename();
extern FILE 	*tmpfile();
extern char 	*tmpnam();
extern int 	fclose();
extern int 	fflush();
extern FILE	*fopen();
extern FILE 	*freopen();
extern void 	setbuf();
extern int 	setvbuf();
extern int	fprintf(); 
extern int	fscanf();
extern int	printf();
extern int	scanf();
extern int	sprintf(); 
extern int	sscanf(); 
extern int	vfprintf();
extern int	vprintf(); 
extern int	vsprintf();
extern int 	fgetc();
extern char 	*fgets();
extern int 	fputc();
extern int 	fputs();
extern char 	*gets();
extern int 	puts();
extern int	ungetc();
extern int	fgetpos();
extern int 	fseek();
extern int	fsetpos();
extern long	ftell();
extern void	rewind();
extern void 	perror(); 

#else			/* use ANSI C required prototypes */

extern int	_flsbuf(unsigned char c, FILE *iop);
extern int	_filbuf(FILE *iop);
extern int 	ferror(FILE *stream);
extern int 	feof(FILE *stream);
extern void 	clearerr(FILE * stream);
extern int 	putchar(int c);
extern int 	getchar(void);
extern int 	putc(int c, FILE *stream);
extern int 	getc(FILE *stream);
extern int	remove(const char *filename);
extern int	rename(const char *old, const char *new);
extern FILE 	*tmpfile(void);
extern char 	*tmpnam(char *s);
extern int 	fclose(FILE *stream);
extern int 	fflush(FILE *stream);
extern FILE	*fopen(const char *filename, const char *mode);
extern FILE 	*freopen(const char *filename, const char *mode, FILE *stream);
extern void 	setbuf(FILE *stream, char *buf);
extern int 	setvbuf(FILE *stream, char *buf, int mode, size_t size);
extern int	fprintf(FILE *stream, const char *format, ...); 
extern int	fscanf(FILE *stream, const char *format, ...);
extern int	printf(const char *format, ...); 
extern int	scanf(const char *format, ...); 
extern int	sprintf(char *s, const char *format, ...); 
extern int	sscanf(const char *s, const char *format, ...); 
#ifdef _VA_LIST
extern int	vfprintf(FILE *stream, const char *format, va_list arg);
extern int	vprintf(const char *format, va_list arg); 
extern int	vsprintf(char *s, const char *format, va_list arg);
#else
extern int	vfprintf();
extern int	vprintf(); 
extern int	vsprintf();
#endif /* _VA_LIST */
extern int 	fgetc(FILE *stream);
extern char 	*fgets(char *s, int n, FILE *stream);
extern int 	fputc(int c, FILE *stream);
extern int 	fputs(const char *s, FILE *stream);
extern char 	*gets(char *s);
extern int 	puts(const char *s);
extern int	ungetc(int c, FILE *stream);
extern int	fgetpos(FILE *stream, fpos_t *pos);
extern int 	fseek(FILE *stream, long int offset, int whence);
extern int	fsetpos(FILE *stream, const fpos_t *pos);
extern long	ftell(FILE *stream);
extern void	rewind(FILE *stream);
extern void 	perror(const char *s); 

#endif /* _NO_PROTO */

#define getc(p)		(--(p)->_cnt < 0 ? _filbuf(p) : (int) *(p)->_ptr++)
#define putc(x, p)	(--(p)->_cnt < 0 ? \
			_flsbuf((unsigned char) (x), (p)) : \
			(int) (*(p)->_ptr++ = (unsigned char) (x)))
#define getchar()	getc(stdin)
#define putchar(x)	putc((x), stdout)
#define clearerr(p)	((void) ((p)->_flag &= ~(_IOERR | _IOEOF)))
#define feof(p)		((p)->_flag & _IOEOF)
#define ferror(p)	((p)->_flag & _IOERR)

#endif /*_ANSI_C_SOURCE */


#ifdef _POSIX_SOURCE
/*
 *   The following are values that have historically been in stdio.h.
 *
 *   They are a part of the POSIX defined stdio.h and therefore are
 *   included when _POSIX_SOURCE and _XOPEN_SOURCE are defined.
 *
 */

#include <sys/types.h>
#define L_ctermid	9
#define L_cuserid	9

#ifdef _NO_PROTO
extern int 	fileno();
extern FILE 	*fdopen();
#else
extern int 	fileno(FILE *stream);
extern FILE 	*fdopen(int fildes,const char *type);
#endif /* _NO_PROTO */

#define fileno(p)	((p)->_file)     

#endif /* _POSIX_SOURCE */

#ifdef _XOPEN_SOURCE
#define P_tmpdir	_P_tmpdir

#ifdef _NO_PROTO
extern int 	getw();
extern int 	putw();
extern char 	*tempnam();
extern FILE 	*popen();
extern int 	pclose();
extern wint_t	getwc();
extern wint_t	getwchar();
extern wchar_t  *getws();
extern wint_t	fgetwc();
extern wchar_t  *fgetws();
extern wint_t   fputwc();
extern int      fputws();
extern wint_t   putwc();
extern wint_t   putwchar();
extern int      putws();
extern wint_t	ungetwc();
#else
extern int 	getw(FILE *stream);
extern int 	putw(int w, FILE *stream);
extern char 	*tempnam(char*,char*);
extern FILE 	*popen(char *cmd, char *mode);
extern int 	pclose(FILE *ptr);
extern wint_t	getwc(FILE *stream);
extern wint_t	getwchar(void);
extern wchar_t  *getws(wchar_t *ws);
extern wint_t	fgetwc(FILE *stream);
extern wchar_t  *fgetws(wchar_t *s,int n,FILE *stream);
extern wint_t   fputwc(wint_t wc,FILE *stream);
extern int      fputws(const wchar_t *ws,FILE *stream);
extern wint_t   putwc(wint_t wc,FILE *stream);
extern wint_t   putwchar(wint_t wc);
extern int      putws(const wchar_t *ws);
extern wint_t	ungetwc(wint_t c,FILE *stream);

#endif /* _NO_PROTO */

#define getwchar()	getwc(stdin)

#endif /*_XOPEN_SOURCE */

#ifdef _ALL_SOURCE

#include <sys/limits.h>   /* limits.h not allowed by Posix.1a.  Must be in _ALL_SOURCE */

#ifndef _VA_LIST
#define _VA_LIST
typedef char *va_list;
#endif /* _VA_LIST */


#ifdef OPEN_MAX
#define _NFILE		OPEN_MAX
#else
#define _NFILE		2000
#endif
			/* For Dynamic iob's. Make sure _NFILE is       */
  			/* a multiple of _NIOBRW (IOB Row).   		*/

#define _NRWS		(_NFILE / _NIOBRW )+1
			/* Number of iob rows should be equal 		*/
			/* to (_NFILE / _NIOBRW ) + 1.  The extra 	*/
			/* row ensures insures that the _NFILE+1th 	*/
			/* fopen fails because of open(). 		*/

/* buffer size for multi-character output to unbuffered files */
#define _SBFSIZ 8

#define _IOREAD		0001
#define _IOWRT		0002
#define _IOMYBUF	0010
#define _IORW		0200
#define	_IONOFD		0400
#define	_IOUNGETC	01000
#define	_IOCLOSE	02000
#define	_IONONSTD	04000


#define _bufend(p)	((p)->_bufendp)
#define _bufsiz(p)	(_bufend(p) - (p)->_base)


#ifdef _NO_PROTO
extern char	*ctermid();
extern char 	*cuserid();
#else
extern char	*ctermid(char *);
extern char 	*cuserid(char *);
#endif /* _NO_PROTO */




#endif /* _ALL_SOURCE */
#endif /* _H_STDIO */

/* @(#)55	1.51  com/inc/sys/types.h, bos, bos320 6/22/91 15:24:24 */
/*
 * COMPONENT_NAME: (INCSTD) Standard Include Files
 *
 * FUNCTIONS:
 *
 * ORIGINS: 3, 26, 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1990
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 *
 */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *      (#)types.h     7.1 (Berkeley) 6/4/86
 */
#ifndef _H_TYPES
#define _H_TYPES
#include <standards.h>
/*
 *
 *      The ANSI and POSIX standards require that certain values be in types.h.
 *      It also requires that if _ANSI_C_SOURCE or _POSIX_SOURCE is defined
 *	then ONLY those values are present. This header includes all the ANSI
 *	and POSIX required entries.
 *
 *      Other entries are included in _ALL_SOURCE.
 *
 */

#ifdef   _ANSI_C_SOURCE
/*
 * ANSI C required typedefs
 */

#ifndef	_VA_LIST
#define _VA_LIST
typedef char *		va_list;
#endif

#ifndef	_PTRDIFF_T
#define _PTRDIFF_T
typedef int		ptrdiff_t;
#endif

#ifndef	_WCHAR_T
#define _WCHAR_T
typedef unsigned short	wchar_t;
#endif

#ifndef _WCTYPE_T
#define _WCTYPE_T
typedef unsigned int wctype_t;
#endif

#ifndef	_FPOS_T
#define _FPOS_T
typedef long		fpos_t;
#endif

#ifndef	_TIME_T
#define _TIME_T
typedef	long		time_t;
#endif

#ifndef	_CLOCK_T
#define _CLOCK_T
typedef int		clock_t;
#endif

#ifndef	_SIZE_T
#define _SIZE_T
typedef	unsigned long	size_t;
#endif

#endif   /* _ANSI_C_SOURCE */

#ifdef   _POSIX_SOURCE
/*
 * shorthand type definitions for unsigned storage classes
 */
typedef	unsigned char	uchar_t;
typedef	unsigned short	ushort_t;
typedef	unsigned int	uint_t;
typedef unsigned long	ulong_t;
typedef signed int	ssize_t;

/*
 * standard AIX type definitions
 */
typedef	struct { int r[1]; } *	physadr_t;
typedef long		level_t;
typedef	long		daddr_t;	/* disk address */
typedef	char *		caddr_t;	/* "core" (i.e. memory) address */
typedef	ulong_t		ino_t;		/* inode number (filesystem) */
typedef short		cnt_t;
typedef ulong_t		dev_t;		/* device number (major+minor) */
typedef	long		chan_t;		/* channel number (minor's minor) */
typedef	long		off_t;		/* file offset */
typedef	long		paddr_t;
typedef	long		key_t;
typedef long		timer_t;	/* timer id */
typedef	short		nlink_t;
typedef	ulong_t		mode_t;		/* file mode */
typedef ulong_t		uid_t;		/* user ID */
typedef ulong_t		gid_t;		/* group ID */
typedef	void *		mid_t;		/* module ID	*/
typedef	int		pid_t;		/* process ID */
typedef char		slab_t[12];	/* security label */
typedef long            mtyp_t;		/* ipc message type */

#ifndef _WINT_T
#define _WINT_T
	typedef	int		wint_t;		/* Wide character */
#endif


#include <sys/m_types.h>		/* machine-dependent type definitions */


/* typedef and structure for signal mask */
/* This must correspond to the "struct sigset" structure in _ALL_SOURCE below */
typedef struct sigset_t	{
	unsigned long losigs;
	unsigned long hisigs;
} sigset_t;

typedef int signal_t;

#endif /* _POSIX_SOURCE */

#ifdef _ALL_SOURCE

#ifndef NULL
#define	NULL	((void *)0)
#endif

#ifndef TRUE
#define TRUE	1
#endif
#ifndef FALSE
#define FALSE	0
#endif

/*
 * shorthand type definitions for unsigned storage classes
 */
typedef	uchar_t		uchar;
typedef	ushort_t	ushort;
typedef	uint_t		uint;
typedef ulong_t		ulong;

typedef	physadr_t	physadr;

/* typedefs for BSD unsigned things */
typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned long	u_long;

typedef	struct	_quad { long val[2]; } quad;
typedef	long	swblk_t;

#define	NBBY	8		/* number of bits in a byte */

/* The sigset structure must correspond to sigset_t in _POSIX_SOURCE above */
struct sigset	{
	unsigned long losigs;
	unsigned long hisigs;
};

/* typedef for the File System Identifier (fsid) */
struct fsid {
	long	val[2];
};
typedef struct fsid fsid_t;
#define	fsid_dev	val[0]
#define	fsid_type	val[1]

/* typedef for the File Identifier (fid) */
#define FHSIZE		32
#define MAXFIDSZ	(FHSIZE - sizeof(fsid_t) - sizeof(uint_t))

struct fid {
	uint_t	fid_len;
	char	fid_data[MAXFIDSZ];
};
typedef struct fid fid_t;

struct fileid {			/* this is for servers only! */
	uint_t	fid_len;
	ino_t	fid_ino;
	uint_t	fid_gen;
	char	fid_x[MAXFIDSZ - (sizeof(ino_t) + 2) - sizeof(uint_t)];
};

/* typedef for the File Handle (fhandle) */
struct fhandle {
	char x[FHSIZE];		/* allows structure assignments */
};
struct filehandle {			/* this is for servers only! */
	fsid_t		fh_fsid;		/* filesystem id */
	struct fileid	fh_fid;			/* file id */
};
typedef struct fhandle fhandle_t;
#define fh_dev	fh_fsid.fsid_dev
#define fh_type	fh_fsid.fsid_type
#define fh_len	fh_fid.fid_len
#define fh_ino	fh_fid.fid_ino
#define fh_gen	fh_fid.fid_gen

/* structure and typedef for volume group IDs and physical volume IDs */
struct unique_id {
       unsigned long word1;
       unsigned long word2;
       unsigned long word3;
       unsigned long word4;
};
typedef struct unique_id unique_id_t;

#ifdef	_BSD			/* Berkeley source compatibility */
#include <sys/select.h>

/*
 * The following macros, major(), minor() and makedev() are identical
 * to the macros in <sys/sysmacros.h>.  Any changes to either need to
 * be reflected in the other.
 */
/* major part of a device */
#define major(x)        (int)((unsigned)(x)>>16)

/* minor part of a device */
#define minor(x)        (int)((x)&0xFFFF)

/* make a device number */
#define makedev(x,y)    (dev_t)(((x)<<16) | (y))

#endif /* _BSD */

#endif   /* _ALL_SOURCE */
#endif /* _H_TYPES */
/* @(#)98	1.31  com/inc/ctype.h, libcchr, bos320, 9142320a 10/10/91 16:47:46 */
/*
 * COMPONENT_NAME: LIBCCHR
 *
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1989, 1991
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 *
 */

#ifndef __H_CTYPE
#define __H_CTYPE

#include <standards.h>
#include <sys/localedef.h>

#ifdef _ANSI_C_SOURCE
#   define _ISALPHA	0x001
#   define _ISALNUM	0x002
#   define _ISBLANK     0x004
#   define _ISCNTRL	0x008
#   define _ISDIGIT	0x010
#   define _ISGRAPH	0x020
#   define _ISLOWER	0x040
#   define _ISPRINT	0x080
#   define _ISPUNCT	0x100
#   define _ISSPACE	0x200
#   define _ISUPPER	0x400
#   define _ISXDIGIT	0x800

#   ifdef _ILS_MACROS	
#	define _VALC(c)		((c)>=0&&(c)<=256)
#	define _IS(c,m)		(__lc_ctype->mask[c] & m)
#	define isalpha(a)	(_VALC(a)?_IS(a,_ISALPHA):0)
#	define isalnum(a)	(_VALC(a)?_IS(a,_ISALNUM):0)
#	define iscntrl(a)	(_VALC(a)?_IS(a,_ISCNTRL):0)
#	define isdigit(a)	(_VALC(a)?_IS(a,_ISDIGIT):0)
#	define isgraph(a)	(_VALC(a)?_IS(a,_ISGRAPH):0)
#	define islower(a)	(_VALC(a)?_IS(a,_ISLOWER):0)
#	define isprint(a)	(_VALC(a)?_IS(a,_ISPRINT):0)
#	define ispunct(a)	(_VALC(a)?_IS(a,_ISPUNCT):0)
#	define isspace(a)	(_VALC(a)?_IS(a,_ISSPACE):0)
#	define isupper(a)	(_VALC(a)?_IS(a,_ISUPPER):0)
#	define isxdigit(a)	(_VALC(a)?_IS(a,_ISXDIGIT):0)
#	define _XUPPER(a)	(__lc_ctype->upper[a])
#	define _XLOWER(a)	(__lc_ctype->lower[a])
#	define toupper(a)	(_VALC(a)?_XUPPER(a):a)
#	define tolower(a)	(_VALC(a)?_XLOWER(a):a)
#   else /* ifdef _ILS_MACROS */
#	ifdef _NO_PROTO
	    extern int	isalpha();
	    extern int	isalnum();
	    extern int	iscntrl();
	    extern int	isdigit();
	    extern int	isgraph();
	    extern int	islower();
	    extern int	isprint();
	    extern int	ispunct();
	    extern int	isspace();
	    extern int	isupper();
	    extern int	isxdigit();
	    extern int	toupper();
	    extern int	tolower();
#	else /* ifdef _NO_PROTO */
	    extern int	isalpha(int c);
	    extern int	isalnum(int c);
	    extern int	iscntrl(int c);
	    extern int	isdigit(int c);
	    extern int	isgraph(int c);
	    extern int	islower(int c);
	    extern int	isprint(int c);
	    extern int	ispunct(int c);
	    extern int	isspace(int c);
	    extern int	isupper(int c);
	    extern int	isxdigit(int c);
	    extern int	toupper(int c);
	    extern int	tolower(int c);
#	endif /* ifdef _NO_PROTO */
#   endif /* ifdef _ILS_MACROS */
#endif /* ifdef _ANSI_C_SOURCE */

#ifdef _XOPEN_SOURCE

#   ifndef _WCHAR_T
#	define _WCHAR_T
	typedef unsigned short	wchar_t;
#   endif /* ifndef _WCHAR_T */

#   ifndef _WCTYPE_T
#	define _WCTYPE_T
	typedef unsigned int	wctype_t;
#   endif /* ifndef _WCTYPE_T */

#   ifndef _WINT_T
#	define _WINT_T
	typedef int	wint_t;
#   endif /* ifndef _WINT_T */

#   ifdef _ILS_MACROS
#	define _SBCK(a,m)	(__lc_ctype->mask[a] & m)
#	define _MBCK(a,m)	(__lc_ctype->mask[__lc_ctype->qidx[a]] & m)
#	define _VALCK(a,m)	((a)<256?_SBCK(a,m):_MBCK(a,m))
#	define iswalpha(a)	((a)>=0?_VALCK(a,_ISALPHA):0)
#	define iswalnum(a)	((a)>=0?_VALCK(a,_ISALNUM):0)
#	define iswcntrl(a)	((a)>=0?_VALCK(a,_ISCNTRL):0)
#	define iswdigit(a)	((a)>=0?_VALCK(a,_ISDIGIT):0)
#	define iswgraph(a)	((a)>=0?_VALCK(a,_ISGRAPH):0)
#	define iswlower(a)	((a)>=0?_VALCK(a,_ISLOWER):0)
#	define iswprint(a)	((a)>=0?_VALCK(a,_ISPRINT):0)
#	define iswpunct(a)	((a)>=0?_VALCK(a,_ISPUNCT):0)
#	define iswspace(a)	((a)>=0?_VALCK(a,_ISSPACE):0)
#	define iswupper(a)	((a)>=0?_VALCK(a,_ISUPPER):0)
#	define iswxdigit(a)	((a)>=0?_VALCK(a,_ISXDIGIT):0)
#	define is_wctype(a,b)	((a)>=0?_VALCK(a,b):0)
#	define towupper(a)	((__lc_charmap->cm_mb_cur_min && \
				  __lc_charmap->cm_mb_cur_max) ? \
				      __lc_ctype->upper[a] : a)
#	define towlower(a)	((__lc_charmap->cm_mb_cur_min && \
				  __lc_charmap->cm_mb_cur_max) ? \
				      __lc_ctype->lower[a] : a)
#   else /* ifdef _ILS_MACROS */
#	ifdef _NO_PROTO
	    extern int	isascii();
	    extern int	iswalpha();
	    extern int	iswalnum();
	    extern int	iswcntrl();
	    extern int	iswdigit();
	    extern int	iswgraph();
	    extern int	iswlower();
	    extern int	iswprint();
	    extern int	iswpunct();
	    extern int	iswspace();
	    extern int	iswupper();
	    extern int	iswxdigit();
	    extern wint_t	towupper();
	    extern wint_t	towlower();
	    extern int	is_wctype();
	    extern int	is_ctype();
	    extern int	toascii();
	    extern wctype_t	get_wctype();
#	else /* ifdef _NO_PROTO */
	    extern int	isascii(int c);
	    extern int	iswalpha(wint_t wc);
	    extern int	iswalnum(wint_t wc);
	    extern int	iswcntrl(wint_t wc);
	    extern int	iswdigit(wint_t wc);
	    extern int	iswgraph(wint_t wc);
	    extern int	iswlower(wint_t wc);
	    extern int	iswprint(wint_t wc);
	    extern int	iswpunct(wint_t wc);
	    extern int	iswspace(wint_t wc);
	    extern int	iswupper(wint_t wc);
	    extern int	iswxdigit(wint_t wc);
	    extern wint_t	towupper(wint_t wc);
	    extern wint_t	towlower(wint_t wc);
	    extern int	is_wctype(wint_t wc, wctype_t mask);
	    extern int	is_ctype(int c, wctype_t mask);
	    extern int	toascii(int c);
	    extern wctype_t	get_wctype(char *prop);
#	endif /* ifdef _NO_PROTO */
#   endif /* ifdef _ILS_MACROS */
#   define _toupper(a)	(__lc_ctype->upper[a])
#   define _tolower(a)	(__lc_ctype->lower[a])
#endif /* ifdef _XOPEN_SOURCE */
#endif /* __H_CTYPE */
/* @(#)01	1.10  com/inc/setjmp.h, bos, bos320 6/16/90 00:13:39 */
/*
 * COMPONENT_NAME: (INCSTD) setjmp.h
 *
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1990
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

#ifndef _H_SETJMP
#define _H_SETJMP
#include <standards.h>

/*
 *
 *      The ANSI and POSIX standards require that certain values be in setjmp.h.
 *      They also require that if _ANSI_C_SOURCE or _POSIX_SOURCE is defined 
 *      then ONLY those standard specific values are present. This header 
 *      includes all the ANSI and POSIX required entries.
 *
 */

#ifdef _ANSI_C_SOURCE
#define _JBLEN	64	/* regs, fp regs, cr, sigmask, context, etc. */
typedef int jmp_buf[_JBLEN];

#ifdef   _NO_PROTO
extern void longjmp();
extern int setjmp(); 

#else  /*_NO_PROTO */

extern void longjmp(jmp_buf env, int val);
extern int setjmp(jmp_buf env); 

#endif /*_NO_PROTO */

#if __STDC__ == 1
#define	setjmp(X)	setjmp(X)
#endif	/* __STDC__ */

#endif /* _ANSI_C_SOURCE */

#ifdef _POSIX_SOURCE

typedef int sigjmp_buf[_JBLEN];
 
#ifdef _NO_PROTO
extern int sigsetjmp();
extern void siglongjmp();
#else
extern int sigsetjmp(sigjmp_buf env, int savemask);
extern void siglongjmp(sigjmp_buf env, int val);
#endif /* _NO_PROTO */

#if __STDC__ == 1
#define	sigsetjmp(X, Y)	sigsetjmp(X, Y)
#endif	/* __STDC__ */

#endif /* _POSIX_SOURCE */

#endif /* _H_SETJMP */
/* @(#)55	1.14  com/inc/stdarg.h, bos, bos320 6/27/91 16:16:08 */
/*
 * COMPONENT_NAME: (INCSTD) Standard Include Files
 *
 * FUNCTIONS: 
 *
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1989, 1991
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

#ifndef _H_STDARG
#define _H_STDARG
#include <standards.h>

/*
 *      The ANSI standard requires that certain values be in stdarg.h.
 *
 *      It also requires that if _ANSI_C_SOURCE is defined then ONLY these
 *      values are present. This header includes all the ANSI required entries. 
 *
 */


#ifdef _ANSI_C_SOURCE

/*
 * 	The definition of va_list is included in <sys/types.h>.  It is
 *	also included here to comply with ANSI standards.
 */

#ifndef	_VA_LIST
#define _VA_LIST
typedef char *	va_list;
#endif

/* Argument list convention: Each argument is allocated a multiple of 4 bytes.
 * Each argument occupies the high-order bytes within its allocated multiple
 * of 4 bytes.  This implementation is restricted to one-quantity arguments.
 * The implementation does not support argument descriptors, which immediately
 * follow their respective argument value word(s) if they are used.
 */
#define va_start(list,parmN) list = (char *)((unsigned int)&(parmN) + (((sizeof(parmN)+3)/4)*4))
#define va_end(list) (list)=(char *)0
#define va_arg(list, mode) ((mode *)( (((list)+=(((sizeof(mode)+3)/4)*4))-sizeof(mode)) ))[0]

#endif /* _ANSI_C_SOURCE */
#endif /* _H_STDARG */
/* @(#)15	1.29  com/inc/sys/time.h, bos, bos320 5/7/91 13:05:36 */
/*
 * COMPONENT_NAME:  SYSPROC
 *
 * ORIGIN: 26, 27
 *
 * Copyright International Business Machines Corp. 1985, 1989
 * All Rights Reserved
 * Licensed Material - Property of IBM
 *
 * RESTRICTED RIGHTS LEGEND
 * Use, Duplication or Disclosure by the Government is subject to
 * restrictions as set forth in paragraph (b)(3)(B) of the Rights in
 * Technical Data and Computer Software clause in DAR 7-104.9(a).
 *
 */

#ifndef _H_SYS_TIME
#define _H_SYS_TIME

#include <sys/types.h>

/*
 * The base timer services are based on the POSIX realtime extensions.
 * These timers services support one or more timers per process and
 * a very fine timer granularity.
 */

/*
 * The following are the BSD labels for the timer types.
 */
#define	ITIMER_REAL		0		/* Real time */
#define	ITIMER_VIRTUAL		1		/* Per-process time */
#define	ITIMER_PROF		2		/* Per-process user time */

/* 
 * The following are the AIX labels for the timer types.
 */
#define	ITIMER_VIRT		3		/* Per-process time */

/* 
 * The following are the Posix labels for the timer types.
 */
#define	TIMEOFDAY		9		/* per process sys clock time */	
/*
 * Constants defining the hard-coded id's for each type of timer.
 */
#define	TIMERID_ALRM	(ITIMER_REAL)		/* alarm() */
#define	TIMERID_REAL	(ITIMER_REAL) 		/* bsd real */
#define	TIMERID_VIRTUAL	(ITIMER_VIRTUAL) 	/* bsd virtual */
#define	TIMERID_PROF	(ITIMER_PROF)		/* bsd profiling */
#define	TIMERID_VIRT	(ITIMER_VIRT)		/* aix virtual */
#define	TIMERID_TOD	(TIMERID_VIRT+1)	/* posix timers */

/*
 * Constants defining the number of timers for each timer type.
 *
 * NALRM  	number of per-process alarm and real timers. 
 * NPROF  	number of per-process profiling timers. 
 * NVIRTUAL  	number of per-process virtual timers. 
 * NTIMEOFDAY	number of per-process time of day timers.
 * NTIMERS	total number of per-process timers.
 */

#define NALRM           1      
#define NPROF           1    
#define NVIRTUAL        2  
#define NTIMEOFDAY      5 
#define NTIMERS         (NALRM + NPROF + NVIRTUAL + NTIMEOFDAY)

/*
 *  The Epoch is the time 0 hours, 0 minutes, 0 seconds, January 1, 1970
 *  Coordinated Universal Time. 
 */
#define	MIN_SECS_SINCE_EPOCH	0

/*
 *  System time constants
 */
#define NS_PER_TICK	(NS_PER_SEC/HZ)	/* nanoseconds per clock tick	*/
#define	uS_PER_SECOND	(1000000)	/* # of microseconds in one sec.*/
#define	NS_PER_uS	(1000)		/* # of nanoseconds in one us	*/
#define	MAX_SECS_TO_uS	4000		/* most seconds representable as*/
					/* microseconds in unsigned int	*/
#define	MAX_NS_TO_uS	294967296	/* most ns representable as us	*/
					/* in uint given MAX_SECS_TO_uS	*/
					/* is reached.			*/
#define	NS_PER_SEC	1000000000		/* # of nanosecs in 1 second*/
#define	uS_PER_SEC	(NS_PER_SEC / 1000)	/* # of microsecs in 1 sec  */
#define	NS_PER_MSEC	(NS_PER_SEC / 1000)	/* # of nanosecs in 1 msec  */

#define	MAX_DEC_SECS	2
#define	MAX_DEC_NS	147483647

/*
 * The following is used to hold a timer value.
 */
struct timestruc_t {
        unsigned long	tv_sec;         /* seconds			*/
        	 long	tv_nsec;        /* and nanoseconds		*/
};

/*
 * The following is used to define a timer.
 */
struct itimerstruc_t {
        struct  timestruc_t it_interval; /* timer interval		*/
        struct  timestruc_t it_value;    /* current value		*/
};

#ifdef _KERNEL
/*
 * External kernel variables 
 */

extern time_t lbolt;              /* number ticks since last boot */
extern time_t time;               /* memory mapped time in secs since Epoch */
extern struct timestruc_t tod; 	  /* memory mapped timer structure */

#endif  /* _KERNEL */

/*
 * Operations on timestruc_t.
 *
 * Note that ntimercmp only works for cmp values of !=, >, and <.
 */
#define	ntimerisset(tvp)	((tvp)->tv_sec || (tvp)->tv_nsec)
#define	ntimerclear(tvp)	(tvp)->tv_sec = (tvp)->tv_nsec = 0
#define	ntimercmp(tvp, fvp, cmp)					\
	((tvp).tv_sec cmp (fvp).tv_sec || 				\
	 (tvp).tv_sec == (fvp).tv_sec && 				\
	 (tvp).tv_nsec cmp (fvp).tv_nsec)
#define	ntimeradd(tvp, svp, rvp)					\
	{								\
		(rvp).tv_sec = (tvp).tv_sec + (svp).tv_sec;		\
		(rvp).tv_nsec = (tvp).tv_nsec + (svp).tv_nsec;		\
		if((rvp).tv_nsec > (NS_PER_SEC - 1))   {		\
			(rvp).tv_nsec -= NS_PER_SEC;			\
			(rvp).tv_sec++;					\
		}							\
		assert((rvp).tv_nsec < NS_PER_SEC);			\
	}
#define	ntimersub(tvp, svp, rvp)					\
	{								\
		assert((tvp).tv_nsec <= NS_PER_SEC);			\
		assert((svp).tv_nsec <= NS_PER_SEC);			\
		assert(!((tvp).tv_sec == 0 && ((svp).tv_nsec > (tvp).tv_nsec)));\
		if((tvp).tv_sec == (svp).tv_sec)  {			\
			assert((tvp).tv_nsec >= (svp).tv_nsec);		\
		}							\
		else  {							\
			assert((tvp).tv_sec > (svp).tv_sec);		\
		}							\
									\
		if((svp).tv_nsec > (tvp).tv_nsec)  {			\
			assert((tvp).tv_sec > (svp).tv_sec);		\
			(rvp).tv_sec = ((tvp).tv_sec - 1) - (svp).tv_sec;\
			(rvp).tv_nsec = ((int)(((uint)((tvp).tv_nsec) + NS_PER_SEC) - (uint)((svp).tv_nsec)));\
		}							\
		else  {							\
			(rvp).tv_sec = (tvp).tv_sec - (svp).tv_sec;	\
			(rvp).tv_nsec = (tvp).tv_nsec - (svp).tv_nsec;	\
		}							\
									\
		assert((tvp).tv_nsec <= NS_PER_SEC);			\
		assert((svp).tv_nsec <= NS_PER_SEC);			\
		assert((rvp).tv_nsec <= NS_PER_SEC);			\
	}

/*
 * The rest of this file is the interface to the BSD timer services.
 * Most of these services are implemented as subroutines that convert
 * the interface to the corresponding POSIX timer service.
 *
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	(#)time.h	7.1 (Berkeley) 6/4/86
 */

/*
 * The following is the BSD equivalent of the timerstruc. The main
 * difference is that the maximum resolution is specificed in microseconds
 * instead of nanoseconds.
 */
struct timeval {
        int		tv_sec;         /* seconds */
        int		tv_usec;        /* microseconds */
};

/*
 * Operations on timevals.
 *
 * Note that timercmp only works for cmp values of !=, >, and <.
 */

#define	timerisset(tvp)		((tvp)->tv_sec || (tvp)->tv_usec)
#define	timerclear(tvp)		(tvp)->tv_sec = (tvp)->tv_usec = 0
#define	timercmp(tvp, fvp, cmp)						\
	((tvp)->tv_sec cmp (fvp)->tv_sec ||				\
	 (tvp)->tv_sec == (fvp)->tv_sec &&				\
	 (tvp)->tv_usec cmp (fvp)->tv_usec)

#define	ADD_TO_NS(tvp, delta)						\
{									\
	(tvp).tv_nsec += delta;						\
	if((tvp).tv_nsec >= NS_PER_SEC)  {				\
		(tvp).tv_sec++;						\
		(tvp).tv_nsec -= NS_PER_SEC;				\
	}								\
}

/*
 *  Adds a value of nanoseconds to a timeval structure (which traditionally
 *  contains microseconds).
 */
#define	ADD_NS_TO_TIMEVAL(tvp, delta)					\
{									\
	(tvp).tv_usec += delta;						\
	if((tvp).tv_usec >= NS_PER_SEC)  {				\
		(tvp).tv_sec++;						\
		(tvp).tv_usec -= NS_PER_SEC;				\
	}								\
}

/*
 * The following is the BSD equivalent of the itimerstruc_t.
 */
struct	itimerval {
	struct		timeval it_interval; /* timer interval */
	struct		timeval it_value; /* current value */
};

/*
 * The following provides a way to convert the current time in GMT
 * to a local time.
 */
struct timezone {
	int		tz_minuteswest;	/* minutes west of Greenwich */
	int		tz_dsttime;	/* type of dst correction */
};
#define	DST_NONE		0	/* not on dst */
#define	DST_USA			1	/* USA style dst */
#define	DST_AUST		2	/* Australian style dst */
#define	DST_WET			3	/* Western European dst */
#define	DST_MET			4	/* Middle European dst */
#define	DST_EET			5	/* Eastern European dst */
#define	DST_CAN			6	/* Canada */

#ifdef	_BSD_INCLUDES
#include <time.h>
#endif	/* _BSD_INCLUDES */

#endif /* _H_SYS_TIME */
/* @(#)46	1.5  com/inc/errno.h, bos, bos320 6/16/90 00:09:46 */
/*
 * COMPONENT_NAME: (INCSTD) Standard Include Files
 *
 * FUNCTIONS: 
 *
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1989
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

#include <sys/errno.h>
extern int errno;
/* @(#)63	1.10  com/inc/sys/socket.h, bos, bos320 9/5/91 13:48:32 */
/*
 * COMPONENT_NAME: (SOCKET) Socket services
 * 
 * FUNCTIONS: 
 *
 * ORIGINS: 26 27 
 *
 * (C) COPYRIGHT International Business Machines Corp. 1988, 1989 
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 *
 * Copyright (c) 1982,1985,1986,1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	 (#)socket.h	7.10 (Berkeley) 6/28/90
 */

#ifndef _H_SOCKET
#define _H_SOCKET


/*
 * Definitions related to sockets: types, address families, options.
 */

/*
 * Types
 */
#define	SOCK_STREAM	1		/* stream socket */
#define	SOCK_DGRAM	2		/* datagram socket */
#define	SOCK_RAW	3		/* raw-protocol interface */
#define	SOCK_RDM	4		/* reliably-delivered message */
#define	SOCK_SEQPACKET	5		/* sequenced packet stream */

/*
 * Option flags per-socket.
 */
#define	SO_DEBUG	0x0001		/* turn on debugging info recording */
#define	SO_ACCEPTCONN	0x0002		/* socket has had listen() */
#define	SO_REUSEADDR	0x0004		/* allow local address reuse */
#define	SO_KEEPALIVE	0x0008		/* keep connections alive */
#define	SO_DONTROUTE	0x0010		/* just use interface addresses */
#define	SO_BROADCAST	0x0020		/* permit sending of broadcast msgs */
#define	SO_USELOOPBACK	0x0040		/* bypass hardware when possible */
#define	SO_LINGER	0x0080		/* linger on close if data present */
#define	SO_OOBINLINE	0x0100		/* leave received OOB data in line */

/*
 * Additional options, not kept in so_options.
 */
#define SO_SNDBUF	0x1001		/* send buffer size */
#define SO_RCVBUF	0x1002		/* receive buffer size */
#define SO_SNDLOWAT	0x1003		/* send low-water mark */
#define SO_RCVLOWAT	0x1004		/* receive low-water mark */
#define SO_SNDTIMEO	0x1005		/* send timeout */
#define SO_RCVTIMEO	0x1006		/* receive timeout */
#define	SO_ERROR	0x1007		/* get error status and clear */
#define	SO_TYPE		0x1008		/* get socket type */

/*
 * Structure used for manipulating linger option.
 */
struct	linger {
	int	l_onoff;		/* option on/off */
	int	l_linger;		/* linger time */
};

/*
 * Level number for (get/set)sockopt() to apply to socket itself.
 */
#define	SOL_SOCKET	0xffff		/* options for socket level */

/*
 * Address families.
 */
#define	AF_UNSPEC	0		/* unspecified */
#define	AF_UNIX		1		/* local to host (pipes, portals) */
#define	AF_INET		2		/* internetwork: UDP, TCP, etc. */
#define	AF_IMPLINK	3		/* arpanet imp addresses */
#define	AF_PUP		4		/* pup protocols: e.g. BSP */
#define	AF_CHAOS	5		/* mit CHAOS protocols */
#define	AF_NS		6		/* XEROX NS protocols */
#define	AF_ISO		7		/* ISO protocols */
#define	AF_OSI		AF_ISO
#define	AF_ECMA		8		/* european computer manufacturers */
#define	AF_DATAKIT	9		/* datakit protocols */
#define	AF_CCITT	10		/* CCITT protocols, X.25 etc */
#define	AF_SNA		11		/* IBM SNA */
#define AF_DECnet	12		/* DECnet */
#define AF_DLI		13		/* DEC Direct data link interface */
#define AF_LAT		14		/* LAT */
#define	AF_HYLINK	15		/* NSC Hyperchannel */
#define	AF_APPLETALK	16		/* Apple Talk */
#define	AF_ROUTE	17		/* Internal Routing Protocol */
#define	AF_LINK		18		/* Link layer interface */
#define	pseudo_AF_XTP	19		/* eXpress Transfer Protocol (no AF) */
#define AF_INTF		20		/* Debugging use only */
#define AF_RIF		21		/* raw interface */
#define PF_INTF         AF_INTF		/* Used by sysx/intf */

#define	AF_NETWARE	22
#define	AF_MAX		30

/*
 * Structure used by kernel to store most
 * addresses.
 */
struct sockaddr {
	u_char	sa_len;			/* total length */
	u_char	sa_family;		/* address family */
	char	sa_data[14];		/* actually longer; address value */
};

/*
 * Structure used by kernel to pass protocol
 * information in raw sockets.
 */
struct sockproto {
	u_short	sp_family;		/* address family */
	u_short	sp_protocol;		/* protocol */
};

/*
 * Protocol families, same as address families for now.
 */
#define	PF_UNSPEC	AF_UNSPEC
#define	PF_UNIX		AF_UNIX
#define	PF_INET		AF_INET
#define	PF_IMPLINK	AF_IMPLINK
#define	PF_PUP		AF_PUP
#define	PF_CHAOS	AF_CHAOS
#define	PF_NS		AF_NS
#define	PF_ISO		AF_ISO
#define	PF_OSI		AF_ISO
#define	PF_ECMA		AF_ECMA
#define	PF_DATAKIT	AF_DATAKIT
#define	PF_CCITT	AF_CCITT
#define	PF_SNA		AF_SNA
#define PF_DECnet	AF_DECnet
#define PF_DLI		AF_DLI
#define PF_LAT		AF_LAT
#define	PF_HYLINK	AF_HYLINK
#define	PF_APPLETALK	AF_APPLETALK
#define	PF_ROUTE	AF_ROUTE
#define	PF_LINK		AF_LINK
#define	PF_XTP		pseudo_AF_XTP	/* really just proto family, no AF */
#define PF_INTF		AF_INTF
#define PF_RIF		AF_RIF

#define	PF_MAX		AF_MAX

/*
 * Maximum queue length specifiable by listen.
 */
#define	SOMAXCONN	10

/*
 * Message header for recvmsg and sendmsg calls.
 * Used value-result for recvmsg, value only for sendmsg.
 */
#if _BSD==44
struct msghdr {
	caddr_t	msg_name;		/* optional address */
	u_int	msg_namelen;		/* size of address */
	struct	iovec *msg_iov;		/* scatter/gather array */
	u_int	msg_iovlen;		/* # elements in msg_iov */
	caddr_t	msg_control;		/* ancillary data, see below */
	u_int	msg_controllen;		/* ancillary data buffer len */
	int	msg_flags;		/* flags on received message */
};
#endif /* _BSD==44 */

#define	MSG_OOB		0x1		/* process out-of-band data */
#define	MSG_PEEK	0x2		/* peek at incoming message */
#define	MSG_DONTROUTE	0x4		/* send without using routing tables */
#define	MSG_EOR		0x8		/* data completes record */
#define	MSG_TRUNC	0x10		/* data discarded before delivery */
#define	MSG_CTRUNC	0x20		/* control data lost before delivery */
#define	MSG_WAITALL	0x40		/* wait for full request or error */

/*
 * Header for ancillary data objects in msg_control buffer.
 * Used for additional information with/about a datagram
 * not expressible by flags.  The format is a sequence
 * of message elements headed by cmsghdr structures.
 */
#if _BSD == 44
struct cmsghdr {
	u_int	cmsg_len;		/* data byte count, including hdr */
	int	cmsg_level;		/* originating protocol */
	int	cmsg_type;		/* protocol-specific type */
/* followed by	u_char  cmsg_data[]; */
};

/* given pointer to struct adatahdr, return pointer to data */
#define	CMSG_DATA(cmsg)		((u_char *)((cmsg) + 1))

/* given pointer to struct adatahdr, return pointer to next adatahdr */
#define	CMSG_NXTHDR(mhdr, cmsg)	\
	(((caddr_t)(cmsg) + (cmsg)->cmsg_len + sizeof(struct cmsghdr) > \
	    (mhdr)->msg_control + (mhdr)->msg_controllen) ? \
	    (struct cmsghdr *)NULL : \
	    (struct cmsghdr *)((caddr_t)(cmsg) + ALIGN((cmsg)->cmsg_len)))

#define	CMSG_FIRSTHDR(mhdr)	((struct cmsghdr *)(mhdr)->msg_control)

/* "Socket"-level control message types: */
#define	SCM_RIGHTS	0x01		/* access rights (array of int) */
#endif /* _BSD == 44 */
#define MSG_MAXIOVLEN   16              /* may not need this */


/*
 * 4.3 compat sockaddr, move to compat file later
 */
struct osockaddr {
	u_short	sa_family;		/* address family */
	char	sa_data[14];		/* up to 14 bytes of direct address */
};

/*
 * 4.3-compat message header (move to compat file later).
 */
#if _BSD != 44
struct msghdr {
	caddr_t	msg_name;		/* optional address */
	int	msg_namelen;		/* size of address */
	struct	iovec *msg_iov;		/* scatter/gather array */
	int	msg_iovlen;		/* # elements in msg_iov */
	caddr_t	msg_accrights;		/* access rights sent/received */
	int	msg_accrightslen;
};
#endif /* _BSD != 44 */
#ifdef _KERNEL
struct omsghdr {
	caddr_t	msg_name;		/* optional address */
	int	msg_namelen;		/* size of address */
	struct	iovec *msg_iov;		/* scatter/gather array */
	int	msg_iovlen;		/* # elements in msg_iov */
	caddr_t	msg_accrights;		/* access rights sent/received */
	int	msg_accrightslen;
};
#endif /* _KERNEL */
#endif /* _H_SOCKET */
/* @(#)44	1.8  com/inc/sys/times.h, bos, bos320 7/12/90 15:11:48 */


/*
 * COMPONENT_NAME: SYSPROC 
 *
 * ORIGINS: 27 
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1990 
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

#ifndef _H_TIMES
#define _H_TIMES
#include <standards.h>
#include <sys/types.h>			/* for clock_t */


#ifdef _POSIX_SOURCE

/*
 * POSIX requires that certain values be included in times.h.  It also
 * requires that when _POSIX_SOURCE is defined only those standard
 * specific values are present.  This header includes all the POSIX
 * required entries.
 */

/*
 * Structure returned by times()
 */
struct tms {
	clock_t	tms_utime;		/* user time */
	clock_t	tms_stime;		/* system time */
	clock_t	tms_cutime;		/* user time, children */
	clock_t	tms_cstime;		/* system time, children */
};

#ifndef	_NONSTD_TYPES
#ifdef _NO_PROTO
extern clock_t times();
#else
extern clock_t times(struct tms *buffer);
#endif /* _NO_PROTO */
#endif	/* _NONSTD_TYPES */

#endif /* _POSIX_SOURCE */
#endif	/* _H_TIMES */
/* @(#)23	1.5  com/inc/signal.h, bos, bos320 6/16/90 00:13:48 */

/*
 * COMPONENT_NAME: SYSPROC 
 *
 * ORIGINS: 27 
 *
 * (C) COPYRIGHT International Business Machines Corp. 1988, 1990 
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */
#include<sys/signal.h>
/* @(#)69	1.26  com/inc/fcntl.h, bos, bos320 7/26/91 11:22:20 */
#ifndef _H_FCNTL
#define _H_FCNTL

/*
 * COMPONENT_NAME: SYSLFS - Logical File System
 *
 * ORIGINS: 27, 3, 26
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1989
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

#include <standards.h>
#include <sys/types.h>

/*
 * POSIX requires that certain values be included in fcntl.h and that only
 * these values be present when _POSIX_SOURCE is defined.  This header
 * includes all the POSIX required entries.
 */

#ifdef _POSIX_SOURCE
#include <sys/flock.h>     /* for flock structure */

#ifdef _NO_PROTO
extern int open();
extern int creat();
extern int fcntl();

#else 		/* use POSIX required prototypes */
#ifndef _KERNEL
extern int open(const char *path, int oflag, ...);
extern int creat(const char *path, mode_t mode);
extern int fcntl(int fildes, int cmd,...);
#endif /* _KERNEL */
#endif /* _NO_PROTO */

/* File access modes for open */
#define	O_RDONLY	00000000
#define	O_WRONLY	00000001
#define	O_RDWR		00000002

/* Mask for use with file access modes */
#define O_ACCMODE	3		

/* File flags accessible to open and fcntl */
#define O_NONBLOCK	00000004	/* POSIX non-blocking I/O	*/
#define	O_APPEND	00000010  	/* all writes append to the end	*/
 
/* Flag flags accessible only to open */
#define	O_CREAT		00000400	/* open with creation		*/
#define	O_TRUNC		00001000	/* open with truncation		*/
#define	O_EXCL		00002000	/* exclusive open		*/
#define O_NOCTTY	00004000	/* do not assign control tty	*/

/* fcntl requests */
#define	 F_DUPFD	0		/* Duplicate file descriptor	*/
#define	 F_GETFD	1		/* Get file descriptor flags	*/
#define	 F_SETFD	2		/* Set file descriptor flags	*/
#define	 F_GETFL	3		/* Get file flags		*/
#define	 F_SETFL	4		/* Set file flags		*/
#define	 F_GETLK	5		/* Get file lock		*/
#define	 F_SETLK	6		/* Set file lock		*/
#define	 F_SETLKW	7		/* Set file lock and wait	*/

/* File descriptor flags used for fcntl F_GETFD and F_SETFD */
#define FD_CLOEXEC      1		/* Close this file during exec	*/

#endif /* _POSIX_SOURCE */

#ifdef _XOPEN_SOURCE
#define O_SYNC		00000020	/* synchronous write option	*/
#endif /* _XOPEN_SOURCE */

#ifdef _ALL_SOURCE

/* Additional open modes */

#define	O_NONE		00000003	/* open without read or write	*/
#define O_EXEC		00000040	/* open for exec		*/
#define O_RSHARE	00010000	/* read shared open		*/
#define O_DEFER		00020000	/* defered update		*/
#define O_DELAY		00040000	/* open with delay		*/
#define O_NDELAY	00100000	/* Non-blocking I/O		*/
#define O_NSHARE	00200000	/* read shared open		*/

/* Additional fcntl requests */

#define	 F_GETOWN	8		/* Get async I/O owner		*/
#define	 F_SETOWN	9		/* Set async I/O owner		*/
#define  F_CLOSEM	10		/* Close multiple files		*/

/* Additional fcntl F_SETFL flags */

#ifdef	_BSD
/* Provide appropriate definition of FNDELAY for BSD */
#define	FNDELAY		O_NONBLOCK
#undef	O_NDELAY
#define	O_NDELAY	O_NONBLOCK
#else	/* System V */
#define	FNDELAY		O_NDELAY
#endif	/* _BSD */

#define	FNONBLOCK 	O_NONBLOCK
#define	FAPPEND		O_APPEND
#define	FSYNC		O_SYNC
#define FASYNC		00400000

/* File flag values corresponding to file access modes */

#define	FOPEN		(-1)
#define	FREAD		(O_RDONLY-FOPEN)
#define	FWRITE		(O_WRONLY-FOPEN)

#ifdef _KERNEL

#define	FMPX		00000200	/* multiplexed open, obsolete	*/

/* FMASK defines the bits that remain in the file flags past the open	*/
#define	FMASK		00374377

/* FFCNTL is all the bits that may be set via fcntl. */
#define	FFCNTL		(FNONBLOCK|FNDELAY|FAPPEND|FSYNC|FASYNC)

/* open only modes */
#define	FCREAT		O_CREAT
#define	FTRUNC		O_TRUNC
#define	FEXCL		O_EXCL

#define FRSHARE 	O_RSHARE
#define FDEFER		O_DEFER
#define FDELAY		O_DELAY
#define FNDELAY		O_NDELAY
#define FNSHARE		O_NSHARE

/* additional modes */
#define	FEXEC		O_EXEC		/* open/close for exec		*/
#define	FNOCTTY		O_NOCTTY	/* do not assign control tty	*/
#define	FMOUNT		01000000	/* open/close for device mounts */
#define	FREVOKED	0x20000000	/* file access has been revoked	*/
#define	FKERNEL		0x40000000	/* caller is in kernel mode	*/
#define FAIO		00000100	/* aio on this fp		*/
#define FDOCLONE	0x10000000	/* reserve DDOCLONE for devices	*/

#endif	/* _KERNEL	*/
#endif	/* _ALL_SOURCE	*/
#endif	/* _H_FCNTL	*/
/* @(#)82	1.37  com/inc/unistd.h, bos, bos320 9/19/91 14:08:21 */
#ifndef _H_UNISTD
#define _H_UNISTD
/*
 * COMPONENT_NAME: (INCSTD) Standard Include Files
 *
 * FUNCTIONS: 
 *
 * ORIGINS: 3 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1989, 1991
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 *
 * Copyright (c) 1984 AT&T	
 * All Rights Reserved  
 *
 * THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	
 * The copyright notice above does not evidence any   
 * actual or intended publication of such source code.
 */

#include <standards.h>
#include <sys/types.h>  /* for time_t */
#include <sys/access.h>	/* for the "access" function */

/*
 * POSIX requires that certain values be included in unistd.h.  It also
 * requires that when _POSIX_SOURCE is defined only those standard
 * specific values are present.  This header includes all the POSIX
 * required entries.
 */

#ifdef _POSIX_SOURCE


/* Symbolic constants for the "lseek" function: */
#define SEEK_SET 0	/* Set file pointer to "offset" */
#define SEEK_CUR 1	/* Set file pointer to current plus "offset" */
#define SEEK_END 2	/* Set file pointer to EOF plus "offset" */

#ifdef _NO_PROTO

#ifndef _KERNEL
extern int access();
extern unsigned int alarm();
extern int chdir();
extern int chown();
extern int close();
extern char *ctermid();
extern char *cuserid();
extern int dup();
extern int dup2();
extern int execl();
extern int execv();
extern int execle(); 
extern int execve();
extern int execlp();
extern int execvp();
extern void _exit();
extern pid_t fork();
extern long fpathconf();
extern char *getcwd();
extern gid_t getegid();
extern uid_t geteuid();
extern gid_t getgid();
extern int getgroups();
extern char *getlogin();
extern pid_t getpgrp();
extern pid_t getpid();
extern pid_t getppid();
extern uid_t getuid();
extern int isatty();
extern int link();
extern off_t lseek();
extern long pathconf();
extern int pause();
extern int pipe();
extern int read();
extern int rename();
extern int rmdir();
extern int setgid();
extern int setpgid();
extern int setsid(); 
extern int setuid();
extern unsigned int sleep();
extern long sysconf();
extern pid_t tcgetpgrp();
extern int tcsetpgrp();
extern char *ttyname();
extern int unlink();
extern int write(); 
#endif		/* !_KERNEL	*/

#else		/* POSIX required prototypes */

#ifndef _KERNEL
extern int access(const char *path, int amode);
extern unsigned int alarm(unsigned int seconds);
extern int chdir(const char *path);
extern int chown(const char *path, uid_t owner, gid_t group);
extern int close(int fildes);
extern char *ctermid(char *s);
extern char *cuserid(char *s);
extern int dup(int fildes);
extern int dup2(int fildes, int fildes2);
extern int execl(const char *path, const char *arg0, ...);
extern int execv(const char *path, char *const argv[]);
extern int execle(const char *path, const char *arg0, ...); 
extern int execve(const char *path, char *const argv[], char *const envp[]);
extern int execlp(const char *path, const char *arg0, ...); 
extern int execvp(const char *file, char *const argv[]);
extern void _exit(int status);
extern pid_t fork(void);
extern long fpathconf(int fildes, int name);
extern char *getcwd(char *buf, size_t size);
extern gid_t getegid(void);
extern uid_t geteuid(void);
extern gid_t getgid(void);
extern int getgroups(int gidsetsize, gid_t grouplist[]);
extern char *getlogin(void);
extern pid_t getpgrp(void);
extern pid_t getpid(void);
extern pid_t getppid(void);
extern uid_t getuid(void);
extern int isatty(int fildes);
extern int link(const char *path1, const char *path2);
extern off_t lseek(int fildes, off_t offset, int whence);
extern long pathconf(const char *path, int name);
extern int pause(void);
extern int pipe(int fildes[]);
extern ssize_t read(int fildes, void *buf, size_t nbyte);
extern int rename(const char *old, const char *new);
extern int rmdir(const char *path);
extern int setgid(gid_t gid);
extern int setpgid(pid_t pid, pid_t pgid);
extern pid_t setsid(void);
extern int setuid(uid_t uid);
extern unsigned int sleep(unsigned int seconds);
extern long sysconf(int name);
extern pid_t tcgetpgrp(int fildes);
extern int tcsetpgrp(int fildes, pid_t pgrp_id);
extern char *ttyname(int fildes);
extern int unlink(const char *path);
extern ssize_t write(int fildes, const void *buf, size_t nbyte); 
#endif		/* !_KERNEL	*/
#endif		/* !_NO_PROTO	*/

#define STDIN_FILENO	0
#define STDOUT_FILENO	1
#define STDERR_FILENO	2

#define _POSIX_JOB_CONTROL	
#define _POSIX_SAVED_IDS
#define _POSIX_VERSION		199009L
#define _POSIX2_VERSION		0L	/* unofficial draft version */

#define _POSIX_CHOWN_RESTRICTED	 0
#define _POSIX_VDISABLE 	 0xFF
#define _POSIX_NO_TRUNC	    	 0

#ifndef NULL 
#define NULL		((void *)0)
#endif

/* arguments for the confstr() function */

#define _CS_PATH	1
#define _CSPATH		"/usr/bin"

/* arguments for the pathconf() function */

#define _PC_CHOWN_RESTRICTED	10
#define _PC_LINK_MAX		11
#define _PC_MAX_CANON		12
#define _PC_MAX_INPUT		13
#define _PC_NAME_MAX		14
#define _PC_NO_TRUNC		15
#define _PC_PATH_MAX		16
#define _PC_PIPE_BUF		17
#define _PC_VDISABLE 		18

/* arguments for the sysconf() function, the defined numbers are used as 
 * array index in sysconf().
 *
 * POSIX.1(1990), Table 4-2 
 */
#define _SC_ARG_MAX		0
#define _SC_CHILD_MAX		1
#define _SC_CLK_TCK		2
#define _SC_NGROUPS_MAX		3
#define _SC_OPEN_MAX		4
#define _SC_STREAM_MAX		5
#define _SC_TZNAME_MAX		6
#define _SC_JOB_CONTROL		7
#define _SC_SAVED_IDS		8
#define _SC_VERSION		9

/* POSIX.1(1990), Table 2-3, required by command getconf */

#define _SC_POSIX_ARG_MAX	10
#define _SC_POSIX_CHILD_MAX	11	
#define _SC_POSIX_LINK_MAX	12
#define _SC_POSIX_MAX_CANON	13
#define _SC_POSIX_MAX_INPUT	14
#define _SC_POSIX_NAME_MAX	15
#define _SC_POSIX_NGROUPS_MAX	16
#define _SC_POSIX_OPEN_MAX	17
#define _SC_POSIX_PATH_MAX	18
#define _SC_POSIX_PIPE_BUF	19
#define _SC_POSIX_SSIZE_MAX	20
#define _SC_POSIX_STREAM_MAX	21
#define _SC_POSIX_TZNAME_MAX	22

/* POSIX.2 (Draft 10), Table 41)	*/

#define _SC_BC_BASE_MAX		23
#define _SC_BC_DIM_MAX		24
#define _SC_BC_SCALE_MAX	25
#define _SC_BC_STRING_MAX	26
#define _SC_EQUIV_CLASS_MAX	27
#define _SC_EXPR_NEST_MAX	28
#define _SC_LINE_MAX		29
#define _SC_RE_DUP_MAX		30
#define _SC_2_VERSION		31
#define _SC_2_C_DEV		32
#define _SC_2_FORT_DEV		33
#define _SC_2_FORT_RUN		34
#define _SC_2_LOCALEDEF		35
#define _SC_2_SW_DEV		36

/* POSIX.2 (Draft 10), Table 13)	*/

#define _SC_POSIX2_BC_BASE_MAX		37
#define _SC_POSIX2_BC_DIM_MAX		38
#define _SC_POSIX2_BC_SCALE_MAX		39
#define _SC_POSIX2_BC_STRING_MAX	40
#define _SC_POSIX2_EQUIV_CLASS_MAX	41
#define _SC_POSIX2_EXPR_NEST_MAX	42
#define _SC_POSIX2_LINE_MAX		43
#define _SC_POSIX2_RE_DUP_MAX		44

/* Others */
#define _SC_PASS_MAX			45
#endif /* _POSIX_SOURCE */

#ifdef _XOPEN_SOURCE
#define _SC_XOPEN_VERSION	46
#define _XOPEN_VERSION		3

#ifdef _NO_PROTO
	extern int	nice();
#else
	extern int	nice(int);
#endif

#endif /* _XOPEN _SOURCE */

/* OSF, AES for sysconf() */
#define _SC_ATEXIT_MAX		47
#define _SC_PAGE_SIZE		48
#define _SC_AES_OS_VERSION	49	/* array index of sysconf() ends here */

#ifdef _ALL_SOURCE
#include <sys/lockf.h>		/* lockf definitions for portability	*/

extern char **environ;

#ifdef _NO_PROTO
	extern int	fchown();
	extern int	ftruncate();
	extern int	readlink();
	extern int 	setgroups();
	extern int 	symlink();
	extern int	truncate();
#else
	extern int	fchown(int, uid_t, gid_t);
	extern int	ftruncate(int, off_t);
	extern int	readlink(char *, char *, int);
	extern int 	setgroups(int, gid_t []);
	extern int 	symlink(char *, char *);
	extern int	truncate(char *, off_t);
#endif

#define _AES_OS_VERSION 1               /* OSF, AES version */

#endif /* _ALL_SOURCE */

#endif /* _H_UNISTD */
/* @(#)06	1.25  com/inc/sys/stat.h, bos, bos320 7/26/91 11:25:04 */

#ifndef _H_STAT
#define _H_STAT

/*
 * COMPONENT_NAME: SYSLFS - Logical File System
 *
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1991
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

#include <standards.h>
#include <sys/types.h>
#include <sys/mode.h>

/*
 * POSIX requires that certain values be included in stat.h.  It also
 * requires that when _POSIX_SOURCE is defined only those standard
 * specific values are present.  This header includes all the POSIX
 * required entries.
 */

#ifdef _POSIX_SOURCE 

/*
 *	Stat structure
 *
 *	The complete structure is returned only for statx() & fstatx().  All
 *	other flavors of stat() return subsets of this structure.  To be exact,
 *	only the fields up to the related comments below are filled in for
 *	each of the specified compatibility interfaces.
 */

struct  stat
{
	dev_t	st_dev;			/* ID of device containing a directory*/
					/*   entry for this file.  File serial*/
					/*   no + device ID uniquely identify */
					/*   the file within the system */
	ino_t	st_ino;			/* File serial number */
#ifdef	_NONSTD_TYPES
	ushort	st_mode_ext;
	ushort  st_mode;
#else
	mode_t	st_mode;		/* File mode; see #define's below */
#endif	/* _NONSTD_TYPES */
	short	st_nlink;		/* Number of links */
#ifdef	_NONSTD_TYPES
	ushort  st_pad_to_word;
	ushort	st_uid_ext;
	ushort	st_uid;
	ushort	st_gid_ext;
	ushort	st_gid;
#else
	uid_t	st_uid;			/* User ID of the file's owner */
	gid_t	st_gid;			/* Group ID of the file's group */
#endif	/* _NONSTD_TYPES */
	dev_t	st_rdev;		/* ID of device */
					/*   This entry is defined only for */
					/*   character or block special files */
	off_t	st_size;		/* File size in bytes */
	time_t	st_atime;		/* Time of last access */
	int	st_spare1;
	time_t	st_mtime;		/* Time of last data modification */
	int	st_spare2;
	time_t	st_ctime;		/* Time of last file status change */
	int	st_spare3;
					/* Time measured in seconds since */
					/*   00:00:00 GMT, Jan. 1, 1970 */
	ulong_t	st_blksize;		/* Optimal blocksize for file system
					   i/o ops */
	ulong_t	st_blocks;		/* Actual number of blocks allocated
					   in DEV_BSIZE blocks */
	int	st_vfstype;		/* Type of fs (see vnode.h) */
	ulong_t	st_vfs;			/* Vfs number */
	ulong_t	st_type;		/* Vnode type */

	/********************************************************************/
	/****  End of initialized data for stat(), fstat(), and lstat()  ****/
	/********************************************************************/

	ulong_t	st_gen;			/* Inode generation number */
	ulong_t	st_flag;		/* Flag word */
	uid_t	Reserved1;		/* Reserved */
	gid_t	Reserved2;		/* Reserved */

	/*****************************************************************/
	/****  End of initialized data for fullstat() and ffullstat() ****/
	/*****************************************************************/

	ushort_t st_access;		/* Process' access to file */
	ulong_t	st_spare4[5];		/* Reserved */
};
			/* End of the stat structure */


#ifndef _KERNEL
#ifndef	_NONSTD_TYPES
#ifdef	_NO_PROTO
	extern mode_t	umask(); 
#else	/* _NO_PROTO */
	extern mode_t	umask(mode_t cmask); 
#endif	/* _NO_PROTO */
#endif	/* _NONSTD_TYPES */
#endif	/* _KERNEL */

#ifdef _NO_PROTO
#ifndef _KERNEL
	extern int	mkdir(); 
	extern int	stat();
	extern int	fstat();
	extern int	chmod();
#endif /* _KERNEL */
	extern int	mkfifo();
#else				/* use POSIX required prototypes */
#ifndef _KERNEL
	extern int	mkdir(const char *path, mode_t mode); 
	extern int	stat(const char *path, struct stat *buf);
	extern int	fstat(int fildes, struct stat *buf);
	extern int	chmod(const char *path, mode_t mode);
#endif /* _KERNEL */
	extern int	mkfifo(const char *path, mode_t mode);
#endif /* _NO_PROTO */

#endif /* _POSIX_SOURCE */

#ifdef _ALL_SOURCE

#ifdef _NO_PROTO
#ifndef _KERNEL
        extern int      lstat();
        extern int      fchmod();
#endif /* _KERNEL */
#else                           /* use POSIX required prototypes */
#ifndef _KERNEL
        extern int      lstat(const char *, struct stat *);
        extern int      fchmod(char *, mode_t);
#endif /* _KERNEL */
#endif /* _NO_PROTO */

/*
 *	st_flag values
 */

#define	FS_VMP		01		/* file is vfs root or mounted over */
#define	FS_MOUNT	FS_VMP		/*   (vfs mount point) */
#define FS_REMOTE	02		/* file is remote */

/*
 * st_dev bit definitions
 */
#define SDEV_REMOTE	0x80000000	/* remote `device' if on	*/

/*
 *	statx() cmd arguments
 */

#define	STX_NORMAL	0x00	/* normal statx processing */
#define	STX_LINK	0x01	/* do not traverse final symbolic link */
#define	STX_MOUNT	0x02	/* do not traverse final mount point */
#define	STX_HIDDEN	0x04	/* do not traverse final hidden directory */


/*
 *	stat structure subset sizes for statx() compatibility routines
 */

#define STATXSIZE	(sizeof(struct stat))
#define STATSIZE	((int)&((struct stat *)0)->st_gen)
#define FULLSTATSIZE	((int)&((struct stat *)0)->st_access)

#endif /* _ALL_SOURCE */
#endif /* _H_STAT */
/* @(#)45	1.35  com/inc/sys/param.h, bos, bos320 2/12/91 17:50:34 */
#ifndef _H_PARAM
#define _H_PARAM

/*
 * COMPONENT_NAME: (SYSPROC) process management
 *
 * FUNCTIONS: none
 *
 * ORIGINS: 26, 9, 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1988, 1990
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 *
 */

#include <sys/m_param.h>
#include <sys/limits.h>

#define MAXUPRC	CHILD_MAX
#define NOFILE	OPEN_MAX

/*
 * fundamental variables
 * don't change too often
 */

#define	MAXPID	PID_MAX		/* max process id			*/
#define	MAXUID	UID_MAX		/* max user id				*/
#define	MAXLINK	LINK_MAX	/* max links				*/

#define MAXBLK	500		/* max blocks possible for phys IO	*/
#define	CANBSIZ	MAX_CANON	/* max size of typewriter line		*/
#define	NCARGS	ARG_MAX		/* # characters in exec arglist		*/
#define NGROUPS NGROUPS_MAX     /* max number of concurrent groups for user */

/*
 * fundamental constants of the implementation--
 * cannot be changed easily
 */
#define PAGESIZE	4096	/* page size in bytes			*/
#define PGSHIFT		12	/* log2 of PAGESIZE			*/
#define	NBPW	sizeof(int)	/* number of bytes in an integer	*/
#define NBPB		(8)	/* number of bits in a byte		*/
#define BPBSHIFT	(3)	/* log2(NBPB)				*/
#define UWSHIFT		(5)	/* log2(number of bits in an integer)	*/

#ifdef		_AIXFS
#define FsTYPE	4		/* Old AIX V2 File system		*/
#include	<aixfs/fsparam.h>
#else	/*	_AIXFS	*/
#include	<jfs/fsparam.h> /* Default to JFS File system		*/
#endif	/* 	_AIXFS	*/

/*
 * UBSIZE: Units for communication with user in ulimit, ustat.
 */
#define UBSIZE	512		/* bytes in a "user block" (<= BSIZE)	*/
#define UBSHIFT 9		/* LOG2(UBSIZE)				*/

#define NCPS	1		/* Number of clicks per segment		*/
#define NBPC	PAGESIZE	/* Number of bytes per click		*/
#define NCPD	1		/* Number of clicks per disk block	*/
#define BPCSHIFT       12	/* LOG2(NBPC) if exact			*/

#ifndef NULL
#define	NULL	((void *)0)
#endif

#define GOOD	0
#define BAD	(-1)

/*
 * MAXPATHLEN defines the longest permissable path length
 * after expanding symbolic links. It is used to allocate
 * a temporary buffer in which to do the name expansion.
 * MAXSYMLINKS defines the maximum number of symbolic links
 * that may be expanded in a path name. It should be set high
 * enough to allow all legitimate uses, but halt infinite loops
 * reasonably quickly.
 */
#define MAXPATHLEN	1024
#define MAXSYMLINKS	20
#ifndef MAXBSIZE
#ifdef	BSIZE
#define	MAXBSIZE	BSIZE
#else
#define	MAXBSIZE	4096
#endif
#endif

#define	MAXPATH	512		/* maximum pathlength for mount args	*/

#define	CMASK	0		/* default mask for file creation	*/
#define	CDLIMIT	(1L<<13)	/* default max write address		*/
#define	NODEVICE	(dev_t)(-1)

#define BASEPRI()	(PS == 7)	/** really ??????? **/

#define BUSOPEN(ics)	   (ics & 0x18)
    /* true if user has /dev/bus open */

#define	lobyte(X)	(((unsigned char *)&(X))[1])
#define	hibyte(X)	(((unsigned char *)&(X))[0])
#define	loword(X)	(((ushort *)&(X))[1])
#define	hiword(X)	(((ushort *)&(X))[0])


/*
 * BSD style param constants for use in AIX
 */

/*
 * bit map related macros
 */
#define	setbit(a,i)	((a)[(i)/NBBY] |= 1<<((i)%NBBY))
#define	clrbit(a,i)	((a)[(i)/NBBY] &= ~(1<<((i)%NBBY)))
#define	isset(a,i)	((a)[(i)/NBBY] & (1<<((i)%NBBY)))
#define	isclr(a,i)	(((a)[(i)/NBBY] & (1<<((i)%NBBY))) == 0)

/*
 * Macros for fast min/max.
 */
#ifndef MIN
#define	MIN(a,b) (((a)<(b))?(a):(b))
#endif /* MIN */
#ifndef MAX
#define	MAX(a,b) (((a)>(b))?(a):(b))
#endif /* MAX */

/*
 * Macros for counting and rounding.
 */
#ifndef howmany
#define	howmany(x, y)	(((x)+((y)-1))/(y))
#endif
#define	roundup(x, y)	((((x)+((y)-1))/(y))*(y))

/*
 * Maximum size of hostname recognized and stored in the kernel.
 */
#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN	32	/* see AIX <sys/utsname.h> SYS_NMLN	*/
#endif	/* MAXHOSTNAMELEN */

/* for AIX sockets */
/* MBUF CLUSTER defines, totally unlike the vax */
#define NBPG		PAGESIZE
/* This used to be CLSIZE, changed to avoid name collision, see sys/tty.h */
#define CLUSTERSIZE	1			/* one cluster per page	*/
#define	CLBYTES		(CLUSTERSIZE*PAGESIZE)	/* PAGESIZE from AIX	*/
#define	CLOFSET		(CLUSTERSIZE*PAGESIZE-1)/* for clusters, like PGOFSET */
#define	claligned(x)	((((int)(x))&CLOFSET)==0)
#define	CLOFF		CLOFSET
#define	CLSIZELOG2	0			/* CLUSTERSIZE == PAGESIZE */
#define	CLSHIFT		(BPCSHIFT+CLSIZELOG2)	/* Bytes per Click shift AIX */

#if CLUSTERSIZE==1
#define	clbase(i)	(i)
#define	clrnd(i)	(i)
#else
/* give the base virtual address (first of CLUSTERSIZE) */
#define	clbase(i)	((i) &~ (CLUSTERSIZE-1))
/* round a number of clicks up to a whole cluster */
#define	clrnd(i)	(((i) + (CLUSTERSIZE-1)) &~ (CLUSTERSIZE-1))
#endif

/* BSD param.h also includes signal.h */
#ifdef	_BSD
#ifndef _KERNEL
#include <sys/signal.h>
#endif /* _KERNEL */
#endif	/* _BSD */

#include "sys/sysmacros.h"

#endif /* _H_PARAM */

/* @(#)93	1.16  com/inc/sys/file.h, bos, bos320 6/16/90 00:27:32 */
#ifndef _H_FILE
#define _H_FILE

/*
 * COMPONENT_NAME: SYSLFS - Logical File System
 *
 * ORIGINS: 27, 3, 26
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1990
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

#include <sys/types.h>
#include <sys/param.h>
#include <sys/access.h>
#include <fcntl.h>

#ifdef _KERNEL
/*
 * One file structure is allocated for each open/creat/pipe call.
 * Main use is to hold the read/write pointer associated with
 * each open file.
 */
struct	file {
	long	f_flag;
	long	f_count;	/* reference count */
	short	f_type;		/* descriptor type */
	short	f_msgcount;	/* references from message queue */
	union {
		struct vnode	*f_uvnode;	/* pointer to vnode structure */
		struct file	*f_unext;	/* next entry in freelist */
	} f_up;
	off_t	f_offset;	/* read/write character pointer */
	caddr_t	f_vinfo;	/* any info vfs needs, ie NFS cred */ 
	off_t	f_dir_off;	/* BSD style directory offsets */
	struct fileops
	{
		int	(*fo_rw)();
		int	(*fo_ioctl)();
		int	(*fo_select)();
		int	(*fo_close)();
		int	(*fo_fstat)();
	} *f_ops;
};

#define	f_data		f_up.f_uvnode
#define	f_vnode		f_up.f_uvnode
#define	f_next		f_up.f_unext

/* f_type values */

#define	DTYPE_VNODE	1	/* file */
#define	DTYPE_SOCKET	2	/* communications endpoint */
#define	DTYPE_GNODE	3	/* device */

extern struct file file[];	/* The file table itself */
extern struct file *ffreelist;	/* Head of freelist pool */
#endif

/*
 * Flock call.
 */
#define	LOCK_SH		1	/* shared lock */
#define	LOCK_EX		2	/* exclusive lock */
#define	LOCK_NB		4	/* don't block when locking */
#define	LOCK_UN		8	/* unlock */

/*
 * Lseek call.
 */
#define	L_SET		0	/* absolute offset */
#define	L_INCR		1	/* relative to current offset */
#define	L_XTND		2	/* relative to end of file */

#endif /* _H_FILE */
/* @(#)49	1.9  com/inc/memory.h, bos, bos320 6/16/90 00:12:04 */
/*
 * COMPONENT_NAME: (LIBCGEN) Standard C Library General Functions
 *
 * FUNCTIONS: 
 *
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1989
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

#ifndef _H_MEMORY
#define _H_MEMORY

/*
 * libc memory function declarations...
 *	The ANSI C standard specifies its memory move functions to be
 *	declared in <string.h>.  Therefore, all the memory move functions
 *	have been put in that header.  <memory.h> is maintained for
 *	compatibility with earlier AIX releases.
 */

#include <string.h>

#endif /* _H_MEMORY */
/* @(#)53	1.19  com/inc/sys/vfs.h, bos, bos320 6/13/91 21:05:21 */

#ifndef _H_VFS
#define _H_VFS

/*
 * COMPONENT_NAME: SYSLFS - Logical File System
 *
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1989
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

#include <sys/types.h>
#include <sys/gfs.h>
#include <sys/vnode.h>
#include <sys/vmount.h>

/*
** where vfs data is stored (VFSfile)
** where the helpers reside (VFSdir) by default
** where to find filesystem data (from IN/FSdefs.h)
*/

#define VFSfile "/etc/vfs"
#define VFSdir  "/sbin/helpers/"
#define FSfile  FSYSname

/*
** struct vfs_ent discribes an entry in the VFSfile
**
** these are returned by various getvfsent(), etc. routines.
**
** many of the vfs_ prefixes are disguises for gfs objects
** so be careful about naming when using this header file
*/

struct vfs_ent
{
  char *vfsent_name;      /* name (ie. "aix3", "nfs", etc.) */
  int   vfsent_type;      /* in vmount.h */
  int   vfsent_flags;
  char *vfsent_mnt_hlpr;  /* mount helper pathname */ 
  char *vfsent_fs_hlpr;   /* filesystem helper pathname */
};

/*
** no helper string
*/  

#define NO_HELPER "none"

/*
** as keyed by the %default VFSfile control directive
*/

#define VFS_DFLT_LOCAL     0x1
#define VFS_DFLT_REMOTE    0x2

/*
 * the vfs structure represents a virtual file system.
 *	One of them is created for each mounted file-system/object.
 */
struct vfs {
	struct vfs	*vfs_next;	/* vfs's are a linked list */
	struct gfs	*vfs_gfs;	/* ptr to gfs of vfs */
	struct vnode	*vfs_mntd;	/* pointer to mounted vnode, */
					/*	the root of this vfs */
	struct vnode	*vfs_mntdover;	/* pointer to mounted-over */
					/*	vnode		 */
	int		vfs_lock[2];	/* locking */
	struct vnode	*vfs_vnodes;	/* all vnodes in this vfs */
	caddr_t		vfs_data;	/* private data area pointer */
	unsigned int	vfs_number;	/* serial number to help */
					/*  distinguish between */
					/*  different mounts of the */
					/*  same object */
	int		vfs_bsize;	/* native block size */
#ifdef	_SUN
	short		vfs_exflags;	/* for SUN, exported fs flags */
	unsigned short	vfs_exroot;	/* for SUN, " fs uid 0 mapping */
#else
	short		vfs_rsvd1;	/* Reserved */
	unsigned short	vfs_rsvd2;	/* Reserved */
#endif	/* _SUN */
	struct vmount	*vfs_mdata;	/* record of mount arguments */
};
/* these defines are for backwards compatibility */
#define vfs_fsid	vfs_mdata->vmt_fsid 
#define vfs_date	vfs_mdata->vmt_time 
#define vfs_flag	vfs_mdata->vmt_flags
#define vfs_type	vfs_gfs->gfs_type 
#define vfs_ops		vfs_gfs->gfs_ops 

#ifdef	_SUN
/* the defines are for the SUN style names */
#define vfs_vnodecovered vfs_mntdover
#define vfs_pdata vfs_data
#endif	/* _SUN */

/* note no define for vfs_op, use VFS_func() macros! */

/*
 * Definitions for bits in the vfs_flag
 * field of the vfs structure.
 */
#define	VFS_READONLY	MNT_READONLY	/* file system mounted rdonly	*/
#define	VFS_REMOVABLE	MNT_REMOVABLE	/* removable (diskette) mount	*/
#define	VFS_DEVMOUNT	MNT_DEVICE	/* device mount			*/
#define	VFS_REMOTE	MNT_REMOTE	/* remote file system		*/
#define	VFS_DISCONNECTED	0x0010	/* disconnected/incomplete	*/
					/*	mount			*/
#define	VFS_UNMOUNTING	MNT_UNMOUNTING	/* originated by unmount()	*/
#define	VFS_SHUTDOWN	0x0040		/* forced unmount for shutdown	*/
#define VFS_VMOUNTOK	0x0080		/* dir/file mnt permission flag	*/
#define	VFS_SUSER	0x0100		/* client-side suser perm. flag */
#define VFS_INTRANSIT	0x0200		/* Mount is occuring.		*/
#define VFS_SYSV_MOUNT	MNT_SYSV_MOUNT	/* System V style mount		*/
#define	VFS_NOSUID	MNT_NOSUID	/* don't maintain suid-ness	*/
					/* across this mount		*/
#define	VFS_NODEV	MNT_NODEV	/* don't allow device access	*/
					/* across this mount		*/
#define	DUMMY		0x8000		/* Dummy virtual file system	*/

#ifdef	_SUN
#define VFS_EXPORTED	0x0200		/* file system is exported (NFS) */

/* exported vfs flags */
#define EX_RDONLY	0x01		/* exported read only */
#endif	/* _SUN */

/* 
 * Structure containing pointers to routines
 * which implement vfs operations
 */
struct vfsops {
	int (*vfs_mount)();	/* (vfsp) */
	int (*vfs_unmount)();	/* (vfsp, flag) */
	int (*vfs_root)();	/* (vfsp,vpp,flags) - get root vnode */
	int (*vfs_statfs)();	/* (vfsp,statfsp) - get fs statistics */
	int (*vfs_sync)();	/* (vfsp) */
	int (*vfs_vget)();	/* (vfsp,vpp,fidp) get vnode by file handle */
	int (*vfs_cntl)();	/* (vfsp,cmd,arg,argsiz) fscntl syscall */
				/* extended vfs ops follow */
	int (*vfs_quotactl)();	/* (vfsp,cmd,id,arg) quotactl syscall  */
};

#ifdef	_KERNEL
#ifdef	_VOPS

/* Handy macros for calling the vfs ops */

#define VFS_MOUNT(VFSP)		(*(VFSP)->vfs_ops->vfs_mount)(VFSP)
#define VFS_UNMOUNT(VFSP, FLG)	(*(VFSP)->vfs_ops->vfs_unmount)(VFSP, FLG)
#define VFS_ROOT(VFSP, VPP)	(*(VFSP)->vfs_ops->vfs_root)(VFSP,VPP)
#define VFS_STATFS(VFSP, SBP)	(*(VFSP)->vfs_ops->vfs_statfs)(VFSP,SBP)
#define VFS_SYNC(VFSP)		(*(VFSP)->vfs_ops->vfs_sync)(VFSP)
#define VFS_VGET(VFSP,VPP,FIDP)	(*(VFSP)->vfs_ops->vfs_vget)(VFSP,VPP,FIDP)
#define VFS_CNTL(VFSP,CMD,ARG,ARGSIZ)	(*(VFSP)->vfs_ops->vfs_cntl)(VFSP,CMD,ARG,ARGSIZ)

/* extended vfs ops follow */
#define VFS_QUOTACTL(VFSP,CMD,ID,ARG) \
	(VFSP)->vfs_gfs->gfs_flags & GFS_EXTOPS ? \
	(*(VFSP)->vfs_ops->vfs_quotactl)(VFSP,CMD,ID,ARG) : EOPNOTSUPP

#endif /* _VOPS */
#endif /* _KERNEL */

#ifdef	_SUN
/* NOTE: statfs structure moved to its own include file, this is for SUN */
#include <sys/statfs.h>
#endif	/* _SUN */

#endif /* _H_VFS */
/* @(#)47	1.11  com/inc/netinet/in.h, bos, bos320 6/19/91 08:01:29 */
/* 
 * COMPONENT_NAME: (SOCKET) Socket services
 * 
 * FUNCTIONS: IN_BADCLASS, IN_CLASSA, IN_CLASSB, IN_CLASSC, IN_CLASSD, 
 *            IN_EXPERIMENTAL, IN_MULTICAST, htonl, htons, ntohl, 
 *            ntohs 
 *
 * ORIGINS: 26 27 
 *
 * (C) COPYRIGHT International Business Machines Corp. 1988, 1989 
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 *
 * Copyright (c) 1982, 1986, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)in.h	7.10 (Berkeley) 6/28/90
 *
 */

#ifndef _H_IN
#define _H_IN

/* number representation macros moved to net/nh.h */
#include <net/nh.h>

/*
 * Constants and structures defined by the internet system,
 * Per RFC 790, September 1981.
 */

/*
 * Protocols
 */
#define	IPPROTO_IP		0		/* dummy for IP */
#define	IPPROTO_ICMP		1		/* control message protocol */
#define	IPPROTO_GGP		3		/* gateway^2 (deprecated) */
#define	IPPROTO_TCP		6		/* tcp */
#define	IPPROTO_EGP		8		/* exterior gateway protocol */
#define	IPPROTO_PUP		12		/* pup */
#define	IPPROTO_UDP		17		/* user datagram protocol */
#define	IPPROTO_IDP		22		/* xns idp */
#define	IPPROTO_TP		29 		/* tp-4 w/ class negotiation */
#define	IPPROTO_EON		80		/* ISO cnlp */

#define	IPPROTO_RAW		255		/* raw IP packet */
#define	IPPROTO_MAX		256


/*
 * Local port number conventions:
 * Ports < IPPORT_RESERVED are reserved for
 * privileged processes (e.g. root).
 * Ports > IPPORT_USERRESERVED are reserved
 * for servers, not necessarily privileged.
 */
#define	IPPORT_RESERVED		1024
#define	IPPORT_USERRESERVED	5000
#define	IPPORT_TIMESERVER	37

/*
 * Internet address (a structure for historical reasons)
 */
struct in_addr {
	u_long s_addr;
};

/*
 * Definitions of bits in internet address integers.
 * On subnets, the decomposition of addresses to host and net parts
 * is done according to subnet mask, not the masks here.
 */
#define	IN_CLASSA(i)		(((long)(i) & 0x80000000) == 0)
#define	IN_CLASSA_NET		0xff000000
#define	IN_CLASSA_NSHIFT	24
#define	IN_CLASSA_HOST		0x00ffffff
#define	IN_CLASSA_MAX		128

#define	IN_CLASSB(i)		(((long)(i) & 0xc0000000) == 0x80000000)
#define	IN_CLASSB_NET		0xffff0000
#define	IN_CLASSB_NSHIFT	16
#define	IN_CLASSB_HOST		0x0000ffff
#define	IN_CLASSB_MAX		65536

#define	IN_CLASSC(i)		(((long)(i) & 0xe0000000) == 0xc0000000)
#define	IN_CLASSC_NET		0xffffff00
#define	IN_CLASSC_NSHIFT	8
#define	IN_CLASSC_HOST		0x000000ff

#define	IN_CLASSD(i)		(((long)(i) & 0xf0000000) == 0xe0000000)
#define	IN_MULTICAST(i)		IN_CLASSD(i)

#define	IN_EXPERIMENTAL(i)	(((long)(i) & 0xe0000000) == 0xe0000000)
#define	IN_BADCLASS(i)		(((long)(i) & 0xf0000000) == 0xf0000000)

#define	INADDR_ANY		(u_long)0x00000000
#define INADDR_LOOPBACK		(u_long)0x7f000001	/* local loopback */
#define	INADDR_BROADCAST	(u_long)0xffffffff	/* must be masked */
#ifndef _KERNEL
#define	INADDR_NONE		0xffffffff		/* -1 return */
#endif

#define	IN_LOOPBACKNET		127			/* official! */

/*
 * Socket address, internet style.
 */
struct sockaddr_in {
	u_char	sin_len;
	u_char	sin_family;
	u_short	sin_port;
	struct	in_addr sin_addr;
	char	sin_zero[8];
};

/*
 * Structure used to describe IP options.
 * Used to store options internally, to pass them to a process,
 * or to restore options retrieved earlier.
 * The ip_dst is used for the first-hop gateway when using a source route
 * (this gets put into the header proper).
 */
struct ip_opts {
	struct	in_addr ip_dst;		/* first hop, 0 w/o src rt */
	char	ip_opts[40];		/* actually variable in size */
};

/*
 * Options for use with [gs]etsockopt at the IP level.
 * First word of comment is data type; bool is stored in int.
 */
#define	IP_OPTIONS	1	/* buf/ip_opts; set/get IP per-packet options */
#define	IP_HDRINCL	2	/* int; header is included with data (raw) */
#define	IP_TOS		3	/* int; IP type of service and precedence */
#define	IP_TTL		4	/* int; IP time to live */
#define	IP_RECVOPTS	5	/* bool; receive all IP options w/datagram */
#define	IP_RECVRETOPTS	6	/* bool; receive IP options for response */
#define	IP_RECVDSTADDR	7	/* bool; receive IP dst addr w/datagram */
#define	IP_RETOPTS	8	/* ip_opts; set/get IP per-packet options */

#ifdef _KERNEL
extern	struct domain inetdomain;
extern	struct protosw inetsw[];
struct	in_addr in_makeaddr();
u_long	in_netof(), in_lnaof();
#endif

#endif /* _H_IN */
/* @(#)43	1.5  com/inc/netdb.h, bos, bos320 6/24/91 16:15:38 */
/* 
 * COMPONENT_NAME: (SOCKET) Socket services
 * 
 * FUNCTIONS: 
 *
 * ORIGINS: 26 27 
 *
 * (C) COPYRIGHT International Business Machines Corp. 1988, 1989 
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 *
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 */


#ifndef _H_NETDB
#define _H_NETDB

#define _PATH_HEQUIV    "/etc/hosts.equiv"
#define _PATH_HOSTS     "/etc/hosts"
#define _PATH_NETWORKS  "/etc/networks"
#define _PATH_PROTOCOLS "/etc/protocols"
#define _PATH_SERVICES  "/etc/services"

/*
 * Structures returned by network
 * data base library.  All addresses
 * are supplied in host order, and
 * returned in network order (suitable
 * for use in system calls).
 */
struct	hostent {
	char	*h_name;	/* official name of host */
	char	**h_aliases;	/* alias list */
	int	h_addrtype;	/* host address type */
	int	h_length;	/* length of address */
	char	**h_addr_list;	/* list of addresses from name server */
#define	h_addr	h_addr_list[0]	/* address, for backward compatiblity */
};

/*
 * Assumption here is that a network number
 * fits in 32 bits -- probably a poor one.
 */
struct	netent {
	char		*n_name;	/* official name of net */
	char		**n_aliases;	/* alias list */
	int		n_addrtype;	/* net address type */
	unsigned long	n_net;		/* network # */
};

struct	servent {
	char	*s_name;	/* official service name */
	char	**s_aliases;	/* alias list */
	int	s_port;		/* port # */
	char	*s_proto;	/* protocol to use */
};

struct	protoent {
	char	*p_name;	/* official protocol name */
	char	**p_aliases;	/* alias list */
	int	p_proto;	/* protocol # */
};
#ifdef _SUN
struct rpcent {
	char	*r_name;		/* name of server for this rpc program */
	char	**r_aliases;	/* alias list */
	int		r_number;		/* rpc program number */
};
#endif

struct hostent	*gethostbyname(), *gethostbyaddr(), *gethostent();
struct netent	*getnetbyname(), *getnetbyaddr(), *getnetent();
struct servent	*getservbyname(), *getservbyport(), *getservent();
struct protoent	*getprotobyname(), *getprotobynumber(), *getprotoent();
#ifdef _SUN
struct rpcent	*getrpcbyname(), *getrpcbynumber(), *getrpcent();
#endif

/*
 * Error return codes from gethostbyname() and gethostbyaddr()
 * (left in extern int h_errno).
 */
extern	int h_errno;

#define	HOST_NOT_FOUND	1 /* Authoritative Answer Host not found */
#define	TRY_AGAIN	2 /* Non-Authoritive Host not found, or SERVERFAIL */
#define	NO_RECOVERY	3 /* Non recoverable errors, FORMERR, REFUSED, NOTIMP */
#define	NO_DATA		4 /* Valid name, no data record of requested type */
#define	NO_ADDRESS	NO_DATA		/* no address, look for MX record */

#endif /* _H_NETDB */
/* @(#)73	1.8  com/inc/sys/un.h, bos, bos320 5/8/91 10:26:47 */
/*
 * COMPONENT_NAME: (SOCKET) Socket services
 *
 * FUNCTIONS:
 *
 * ORIGINS: 26 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1988, 1991
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 *
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	 (#)un.h	7.7 (Berkeley) 6/28/90
 */

#ifndef _H_UN
#define _H_UN 

/*
 * Definitions for UNIX IPC domain.
 */
struct	sockaddr_un {
	u_char	sun_len;		/* sockaddr len including null */
	u_char	sun_family;		/* AF_UNIX */
	char	sun_path[108];		/* path name (gag) */
};

#ifdef _KERNEL
int	unp_discard();
#endif

/* actual length of an initialized sockaddr_un */
#define SUN_LEN(su) \
	(sizeof(*(su)) - sizeof((su)->sun_path) + strlen((su)->sun_path))

#endif /* _H_UN */
/* @(#)05	1.8  com/inc/dirent.h, bos, bos320 6/16/90 00:09:27 */

#ifndef	_H_DIRENT
#define	_H_DIRENT
/*
 * COMPONENT_NAME: SYSLFS - Logical File System
 *
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1989
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */
#include <sys/dir.h>
#endif	/* _H_DIRENT */
/* @(#)51	1.4  com/inc/termio.h, bos, bos320 6/16/90 00:15:06 */

/*
 * COMPONENT_NAME: (sysxtty) System Extension for tty support
 *
 * FUNCTIONS: 
 *
 * ORIGINS: 3, 9, 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1989
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

#include <sys/termio.h>
/* @(#)29	1.13  com/inc/pwd.h, bos, bos320 9/19/91 14:07:49 */
/*
 * COMPONENT_NAME: (CMDOPER) Commands needed for basic system needs
 *
 * FUNCTIONS: none
 *                                                                    
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1989
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */                                                                   

#ifndef _H_PWD
#define _H_PWD
#include <standards.h>
#include <sys/types.h>

/* The POSIX standard requires that certain elements be included in pwd.h. 
 * It also requires that when _POSIX_SOURCE is defined, only those standard
 * specific elements are made available.
 * This header includes all the POSIX required entries.
 */
extern struct passwd *getpwent();

#ifdef _POSIX_SOURCE

#ifdef _NO_PROTO
	extern struct passwd *getpwuid();
	extern struct passwd *getpwnam();
#else
	extern struct passwd *getpwuid(uid_t uid);
	extern struct passwd *getpwnam(const char *name);
#endif /* _NO_PROTO */

struct passwd {
	char	*pw_name;
	char	*pw_passwd;
	uid_t	pw_uid;
	gid_t	pw_gid;
	char    *pw_gecos;
	char	*pw_dir;
	char	*pw_shell;
};
#endif /* _POSIX_SOURCE */

#ifdef _ALL_SOURCE 

#define pw_etc pw_gecos

#endif /* _ALL_SOURCE */ 

#endif /* _H_PWD */
/* @(#)32	1.15  com/inc/sys/wait.h, bos, bos320 7/12/90 15:12:12 */
#ifndef _H_WAIT
#define _H_WAIT
#include <standards.h>


/*
 * COMPONENT_NAME: SYSPROC 
 *
 * ORIGINS: 3, 27 
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1990 
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

#include <sys/types.h>

/*
 * This file holds definitions relevent to the waitpid(), wait(), and wait3()
 * system calls.  The rusage option is only available with the wait3() call.
 * The options field in wait3() and waitpid() determines the behavior of the
 * call, while the process ID field in the waitpid() call determines which
 * group of children to search.
 */

/*
 * POSIX requires that certain values be included in wait.h.  It also
 * requires that when _POSIX_SOURCE is defined only those standard
 * specific values are present.  This header includes all the POSIX
 * required entries.
 */
#ifdef _POSIX_SOURCE

#ifndef	_NONSTD_TYPES
#ifdef _NO_PROTO
extern pid_t wait();
extern pid_t waitpid();
#else
extern pid_t wait(int *stat_loc);
extern pid_t waitpid(pid_t pid, int *stat_loc, int options);
#endif /* _NO_PROTO */
#endif	/* _NONSTD_TYPES */

/*
 * The option field for wait3() and waitpid() is defined as follows:
 * WNOHANG causes the wait to not hang if there are no stopped or terminated
 * processes, rather returning an error indication in this case (pid==0).
 * WUNTRACED indicates that the caller should receive status about untraced
 * children which stop due to signals.  If children are stopped and a wait
 * without this option is done, it is as though they were still running...
 * nothing about them is returned.
 */
#define WNOHANG		0x1	/* dont hang in wait			 */
#define WUNTRACED	0x2	/* tell about stopped, untraced children */

/*
 * Stopped process status.  Returned only for traced children unless requested
 * with the WUNTRACED option bit.  Lower byte gives the reason, next byte is
 * the last signal received, i.e. p->p_cursig.
 */
#define	_W_STOPPED	0x00000040	/* bit set if stopped		*/
#define	_W_SLWTED	0x0000007c	/* value if stopped after load	*/
#define	_W_SEWTED	0x0000007d	/* value if stopped after exec	*/
#define	_W_SFWTED	0x0000007e	/* value if stopped after fork	*/
#define	_W_STRC		0x0000007f	/* value if stopped after trace	*/

/*
 * MACRO defines for application interfacing to waitpid(), wait(), and wait3()
 */
/* evaluates to a non-zero value if status returned for a stopped child	*/
#define	WIFSTOPPED(x)	((x) & _W_STOPPED )
/* evaluates to the number of the signal that caused the child to stop	*/
#define	WSTOPSIG(x)	(int)(WIFSTOPPED(x) ? (((x) >> 8) & 0x7f) : -1)
/* evaluates to a non-zero value if status returned for normal termination */
#define	WIFEXITED(x)	( !((x) & 0xff) )
/* evaluates to the low-order 8 bits of the child exit status	*/
#define	WEXITSTATUS(x)	(int)(WIFEXITED(x) ? (((x) >> 8) & 0xff) : -1)
/* evaluates to a non-zero value if status returned for abnormal termination */
#define	WIFSIGNALED(x)	(  !WIFEXITED(x) && !WIFSTOPPED(x) )
/* evaluates to the number of the signal that caused the child to terminate */
#define	WTERMSIG(x)	(int)(WIFSIGNALED(x) ? ((x) & 0x7f) : -1)

#endif /* _POSIX_SOURCE */

#ifdef _ALL_SOURCE

#ifndef	_NONSTD_TYPES
extern pid_t wait3();
#endif	/* _NONSTD_TYPES */
/*
pid_t
wait3( stat_loc, options, rusage )
int		*stat_loc;	** user location for returned status	**
int		options;	** options to vary function, see wait.h	**
struct rusage	*rusage;	** pointer to child resource usage area	**
*/

#define	W_STOPPED	_W_STOPPED
#define	W_SLWTED 	_W_SLWTED
#define	W_SEWTED 	_W_SEWTED
#define	W_SFWTED 	_W_SFWTED
#define	W_STRC    	_W_STRC

/* define for BSD compatibility					*/

#define	WSTOPPED	_W_STOPPED

#ifdef _BSD

#include <sys/m_wait.h>

#endif /* _BSD */

#endif /* _ALL_SOURCE */
#endif /* _H_WAIT */
/* @(#)90	1.9  com/inc/sys/select.h, bos, bos320 6/16/90 00:35:15 */
#ifndef _H_SELECT
#define _H_SELECT
/*
 * COMPONENT_NAME: (SYSIOS) Select system call header file
 *
 * ORIGINS: 26, 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1989
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 *
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifdef _NO_PROTO
extern	int	select();

#else	/* _NO_PROTO */
#include <sys/time.h>

extern	int select(
	unsigned long nfdsmsgs,	/* #file descriptors/message queues */
	void *readlist,		/* file descriptors to check for read */
	void *writelist,	/* file descriptors to check for write */
	void *exceptlist,	/* file descriptors to check for exceptions */
	struct timeval *timeout);	/* length of time to wait for events */
#endif /* _NO_PROTO */

/*
 * Timeout values.
 */
#define	NO_TIMEOUT	0	/* don't wait for a response		*/
#define	INF_TIMEOUT	-1	/* wait until a response is received	*/

/*
 * Number of file descriptors (bits).
 * Apply to nfdsmsgs parameter of SELECT system
 * call &/or return value from SELECT.
 */
#define NFDS(x)		((x) & 0x0000FFFF)

/*
 * Lower half of word.
 * Used in hashing algorithm for devsel_hash chain.
 */
#define	LOW_HALF(x)	NFDS(x)

/*
 * Number of message queues.
 * Apply to nfdsmsgs parameter of SELECT system
 * call &/or return value from SELECT.
 */
#define NMSGS(x)	(((x) >> 16) & 0x0000FFFF)

/*
 * Upper half of word.
 * Used in hashing algorithm for devsel_hash chain.
 */
#define	HIGH_HALF(x)	NMSGS(x)

/*
 * Macro used to define a sellist structure
 * for the SELECT system call.
 */
#define	SELLIST(F,M)		\
struct				\
{				\
	unsigned int   fdsmask[F];	\
	int   msgids[M];	\
}

/*
 * Select uses bit masks of file descriptors.
 * These macros manipulate such bit fields.
 * FD_SETSIZE may be defined by the user to the maximum valued file
 * descriptor to be selected; the default here should be >= OPEN_MAX
 */
#ifndef	FD_SETSIZE
#define	FD_SETSIZE	2048
#endif

typedef unsigned int	fd_mask;
#define NFDBITS	(sizeof(fd_mask) * NBBY)	/* bits per mask */
#ifndef howmany
#define	howmany(x, y)	(((x)+((y)-1))/(y))
#endif

typedef	struct fd_set {
	fd_mask	fds_bits[howmany(FD_SETSIZE, NFDBITS)];
} fd_set;

#define	FD_SET(n, p)	((p)->fds_bits[(n)/NFDBITS] |= (1 << ((n) % NFDBITS)))
#define	FD_CLR(n, p)	((p)->fds_bits[(n)/NFDBITS] &= ~(1 << ((n) % NFDBITS)))
#define	FD_ISSET(n, p)	((p)->fds_bits[(n)/NFDBITS] & (1 << ((n) % NFDBITS)))
#define FD_ZERO(p)	bzero((char *)(p), sizeof(*(p)))

#endif  /* _H_SELECT */
#ifndef _H_STATFS
#define _H_STATFS

/* @(#)17	1.6  com/inc/sys/statfs.h, bos, bos320 6/16/90 00:36:39 */
/*
 * SYSLFS: Logical Filesystem
 *
 * FUNCTIONS: statfs header
 *
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1988
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

/*
 * statfs.h - statfs system call return structure
 */

/*
 * file system statistics
 * NOTE: f_version and f_nlsdirtype are UNUSED NOW, but should be set to 0!
 * NOTE: all other reserved fields should be cleared by the vfs implementation
 */
struct statfs {
	long f_version;		/* version/type of statfs, 0 for now */
	long f_type;		/* type of info, zero for now */
	long f_bsize;		/* fundamental file system block size */
	long f_blocks;		/* total data blocks in file system */
	long f_bfree;		/* free block in fs */
	long f_bavail;		/* free blocks avail to non-superuser */
	long f_files;		/* total file nodes in file system */
	long f_ffree;		/* free file nodes in fs */
	fsid_t f_fsid;		/* file system id */
	long f_vfstype;		/* what type of vfs this is */
	long f_nlsdirtype;	/* reserved for NLS dirs later, set to 0 now! */
	long f_vfsnumber;	/* vfs indentifier number */
	long f_vfsoff;		/* reserved, for vfs specific data offset */
	long f_vfslen;		/* reserved, for len of vfs specific data */
	long f_vfsvers;		/* reserved, for vers of vfs specific data */
	char f_fname[32];	/* file system name (usually mount pt.) */
	char f_fpack[32];	/* file system pack name */
	long f_name_max;	/* maximum component name length for posix */
};

#endif /* _H_STATFS */
/*------------------------------------------------------------------------
--                                                                      --
--          H E L I O S   I N P U T / O U T P U T   S E R V E R         --
--          ---------------------------------------------------         --
--                                                                      --
--               Copyright (C) 1987, Perihelion Software Ltd.           --
--                            All Rights Reserved.                      --
--                                                                      --
--  sunlocal.h                                                          --
--                                                                      --
--  Author:  DJCH (Bath University), BLV                                --
--                                                                      --
------------------------------------------------------------------------*/
/* SccsId: 3.8 28/3/90\ Copyright (C) 1987, Perihelion Software Ltd.        */

#ifndef Files_Module
extern struct stat searchbuffer;
#endif

#if ANSI_prototypes
extern void socket_read (int, BYTE *, int, char *);
extern void socket_write (int, BYTE *, int, char *);
#else
extern void socket_write();
extern void socket_read();
#endif

#ifdef Local_Module
#if ANSI_prototypes
static  void pipe_broken(char *);
#else
static  void pipe_broken();
#endif
#endif

#ifdef CLK_TCK
#undef CLK_TCK
#endif

#if !(i486V4 || SUN4 || SUN3 || RS6000 || HP9000)
#ifndef __clock_t
#define __clock_t 1
typedef long clock_t;
#endif
#endif

#define CLK_TCK       1	
#define clock() time(NULL)

#ifndef SEEK_SET
#define SEEK_SET	0
#endif

extern char *sys_errlist[];
extern int  sys_nerr;

/**
*** These define the interface between the I/O Server and the
*** server window program
**/ 
#define FUNCTION_CODE	0xFF
#define WINDOW_SIZE     0x01
#define WINDOW_KILL     0x02
#define WINDOW_MESS	0x03
#define WINDOW_PANEL    0x04
#define WINDOW_DIED     0x05

/* third byte for debug function codes */

#define WIN_MEMORY      0x01
#define WIN_RECONF      0x02
#define WIN_MESSAGES	0x03
#define WIN_SEARCH	0x04
#define WIN_OPEN	0x05
#define WIN_CLOSE	0x06
#define IOWIN_NAME	0x07
#define WIN_READ	0x08
#define WIN_BOOT	0x09
#define WIN_KEYBOARD	0x0A
#define WIN_INIT	0x0B
#define WIN_WRITE	0x0C
#define WIN_QUIT	0x0D
#define WIN_GRAPHICS	0x0E
#define WIN_TIMEOUT     0x0F
#define WIN_OPENREPLY   0x10
#define WIN_FILEIO      0x11
#define WIN_DELETE      0x12
#define WIN_DIRECTORY   0x13
#ifdef NEVER
#define WIN_COM         0x15
#define WIN_HARDDISK    0x16
#endif
#define WIN_ALL		0x14
/* Not needed : nopop, listall */
/* ALL and logger are separate */

#define WIN_REBOOT	0x21
#define WIN_DEBUGGER	0x22
#define WIN_STATUS	0x23
#define WIN_EXIT	0x24
#define WIN_LOG_FILE	0x25
#define WIN_LOG_SCREEN	0x26
#define WIN_LOG_BOTH	0x27
#define WIN_DEBUG	0x28

#define WIN_OFF         0x00
#define WIN_ON		0x01
/*------------------------------------------------------------------------
--                                                                      --
--         H E L I O S   I N P U T / O U T P U T   S E R V E R          --
--         ---------------------------------------------------          --
--                                                                      --
--              Copyright (C) 1987, Perihelion Software Ltd.            --
--                         All Rights Reserved.                         --
--                                                                      --
--      Defines.h                                                       --
--                                                                      --
--      Author:  BLV 15/5/88                                            --
--                                                                      --
------------------------------------------------------------------------*/

/* RcsId: $Id: defines.h,v 1.20 1994/06/29 13:42:25 tony Exp $ */
/* Copyright (C) 1987, Perihelion Software Ltd.       			*/

/**
*** This header file defines the facilities available with the various
*** configurations on which the input/output server runs. The server should
*** be made with one and only one of the following strings #define'd :
*** ST, PC, AMIGA, SUN, TRIPOS, VAX, OS2, OS9, XENIX (your own additions)
**/

#ifdef ST
#undef ST
#define ST 1
#else
#define ST 0
#endif

#ifdef PC
#undef PC
#define PC 1
#else
#define PC 0
#endif

#ifdef NECPC
#undef NECPC
#define NECPC 1
#else
#define NECPC 0
#endif

#ifdef AMIGA
#undef AMIGA       /* Lattice C defines AMIGA as well, a couple of times */
#undef AMIGA
#define AMIGA 1
#else
#define AMIGA 0
#endif

#ifdef TRIPOS
#undef TRIPOS
#define TRIPOS 1
#else
#define TRIPOS 0
#endif

#ifdef SUN3
#undef SUN3
#define SUN 1
#define SUN3 1
#define UNIX 1
#else
#define SUN3 0
#endif

#ifdef SOLARIS
#undef SOLARIS
#define SOLARIS	1
#ifndef SUN4
#define SUN4	1
#define UNIX	1
#endif
#else
#define SOLARIS 0
#endif

#ifdef SUN4
#undef SUN4
#define SUN 1
#define SUN4 1
#define UNIX 1
#else
#define SUN4 0
#endif

#ifdef SUN386
#undef SUN386
#define SUN 1
#define SUN386 1
#define UNIX 1
#else
#define SUN386 0
#endif

#ifdef SM90
#undef SM90
#define SM90 1
#define UNIX 1
#else
#define SM90 0
#endif

#ifdef TR5
#undef TR5
#define TR5 1
#define UNIX 1
#else
#define TR5 0
#endif

#ifdef i486V4
#undef i486V4
#define i486V4 1
#define UNIX 1
#else
#define i486V4 0
#endif

#ifdef SCOUNIX
#undef SCOUNIX
#define SCOUNIX 1
#define UNIX 1
#define USE_sendto
#define USE_recvfrom
#else
#define SCOUNIX 0
#endif

#ifdef ARMBSD
#undef ARMBSD
#define ARMBSD 1
#define UNIX 1
#else
#define ARMBSD 0
#endif

#ifdef MEIKORTE
#undef MEIKORTE
#define MEIKORTE 1
#define UNIX 1       /* for now */
#else
#define MEIKORTE 0
#endif

                /* Bleistein-Rohde Systemtechnik GmbH Port */
                /* to a 386 box */
#ifdef UNIX386
#undef UNIX386
#define UNIX386 1
#define UNIX    1
#else
#define UNIX386 0
#endif

/* define for a 386 box running interactive 386/ix */
#ifdef IUNIX386
#undef IUNIX386
#define IUNIX386 1
#define UNIX    1
#else
#define IUNIX386 0
#endif

#ifndef SUN
#define SUN 0
#else
#endif

#ifdef RS6000
#undef RS6000
#define RS6000 1
#define UNIX 1
#else
#define RS6000 0
#endif

#ifdef HP9000
#undef HP9000
#define HP9000 1
#define UNIX 1
#else
#define HP9000 0
#endif

#ifndef UNIX
#define UNIX 0
#endif

#ifdef VAX
#undef VAX
#define VAX 1
#else
#define VAX 0
#endif


#ifdef OS2
#undef OS2
#define OS2 1
#else
#define OS2 0
#endif


#ifdef OS9
#undef OS9
#define OS9 1
#else
#define OS9 0
#endif


#ifdef XENIX
#undef XENIX
#define XENIX 1
#else
#define XENIX 0
#endif

#ifdef APOLLO
#undef APOLLO
#define APOLLO 1
#else
#define APOLLO 0
#endif

#ifdef FLEXOS
#undef FLEXOS
#define FLEXOS 1
#else
#define FLEXOS 0
#endif

#ifdef MAC
#undef MAC
#define MAC 1
#else
#define MAC 0
#endif

#ifdef MSWINDOWS
#undef MSWINDOWS
#define MSWINDOWS 1
#else
#define MSWINDOWS 0
#endif

	/* The I/O Server running under Helios in conjunction with	*/
	/* native networks, primarily a debugging tool.			*/
#ifdef HELIOS
#undef HELIOS
#define HELIOS 1
#else
#define HELIOS 0
#endif

#ifdef ETC_DIR
#undef ETC_DIR
#define ETC_DIR 1
#else
#define ETC_DIR	0
#endif

#if ST
/**
*** The following lines define the hardware used. Only one of the #define's
*** should have a 1. In the main code I use tests like #if ST or
*** #if (ST || PC) to provide all conditional compilation.
***
*** This determines whether the host machine has the same byte ordering as
*** a transputer or a different one.
**/
#define swapping_needed              1
/**
*** These lines specify which of the optional devices are supported
**/
#define gem_supported                0
#define mouse_supported              1
#define keyboard_supported           1
/**
*** The main use of the RS232 is for use with kermit and terminal emulator
*** programs, so that you can access other machines without leaving Helios.
*** It is not intended as a device which can be used for arbitrary networking.
**/
#define RS232_supported              1
#define Centronics_supported         1
#define Midi_supported               0
/**
*** For some machines there is a separate printer device, which might map
*** onto either the parallel or the serial port or which might send data
*** to a printer spooler of some sort. On something like a Sun or Vax you
*** should probably just support this, and not a separate Centronics device.
**/
#define Printer_supported            1
/**
*** Some machines may have ethernet boards accessible from Helios
**/
#define Ether_supported              0
/**
*** A rawdisk device allows you to run the Helios filing system on the
*** transputer. It requires reads and writes in terms of disk sectors
*** rather than files and directories.
**/
#define Rawdisk_supported            1
/**
*** The /x device is the standard Helios X server
**/
#define X_supported                  0
/**
*** The /NetworkController device provides reset and link configuration
*** support, if these have to be provided by the I/O Server rather than
*** by the root transputer (the latter is greatly preferred).
**/
#define Network_supported            0
/**
*** If the machine has multiple drives which must be treated specially and which
*** are readily distinguished from normal subdirectories, the following should
*** be defined.
**/
#define drives_are_special           1
/**
*** If the machine has floppy disks then the server provides limited support
*** for the special errors generated. Unfortunately much of this is hardware
*** dependant.
**/
#define floppies_available           1
/**
*** If it is desirable for the server to do its own memory allocation then the
*** following should be defined.
**/
#define use_own_memory_management    1
/**
*** Redirecting stdout under the Helios shell involves a file being opened twice
*** for write-only mode. Many systems including TOS do not allow this.
*** To get around this, Helios allows servers to close streams at any time,
*** so that I can keep track of which files are open and close the stream if
*** a get an open request for a file which is already open.
**/
#define files_cannot_be_opened_twice 1
/**
*** If it is important that the Server does not hog the entire the machine,
*** denying its use to other users or tasks, then the following should be
*** defined. It does mean more work in porting the server !
**/
#define multi_tasking                0
/**
*** There is a lot of demand for some interaction facility between Helios
*** programs and programs or routines on the Host machine. Although I do not
*** like the idea, I have to support it. Hence there is a special device,
*** /ST or /PC or whatever to which messages can be sent.
**/
#define interaction_supported        1
/**
*** It is rather desirable to have the host name available as a string.
*** At the moment though, this is only used in conjunction with
*** interaction_supported above.
**/
#define machine_name                 "st"
/**
*** If the IO Server supports multiple windows, you can define the following.
*** Note that multiple_windows should be set on nearly all machines,
*** implemented either as real windows or as pseudo-windows with a
*** hot-key switching mechanism.
**/
#define multiple_windows             1
/**
*** I provide a general-purpose ANSI screen emulator, which can be
*** incorporated into the main server fairly easily. Define the following
*** if you want it(recommended)
**/
#define use_ANSI_emulator            1
/**
*** The built-in debugger is optional, as a way of saving some memory.
*** Anyway, it is of little use to the man in the street.
**/
#define debugger_incorporated        1
/**
*** If the compiler supports ANSI-style function prototypes then this
*** should be defined.
**/
#define ANSI_prototypes              0
/**
*** If the Server should use the routines in the linkio.c module, this
*** should be defined.
**/
#define Use_linkio                   0
/**
*** If the Server should use the hosts socket/internet support (UNIX) this
*** should be defined.
**/
#define internet_supported	     0
#endif

#if PC
/**
*** The PC I/O Server comes in two flavours, a DOS one and a Windows one.
*** The latter is controlled by an additional -DMSWINDOWS
**/
#define swapping_needed              0
#define Midi_supported               0
#define Ether_supported              1
#define Rawdisk_supported            1
#define use_own_memory_management    1
#ifdef HUNTROM
#define Romdisk_supported            1
#endif
#define X_supported                  0
#define RS232_supported              1
#define Network_supported            0
#define drives_are_special           1
#define floppies_available           1
#define files_cannot_be_opened_twice 0
#define multi_tasking                0
#define machine_name                 "pc"
#define multiple_windows             1
#define use_ANSI_emulator            1
#define ANSI_prototypes              1
#define Use_linkio                   1
#define internet_supported	     0
#ifdef SMALL
#define gem_supported                0
#define debugger_incorporated        0
#else
#define gem_supported                1
#define debugger_incorporated        1
#endif
#if MSWINDOWS
#undef gem_supported
#define gem_supported		     0
#define keyboard_supported           0
#define mouse_supported              0
#define Centronics_supported         0
#define Printer_supported            0
#define interaction_supported        0
#else
#define keyboard_supported           1
#define mouse_supported              1
#define Centronics_supported         1
#define Printer_supported            1
#define interaction_supported        1
#endif
#endif

#if NECPC
#define swapping_needed              0
#define gem_supported                0
#define mouse_supported              1
#define keyboard_supported           1
#define RS232_supported              0
#define Centronics_supported         0
#define Printer_supported            0
#define Midi_supported               0
#define Ether_supported              0
#define Rawdisk_supported            0
#define X_supported                  0
#define Network_supported            0
#define drives_are_special           1
#define floppies_available           1
#define use_own_memory_management    1
#define files_cannot_be_opened_twice 0
#define multi_tasking                0
#define interaction_supported        1
#define machine_name                 "pc"
#define multiple_windows             1
#define use_ANSI_emulator            1
#define debugger_incorporated        1
#define internet_supported	     0
#define ANSI_prototypes              1
#define Use_linkio                   1
#endif

#if AMIGA
#define swapping_needed              1
#define gem_supported                0
#define mouse_supported              0
#define keyboard_supported           0
#define RS232_supported              0
#define Centronics_supported         0
#define Printer_supported            0
#define Midi_supported               0
#define Ether_supported              0
#define Rawdisk_supported            0
#define X_supported                  0
#define Network_supported            0
#define drives_are_special           1
#define floppies_available           0
#define use_own_memory_management    0
#define files_cannot_be_opened_twice 0
#define multi_tasking                0
#define interaction_supported        0
#define machine_name                 "amiga"
#define multiple_windows             1
#define use_ANSI_emulator            0
#define debugger_incorporated        1
#define internet_supported	     0
#define ANSI_prototypes              1
#define Use_linkio                   0
#endif

#if TRIPOS
#define swapping_needed              1
#define gem_supported                0
#define mouse_supported              0
#define keyboard_supported           0
#define RS232_supported              0
#define Centronics_supported         0
#define Printer_supported            0
#define Midi_supported               0
#define Ether_supported              0
#define Rawdisk_supported            0
#define X_supported                  0
#define Network_supported            0
#define drives_are_special           0
#define floppies_available           0
#define use_own_memory_management    0
#define files_cannot_be_opened_twice 0
#define multi_tasking                0
#define interaction_supported        0
#define machine_name                 "tripos"
#define multiple_windows             0
#define use_ANSI_emulator            0
#define debugger_incorporated        1
#define internet_supported	     0
#define ANSI_prototypes              1
#define Use_linkio                   0
#endif

#if UNIX 
#if (SUN3 || SUN4 || SM90 || TR5 || RS6000 || HP9000)
#define swapping_needed              1
#else
#define swapping_needed              0
#endif

#if (SUN3 || SUN4 || RS6000 || HP9000 || SCOUNIX)
#define use_separate_windows         1
#else
#define use_separate_windows         0
#endif

#define gem_supported                0
#define RS232_supported              0
#define Centronics_supported         0
#define Printer_supported            0
#define Midi_supported               0
#define Ether_supported              0
#define Rawdisk_supported            0
#define drives_are_special           0
#define floppies_available           0
#define use_own_memory_management    0
#define files_cannot_be_opened_twice 0
#define Use_linkio                   1
#define interaction_supported        0
#define multiple_windows             1
#define use_ANSI_emulator            1
#define debugger_incorporated        1
#if ARMBSD
#define internet_supported	     0
#else
#define internet_supported	     1
#endif
#if (0)
#define X_supported                  1
#else
#define X_supported                  0
#endif

#if (MEIKORTE)
#define multi_tasking                0
#define Network_supported            1
#else
#define Network_supported            0
#define multi_tasking                1
#endif

#if (RS6000 || HP9000 || SOLARIS || __GNU__ > 0)
#define ANSI_prototypes              1
#else
#define ANSI_prototypes              0
#endif

#if (SM90)
#define mouse_supported              1
#define keyboard_supported           1
#else
#define mouse_supported              0
#define keyboard_supported           0
#endif

#endif  /* UNIX */

#if VAX
#define swapping_needed              0
#define gem_supported                0
#define mouse_supported              0
#define keyboard_supported           0
#define RS232_supported              0
#define Centronics_supported         0
#define Printer_supported            0
#define Midi_supported               0
#define Ether_supported              0
#define Rawdisk_supported            0
#define X_supported                  0
#define Network_supported            0
#define drives_are_special           0
#define floppies_available           0
#define use_own_memory_management    0
#define files_cannot_be_opened_twice 0
#define multi_tasking                0
#define interaction_supported        0
#define machine_name                 "vax"
#define multiple_windows             0
#define use_ANSI_emulator            0
#define debugger_incorporated        1
#define internet_supported	     0
#define ANSI_prototypes              0
#define Use_linkio                   0
#endif


#if OS2
#define swapping_needed              0
#define gem_supported                0
#define mouse_supported              0
#define keyboard_supported           0
#define RS232_supported              0
#define Centronics_supported         0
#define Printer_supported            0
#define Midi_supported               0
#define Ether_supported              0
#define Rawdisk_supported            0
#define X_supported                  0
#define Network_supported            0
#define drives_are_special           0
#define floppies_available           0
#define use_own_memory_management    0
#define files_cannot_be_opened_twice 0
#define multi_tasking                0
#define interaction_supported        0
#define machine_name                 "OS2"
#define multiple_windows             0
#define use_ANSI_emulator            0
#define debugger_incorporated        1
#define internet_supported	     0
#define ANSI_prototypes              0
#define Use_linkio                   0
#endif

#if OS9
#define swapping_needed              0
#define gem_supported                0
#define mouse_supported              0
#define keyboard_supported           0
#define RS232_supported              0
#define Centronics_supported         0
#define Printer_supported            0
#define Midi_supported               0
#define Ether_supported              0
#define Rawdisk_supported            0
#define X_supported                  0
#define Network_supported            0
#define drives_are_special           0
#define floppies_available           0
#define use_own_memory_management    0
#define files_cannot_be_opened_twice 0
#define multi_tasking                0
#define interaction_supported        0
#define machine_name                 "OS9"
#define multiple_windows             0
#define use_ANSI_emulator            0
#define debugger_incorporated        1
#define internet_supported	     0
#define ANSI_prototypes              0
#define Use_linkio                   0
#endif

#if XENIX
#define swapping_needed              0
#define gem_supported                0
#define mouse_supported              0
#define keyboard_supported           0
#define RS232_supported              0
#define Centronics_supported         0
#define Printer_supported            0
#define Midi_supported               0
#define Ether_supported              0
#define Rawdisk_supported            0
#define X_supported                  0
#define Network_supported            0
#define drives_are_special           0
#define floppies_available           0
#define use_own_memory_management    0
#define files_cannot_be_opened_twice 0
#define multi_tasking                0
#define interaction_supported        0
#define machine_name                 "XENIX"
#define multiple_windows             0
#define use_ANSI_emulator            0
#define debugger_incorporated        1
#define internet_supported	     0
#define ANSI_prototypes              0
#define Use_linkio                   0
#endif

#if APOLLO
#define swapping_needed              0
#define gem_supported                0
#define mouse_supported              0
#define keyboard_supported           0
#define RS232_supported              0
#define Centronics_supported         0
#define Printer_supported            0
#define Midi_supported               0
#define Ether_supported              0
#define Rawdisk_supported            0
#define X_supported                  0
#define Network_supported            0
#define drives_are_special           0
#define floppies_available           0
#define use_own_memory_management    0
#define files_cannot_be_opened_twice 0
#define multi_tasking                0
#define interaction_supported        0
#define machine_name                 "apollo"
#define multiple_windows             0
#define use_ANSI_emulator            0
#define debugger_incorporated        1
#define internet_supported	     0
#define ANSI_prototypes              0
#define Use_linkio                   0
#endif

#if FLEXOS
#define swapping_needed              0
#define gem_supported                0
#define mouse_supported              0
#define keyboard_supported           0
#define RS232_supported              0
#define Centronics_supported         0
#define Printer_supported            0
#define Midi_supported               0
#define Ether_supported              0
#define Rawdisk_supported            0
#define X_supported                  0
#define Network_supported            0
#define drives_are_special           0
#define floppies_available           0
#define use_own_memory_management    0
#define files_cannot_be_opened_twice 0
#define multi_tasking                0
#define interaction_supported        0
#define machine_name                 "flexos"
#define multiple_windows             0
#define use_ANSI_emulator            0
#define debugger_incorporated        1
#define internet_supported	     0
#define ANSI_prototypes              0
#define Use_linkio                   0
#endif

#if MAC
#define swapping_needed              1
#define gem_supported                0
#define mouse_supported              0
#define keyboard_supported           0
#define RS232_supported              0
#define Centronics_supported         0
#define Printer_supported            0
#define Midi_supported               0
#define Ether_supported              0
#define Rawdisk_supported            0
#define X_supported                  0
#define Network_supported            0
#define interrupts_use_clock         0
#define drives_are_special           1
#define floppies_available           0
#define use_own_memory_management    0
#define files_cannot_be_opened_twice 1
#define limit_on_files_open          20
#define multi_tasking                0
#define interaction_supported        0
#define machine_name                 "mac"
#define multiple_windows             0
#define use_ANSI_emulator            0
#define debugger_incorporated        1
#define internet_supported	     0
#define ANSI_prototypes              1
#endif

#if HELIOS
#define swapping_needed              0
#define gem_supported                0
#define mouse_supported              0
#define keyboard_supported           0
#define RS232_supported              0
#define Centronics_supported         0
#define Printer_supported            0
#define Midi_supported               0
#define Ether_supported              0
#define Rawdisk_supported            0
#define X_supported                  0
#define Network_supported            1
#define interrupts_use_clock         0
#define drives_are_special           0
#define floppies_available           0
#define use_own_memory_management    0
#define files_cannot_be_opened_twice 0
#define limit_on_files_open          20
#define multi_tasking                0
#define interaction_supported        0
#define machine_name                 "helios"
#define multiple_windows             1
#define use_ANSI_emulator            0
#define debugger_incorporated        1
#define internet_supported	     0
#define ANSI_prototypes              1
#define Use_linkio                   1
#endif

/**
*** Here I worry about some missing entries in the defines table
**/

#ifndef swapping_needed
To be or not to be, that is the question...

You must either define one of the machines, or extend this file to include
yours.
#endif

#ifndef gem_supported
#define gem_supported                0
#endif
#ifndef mouse_supported
#define mouse_supported              0
#endif
#ifndef keyboard_supported
#define keyboard_supported           0
#endif
#ifndef RS232_supported
#define RS232_supported              0
#endif
#ifndef Centronics_supported
#define Centronics_supported         0
#endif
#ifndef Printer_supported
#define Printer_supported            0
#endif
#ifndef Midi_supported
#define Midi_supported               0
#endif
#ifndef Ether_supported
#define Ether_supported              0
#endif
#ifndef Rawdisk_supported
#define Rawdisk_supported            0
#endif
#ifndef X_supported
#define X_supported                  0
#endif
#ifndef Network_supported
#define Network_supported            0
#endif
#ifndef drives_are_special
#define drives_are_special           0
#endif
#ifndef floppies_available
#define floppies_available           0
#endif
#ifndef use_own_memory_management
#define use_own_memory_management    0
#endif
#ifndef files_cannot_be_opened_twice
#define files_cannot_be_opened_twice 0
#endif
#ifndef multi_tasking
#define multi_tasking                0
#endif
#ifndef interaction_supported
#define interaction_supported        0
#endif
#ifndef machine_name
#define machine_name                 "Host"
#endif
#ifndef multiple_windows
#define multiple_windows             0
#endif
#ifndef use_ANSI_emulator
#define use_ANSI_emulator            0
#endif
#ifndef debugger_incorporated
#define debugger_incorporated        1
#endif
#ifndef ANSI_prototypes
#define ANSI_prototypes              0
#endif
#ifndef Use_linkio
#define Use_linkio                   0
#endif
#ifndef internet_supported
#define internet_supported	     0
#endif
#ifndef Romdisk_supported
#define Romdisk_supported	     0
#endif

/**
*** It is useful for me to know whether any of the communication ports are
*** supported, because they share code.
**/
#if (RS232_supported || Centronics_supported || Printer_supported || Midi_supported)
#define Ports_used 1
#else
#define Ports_used 0
#endif

/**
*** Now I need to know additional details about certain devices
**/

#if Centronics_supported
/**
*** Is it possible to read from the Centronics port as well as write to it ?
**/
#if (ST || PC)
#define Centronics_readable 0
#else
#define Centronics_readable 1
#endif

/**
*** Is there always a Centronics port, or is it optional ?
**/

#if (ST)
#define Centronics_Always_Available 1
#else
#define Centronics_Always_Available 0
#endif

#endif

#if RS232_supported

#if (ST)
#define RS232_Always_Available 1
#else
#define RS232_Always_Available 0
#endif

#endif

#if Printer_supported

#if (ST)
#define Printer_Always_Available 1
#else
#define Printer_Always_Available 0
#endif

#endif

#if Midi_supported

#if (0)
#define Midi_Always_Available 1
#else
#define Midi_Always_Available 0
#endif

#endif


#if gem_supported

#if 0
#define Gem_Always_Available 1
#else
#define Gem_Always_Available 0
#endif

#endif

#if mouse_supported

#if (ST)
#define Mouse_Always_Available 1
#else
#define Mouse_Always_Available 0
#endif

#endif

#if keyboard_supported

#if (ST)
#define Keyboard_Always_Available 1
#else
#define Keyboard_Always_Available 0
#endif

#endif

#ifndef Gem_Always_Available
#define Gem_Always_Available 0
#endif

#ifndef RS232_Always_Available
#define RS232_Always_Available 0
#endif

#ifndef Centronics_Always_Available
#define Centronics_Always_Available 0
#endif

#ifndef Centronics_readable
#define Centronics_readable 0
#endif

#ifndef Printer_Always_Available
#define Printer_Always_Available 0
#endif

#ifndef Midi_Always_Available
#define Midi_Always_Available 0
#endif

#ifndef Mouse_Always_Available
#define Mouse_Always_Available 0
#endif

#ifndef Keyboard_Always_Available
#define Keyboard_Always_Available 0
#endif


/*------------------------------------------------------------------------
--                                                                      --
--          H E L I O S   I N P U T / O U T P U T   S E R V E R         --
--          ---------------------------------------------------         --
--                                                                      --
--             Copyright (C) 1989, Perihelion Software Ltd.             --
--                        All Rights Reserved.                          --
--   protocol.h                                                         --
--                                                                      --
--    A header file defining the Helios protocols used by the Server.   --
--    Based on the Helios header files in /helios/include/syslib.h,     --
--    but modified to cope with 16-bit compilers.                       --
--                                                                      --
--    Author:  BLV 21/1/89                                              --
--                                                                      --
------------------------------------------------------------------------*/
/* RcsId: $Id: protocol.h,v 1.11 1994/06/29 13:42:25 tony Exp $ */
/* Copyright (C) 1989, Perihelion Software Ltd.        			*/

/*------------------------------------------------------------------------
--                                                                      --
-- helios.h                                                             --
--                                                                      --
------------------------------------------------------------------------*/

/* standard type definitions */

#if TR5
	/* 64 bit longs I assume */
typedef  int            WORD    ;       /* a machine word, 32 bits      */
typedef  unsigned int   UWORD   ;       /* a machine word, 32 bits      */
typedef  WORD           INT     ;       /* a synonym                    */
typedef  WORD           word    ;       /* another synonym              */
typedef  WORD           Offset  ;       /* and another                  */

#else

#if (!MSWINDOWS)
	/* MSWINDOWS already defines WORD and BYTE... */
typedef  long           WORD    ; 
#endif

typedef  unsigned long  UWORD   ;       /* a machine word, 32 bits      */
typedef  long           INT     ;       /* a synonym                    */
typedef  long           word    ;       /* another synonym              */
typedef  long           Offset  ;       /* and another                  */
#endif

typedef  short int      SHORT   ;       /* a 16 bit word                */
typedef  unsigned short USHORT  ;       /* an unsigned 16 bit value     */

#if (!MSWINDOWS)
typedef  char		BYTE;
#endif
typedef  char           byte    ;       /* a synonym                    */
typedef  unsigned char  UBYTE   ;       /* an unsigned byte             */
typedef  char           *STRING ;       /* character string             */
typedef  char           *string ;       /* synonym                      */
typedef  word           bool    ;       /* boolean value                */

typedef  void           (*VoidFnPtr)(); /* pointer to void function     */
typedef  word           (*WordFnPtr)(); /* pointer to word function     */

#define PUBLIC          extern          /* an exported symbol           */
#define PRIVATE         static          /* an unexported symbol         */
#define FORWARD         extern          /* forward proc reference       */


/* Syntactic enrichment... */

#define forever         for(;;)
#define unless(x)       if(!(x))
#define until(x)        while(!(x))
#define elif(x)         else if(x)

#ifndef TRUE
#define TRUE            1l
#endif
#define true            1l
#ifndef FALSE
#define FALSE           0l
#endif
#define false           0l
#define Variable        1
#define MinInt          0x80000000L

#define MemStart  MinInt+0x70L
#define LoadBase  (MinInt+0x1000L)

#define Null(_type) ((_type *)NULL)

#define NameMax   32
#define c_dirchar '/'                   /* Helios directory separator */

#define OneSec           1000000L       /* one second in micro-seconds */

/* end of iohelios.h */

/*------------------------------------------------------------------------
--                                                                      --
-- ioattrib.h                                                           --
--                                                                      --
------------------------------------------------------------------------*/

typedef struct Attributes {
                            word  Input;
                            word  Output;
                            word  Control;
                            word  Local;
#if swapping_needed
                            short Time;
                            short Min;
#else
                            short Min;
                            short Time;
#endif
} Attributes;

typedef word Attribute;

#define ConsoleEcho           0x00000007L
#define ConsoleIgnoreBreak    0x00000100L
#define ConsoleBreakInterrupt 0x00000200L
#define ConsolePause          0x00000400L
#define ConsoleRawInput       0x0000000bL
#define ConsoleRawOutput      0x00000101L

#define RS232_IgnPar          0x00000800L
#define RS232_ParMrk          0x00001000L
#define RS232_InPck           0x00002000L
#define RS232_IXON            0x00004000L
#define RS232_IXOFF           0x00008000L
#define RS232_Istrip          0x00010000L
#define RS232_IgnoreBreak     0x00000100L
#define RS232_BreakInterrupt  0x00000200L
#define RS232_Cstopb          0x00000102L
#define RS232_Cread           0x00000202L
#define RS232_ParEnb          0x00000402L
#define RS232_ParOdd          0x00000802L
#define RS232_HupCl           0x00001002L
#define RS232_CLocal          0x00002002L
#define RS232_Csize           0x0003C000L   /* Mask for the sizes */
#define RS232_Csize_5         0x00004002L
#define RS232_Csize_6         0x00008002L
#define RS232_Csize_7         0x00010002L
#define RS232_Csize_8         0x00020002L

#define RS232_B0              0
#define RS232_B50             1
#define RS232_B75             2
#define RS232_B110            3
#define RS232_B134            4
#define RS232_B150            5
#define RS232_B200            6
#define RS232_B300            7
#define RS232_B600            8
#define RS232_B1200           9
#define RS232_B1800          10
#define RS232_B2400          11
#define RS232_B4800          12
#define RS232_B9600          13
#define RS232_B19200         14
#define RS232_B38400         15
#define RS232_B56000         16

/* end of ioattrib.h */

/*------------------------------------------------------------------------
--                                                                      --
-- iocodes.h                                                            --
--                                                                      --
------------------------------------------------------------------------*/

/*----------------------------------------------------------------
-- Subsystems
----------------------------------------------------------------*/

#define SS_Mask         0x1f000000L

#define SS_Unknown      0x00000000L
#define SS_Kernel       0x01000000L
#define SS_SysLib       0x02000000L
#define SS_ProcMan      0x03000000L
#define SS_Loader       0x04000000L
#define SS_TFM          0x05000000L
#define SS_RamDisk      0x06000000L
#define SS_HardDisk     0x07000000L
#define SS_Fifo         0x08000000L
#define SS_NameTable    0x09000000L
#define SS_IOProc       0x0A000000L
#define SS_Window       0x0B000000L
#define SS_IOC          0x0C000000L
#define SS_NullDevice   0x0d000000
#define SS_Pipe         0x0e000000
#define SS_Batch        0x0f000000
#define SS_Login        0x10000000
#define SS_NetServ      0x11000000
#define SS_SM           0x12000000
#define SS_Device       0x13000000
#define SS_InterNet     0x14000000

/*----------------------------------------------------------------
-- Function Codes
----------------------------------------------------------------*/
/*----------------------------------------------------------------
-- Function Classes
----------------------------------------------------------------*/

#define FC_Mask         0x60000000L
#define FC_GSP          0x00000000L
#define FC_Private      0x60000000L

/*----------------------------------------------------------------
-- Retry Counter
----------------------------------------------------------------*/

#define FR_Mask         0x00F00000
#define FR_Inc          0x00100000

/*----------------------------------------------------------------
-- General Functions
----------------------------------------------------------------*/

#define FG_Mask         0x00FFFFF0L

/* IOC requests */
#define FG_Unknown      0x00000000L
#define FG_Open         0x00000010L
#define FG_Create       0x00000020L
#define FG_Locate       0x00000030L
#define FG_ObjectInfo   0x00000040L
#define FG_ServerInfo   0x00000050L
#define FG_Delete       0x00000060L
#define FG_Rename       0x00000070L
#define FG_Link         0x00000080L
#define FG_Protect      0x00000090L
#define FG_SetDate      0x000000a0L
#define FG_Refine       0x000000b0L
#define FG_CloseObj     0x000000c0L

/* direct server requests */
#define FG_Read                 0x00001010L
#define FG_Write                0x00001020L
#define FG_GetSize              0x00001030L
#define FG_SetSize              0x00001040L
#define FG_Close                0x00001050L
#define FG_Seek                 0x00001060L
#define FG_GetAttr              0x00001070L
#define FG_SetAttr              0x00001080L
#define FG_EnableEvents         0x00001090L
#define FG_Acknowledge          0x000010A0L
#define FG_NegAcknowledge       0x000010B0L
#define FG_Select               0x000010C0L

/* Distributed search codes */
#define FG_Search               0x00002010L
#define FG_FollowTrail          0x00002020L

/*Socket Related Requests*/
#define FG_Socket               0x00008010  /* create socket */
#define FG_Bind                 0x00008020  /* bind socket to address */
#define FG_Listen               0x00008030  /* set socket connection queue size */
#define FG_Accept               0x00008040  /* accept a connection */
#define FG_Connect              0x00008050  /* connect to a remote service */
#define FG_SendMessage          0x00008060  /* send datagram or other message */
#define FG_RecvMessage          0x00008070  /* receieve datagram or other message */

/*Device Requests*/
#define FG_Format               0x0000a010  /* format disc */
#define FG_WriteBoot            0x0000a020  /* write boot area */

/* General Server Terminate */
#define FG_Terminate            0x00001FF0
#define FG_Reboot               0x00002FF0

/*----------------------------------------------------------------
-- Error Codes
----------------------------------------------------------------*/

#define ErrBit          0x80000000L     /* set for all error codes */
#define Err_Null        0L              /* no error at all         */

/*----------------------------------------------------------------
-- Error Classes
----------------------------------------------------------------*/

#define EC_Mask         0xe0000000L

#define EC_Recover      0x80000000L      /* a retry might succeed */
#define EC_Warn         0xA0000000L      /* recover & try again   */
#define EC_Error        0xC0000000L      /* client fatal          */
#define EC_Fatal        0xE0000000L      /* system fatal          */

/*----------------------------------------------------------------
-- General Error codes
----------------------------------------------------------------*/

#define EG_Mask         0x00FF0000L      /* mask to isolate             */

#define EG_UnknownError 0x00000000L
#define EG_NoMemory     0x00010000L     /* memory allocation failure    */
#define EG_Create       0x00020000L     /* failed to create             */
#define EG_Delete       0x00030000L     /* failed to delete             */
#define EG_Protected    0x00040000L     /* object is protected          */
#define EG_Timeout      0x00050000L     /* timeout                      */
#define EG_Unknown      0x00060000L     /* object not found             */
#define EG_FnCode       0x00070000L     /* unknown function code        */
#define EG_Name         0x00080000L     /* mal-formed name              */
#define EG_Invalid      0x00090000L     /* invalid/corrupt object       */
#define EG_InUse        0x000a0000L     /* object in use/locked         */
#define EG_Congested    0x000b0000L     /* server/route overloaded      */
#define EG_WrongFn      0x000c0000L     /* fn inappropriate to object   */
#define EG_Broken       0x000d0000L     /* object broken in some way    */
#define EG_Exception    0x000e0000L     /* exception message            */
#define EG_WrongSize    0x000f0000L     /* object wrong size            */
#define EG_ReBooted     0x00100000L     /* server/processor rebooted    */
#define EG_Open         0x00110000L
#define EG_Execute      0x00120000L
#define EG_Boot         0x00130000L
#define EG_State        0x00140000L
#define EG_NoResource   0x00150000L
#define EG_Errno        0x00160000L
#define EG_Parameter    0x00ff0000L     /* bad parameter value          */

/*----------------------------------------------------------------
-- Object codes for general errors
----------------------------------------------------------------*/

#define EO_Unknown       0x00000000L
#define EO_Message       0x00008001L     /* error refers to a message    */
#define EO_Task          0x00008002L     /* error refers to a task       */
#define EO_Port          0x00008003L     /* error refers to a port       */
#define EO_Route         0x00008004L     /* error refers to a route      */
#define EO_Directory     0x00008005L     /* error refers to a directory  */
#define EO_Object        0x00008006L     /* error refers to Object struct*/
#define EO_Stream        0x00008007L     /* error refers to Stream       */
#define EO_Program       0x00008008L
#define EO_Module        0x00008009L
#define EO_Matrix        0x0000800aL     /* access matrix                */
#define EO_Fifo          0x0000800bL
#define EO_File          0x0000800cL
#define EO_Capability    0x0000800dL
#define EO_Name          0x0000800eL     /* name in name table           */
#define EO_Window        0x0000800fL
#define EO_Server        0x00008010L
#define EO_TaskForce     0x00008011L
#define EO_Link          0x00008012L
#define EO_Memory        0x00008013L
#define EO_Pipe          0x00008014L
#define EO_NetServ       0x00008015L     /* error refers to NS           */
#define EO_Subnetwork    0x00008016L     /* error refers to Subnetwork   */
#define EO_User          0x00008017L
#define EO_Session       0x00008018L
#define EO_Loader        0x00008019L
#define EO_ProcMan       0x0000801AL
#define EO_TFM           0x0000801BL
#define EO_Attribute     0x0000801CL
#define EO_NoProcessors  0x0000801DL
#define EO_ProcessorType 0x0000801EL
#define EO_Processor     0x0000801FL
#define EO_Socket        0x00008020L
#define EO_Medium        0x00008021L

/*----------------------------------------------------------------------*/
/*-- Exception codes                                                  --*/
/*----------------------------------------------------------------------*/

#define	EE_Mask		    0x0000ffff
#define	EE_Shift	    0

#define	EE_Null		    0x00000000
#define	EE_Kill		    0x00000004
#define	EE_Abort	    0x00000005
#define	EE_Suspend	    0x00000006
#define	EE_Restart	    0x00000007
#define	EE_Interrupt	    0x00000008
#define	EE_ErrorFlag	    0x00000009
#define	EE_StackError	    0x0000000a
#define	EE_Signal	    0x00007f00

/* End of iocodes.h */

/*------------------------------------------------------------------------
--                                                                      --
-- ioprot.h                                                             --
--                                                                      --
------------------------------------------------------------------------*/

typedef unsigned long   Matrix;    /* access matrix */
typedef UBYTE           AccMask;   /* access mask   */
typedef word            Key;       /* encryption key */

/*----------------------------------------------------------------------*/
/* Access capability                                                    */
/*----------------------------------------------------------------------*/

typedef struct Capability {
        AccMask         Access;   /* access mask      */
        UBYTE           Valid[7]; /* validation value */
} Capability;

/*----------------------------------------------------------------------*/
/* Access mask bits                                                     */
/*----------------------------------------------------------------------*/

#define AccMask_Full    0xff            /* all bits set       */

/* All */
#define AccMask_R       0x01            /* Read permission   */
#define AccMask_W       0x02            /* Write permission  */
#define AccMask_D       0x40            /* Delete permission */
#define AccMask_A       0x80            /* Alter permission  */

/* Files only */
#define AccMask_E       0x04            /* Execute permission */
#define AccMask_F       0x08            /* unused - arbitrary letters */
#define AccMask_G       0x10
#define AccMask_H       0x20

/* Directories only */
#define AccMask_V       0x04            /* V access category  */
#define AccMask_X       0x08            /* X access category  */
#define AccMask_Y       0x10            /* Y access category  */
#define AccMask_Z       0x20            /* Z access category  */

/* Tasks only */
#define AccMask_K       AccMask_D       /* Kill task (== Delete) */

/*----------------------------------------------------------------------*/
/* Access Matrix category masks                                         */
/*----------------------------------------------------------------------*/

#define AccMatrix_V     0x000000ffL
#define AccMatrix_X     0x0000ff00L
#define AccMatrix_Y     0x00ff0000L
#define AccMatrix_Z     0xff000000L

/*----------------------------------------------------------------------*/
/* Printed matrix letters                                               */
/*----------------------------------------------------------------------*/

#define FileChars   "rwefghda"
#define DirChars    "rwvxyzda"
#define TaskChars   "rw????ka"
#define ModChars    "r?e???da"
#define ProgChars   "rwe???da"

/*----------------------------------------------------------------------*/
/* Default Matrices                                                     */
/*----------------------------------------------------------------------*/

#define DefDirMatrix    0x21134BC7L     /* DARWV/DRWX/RWY/RZ */
#define DefFileMatrix   0x010343C3L     /* DARW/DRW/RW/R     */
#define DefLinkMatrix   0x201088C4L     /* dav:dx:y:z        */
#define DefTFMatrix     0x21134BC7L     /* darwv:drwx:rwy:rz */
#define DefTaskMatrix   0x010343C3L     /* darw:drw:rw:r     */
#define DefModuleMatrix 0x010545C5L     /* dare:dre:re:r     */
#define DefProgMatrix   0x010545C5L     /* dare:dre:re:r     */
#define DefNameMatrix   0x21110907L     /* rwv:rx:ry:rz      */
#define DefRootMatrix   0x21130B87L     /* arwv:rwx:rwy:rz   */


/* End of ioprot.h */

/*------------------------------------------------------------------------
--                                                                      --
-- iomess.h                                                             --
--                                                                      --
------------------------------------------------------------------------*/

/* Message port */

typedef UWORD           Port;           /* true structure hidden      */
#define NullPort        ((Port)0L)      /* zero is never a valid port */

typedef struct MsgHdr {
#if swapping_needed
        byte            Flags;          /* flag byte           */
        byte            ContSize;       /* control vector size */
        USHORT          DataSize;       /* 16 bit data size    */
#else
        USHORT          DataSize;
        byte            ContSize;
        byte            Flags;
#endif
        Port            Dest;           /* destination port descriptor */
        Port            Reply;          /* reply port descriptor       */
        word            FnRc;           /* function/return code        */
} MsgHdr;

#define MsgHdr_Flags_nothdr     0x80    /* used by kernel              */
#define MsgHdr_Flags_preserve   0x40    /* preserve destination route  */
#define MsgHdr_Flags_exception  0x20    /* exception message           */
#define MsgHdr_Flags_sacrifice  0x10    /* kernel may throw message away */
#define MsgHdr_Flags_bytesex    0x08    /* 0 = even, 1 = odd           */
#define MsgHdr_Flags_bytealign  0x03    /* used by C40 for non word    */
					/* aligned data */

/* Message control block */

typedef struct MCB {
        MsgHdr          MsgHdr;         /* message header buffer       */
        word            Timeout;        /* message timeout             */
        word            *Control;       /* pointer to control buffer   */
        byte            *Data;          /* pointer to data buffer      */
} MCB;

/* -- End of iomess.h */

/*------------------------------------------------------------------------
--                                                                      --
--      iogsp.h                                                         --
--                                                                      --
------------------------------------------------------------------------*/

/* offsets within the control vector for GSP requests   */      

#define Context_off          0   /* offsets in the control vector for all */
#define Pathname_off         1   /* directory requests */
#define Nextname_off         2
#define Cap1_off             3
#define Cap2_off             4
#define cont_minsize         5

#define arg1_off             5  /* additional offsets for directory requests */
#define arg2_off             6
#define arg3_off             7
#define arg4_off             8

#define OpenMode_off         5  /* additional offsets for individual requests */
#define CreateType_off       5
#define CreateSize_off       6
#define CreateInfo_off       7
#define RenameToname_off     5
#define LinkPathname_off     5
#define LinkCap1_off         6
#define LinkCap2_off         7
#define ProtectNewmatrix_off 5
#define RefineAccessMask_off 5
#define SetDateDate_off      7  /* Only interested in Modified */

#define ReadPos_off          0   /* offsets for stream requests */
#define ReadSize_off         1
#define ReadTimeout_off      2
#define WritePos_off         0
#define WriteSize_off        1
#define WriteTimeout_off     2
#define SeekPos_off          0
#define SeekMode_off         1
#define SeekNewPos_off       2
#define SetFileSizeSize_off  0
#define EnableEventsMask_off 0
#define AcknowledgeCount_off 0
#define NegAcknowledgeCount_off 0

#define Reply1_off           0     /* plus offsets for replies */
#define Reply2_off           1
#define Reply3_off           2
#define Reply4_off           3
#define Reply5_off           4
#define Reply6_off           5

#define open_reply           6L    /* size of a reply to open, locate, create */

/*
 * The reply to a read will consist of an arbitrary number of messages
 * containing the requested data, or a failure message. In addition to
 * possible error codes, the FnRc field of these messages will contain
 * a sequence number starting from 16 and incrementing in steps of 16
 * (Thus leaving the lower 4 bits clear). These lower 4 bits contain one
 * of the following values.
 */

#define ReadRc_Mask     0xfL    /* mask for lower 4 bits                */
#define ReadRc_More     0L      /* more data to come                    */
#define ReadRc_EOD      1L      /* end of data                          */
#define ReadRc_EOF      2L      /* end of data and of file              */

#define ReadRc_SeqInc   16L     /* increment for sequence numbers       */

/*
 * The first reply to a write will consist of the following structure
 * telling the sender how to format the data transfer. This is so copies
 * may be eliminated at the server end.
 * Once the data has been sent a second reply is made confirming that the
 * data were received.
 */

#define WriteRc_Done    0L
#define WriteRc_Sizes   1L

/*----------------------------------------------------------------------*/
/* some timeout constants                                               */
/*----------------------------------------------------------------------*/

                                      /* a 30-minute timeout in ticks */
#define DefaultStreamTimeout ((word) 30L * 60L * OneSec)
#define IOCDataMax    512             /* maximum size of full filenames   */
#define ControlMax    16              /* maximum size of a control vector */
#define MAXTIME       0x7FFFFFFFL     /* for timeouts of -1 (infinity)  */


/*----------------------------------------------------------------------*/
/* Object Types                                                         */
/*----------------------------------------------------------------------*/

/* bottom 4 bits are flags for major sub type                           */

#define Type_Directory  1L      /* supports GSP directory interface     */
#define Type_Stream     2L      /* supports GSP stream interface        */
#define Type_Private    4L      /* own private protocol                 */
                                /* remaining bit reserved               */

#define Type_File       0x12L   /*(0x10L+Type_Stream)*/
#define Type_Fifo       0x22L   /*(0x20L+Type_Stream)*/
#define Type_Module     0x32L   /*(0x30L+Type_Stream)*/
#define Type_Program    0x42L   /*(0x40L+Type_Stream)*/
#define Type_Task       0x52L   /*(0x50L+Type_Stream)*/
#define Type_Link       (0x60L)
#define Type_Name       (0x70L)
#define Type_TaskForce  0x81L   /*(0x80|Type_Directory)*/
#define Type_LTaskForce 0x91L   /*(0x90|Type_Directory)*/
#define Type_CacheName  (0xa0L)
#define Type_Pipe       0xb2L   /*(0xb0|Type_Stream)*/
#define Type_Pseudo     0xc2L   /*(0xc0|Type_Stream)*/
#define Type_Device     0xd4L   /*(0xd0|Type_Private)*/
#define Type_Session    0xe0L
#define Type_Socket     0xf0L
/**
*** The reply to Open, Locate and Create requests
**/
typedef struct IOCReply1 {
    word        Type;       /* object type code */
    word        Flags;      /* flag word        */
    byte        Access[8];  /* a capability for it */
    Offset      Pathname;   /* full pathname of object */
    word        Object;     /* object value if no reply port*/
} IOCReply1;


/* end of iogsp.h */

/*------------------------------------------------------------------------
--                                                                      --
--      iosyslib.h                                                      --
--                                                                      --
------------------------------------------------------------------------*/

/*----------------------------------------------------------------------*/
/* Directory Entry structure                                            */
/*----------------------------------------------------------------------*/
typedef struct { word           Type;
                 word           Flags;
                 Matrix         Matrix;
                 char           Name[32];
} DirEntry;

/*----------------------------------------------------------------------*/
/* Generic Object Info structure                                        */
/*----------------------------------------------------------------------*/
/* data structure returned in response to an object info request. The structure
   is returned in the data vector of the message, with all the words swapped as
   necessary to get them to work on the transputer.
*/
typedef struct ObjInfo {
        DirEntry        DirEntry;       /* re-iteration of common info  */
        word            Account;        /* accounting identifier        */
        word            Size;           /* object size in bytes         */
        time_t          Creation;       /* three date stamps            */
        time_t          Access;
        time_t          Modified;
} ObjInfo;

/*----------------------------------------------------------------------*/
/* Modes for Open                                                       */
/*----------------------------------------------------------------------*/

#define O_ReadOnly      1L              /* For the Helios side */
#define O_WriteOnly     2L
#define O_ReadWrite     3L
#define O_Execute       4L
#define O_Create        0x0100L          /* create if does not exist   */
#define O_Exclusive     0x0200L          /* get exclusive access       */
#define O_Truncate      0x0400L          /* truncate if already exists */
#define O_NonBlock      0x0800L          /* do not block on read/write */
#define O_Append        0x1000L          /* append data (not to be used */
#define O_Sync          0x2000L          /* synchronous transfers */

/* The flags array of the Select call contains the standard bits in the */
/* bottom 2 bits, plus the following bits...                            */

#define O_Exception	0x04		 /* select for exception */
#define O_Selected      0x10             /* set if stream ready  */

/*----------------------------------------------------------------------*/
/* Modes for Seek                                                       */
/*----------------------------------------------------------------------*/

#define S_Beginning     0L              /* relative to start of file    */
#define S_Relative      1L              /* relative to current pos      */
#define S_End           2L              /* relative to end of file      */
#define S_Last          3L              /* relative to last operation   */

/*----------------------------------------------------------------------*/
/* Flag Bits                                                            */
/*----------------------------------------------------------------------*/

/* First column of comments indicates whose responsibility the state    */
/* of the bit is: S = Server, L = Syslib.                               */

#define Flags_Mode         0x0000000fL  /* L copy of open mode      */
#define Flags_More         0x00000010L  /* S More info available    */
#define Flags_Seekable     0x00000020L  /* S stream is seekable     */
#define Flags_Remote       0x00000040L  /* L server is non-local    */
#define Flags_StripName    0x00000080L  /* S names are stripped before pass on */
#define Flags_CacheName    0x00000100L  /* S name is cached         */
#define Flags_LinkName     0x00000200L  /* S name is for h/w link   */
#define Flags_PStream      0x00000400L  /* L set for PseudoStreams  */
#define Flags_ResetContext 0x00000800L  /* S set for remote servers */
#define Flags_Pipe         0x00001000L  /* S set for pipes          */
#define Flags_CloseOnSend  0x00002000L  /* S close in SendEnv       */
#define Flags_OpenOnGet    0x00004000L  /* S open in GetEnv         */
#define Flags_Selectable   0x00008000L  /* S can be used in Select  */

#define Flags_Interactive 0x00010000L   /* S if stream is interactive   */
#define Flags_MSdos       0x00020000L   /* S MSdos format files         */

#define Flags_CloseMask 0xe0000000L     /*   mask for following flags   */
#define Flags_Closeable 0x20000000L     /* S set if needs closing       */
#define Flags_Server    0x40000000L     /* L set if served stream       */
#define Flags_Stream    0x80000000L     /* L set if stream structure    */

#define closebits_(x)    ((((UWORD)(x))>>Flags_CloseShift)&0x7)

/* end of iosyslib.h */

/*------------------------------------------------------------------------
--                                                                      --
--  ioaddon.h                                                           --
--                                                                      --
------------------------------------------------------------------------*/

#define getfnrc(request)   (((request)->MsgHdr).FnRc & FG_Mask)

#define ReqDie             0x00FFFFF0L         /* still necessary */

#define ReplyOK            (Err_Null)

/* The reply to a ServerInfo request */

typedef struct { word type;        /* always Type_Directory */
                 word size;        /* size in bytes */
                 word used;        /* how many used */
                 word alloc;       /* unit of allocation in bytes */
} servinfo;

/**
*** These are additional error codes for floppies
**/

#define floppy_invalid   1        /* catch-all error code       */
#define floppy_protected 2        /* write-protected            */
#define floppy_removed   3        /* disk removed during access */
#define floppy_full      4        /* insufficient space on disk */

/**
*** And for printer devices.
**/
#define printer_invalid      0x11      /* catch-all */
#define printer_offline      0x12
#define printer_outofpaper   0x13
#define printer_error        0x14

/* End of ioaddon.h */

/*------------------------------------------------------------------------
--                                                                      --
-- ioevents.h                                                           --
--                                                                      --
------------------------------------------------------------------------*/

/* These are valid types for the Type field of an Event */
#define Event_Mouse            0x1L
#define Event_Keyboard         0x2L
#define Event_Break            0x4L
#define Event_SerialBreak      0x8L
#define Event_ModemRing        0x10L

#define Flag_Buffer            0x80000000L

/* additional reply codes */
#define EventRc_Acknowledge     0x1L   /* the other side should acknowledge   */
#define EventRc_IgnoreLost      0x2L   /* unimportant messages have been lost */

#if (mouse_supported || gem_supported || MSWINDOWS)
/**
*** Potential problem here, with the way shorts are packed into words in
*** data structures.
**/
typedef struct {
#if (ST || AMIGA || SUN3)    /* || TRIPOS  I imagine */
                 SHORT  Y;
                 SHORT  X;
#else
                 SHORT  X;
                 SHORT  Y;
#endif
                 word   Buttons;
} Mouse_Event;

#define Buttons_Unchanged       0x00000000L
#define Buttons_Button0_Down    0x00000001L
#define Buttons_Button0_Up      0x00008001L
#define Buttons_Button1_Down    0x00000002L
#define Buttons_Button1_Up      0x00008002L
#define Buttons_Button2_Down    0x00000004L
#define Buttons_Button2_Up      0x00008004L
#define Buttons_Button3_Down    0x00000008L
#define Buttons_Button3_Up      0x00008008L
#endif  /* mouse_supported */

#if keyboard_supported
typedef struct { word   Key;
                 word   What;
} Keyboard_Event;

#define Keys_KeyUp      1L
#define Keys_KeyDown    2L
#endif /* keyboard_supported */

typedef struct { word   junk1;
                 word   junk2;
} Break_Event;   /* this is for ctrl-C */

#if RS232_supported
typedef struct { word    junk1;
                 word    junk2;
} SerialBreak_Event;

typedef struct { word    junk1;
                 word    junk2;
} ModemRing_Event;
#endif

typedef struct IOevent { word Type;
                         word Counter;
                         word Stamp;
                         union {
#if (mouse_supported || gem_supported)
                         Mouse_Event        Mouse;
#endif
#if keyboard_supported
                         Keyboard_Event     Keyboard;
#endif
                         Break_Event        Break;
#if RS232_supported
                         SerialBreak_Event  RS232_Break;
                         ModemRing_Event    ModemRing;
#endif
                    } Device;
} IOEvent;


                /* this structure is used to keep track of event handlers */
typedef struct { word port;
                 word *ownedby; /* to keep track of streams */
} event_handler;

/* end of ioevents.h */

/*------------------------------------------------------------------------
--                                                                      --
-- ioconfig.h                                                           --
--                                                                      --
------------------------------------------------------------------------*/
#define CONFIGSPACE	128

typedef struct Config {
        word    PortTabSize;    /* # slots in port table        */
        word    Incarnation;    /* what booter believes our incarnation is */
        word    loadbase;       /* address at which system was loaded */
        word    ImageSize;      /* size of system image         */
        word    Date;           /* current system date          */
        word    FirstProg;      /* offset of initial program    */
        word    Memory;         /* Size of transputer memory, or 0 */
        word    Flags;          /* Various flags */
        word    Spare;          /* a spare slot                 */
        word    MyName;         /* full path name               */
        word    ParentName;     /* ditto                        */
        word    NLinks;         /* number of links              */
        word    LinkConf[1];    /* NLinks LinkConf structs      */
        char    namespace[CONFIGSPACE];	/* space for extra links and */
					/* MyName / ParentName */
} Config;

/* Config.Flags end up as Root.Flags */
#define Config_Flags_rootnode	   0x00000001	/* This is rootnode */
#define Config_Flags_special	   0x00000002	/* This is special nuc. */
#define Config_Flags_ROM	   0x00000004	/* This is ROMm'ed nuc. */
#define Config_Flags_xoffed	   0x00000100	/* Set if links xoffed	*/
#define Config_Flags_CacheOff	   0x00000200	/* Dont enable cache */

#define Link_Flags_parent	0x40	/* indicates the link which booted us */
#define Link_Flags_ioproc	0x20	/* indicates an io processor	*/
#define Link_Flags_debug	0x10	/* debugging link for IOdebug	*/
#define Link_Flags_report	0x08	/* report state changes		*/
#define Link_Flags_stopped	0x04	/* link traffic has been stopped*/
#define Link_Flags_HalfDuplex	0x80	/* use half duplex protocol     */

#define Link_Mode_Null		0	/* not connected to anything	*/
#define Link_Mode_Dumb		1	/* link is a dumb device	*/
#define Link_Mode_Intelligent	2	/* part of Helios network	*/

#define Link_State_Null		0	/* not connected to anything	*/
#define Link_State_Booting	1	/* booting remote processor	*/
#define Link_State_Dumb		2	/* dumb device			*/
#define Link_State_Running	3	/* live network link		*/
#define Link_State_Timedout	4	/* doing idle exchange		*/
#define Link_State_Crashed	5	/* remote processor has crashed	*/
#define Link_State_Dead		6	/* remote processor not running	*/

	/* These support "special" links, e.g. the seventh link on a	*/
	/* Hema DSP1. The values should be kept in step with kernel.h	*/
#define Link_Mode_Special	3
#define Link_State_DSP1		0x10
#define Link_State_Hydra	0x11

/* -- End of ioconfig.h */

/**
*** Link protocol bytes (words on C40)
**/
#define Proto_Write               0
#define Proto_Read                1
#define Proto_Msg                 2
#define Proto_Null                3
#define Proto_Term                4
#define Proto_Reconfigure         5
#define Proto_SecurityCheck       6
#define Proto_Reset               7
#define Proto_Go		  0x0A
#define Proto_ReSync		  0x7f
#define Proto_Info                0x0F0

#define Proto_Debug               0x064
#define Proto_RemoteReset         0x0F2
#define Proto_RemoteAnalyse       0x0F3
#define Proto_Close               0x0F4
#define Proto_Boot                0x0F5

/**
*** Network control function codes
**/
#define NC_Reset                 0x2010L
#define NC_Analyse               0x2020L
#define NC_Connect               0x2030L
#define NC_Disconnect            0x2040L
#define NC_Enquire               0x2050L

#define ND_INVALID               0x0000L
#define ND_HARDWIRED             0x0001L
#define ND_SOFTWIRED             0x0002L
#define ND_NOCONNECT             0x0003L

/**
*** Bootstrap stuff, used by tload.c and hydra
**/
#define Processor_Trannie   1
#define Processor_Arm       2
#define Processor_i860      3
#define Processor_68000     4
#define Processor_C40	    5
/* and lots of others... */

#define B_Reset_Processor    0x0001
#define B_Send_Bootstrap     0x0002
#define B_Send_Image         0x0004
#define B_Send_Config        0x0008
#define B_Wait_Sync          0x0010
#define B_Check_Processor    0x0020
#define B_Send_Sync          0x0040
#define B_Send_IdRom         0x0080

#ifdef internet_supported

#ifndef SOL_SOCKET
/*
 * Level number for (get/set)sockopt() to apply to socket itself.
 */
#define	SOL_SOCKET	0xffff		/* options for socket level */
#endif

/*
 * Level number for (get/set)sockopt() to apply to system.
 */
#define SOL_SYSTEM	0xfff0		/* options for system level */

/*
 * Level number for (get/set)sockopt() to apply an ioctl (yuk).
 */
#define SOL_IOCTL	0xfff1		/* options for ioctl level */


#ifndef SO_DEBUG
/*
 * Option flags per-socket.
 */
#define	SO_DEBUG	0x0001		/* turn on debugging info recording */
#define	SO_ACCEPTCONN	0x0002		/* socket has had listen() */
#define	SO_REUSEADDR	0x0004		/* allow local address reuse */
#define	SO_KEEPALIVE	0x0008		/* keep connections alive */
#define	SO_DONTROUTE	0x0010		/* just use interface addresses */
#define	SO_BROADCAST	0x0020		/* permit sending of broadcast msgs */
#define	SO_USELOOPBACK	0x0040		/* bypass hardware when possible */
#define	SO_LINGER	0x0080		/* linger on close if data present */
#define	SO_OOBINLINE	0x0100		/* leave received OOB data in line */

#endif
/*
 * Additional options, not kept in so_options.
 */
#define SO_SNDBUF	0x1001		/* send buffer size */
#define SO_RCVBUF	0x1002		/* receive buffer size */
#define SO_SNDLOWAT	0x1003		/* send low-water mark */
#define SO_RCVLOWAT	0x1004		/* receive low-water mark */
#define SO_SNDTIMEO	0x1005		/* send timeout */
#define SO_RCVTIMEO	0x1006		/* receive timeout */
#define	SO_ERROR	0x1007		/* get error status and clear */
#define	SO_TYPE		0x1008		/* get socket type */

/* Extra options for Helios */
#define	SO_HOSTID	0x8001		/* Host Id (system level)	*/
#define	SO_HOSTNAME	0x8002		/* Host Name (system level)	*/
#define SO_PEERNAME	0x8003		/* Peer name (Get only)		*/
#define SO_SOCKNAME	0x8004		/* socket name (Get only)	*/

#endif
/*------------------------------------------------------------------------
--                                                                      --
--          H E L I O S   I N P U T / O U T P U T   S E R V E R         --
--          ---------------------------------------------------         --
--                                                                      --
--             Copyright (C) 1987, Perihelion Software Ltd.             --
--                        All Rights Reserved.                          --
--                                                                      --
--      barthdr                                                         --
--                                                                      --
--         A header file for all my C programs, containing odds and     --
--                                                                      --
--         ends to turn C into a slightly less useless language.        --
--                                                                      --
--     Author:  BLV 8/10/87                                             --
--                                                                      --
------------------------------------------------------------------------*/
/* RcsId: $Id: barthdr,v 1.6 1994/07/06 10:44:59 mgun Exp $ */
/* Copyright (C) 1987, Perihelion Software Ltd.         		*/

typedef long *ptr;
/* RTE : may not be required */
#if (ST || PC || AMIGA || ARMBSD || MEIKORTE || HELIOS)
typedef unsigned int uint;
#endif
#define eq ==
#define ne !=
#define MAXINT 0x7FFFFFFFL

/**
*** This macro takes care of ANSI prototyping for ANSI and for non-ANSI
*** compilers - I hope. Functions can be defined by e.g.
*** extern void fn(xyz, (char *));
*** which expands to
*** extern void xyz (char *);   or
*** extern void xyz();
*** I have checked these macros using 5 different C compilers, and they
*** seem to work fine.
**/
#if ANSI_prototypes
#define fn(a,b) a b
#else
#define fn(a,b) a()
#endif

/**
*** The following macro is designed to get around problems with compilers
*** that complain about variables not being used. If your compiler suffers from
*** the same problem then you can || your defines.h entry with the #if ST
**/

#if (ST || ARMBSD || MEIKORTE || HELIOS)
#define use(a) a=a;
#else
#if (MAC)
#define use(a) #pragma unused (a)
#else
#define use(a)
#endif /*MAC*/
#endif

/**
*** And another macro for compiler problems : func(x) expands to &x if your
*** compiler expects the address of a function to be passed as argument,
*** or to just x otherwise.
***
*** For C++ compilers, which have stricter type checking, I've also
*** placed the cast of the function to VoidFnPtr here as well.
**/
#if (ST || HELIOS)
#define func(x) (&x)
#else
#  ifdef cplusplus
#    define func(x) ((VoidFnPtr)(x))
#  else
#    define func(x) (x)
#  endif
#endif

/**
*** On some machines, tolower and toupper work only  if the character is
*** already uppercase or lowercase respectively. On others the library bothers
*** to check. The following takes care of this bit of nastiness.
**/

#if (PC)
        /* Microsoft C 5.0 library does it for me */
#define ToLower(x) tolower(x)
#define ToUpper(x) toupper(x)

#else

#ifdef Server_Module

#if ANSI_prototypes
int ToLower (int x)
#else
int ToLower(x)
int x;
#endif
{ return(isupper(x) ? tolower(x) : x);
}

#if ANSI_prototypes
int ToUpper (int x)
#else
int ToUpper(x)
int x;
#endif
{ return(islower(x) ? toupper(x) : x);
}

#else

extern int fn(ToLower, (int));
extern int fn(ToUpper, (int));

#endif /* Server_Module */

#endif

/**
*** Mark Williams C does not support memcpy...
**/
#if (ST)

#ifdef Server_Module
void memcpy(dest, source, count)
char *dest, *source;
int count;
{ for ( ; count > 0; count--) *dest++ = *source++;
}

void memmove(dest, source, count)
char *dest, *source;
int count;
{ if (dest < source)
   for ( ; count > 0; count--) *dest++ = *source++;
  else
    for (dest += count, source += count;  count > 0; count--)
      *(--dest) = *(--source);
}

void memset(dest, val, count)
char *dest;
int val, count;
{ for ( ; count > 0; count--) *dest++ = val;
}

#else
extern void fn(memcpy, (char *dest, char *source, int count));
extern void fn(memmove, (char *dest, char *source, int count));
extern void fn(memset, (char *dest, int val, int count));
#endif /* Server_Module */
#endif /* ST */

#if (SUN || ARMBSD)
  /* These have memcpy() and memset(), but not memmove() ... */
#if (!SOLARIS)
  /* ... which does */
#define memmove(a, b, c) bcopy(b,a,c)
#endif
#endif

#if i486V4
#define bcopy(x, y, l) memcpy(y, x, l)
#define bzero(x, l) memset(x, 0, l)
#endif

#if (PC)
extern long fn( divlong, (long, long));
#else
#define divlong(a, b) (a) / (b)
#endif

/*------------------------------------------------------------------------
--                                                                      --
--          H E L I O S   I N P U T / O U T P U T   S E R V E R         --
--          ---------------------------------------------------         --
--                                                                      --
--             Copyright (C) 1987, Perihelion Software Ltd.             --
--                        All Rights Reserved.                          --
--                                                                      --
--      Debugopt.h                                                      --
--                                                                      --
--  Author:  BLV 10/12/87                                               --
--                                                                      --
------------------------------------------------------------------------*/
/* RcsId: $Id: debugopt.h,v 1.3 1993/08/12 14:24:15 bart Exp $ */
/* Copyright (C) 1987, Perihelion Software Ltd.   			*/

#ifndef Daemon_Module

/**
*** This header file is used to control the debugging options available in
*** the Server. There is a set of flags, debugflags, and a debugging option
*** is on when the corresponding bit is set in debugflags. The various flags
*** are declared. To keep track of all the debugging options I use a structure
*** debug_options, each option consisting of a character and the corresponding
*** flag, and I have an array of these options.
***
*** For example, when ctrl-shift M is pressed to toggle the message debugging
*** flag the system looks down options_list to find an entry with the letter
*** 'm'. It finds this, and xors debugflags with the corresponding flag
*** Message_Flag.
**/

typedef struct debug_options {
         int    flagchar;
         word   flag;
         char   *name;
} debug_options;

#define Message_Flag                 0x1L
#define Search_Flag                  0x2L
#define Open_Flag                    0x4L
#define Name_Flag                    0x8L
#define Read_Flag                   0x10L
#define Boot_Flag                   0x20L
#define Memory_Flag                 0x40L
#define Keyboard_Flag               0x80L
#define Init_Flag                  0x100L
#define Com_Flag                   0x200L
#define Write_Flag                 0x400L
#define Quit_Flag                  0x800L
#define Close_Flag                0x1000L
#define HardDisk_Flag             0x2000L
#define Log_Flag                  0x4000L
#define Graphics_Flag             0x8000L
#define Reconfigure_Flag         0x10000L
#define Timeout_Flag             0x20000L
#define OpenReply_Flag           0x40000L
#define FileIO_Flag              0x80000L
#define Delete_Flag             0x100000L
#define Directory_Flag          0x200000L
#define Nopop_Flag              0x400000L
#define ListAll_Flag            0x800000L
#define Error_Flag             0x1000000L
#define DDE_Flag               0x2000000L
/**
*** All_Debug_Flags is a mask for all the debugging options except the
*** one-off ones : memory, log, reconfigure, nopop, listall.
*** It is used for -a etc.
**/
#define All_Debug_Flags       0x033EBFBFL
              
#define Log_to_screen           1
#define Log_to_file             2
#define Log_to_both             3

#ifdef Server_Module
word debugflags;
int  log_dest = Log_to_screen;
debug_options options_list[] = 
                               {
             /* 'a' == all */
             { 'b', Boot_Flag,        "boot"        },
             { 'c', Com_Flag,         "serial"      },
             { 'd', Delete_Flag,      "delete"      },
             { 'e', Error_Flag,       "errors"      },
             { 'f', FileIO_Flag,      "file I/O"    },
             { 'g', Graphics_Flag,    "graphics"    },
             { 'h', HardDisk_Flag,    "raw disk"    },
             { 'i', Init_Flag,        "init"        },
             { 'j', Directory_Flag,   "directory"   },
             { 'k', Keyboard_Flag,    "keyboard"    },
             { 'l', Log_Flag,         "logger"      },
             { 'm', Message_Flag,     "messages"    }, 
             { 'n', Name_Flag,        "names"       },
             { 'o', Open_Flag,        "open"        },
             { 'p', Close_Flag,       "close"       },
             { 'q', Quit_Flag,        "exit"        },
             { 'r', Read_Flag,        "read"        },
             { 's', Search_Flag,      "search"      },
             { 't', Timeout_Flag,     "timeouts"    },
             { 'u', Nopop_Flag,       "nopop"       },
             { 'v', OpenReply_Flag,   "open reply"  },
             { 'w', Write_Flag,       "write"       },
             { 'x', Memory_Flag,      "resources"   },
             { 'y', ListAll_Flag,     "list"        },
             { 'z', Reconfigure_Flag, "reconfigure" },
             { '\0', 0L              } };

#endif
#ifndef Server_Module
extern word debugflags;
extern int  log_dest;
extern debug_options options_list[];
#endif

/**
*** Where the bootstrap program is loaded, assumes T800 but not really
*** important
**/
#ifndef MemStart
#define MemStart  MinInt+0x70
#endif

/**
*** arguments to boot_transputer() in module tload.c
**/
#define debugboot       1
#define serverboot      2

/**
*** If the I/O Server is compiled with -DSMALL, most of the debugging
*** options disappear
**/
#ifdef SMALL
#define Debug(a, b)
#else
#define Debug(a,b) if (debugflags & a) ServerDebug b
#endif /* SMALL */

#endif  /* Daemon module */
/*------------------------------------------------------------------------
--                                                                      --
--          H E L I O S   I N P U T / O U T P U T   S E R V E R         --
--          ---------------------------------------------------         --
--                                                                      --
--             Copyright (C) 1988, Perihelion Software Ltd.             --
--                        All Rights Reserved.                          --
--                                                                      --
--      structs.h                                                       --
--                                                                      --
--         Declarations of the server's manifests and data structures   --
--                                                                      --
--      Author:  BLV 9/6/87                                             --
--                                                                      --
------------------------------------------------------------------------*/
/* RcsId: $Id: structs.h,v 1.7 1994/06/29 13:42:25 tony Exp $ */
/* Copyright (C) 1987, Perihelion Software Ltd.   			*/


/**
***   The Server makes extensive use of arrays of functions to handle all
***  the Helios requests in a general way, and the following manifests
***  declare offsets within the arrays for these requests.
**/
#define InitServer_off      0
#define TidyServer_off      1
#define Private_off         2
#define Testfun_off         3
#define Open_off            4
#define Create_off          5 
#define Locate_off          6
#define ObjectInfo_off      7
#define ServerInfo_off      8
#define Delete_off          9
#define Rename_off         10
#define Link_off           11
#define Protect_off        12
#define SetDate_off        13
#define Refine_off         14
#define CloseObj_off       15
#define handler_max        16

#define InitStream_off      0
#define TidyStream_off      1
#define StreamPrivate_off   2
#define Read_off            3
#define Write_off           4
#define GetSize_off         5
#define SetSize_off         6
#define Close_off           7
#define Seek_off            8
#define GetAttr_off         9
#define SetAttr_off        10
#define EnableEvents_off   11
#define Acknowledge_off    12
#define NegAcknowledge_off 13
#define Select_off         14
#define Stream_max         15

/**
*** This is returned by get_int_config() in module server.c to indicate failure
**/ /* @@@ Bart, this is going to give us real grief on day! */
#define Invalid_config     0x64928AF2L

/**
*** The main data structure in the Server is the linked list.
**/
typedef struct Node { struct Node *next;       /* next node in list */
                      struct Node *prev;       /* previous node in list */
} Node;

#ifdef __cplusplus
typedef void	(*VoidNodeFnPtr)(Node *, ...);
typedef word	(*WordNodeFnPtr)(Node *, ...);
#else
typedef VoidFnPtr	VoidNodeFnPtr;
typedef WordFnPtr	WordNodeFnPtr;
#endif

typedef struct List { Node *head;       /* first node in list  */
                      Node *earth;      /* always NULL pointer */
                      Node *tail;       /* last node in list   */
} List;

typedef struct GenData {
                 Node    node;
                 uint    size;
                 int     junk;     /* To guarantee word alignment of data */
                 byte    data[1];
} GenData;

typedef struct Semaphore {
                 List    list;
                 int     count;
} Semaphore;

/**                                                                                                             
*** The Server depends heavily on coroutines. All the coroutines are held in
*** linked lists, using the following structure.
**/
typedef struct Conode_str { Node node;           /* conodes held in linked list  */
                 word id;             /* unique integer identifier    */
                 word type;           /* usually zero, may be suicide */
                                      /* or timedout or ready         */
                 word flags;
                 word timelimit;      /* when stream should die       */
                 ptr  cobase;         /* from CreateCo                */
#ifdef __cplusplus
		 void (**handlers)(struct Conode_str *);
#else
                 void (**handlers)(); /* request handlers             */
#endif
                 ptr  extra;          /* coroutines global data       */
                 char name[128];      /* a name field if required     */
} Conode;

/**
*** Various odds and ends for use with coroutines
**/
#define CoSuicide     (654321L)
#define CoTimeout     (666666L)
#define CoAbortSelect (655321L)
#define CoReady       (777777L)

#define CoFlag_Floppy       0x0001L
#define CoFlag_CtrlCed      0x0002L
#define CoFlag_FileStream   0x0004L
#define CoFlag_Waiting      0x0008L
#define CoFlag_EOFed        0x0010L

/**
*** This structure is used to declare my devices. The type is File, Directory
*** or Private, the name gives the device name e.g. Console, and the handlers
*** field gives the Helios request handlers. I.E. there is an entry in this
*** array to deal with Open requests, another for Locate requests, etc.
**/

#ifdef __cplusplus

typedef void		(*VoidConFnPtr)(Conode *);
typedef void		(*VoidWordFnPtr)(word *);
typedef word		(*WordConFnPtr)(Conode *);

#else

typedef VoidFnPtr	VoidConFnPtr;
typedef VoidFnPtr	VoidWordFnPtr;
typedef WordFnPtr	WordConFnPtr;

#endif

typedef struct device_declaration {
       word type;
       char *name;
       VoidConFnPtr handlers[handler_max];
} device_declaration;

/**
*** The Server contains various directories. In addition to disk directories,
*** the IO processor itself is a directory of servers, and some of these servers
*** such as the communication port servers are directories of ports. Hence I
*** share code to deal with these various directories, possible by keeping the
*** directory in a linked list of ObjNode's. There are a number of
*** supersets of ObjNodes.  DirEntryNode is an alias for ObjNode, for
*** compatibility.
**/
   
typedef struct { Node       node;
                 DirEntry   direntry;
                 word       size;
                 word       account;
} ObjNode;

#define DirEntryNode ObjNode

typedef struct { List list;
                 word entries;
} DirHeader;

/**
*** Communication Port_node's are slightly more complicated because I need
*** additional information.
**/

typedef struct { Attributes     attr;
                 Semaphore      lock;
                 word           id;
                 VoidFnPtr      error_fn;
                 VoidFnPtr      done_fn;
                 VoidFnPtr      configure_fn;
                 WordFnPtr      send_fn;
                 WordFnPtr      pollwrite_fn;
                 WordFnPtr      abortwrite_fn;
                 WordFnPtr      receive_fn;
                 WordFnPtr      pollread_fn;
                 WordFnPtr      abortread_fn;
} ComsPort;

typedef struct { ObjNode      objnode;
                 ComsPort     *port;
                 VoidFnPtr    *handlers;
} Port_node;

/**
*** Initially the server boots up a simple network consisting of the root
*** processor and the IO processor. These must be named during the booting
*** processes, so the following manifests define the default names.
**/
#define DefaultServerName "IO"
#define DefaultRootName   "00"
#define slashDefaultServerName "/IO"
#define slashDefaultRootName   "/00"

/**
*** The following structures are used for handling windows and the console
*** device. Microwave is used to handle cooked input processing (pun
*** definitely intended), Screen is used by the ANSI emulator, and Window is
*** used to point at a window structure.
**/
#define       Console_limit 255

typedef struct Microwave {
        UBYTE  buffer[Console_limit+1];/* where data is processed    */
        int    count;                  /* amount in  buffer */
} Microwave;

#define Cooked_EOF  1               /* ctrl-D detected         */
#define Cooked_EOD  2               /* no more data in buffer  */   
#define Cooked_Done 3               /* Read has been satisfied */

#if use_ANSI_emulator
typedef struct Screen { byte          **map;
                        byte          *whole_screen;
                        int           Rows;
                        int           Cols;
                        int           Cur_x;
                        int           Cur_y;
                        int           mode;
                        int           flags;
                        int           args[5];
                        int           *args_ptr;
                        int           gotdigits;
} Screen;

#define ANSI_in_escape     0x01
#define ANSI_escape_found  0x02
#define ANSI_firstdigit    0x04
#define ANSI_dirty         0x08

#endif

typedef struct Window { ObjNode       node;
                        Attributes    attr;
                        event_handler break_handler;
                        Microwave     cooker;
                        Semaphore     read_lock;
                        Conode        *readerco;
                        Semaphore     write_lock;
                        Conode        *writerco;
#if use_ANSI_emulator
                        Screen        screen;
#endif
                        UBYTE         Table[Console_limit];
			word	      handle;
                        int           head, tail;
                        int           XOFF;
#if multi_tasking
                        word          any_data;
#endif
} Window;

#define WindowFlags_Deleted  0x00000001L

/**
*** Multi-tasking support. To prevent the server from hogging all the
*** CPU time, the main sources inform the local module whenever it is
*** waiting for particular input, e.g. for a key to be pressed or for
*** the mouse to be moved. Then at a strategic point in the server's main
*** loop I call a multiwait function. This can suspend the CPU for upto
*** half a second or until one of the devices specified is ready.
*** The following constants define the various forms of IO which the
*** server can be waiting on.
**/
#if multi_tasking
#define Multi_LinkMessage          1L
#define Multi_WindowInput          2L
#define Multi_GemInput             3L
#define Multi_MouseInput           4L
#define Multi_KeyboardInput        5L
#define Multi_RS232Event           6L
#define Multi_PortIO               7L
#define Multi_SocketInput          8L
#define Multi_SocketOutput         9L
#define Multi_SocketExcp          10L
#define Multi_StreamInput         11L
#define Multi_StreamOutput        12L
#endif

/**
*** The following structure defines a transputer link.
**/
typedef struct Trans_link {
        word    state;
        word    fildes;
        word    ready;
        word    flags;
        word    connection;
        byte    link_name[32];
        word	last_send;
} Trans_link;

/**
*** These flags have the following meanings :
*** waiting : there is a Multiwait active on this link
*** free    : the link has been successfully opened etc, and may be used
***           by any software that needs it
*** unused  : the link was not opened successfully, typically because another
***           user has locked it, but it may be useable in the future
*** not_selectable : the link does not support the select call
*** uninitialised  : the link is in an unknown state, it has not been
***           reset since a successful open, so another user may have put
***           it into a funny mode
*** firsttime : the link has never been initialised, so if something goes
***           wrong it is reasonable to display an error message
*** word    : this link always transfers a word multiple rather than a
***           byte multiple.
**/
#define Link_flags_waiting        0x01
#define Link_flags_free           0x02
#define Link_flags_unused         0x04
#if (UNIX)
#define Link_flags_not_selectable 0x010000  
#define Link_flags_uninitialised  0x020000
#define Link_flags_firsttime      0x040000
#define Link_flags_datareadysent  0x080000
#define Link_flags_word           0x100000
#define Link_flags_messagemode    0x200000
#define Link_flags_halfduplex     0x400000
#endif

#define Link_Reset          1
#define Link_Booting        2
#define Link_Running        3

#if (UNIX)
/**
*** This structure is used between hydra and the server
**/
typedef struct socket_msg {
        long fnrc;
        long extra;
        char userid[16];   /* cannot use l_cuserid, server and hydra may be */
        char hostname[64]; /* compiled with different headers. */
        char linkname[32]; /* name of the link */
} socket_msg;
#define Any_Link           0x12345600
#define Link_Unavailable   0x12345601
#define Invalid_Link       0x12345602
#define Debug_Connection   0x12345603
#define Hydra_Busy         0x12345604

typedef struct debug_msg {
	long fnrc;
        long link;
} debug_msg;
#define Debug_Info       0x12345605
#define Debug_Disconnect 0x12345606
#define Debug_Close      0x12345607
#define Debug_Use        0x12345608
#define Debug_Free       0x12345609
#define Debug_Exit	 0x1234560A

#endif /* UNIX */

#define Mode_Normal      1
#define Mode_Auxiliary   2
#define Mode_Subordinate 3
#define Mode_Remote      4
#define Mode_Daemon      5

#if (UNIX)
/*
** BLV - for Hydra to support C40 boards a significant amount of work has
** been needed. Previously Hydra had a full understanding of the protocols
** used between the root processor and the I/O Server, and knew when the
** processor was reset, booting, or running. This was fine for a single
** bootstrap mechanism but not for multiple bootstrap mechanisms - the
** code would get far too convoluted. Therefore I have replaced this
** approach with a system whereby Hydra can always accept some data from
** either the root processor or the I/O Server, and there is a
** higher level protocol.
**
** By default Hydra will check every link for data from the root processor.
** If any data is present then Hydra will inform the I/O Server via a suitable
** message, but will not actually read the data. The I/O Server will be woken up
** inside its call to select() and will request the appropriate amount of data.
**
** The I/O Server can send the following requests:
**   Reset
**   Analyse
**   Write block of n bytes, followed immediately by the data
**   Read block of n bytes. This request is likely to come after Hydra
**   has indicated that data is ready, but may not. In particular during
**   a bootstrap the I/O Server may perform a read without checking that the
**   root processor has data ready.
**
** Great care has to be taken to cope sensibly with errors and the fact that
** the I/O Server and Hydra can get out of sync, for example the I/O Server could
** send a Reset request at the same time that Hydra is sending a data ready
** message.
*/

typedef struct Hydra_Message {
	int	FnRc;
	union
	{
		int	Size;
		BYTE	Buf[4];
	} Extra;
} Hydra_Message;

#define Hydra_ResetRequest	0x01
#define Hydra_ResetAck		0x02
#define Hydra_AnalyseRequest	0x03
#define Hydra_AnalyseAck	0x04
#define Hydra_DataReadyByte	0x05
#define Hydra_DataReadyWord	0x06
#define Hydra_ReadRequest	0x07
#define Hydra_ReadAck		0x08
#define Hydra_WriteRequest	0x09
#define Hydra_WriteAck		0x0A
#define Hydra_Done		0x0B
#define Hydra_Broken		0x0C
#define Hydra_Nop		0x0D
#define Hydra_MessageMode	0x0E
#define Hydra_MessageAck	0x0F

	/* Special cases for exchanging bytes and words are worthwhile	*/
	/* because the data can fit into the Size field, and does not	*/
	/* require separate socket operations.				*/
#define Hydra_WriteByte		0x10
#define Hydra_WriteWord		0x11
#define Hydra_ReadByte		0x12
#define Hydra_ReadWord		0x13

#endif

/*----------------------------------------------------------------------*/
/* Common substructure for all IOC messages				*/
/*----------------------------------------------------------------------*/

typedef word Struct;
typedef word String;

typedef struct IOCCommon {
	String		Context;	/* offset of context string	*/
	String		Name;		/* offset of object name string	*/
	String		Next;		/* offset of next element in path */
	Capability	Access;		/* capability of context object	*/
} IOCCommon;


/* Messages for Sockets */

typedef struct IOCBind
{
	IOCCommon	Common;
	word		Protocol;
	Struct		Addr;
} IOCBind;

/* Messages for Sockets */

typedef struct AcceptReply
{
	word		Type;		/* object type code 		*/
	word 		Flags;		/* flag word			*/
	Capability	Access;		/* a capability for it		*/
	String		Pathname;	/* full pathname of object	*/
	Struct		Addr;		/* network address of connector	*/	
} AcceptReply;

typedef struct ConnectRequest {
	Struct		DestAddr;	/* network address for connection */
	Struct		SourceAddr;	/* address of source		  */
} ConnectRequest;

/* This structure is built progressively as it is passed from program to*/
/* program, SendMessage builds it as far as DestAddr, the server adds	*/
/* SourceAddr and passes it back to SendMessage which adds the data and	*/
/* forwards it to RecvMessage.						*/

typedef struct DataGram {
	word		Flags;		/* flag word			*/
	word		DataSize;	/* actual data size		*/
	word		Timeout;	/* time to wait for tfr		*/
	Struct		AccRights;	/* access rights		*/
	Struct		DestAddr;	/* destination address		*/
	Struct		SourceAddr;	/* source address		*/
	Offset		Data;		/* message data			*/
} DataGram;

typedef struct SocketInfoReq
{
	word		Level;		/* option level			*/
	word		Option;		/* option name			*/
	Struct		Optval;		/* option value (optional)	*/
} SocketInfoReq;

/* Any new message structures should be added to the appropriate union here */

typedef struct ReadWrite {
	word		Pos;		/* file position		*/
	word		Size;		/* size of transfer		*/
	word		Timeout;	/* transfer completion time	*/
} ReadWrite;

typedef struct GetSizeReply {
	word		Size;		/* file size in bytes		*/
} GetSizeReply;

typedef struct SeekRequest {
	word		CurPos;		/* current file position	*/
	word		Mode;		/* seek mode			*/
	word		NewPos;		/* new position (rel. to mode)	*/
} SeekRequest;


union StreamRequestSet {
	ReadWrite	ReadWrite;
	SeekRequest	SeekRequest;
	ConnectRequest	ConnectRequest;
	SocketInfoReq	SocketInfoReq;
};

typedef struct SeekReply {
	word		NewPos;		/* new file position		*/
} SeekReply;

typedef struct WriteReply {
	word		first;		/* size of first data message	*/
	word		rest;		/* size of rest of messages	*/
	word		max;		/* max qty of data to send	*/
					/* this is present only in the	*/
					/* extended protocol format	*/
} WriteReply;

union StreamReplySet {
	GetSizeReply	GetSizeReply;
	SeekReply	SeekReply;
	WriteReply	WriteReply;
	AcceptReply	AcceptReply;
};


/* C40 hardware configuration word flags */

#define HW_NucleusLocalS0	0	/* load nuc. onto Local bus strobe 0 */
					/* Above is the default */
#define HW_NucleusLocalS1	1	/* load nuc. onto Local bus strobe 1 */
#define HW_NucleusGlobalS0	2	/* load nuc. onto Global bus strobe 0 */
#define HW_NucleusGlobalS1	4	/* load nuc. onto Global bus strobe 1 */
#define HW_PseudoIDROM		8	/* send pseudo IDROM */
#define HW_ReplaceIDROM		16	/* send replacement IDROM */
#define HW_CacheOff		32	/* disable cache */


/* This structure defines the contents of the 'C40 TIM-40 IDROM. The IDROM   */
/* characterises the C40 system Helios is running on. If the board has	     */
/* no built-in IDROM a pseudo one is constructed and sent by the I/O Server. */
/* For more information see the TIM-40 specification.                        */

typedef struct IDROM {
	word	SIZE;		/* self inclusive size of this block */

	short	MAN_ID;		/* TIM-40 module manufacturers ID */
	byte	CPU_ID;		/* CPU type (00 = C40) */
	byte	CPU_CLK;	/* CPU cycle time (60ns = 59) */

	short	MODEL_NO;	/* TIM-40 module model number */
	byte	REV_LVL;	/* module revision level */
	byte	RESERVED;	/* currently unused (align to word boundary) */

	word	GBASE0;		/* address base of global bus strobe 0 */
	word	GBASE1;		/* address base of global bus strobe 1 */
	word	LBASE0;		/* address base of local bus strobe 0 */
	word	LBASE1;		/* address base of local bus strobe 1 */

				/* sizes are in words */
	word	GSIZE0;		/* size of memory on global bus strobe 0 */
	word	GSIZE1;		/* size of memory on global bus strobe 1 */
	word	LSIZE0;		/* size of memory on local bus strobe 0 */
	word	LSIZE1;		/* size of memory on local bus strobe a */

	word	FSIZE;		/* size of fast ram pool (inc. on-chip RAM) */

	/* Each of the following bytes contains two nibbles, one for */
	/* strobe 0 and one for strobe 1. The nibbles define how many cycles */
	/* it takes to read a word from that strobes associated memory. */
	byte	WAIT_G;		/* within page on global bus */
	byte	WAIT_L;		/* within page on local bus */
	byte	PWAIT_G;	/* outside page on global bus */
	byte	PWAIT_L;	/* outside page on local bus */

	word	TIMER0_PERIOD;	/* period time for 1ms interval on timer 0 */
	word	TIMER1_PERIOD;	/* period for DRAM refresh timer (optional) */
	short	TIMER0_CTRL;	/* contents set TCLK0 to access RAM not IDROM */
	short	TIMER1_CTRL;	/* sets up timer to refresh DRAM (optional) */

	word	GBCR;		/* global bus control register */
	word	LBCR;		/* local bus control register */

	word	AINIT_SIZE;	/* total size of auto-initialisation data */
} IDROM;

#ifdef __cplusplus
typedef WORD	(*WordUbyteFnPtr)(UBYTE *);
typedef WORD	(*WordIntFnPtr)(int);
typedef WORD	(*WordIntplusFnPtr)(int, BYTE *, ...);
#else
typedef WordFnPtr	WordUbyteFnPtr;
typedef WordFnPtr	WordIntFnPtr;
typedef WordFnPtr	WordIntplusFnPtr;
#endif

/* end of structs.h */
/*------------------------------------------------------------------------
--                                                                      --
--          H E L I O S   I N P U T / O U T P U T   S E R V E R         --
--          ---------------------------------------------------         --
--                                                                      --
--             Copyright (C) 1987, Perihelion Software Ltd.             --
--                        All Rights Reserved.                          --
--                                                                      --
--      fundefs.h                                                       --
--                                                                      --
--         Declarations of the functions shared between modules         --
--                                                                      --
--      Author:  BLV 8/10/87                                            --
--                                                                      --
------------------------------------------------------------------------*/
/* RcsId: $Id: fundefs.h,v 1.14 1994/06/29 13:42:25 tony Exp nickc $ */
/* Copyright (C) 1987, Perihelion Software Ltd.       			*/

#ifdef __cplusplus
#define iofree(x)	free ((void *)(x))
#else
#define iofree(x)	free (x)
#endif

/**
*** Linked list library, the same linked lists as under AmigaDos and Helios
**/
PUBLIC List *fn( MakeHeader,  (void));
PUBLIC void fn(  InitList,    (List *));
PUBLIC void fn(  WalkList,    (List *, VoidNodeFnPtr, ...));
PUBLIC void fn(  FreeList,    (List *));
PUBLIC Node *fn( AddHead,     (Node *, List *));
PUBLIC Node *fn( AddTail,     (Node *, List *));
PUBLIC Node *fn( listRemove,  (Node *));
PUBLIC Node *fn( NextNode,    (Node *));
PUBLIC word fn(  Wander,      (List *, WordNodeFnPtr, ...));
PUBLIC word fn(  TstList,     (List *));
PUBLIC word fn(  ListLength,  (List *));
PUBLIC void fn(  PreInsert,   (Node *, Node *));
PUBLIC void fn(  PostInsert,  (Node *, Node *));

/**
*** There are problems with Remove because all ANSI and pseudo-ANSI libraries
*** including the Microsoft one for the PC have a routine remove(), which
*** clashes with the list Remove with some linkers.
**/
#define Remove listRemove

/**
*** an 8086 has the same byte ordering as the transputer, a 68000 does not
**/
#if swapping_needed
PUBLIC word fn( swap, (word));
#else
#define swap(a) (a)
#endif

PUBLIC char *fn( get_config,      (char *));
PUBLIC word  fn(  get_int_config,  (char *));
PUBLIC word  fn(  mystrcmp,        (char *, char *));

PUBLIC int fn (setsu, (int));
PUBLIC void fn (convert_errno, (void));

#if (UNIX && !MEIKORTE && !UNIX386 && !IUNIX386)
PUBLIC void fn (check_helios_name, (char *));
#endif

#if (UNIX)
PUBLIC void fn (unix_restore_devices, (void));
PUBLIC void fn (unix_initialise_devices, (void));
#endif

#ifndef Daemon_Module

/**
*** A higher level coroutine library using the above linked lists.
**/
PUBLIC Conode *fn( NewCo,     (VoidConFnPtr));
PUBLIC void   fn(  TidyColib, (void));
PUBLIC void   fn(  StartCo,   (Conode *));
PUBLIC void   fn(  Suspend,   (void));
PUBLIC void   fn(  Seppuku,   (void));
PUBLIC word   fn(  InitColib, (void));
PUBLIC void   fn(  Wait,        (Semaphore *));
PUBLIC void   fn(  Signal,      (Semaphore *));
PUBLIC void   fn( InitSemaphore,(Semaphore *, int));

/**
*** Some general purpose request handlers. For example, doing things like
*** Rename on the Clock is not sensible, so Clock_Rename is #define'd to be
*** Invalidfn_handler
**/
PUBLIC  void fn( Invalidfn_handler,         (Conode *)); 
PUBLIC  void fn( Create_handler,            (Conode *));
PUBLIC  void fn( Dummy_handler,             (Conode *));

/*
 * PUBLIC  void fn( IgnoreVoid,                (void));
 */
#ifdef __cplusplus
PUBLIC  void fn (IgnoreVoid, 	(Conode *));
#else
PUBLIC  void fn (IgnoreVoid, (void));
#endif

PUBLIC  void fn( Device_ObjectInfo_handler, (Conode *));
PUBLIC  void fn( GetDefaultAttr,            (Conode *));
PUBLIC  void fn( Device_GetSize,            (Conode *));
PUBLIC  void fn( NullFn,                    (Conode *));
PUBLIC  word fn( Ignore,                    (void));
PUBLIC  void fn( Forward_handler,           (Conode *));
PUBLIC  void fn( Dir_Locate,                (Conode *));
PUBLIC  void fn( Dir_ObjInfo,               (Conode *));
PUBLIC  void fn( Dir_TidyServer,            (Conode *));
PUBLIC  ObjNode      *fn( Dir_find_node,    (Conode *));
#define Nullfn ((VoidFnPtr) NULL)
PUBLIC  ObjNode      *fn( NewObjNode,       (void));
#define NewDirEntryNode NewObjNode
PUBLIC  void fn( FreeObjNode,               (ObjNode *));
PUBLIC  void fn( Protect_handler,           (Conode *));
PUBLIC  void fn( Refine_handler,            (Conode *));
PUBLIC  void fn( Select_handler,            (Conode *));

/**
*** Some utilities
**/
PUBLIC word fn(  convert_name,    (void));
PUBLIC word fn(  flatten,         (char *));
PUBLIC word fn(  mystrcmp,        (char *, char *));
PUBLIC word fn(  get_unix_time,   (void));
PUBLIC void fn(  NewStream,       (word, word, word, VoidConFnPtr *));
PUBLIC void fn(  AddAttribute,    (Attributes *, Attribute));
PUBLIC void fn(  RemoveAttribute, (Attributes *, Attribute));
PUBLIC void fn(  SetInputSpeed,   (Attributes *, word));
PUBLIC void fn(  SetOutputSpeed,  (Attributes *, word));
PUBLIC void fn(  InitAttributes,  (Attributes *));
PUBLIC void fn(  CopyAttributes,  (Attributes *, Attributes *));
PUBLIC word fn(  IsAnAttribute,   (Attributes *, Attribute));
PUBLIC word fn(  GetInputSpeed,   (Attributes *));
PUBLIC word fn(  GetOutputSpeed,  (Attributes *));
PUBLIC word fn(  Request_Stat,    (void));
PUBLIC void fn(  Request_Return,  (word, word, word));
PUBLIC void fn(  goto_sleep,      (word));
PUBLIC word fn(  FormOpenReply,   (word, word, word, word));
PUBLIC void fn(  copy_event,      (IOEvent *, IOEvent *));
PUBLIC void fn(  pathcat,         (char *, char *));

/**
*** The various options for console/window devices.
**/
PUBLIC void fn( initialise_windowing, (void));
PUBLIC void fn( restore_windowing,    (void));
PUBLIC void fn( restart_windowing,    (void));

#if !(multiple_windows)

#if use_ANSI_emulator

#define output(a)     ANSI_out(a, &Server_window)
#define window_output ANSI_out 
PUBLIC  void fn( ANSI_out, (char *, Window *));

#if PC
PUBLIC  void fn( send_ch, (int));
#endif
#if ST
#define send_ch(x) Bconout(2, x)
#endif

#else /* use_ANSI_emulator */

#define window_output(a, b)  output(a)
PUBLIC void fn( output, (char *));

#endif /* use_ANSI_emulator */

#else /* multiple_windows */

#ifdef __cplusplus
extern "C"
{
#endif
PUBLIC word fn( create_a_window, (char *));
PUBLIC void fn( window_size,     (word, word *, word *));
PUBLIC void fn( close_window,    (word));
PUBLIC void fn( send_to_window,  (char *, Window *));
#ifdef __cplusplus
}
#endif
#if use_ANSI_emulator

PUBLIC  void fn( ANSI_out, (char *, Window *));
#define window_output ANSI_out

#else /* use_ANSI_emulator */

#define window_output send_to_window

#endif /* use_ANSI_emulator     */

#define output(a) window_output(a, &Server_window)

#endif /* multiple_windows */

PUBLIC void fn( ServerDebug, (char *, ...));
PUBLIC void fn( outputch,    (int, Window *));
#if use_ANSI_emulator
PUBLIC word fn( Init_Ansi,   (Window *, word, word));
PUBLIC void fn( Tidy_Ansi,   (Screen *));
PUBLIC void fn( Resize_Ansi, (Window *, word, word, word));
#endif

#if MSWINDOWS
	/* Under windows, printf calls should be replaced by an	*/
	/* error box mechanism.					*/
#define printf err_out
#endif

/**
*** The following declarations are used in header file server.h and module
*** server.c to indicate the functions available on each device.
*** Many functions are no-ops or cannot be implemented satisfactorily, so
*** they are hash-defined to suitable defaults. Also, some devices can
*** share code to handle particular requests.
**/

PUBLIC  void fn( Helios_InitServer, (Conode *));
#define Drive_InitServer            IgnoreVoid
#define Drive_TidyServer            IgnoreVoid
#define Drive_Private               Invalidfn_handler
#define Drive_Testfun               Nullfn
PUBLIC  void fn( Drive_Locate,      (Conode *));
PUBLIC  void fn( Drive_Open,        (Conode *));
PUBLIC  void fn( Drive_Create,      (Conode *));
PUBLIC  void fn( Drive_Delete,      (Conode *));
PUBLIC  void fn( Drive_ObjectInfo,  (Conode *));
PUBLIC  void fn( Drive_ServerInfo,  (Conode *));
PUBLIC  void fn( Drive_Rename,      (Conode *));
PUBLIC  void fn( Drive_Link,        (Conode *));
#define Drive_Protect               Protect_handler
PUBLIC  void fn( Drive_SetDate,     (Conode *));
#define Drive_Refine                Refine_handler
#define Drive_CloseObj              Invalidfn_handler

PUBLIC  word fn( File_InitStream, (Conode *));
PUBLIC  word fn( File_TidyStream, (Conode *));
#define File_PrivateStream        Invalidfn_handler
PUBLIC  void fn( File_Read,       (Conode *));
PUBLIC  void fn( File_Write,      (Conode *));
PUBLIC  void fn( File_Close,      (Conode *));
PUBLIC  void fn( File_GetSize,    (Conode *));
#define File_SetSize              Invalidfn_handler
PUBLIC  void fn( File_Seek,       (Conode *));
#define File_GetAttr              GetDefaultAttr
#define File_SetAttr              NullFn
#define File_EnableEvents         Invalidfn_handler
#define File_Acknowledge          Invalidfn_handler
#define File_NegAcknowledge       Invalidfn_handler
#define File_Select               Select_handler

PUBLIC  word fn( Dir_InitStream,  (Conode *));
PUBLIC  word fn( Dir_TidyStream,  (Conode *));
#define Dir_PrivateStream         Invalidfn_handler
PUBLIC  void fn( Dir_Read,        (Conode *));
#define Dir_Write                 Invalidfn_handler
PUBLIC  void fn( Dir_Close,       (Conode *));
PUBLIC  void fn( Dir_GetSize,     (Conode *));
#define Dir_SetSize               Invalidfn_handler
#define Dir_Seek                  Invalidfn_handler
#define Dir_GetAttr               GetDefaultAttr
#define Dir_SetAttr               NullFn
#define Dir_EnableEvents          Invalidfn_handler
#define Dir_Acknowledge           Invalidfn_handler
#define Dir_NegAcknowledge        Invalidfn_handler
#define Dir_Select                Select_handler

PUBLIC  void fn( IOPROC_InitServer, (Conode *));
PUBLIC  void fn( IOPROC_TidyServer, (Conode *));
PUBLIC  void fn( IOPROC_Private,    (Conode *));
#define IOPROC_Testfun              Nullfn
PUBLIC  void fn( IOPROC_Open,       (Conode *));
PUBLIC  void fn( IOPROC_Locate,     (Conode *));
#define IOPROC_Create               IOPROC_Locate
#define IOPROC_Delete               Forward_handler
PUBLIC  void fn( IOPROC_ObjectInfo, (Conode *));
#define IOPROC_ServerInfo           Forward_handler
PUBLIC  void fn( IOPROC_Rename,     (Conode *));
#define IOPROC_Link                 Forward_handler
#define IOPROC_Protect              Forward_handler
#define IOPROC_SetDate              Forward_handler
#define IOPROC_Refine               Forward_handler
#define IOPROC_CloseObj             Forward_handler

#define IOPROC_InitStream           Ignore
#define IOPROC_TidyStream           Ignore
#define IOPROC_PrivateStream        Invalidfn_handler
#define IOPROC_Read                 Dir_Read
#define IOPROC_Write                Invalidfn_handler
PUBLIC  void fn( IOPROC_Close,      (Conode *));
#define IOPROC_GetSize              Dir_GetSize
#define IOPROC_SetSize              Invalidfn_handler
#define IOPROC_Seek                 Invalidfn_handler
#define IOPROC_GetAttr              GetDefaultAttr
#define IOPROC_SetAttr              NullFn
#define IOPROC_EnableEvents         Invalidfn_handler
#define IOPROC_Acknowledge          IgnoreVoid
#define IOPROC_NegAcknowledge       IgnoreVoid
#define IOPROC_Select               Select_handler

#if multiple_windows
PUBLIC  void fn( Window_InitServer, (Conode *));
PUBLIC  void fn( Window_TidyServer, (Conode *));
#define Window_Private              Invalidfn_handler
PUBLIC  void fn( Window_Testfun,    (bool *));
PUBLIC  void fn( Window_Open,       (Conode *));
#define Window_Locate               Dir_Locate
PUBLIC  void fn( Window_Create,     (Conode *));
PUBLIC  void fn( Window_Delete,     (Conode *));
#define Window_ObjectInfo           Dir_ObjInfo
#define Window_ServerInfo           Invalidfn_handler
#define Window_Rename               Invalidfn_handler
#define Window_Link                 Invalidfn_handler
#define Window_Protect              Protect_handler
#define Window_SetDate              Invalidfn_handler
#define Window_Refine               Refine_handler
#define Window_CloseObj             Invalidfn_handler

#define WindowDir_InitStream        Ignore
#define WindowDir_TidyStream        Ignore
#define WindowDir_PrivateStream     Invalidfn_handler
#define WindowDir_Read              Dir_Read
#define WindowDir_Write             Invalidfn_handler
#define WindowDir_Close             IOPROC_Close
#define WindowDir_GetSize           Dir_GetSize
#define WindowDir_SetSize           Invalidfn_handler
#define WindowDir_Seek              Invalidfn_handler
#define WindowDir_GetAttr           GetDefaultAttr
#define WindowDir_SetAttr           NullFn
#define WindowDir_EnableEvents      Invalidfn_handler
#define WindowDir_Acknowledge       IgnoreVoid
#define WindowDir_NegAcknowledge    IgnoreVoid
#define WindowDir_Select            Select_handler
#endif

/**
*** The console device is always optionally available.
**/

PUBLIC  void fn( Console_InitServer, (Conode *));
#define Console_TidyServer           IgnoreVoid
#define Console_Private              Invalidfn_handler
#if multiple_windows
PUBLIC  void fn( Console_Testfun,    (bool *));
#else
#define Console_Testfun              Nullfn
#endif
PUBLIC  void fn( Console_Open,       (Conode *));
#define Console_Locate               Create_handler
#define Console_Create               Create_handler
#define Console_Delete               Invalidfn_handler
#define Console_ObjectInfo           Device_ObjectInfo_handler
#define Console_ServerInfo           Invalidfn_handler
#define Console_Rename               Invalidfn_handler
#define Console_Link                 Invalidfn_handler
#define Console_Protect              Protect_handler
#define Console_SetDate              Invalidfn_handler
#define Console_Refine               Refine_handler
#define Console_CloseObj             Invalidfn_handler

PUBLIC  word fn( Console_InitStream,   (Conode *));
PUBLIC  word fn( Console_TidyStream,   (Conode *));
#define Console_PrivateStream          Invalidfn_handler
PUBLIC  void fn( Console_Read,         (Conode *));
PUBLIC  void fn( Console_Write,        (Conode *));
PUBLIC  void fn( Console_Close,        (Conode *));
#define Console_GetSize                Device_GetSize
#define Console_SetSize                Invalidfn_handler
#define Console_Seek                   Invalidfn_handler
PUBLIC  void fn( Console_GetAttr,      (Conode *));
PUBLIC  void fn( Console_SetAttr,      (Conode *));
PUBLIC  void fn( Console_EnableEvents, (Conode *));
#define Console_Acknowledge            IgnoreVoid
#define Console_NegAcknowledge         IgnoreVoid
PUBLIC  void fn(Console_Select,	       (Conode*));

PUBLIC  void fn( write_to_log,      (char *));
PUBLIC  void fn( init_logger,       (void));
PUBLIC  void fn( tidy_logger,       (void));

#define Logger_InitServer           IgnoreVoid
#define Logger_TidyServer           IgnoreVoid
#define Logger_Private              Invalidfn_handler
#define Logger_Testfun              Nullfn
PUBLIC  void fn( Logger_Open,       (Conode *));
#define Logger_Locate               Create_handler
#define Logger_Create               Create_handler
PUBLIC  void fn( Logger_Delete,     (Conode *));
PUBLIC  void fn( Logger_ObjectInfo, (Conode *));
#define Logger_ServerInfo           Invalidfn_handler
#define Logger_Rename               Invalidfn_handler
#define Logger_Link                 Invalidfn_handler
#define Logger_Protect              Protect_handler
#define Logger_SetDate              Invalidfn_handler
#define Logger_Refine               Refine_handler
#define Logger_CloseObj             Invalidfn_handler

#define Logger_InitStream             Ignore
#define Logger_TidyStream             Ignore
PUBLIC  void fn( Logger_PrivateStream,(Conode *));
PUBLIC  void fn( Logger_Read,         (Conode *));
PUBLIC  void fn( Logger_Write,        (Conode *));
PUBLIC  void fn( Logger_Close,        (Conode *));
PUBLIC  void fn( Logger_GetSize,      (Conode *));
#define Logger_SetSize                Invalidfn_handler
PUBLIC  void fn( Logger_Seek,         (Conode *));
#define Logger_GetAttr                Invalidfn_handler
#define Logger_SetAttr                Invalidfn_handler
#define Logger_EnableEvents           Invalidfn_handler
#define Logger_Acknowledge            IgnoreVoid
#define Logger_NegAcknowledge         IgnoreVoid
#define Logger_Select                 Select_handler

#define Clock_InitServer           IgnoreVoid
#define Clock_TidyServer           IgnoreVoid
#define Clock_Private              Invalidfn_handler
#define Clock_Testfun              Nullfn
#define Clock_Locate               Create_handler
#define Clock_Open                 Invalidfn_handler
#define Clock_Create               Create_handler
#define Clock_Delete               Invalidfn_handler
PUBLIC  void fn( Clock_ObjectInfo, (Conode *));
#define Clock_ServerInfo           Invalidfn_handler
#define Clock_Rename               Invalidfn_handler
#define Clock_Link                 Invalidfn_handler
#define Clock_Protect              Protect_handler
#if (UNIX || MAC || HELIOS)
#define Clock_SetDate              Invalidfn_handler
#else
PUBLIC  void fn( Clock_SetDate,    (Conode *));
#endif
#define Clock_Refine               Refine_handler
#define Clock_CloseObj             Invalidfn_handler

#if interaction_supported
#define Host_InitServer              IgnoreVoid
#define Host_TidyServer              IgnoreVoid
#define Host_Private                 Invalidfn_handler
#define Host_Testfun                 Nullfn
#define Host_Locate                  Create_handler
PUBLIC  void fn( Host_Open,          (Conode *));
#define Host_Create                  Create_handler
#define Host_Delete                  Invalidfn_handler
#define Host_ObjectInfo              Device_ObjectInfo_handler
#define Host_ServerInfo              Invalidfn_handler
#define Host_Rename                  Invalidfn_handler
#define Host_Link                    Invalidfn_handler
#define Host_Protect                 Protect_handler
#define Host_SetDate                 Invalidfn_handler
#define Host_Refine                  Refine_handler
#define Host_CloseObj                Invalidfn_handler

#define Host_InitStream              Ignore
#define Host_TidyStream              Ignore
PUBLIC  void fn( Host_PrivateStream, (Conode *));
#define Host_Read                    Invalidfn_handler 
#define Host_Write                   Invalidfn_handler
PUBLIC  void fn( Host_Close,         (Conode *));
#define Host_GetSize                 Invalidfn_handler
#define Host_SetSize                 Invalidfn_handler
#define Host_Seek                    Invalidfn_handler
#define Host_GetAttr                 Invalidfn_handler
#define Host_SetAttr                 Invalidfn_handler
#define Host_EnableEvents            Invalidfn_handler
#define Host_Acknowledge             IgnoreVoid
#define Host_NegAcknowledge          IgnoreVoid
#define Host_Select                  Select_handler
#endif /* interaction_supported */

#if (MSWINDOWS)
PUBLIC  void  fn(Graph_InitServer,   (Conode *));
PUBLIC  void  fn(Graph_TidyServer,   (Conode *));
#define Graph_Private                Forward_handler
PUBLIC  void  fn(Graph_Testfun,      (bool *));
PUBLIC  void  fn(Graph_Locate,       (Conode *));
PUBLIC  void  fn(Graph_Open,         (Conode *));
#define Graph_Create		     Graph_Locate
PUBLIC  void  fn(Graph_Delete,       (Conode *));
PUBLIC  void  fn(Graph_ObjectInfo,   (Conode *));
#define Graph_ServerInfo	     Invalidfn_handler
#define Graph_Rename		     Invalidfn_handler
#define Graph_Link		     Invalidfn_handler
#define Graph_Protect		     Protect_handler
#define Graph_SetDate		     Invalidfn_handler
#define Graph_Refine		     Refine_handler
#define Graph_CloseObj 	             Invalidfn_handler

PUBLIC  word fn(Graph_InitStream,    (Conode *));
PUBLIC  word fn(Graph_TidyStream,    (Conode *));
PUBLIC  void fn(Graph_PrivateStream, (Conode *));
PUBLIC  void fn(Graph_Read,          (Conode *));
PUBLIC  void fn(Graph_Write,         (Conode *));
PUBLIC  void fn(Graph_Close,         (Conode *));
#define Graph_GetSize                Device_GetSize
#define Graph_SetSize                Invalidfn_handler
#define Graph_Seek                   Invalidfn_handler
PUBLIC  void fn(Graph_GetAttr,       (Conode *));
PUBLIC  void fn(Graph_SetAttr,       (Conode *));
#define Graph_EnableEvents           Invalidfn_handler
#define Graph_Acknowledge            IgnoreVoid
#define Graph_NegAcknowledge         IgnoreVoid
#define Graph_Select                 Select_handler
#endif

#if gem_supported
PUBLIC  void fn( Gem_InitServer,     (Conode *));
PUBLIC  void fn( Gem_TidyServer,     (Conode *));
#define Gem_Private                  Invalidfn_handler
#if Gem_Always_Available
#define Gem_Testfun                  Nullfn
#else
PUBLIC  void fn( Gem_Testfun,        (bool *));
#endif
#define Gem_Locate                   Create_handler
PUBLIC  void fn( Gem_Open,           (Conode *));
#define Gem_Create                   Create_handler
#define Gem_Delete                   Invalidfn_handler
#define Gem_ObjectInfo               Device_ObjectInfo_handler
#define Gem_ServerInfo               Invalidfn_handler
#define Gem_Rename                   Invalidfn_handler
#define Gem_Link                     Invalidfn_handler
#define Gem_Protect                  Protect_handler
#define Gem_SetDate                  Invalidfn_handler
#define Gem_Refine                   Refine_handler
#define Gem_CloseObj                 Invalidfn_handler

PUBLIC  word fn( Gem_InitStream,     (Conode *));
#define Gem_TidyStream               Ignore
PUBLIC  void fn( Gem_PrivateStream,  (Conode *));
#define Gem_Read                     Invalidfn_handler
#define Gem_Write                    Invalidfn_handler
PUBLIC  void fn( Gem_Close,          (Conode *));
#define Gem_GetSize                  Invalidfn_handler
#define Gem_SetSize                  Invalidfn_handler
#define Gem_Seek                     Invalidfn_handler
#define Gem_GetAttr                  Invalidfn_handler
#define Gem_SetAttr                  Invalidfn_handler
PUBLIC  void fn( Gem_EnableEvents,   (Conode *));
#define Gem_Acknowledge              IgnoreVoid
#define Gem_NegAcknowledge           IgnoreVoid
#define Gem_Select                   Select_handler

PUBLIC void fn( poll_gem,    (void));
PUBLIC void fn( restart_gem, (void));
PUBLIC void fn( vdi,         (int **));
#endif /* gem_supported */

/**
*** The RS232, Centronics Midi and Printer devices share most of their code
**/
#if (Ports_used || Rawdisk_supported)
PUBLIC void fn( Port_Open,       (Conode *));
PUBLIC void fn( Port_TidyServer, (Conode *));
PUBLIC void fn( Port_Close,      (Conode *));
PUBLIC void fn( Port_Rename,     (Conode *));
PUBLIC void fn( Port_Read,       (Conode *));
PUBLIC void fn( Port_Write,      (Conode *));
PUBLIC void fn( Port_GetAttr,    (Conode *));
PUBLIC void fn( Port_SetAttr,    (Conode *));

#define PortDir_InitStream       Ignore
#define PortDir_TidyStream       Ignore
#define PortDir_PrivateStream    Invalidfn_handler
#define PortDir_Read             Dir_Read
#define PortDir_Write            Invalidfn_handler
#define PortDir_Close            IOPROC_Close
#define PortDir_GetSize          Dir_GetSize
#define PortDir_SetSize          Invalidfn_handler
#define PortDir_Seek             Invalidfn_handler
#define PortDir_GetAttr          GetDefaultAttr
#define PortDir_SetAttr          NullFn
#define PortDir_EnableEvents     Invalidfn_handler
#define PortDir_Acknowledge      IgnoreVoid
#define PortDir_NegAcknowledge   IgnoreVoid
#define PortDir_Select           Select_handler

#endif

#if RS232_supported
PUBLIC  void fn( RS232_InitServer,   (Conode *));
PUBLIC  void fn( RS232_TidyServer,   (Conode *));
#define RS232_Private                Invalidfn_handler
#if RS232_Always_Available
#define RS232_Testfun                Nullfn
#else
PUBLIC  void fn( RS232_Testfun,      (bool *));
#endif
#define RS232_Locate                 Dir_Locate
#define RS232_Open                   Port_Open
#define RS232_Create                 Dir_Locate
#define RS232_Delete                 Invalidfn_handler
#define RS232_ObjectInfo             Dir_ObjInfo
#define RS232_ServerInfo             Invalidfn_handler
#define RS232_Rename                 Port_Rename
#define RS232_Link                   Invalidfn_handler
#define RS232_Protect                Protect_handler
#define RS232_SetDate                Invalidfn_handler
#define RS232_Refine                 Refine_handler
#define RS232_CloseObj               Invalidfn_handler
#define RS232_InitStream             Ignore
#define RS232_TidyStream             Ignore
#define RS232_PrivateStream          Invalidfn_handler
#define RS232_Read                   Port_Read
#define RS232_Write                  Port_Write
#define RS232_Close                  Port_Close
#define RS232_GetSize                Device_GetSize
#define RS232_SetSize                Invalidfn_handler
#define RS232_Seek                   Invalidfn_handler
#define RS232_GetAttr                Port_GetAttr
#define RS232_SetAttr                Port_SetAttr
PUBLIC  void fn( RS232_EnableEvents, (Conode *));
#define RS232_Acknowledge            IgnoreVoid
#define RS232_NegAcknowledge         IgnoreVoid
#define RS232_Select                 Select_handler

PUBLIC  word fn( RS232_initlist,       (List *, ComsPort **));
PUBLIC  void fn( RS232_check_events,   (void));
PUBLIC  void fn( RS232_disable_events, (ComsPort *));
PUBLIC  void fn( RS232_enable_events,  (ComsPort *, word, word));
#endif

#if Centronics_supported
PUBLIC  void fn( Centronics_InitServer, (Conode *));
#define Centronics_TidyServer           Port_TidyServer
#define Centronics_Private              Invalidfn_handler
#if Centronics_Always_Available
#define Centronics_Testfun              Nullfn
#else
PUBLIC  void fn( Centronics_Testfun,    (bool *));
#endif
#define Centronics_Locate               Dir_Locate
#define Centronics_Open                 Port_Open
#define Centronics_Create               Dir_Locate
#define Centronics_Delete               Invalidfn_handler
#define Centronics_ObjectInfo           Dir_ObjInfo
#define Centronics_ServerInfo           Invalidfn_handler
#define Centronics_Rename               Port_Rename
#define Centronics_Link                 Invalidfn_handler
#define Centronics_Protect              Protect_handler
#define Centronics_SetDate              Invalidfn_handler
#define Centronics_Refine               Refine_handler
#define Centronics_CloseObj             Invalidfn_handler

#define Centronics_InitStream           Ignore
#define Centronics_TidyStream           Ignore
#define Centronics_PrivateStream        Invalidfn_handler
#if Centronics_readable
#define Centronics_Read                 Port_Read
#else
#define Centronics_Read                 Invalidfn_handler
#endif
#define Centronics_Write                Port_Write
#define Centronics_Close                Port_Close
#define Centronics_GetSize              Device_GetSize
#define Centronics_SetSize              Invalidfn_handler
#define Centronics_Seek                 Invalidfn_handler
#define Centronics_GetAttr              GetDefaultAttr
#define Centronics_SetAttr              NullFn
#define Centronics_EnableEvents         Invalidfn_handler
#define Centronics_Acknowledge          IgnoreVoid
#define Centronics_NegAcknowledge       IgnoreVoid
#define Centronics_Select               Select_handler

PUBLIC  word fn( Centronics_initlist,   (List *, ComsPort **));
#endif

#if Printer_supported
PUBLIC  void fn( Printer_InitServer,    (Conode *));
#define Printer_TidyServer              Port_TidyServer
#define Printer_Private                 Invalidfn_handler
#if Printer_Always_Available
#define Printer_Testfun                 Nullfn
#else
PUBLIC  void fn( Printer_Testfun,       (bool *));
#endif
#define Printer_Locate                  Dir_Locate
#define Printer_Open                    Port_Open
#define Printer_Create                  Dir_Locate
#define Printer_Delete                  Invalidfn_handler
#define Printer_ObjectInfo              Dir_ObjInfo
#define Printer_ServerInfo              Invalidfn_handler
#define Printer_Rename                  Port_Rename
#define Printer_Link                    Invalidfn_handler
#define Printer_Protect                 Protect_handler
#define Printer_SetDate                 Invalidfn_handler
#define Printer_Refine                  Refine_handler
#define Printer_CloseObj                Invalidfn_handler

#define Printer_InitStream              Ignore
#define Printer_TidyStream              Ignore
#define Printer_PrivateStream           Invalidfn_handler
#define Printer_Read                    Invalidfn_handler
#define Printer_Write                   Port_Write
#define Printer_Close                   Port_Close
#define Printer_GetSize                 Device_GetSize
#define Printer_SetSize                 Invalidfn_handler
#define Printer_Seek                    Invalidfn_handler
#define Printer_GetAttr                 Port_GetAttr
#define Printer_SetAttr                 Port_SetAttr
#define Printer_EnableEvents            Invalidfn_handler
#define Printer_Acknowledge             IgnoreVoid
#define Printer_NegAcknowledge          IgnoreVoid
#define Printer_Select                  Select_handler

PUBLIC  word fn( Printer_initlist,      (List *, ComsPort **));
#endif

#if Midi_supported
PUBLIC  void fn( Midi_InitServer,       (Conode *));
#define Midi_TidyServer                 Port_TidyServer
#define Midi_Private                    Invalidfn_handler
#if Midi_Always_Available
#define Midi_Testfun                    Nullfn
#else
PUBLIC  void fn( Midi_Testfun,          (bool *));
#endif
#define Midi_Locate                     Dir_Locate
#define Midi_Open                       Port_Open
#define Midi_Create                     Dir_Locate
#define Midi_Delete                     Invalidfn_handler
#define Midi_ObjectInfo                 Dir_ObjInfo
#define Midi_ServerInfo                 Invalidfn_handler
#define Midi_Rename                     Port_Rename
#define Midi_Link                       Invalidfn_handler
#define Midi_Protect                    Protect_handler
#define Midi_SetDate                    Invalidfn_handler
#define Midi_Refine                     Refine_handler
#define Midi_CloseObj                   Invalidfn_handler

#define Midi_InitStream                 Ignore
#define Midi_TidyStream                 Ignore
#define Midi_PrivateStream              Invalidfn_handler
#define Midi_Read                       Invalidfn_handler
#define Midi_Write                      Port_Write
#define Midi_Close                      Port_Close
#define Midi_GetSize                    Device_GetSize
#define Midi_SetSize                    Invalidfn_handler
#define Midi_Seek                       Invalidfn_handler
#define Midi_GetAttr                    Port_GetAttr
#define Midi_SetAttr                    Port_SetAttr
#define Midi_EnableEvents               Invalidfn_handler
#define Midi_Acknowledge                IgnoreVoid
#define Midi_NegAcknowledge             IgnoreVoid
#define Midi_Select                     Select_handler

PUBLIC  word fn( Midi_initlist,         (List *, ComsPort **));
#endif

#if Ether_supported
#define Ether_InitServer                IgnoreVoid
#define Ether_TidyServer                IgnoreVoid
#define Ether_Private                   Invalidfn_handler
extern  void fn( Ether_Testfun,         (bool *));
extern  void fn( Ether_Open,            (Conode *));
#define Ether_Locate                    Create_handler
#define Ether_Create                    Create_handler
#define Ether_Delete                    Invalidfn_handler
#define Ether_ObjectInfo                Device_ObjectInfo_handler
#define Ether_ServerInfo                Invalidfn_handler
#define Ether_Rename                    Invalidfn_handler
#define Ether_Link                      Invalidfn_handler
#define Ether_Protect                   Invalidfn_handler
#define Ether_SetDate                   Invalidfn_handler
#define Ether_Refine                    Invalidfn_handler
#define Ether_CloseObj                  Invalidfn_handler

#define Ether_InitStream                Ignore
#define Ether_TidyStream                Ignore
#define Ether_PrivateStream             Invalidfn_handler
extern  void fn( Ether_Read,            (Conode *));
extern  void fn( Ether_Write,           (Conode *));
extern  void fn( Ether_Close,           (Conode *));
#define Ether_GetSize                   Invalidfn_handler
#define Ether_SetSize                   Invalidfn_handler
#define Ether_Seek                      Invalidfn_handler
extern  void fn( Ether_GetAttr,         (Conode *));
#define Ether_SetAttr                   Invalidfn_handler
#define Ether_EnableEvents              Invalidfn_handler
#define Ether_Acknowledge               IgnoreVoid
#define Ether_NegAcknowledge            IgnoreVoid
#define Ether_Select                    Select_handler
#endif

#if Rawdisk_supported
PUBLIC  void fn( RawDisk_InitServer,    (Conode *));
#define RawDisk_TidyServer              Dir_TidyServer
#define RawDisk_Private                 Invalidfn_handler
PUBLIC  void fn( RawDisk_Testfun,       (bool *));
PUBLIC  void fn( RawDisk_Open,          (Conode *));
#define RawDisk_Locate                  Dir_Locate
#define RawDisk_Create                  Dir_Locate
#define RawDisk_Delete                  Invalidfn_handler
#define RawDisk_ObjectInfo              Dir_ObjInfo
#define RawDisk_ServerInfo              Invalidfn_handler
#define RawDisk_Rename                  Invalidfn_handler
#define RawDisk_Link                    Invalidfn_handler
#define RawDisk_Protect                 Protect_handler
#define RawDisk_SetDate                 Invalidfn_handler
#define RawDisk_Refine                  Refine_handler
#define RawDisk_CloseObj                Invalidfn_handler

#define RawDisk_InitStream         Ignore
#define RawDisk_TidyStream         Ignore
#define RawDisk_PrivateStream      Invalidfn_handler
PUBLIC  void fn( RawDisk_Read,     (Conode *));
PUBLIC  void fn( RawDisk_Write,    (Conode *));
PUBLIC  void fn( RawDisk_Close,    (Conode *));
PUBLIC  void fn( RawDisk_GetSize,  (Conode *));
#define RawDisk_SetSize            Invalidfn_handler
PUBLIC  void fn( RawDisk_Seek,     (Conode *));
#define RawDisk_GetAttr            Invalidfn_handler
#define RawDisk_SetAttr            Invalidfn_handler
#define RawDisk_EnableEvents       Invalidfn_handler
#define RawDisk_Acknowledge        IgnoreVoid
#define RawDisk_NegAcknowledge     IgnoreVoid
#define RawDisk_Select             Select_handler

#endif
#if Romdisk_supported
PUBLIC  void fn( RomDisk_InitServer,    (Conode *));
#define RomDisk_TidyServer              Dir_TidyServer
#define RomDisk_Private                 Invalidfn_handler
PUBLIC  void fn( RomDisk_Testfun,       (bool *));
PUBLIC  void fn( RomDisk_Open,          (Conode *));
#define RomDisk_Locate                  Dir_Locate
#define RomDisk_Create                  Dir_Locate
#define RomDisk_Delete                  Invalidfn_handler
#define RomDisk_ObjectInfo              Dir_ObjInfo
#define RomDisk_ServerInfo              Invalidfn_handler
#define RomDisk_Rename                  Invalidfn_handler
#define RomDisk_Link                    Invalidfn_handler
#define RomDisk_Protect                 Protect_handler
#define RomDisk_SetDate                 Invalidfn_handler
#define RomDisk_Refine                  Refine_handler
#define RomDisk_CloseObj                Invalidfn_handler

#define RomDisk_InitStream         Ignore
#define RomDisk_TidyStream         Ignore
#define RomDisk_PrivateStream      Invalidfn_handler
PUBLIC  void fn( RomDisk_Read,     (Conode *));
#define RomDisk_Write      	   Invalidfn_handler
PUBLIC  void fn( RomDisk_Close,    (Conode *));
PUBLIC  void fn( RomDisk_GetSize,  (Conode *));
#define RomDisk_SetSize            Invalidfn_handler
#define RomDisk_Seek           	   Invalidfn_handler
#define RomDisk_GetAttr            Invalidfn_handler
#define RomDisk_SetAttr            Invalidfn_handler
#define RomDisk_EnableEvents       Invalidfn_handler
#define RomDisk_Acknowledge        IgnoreVoid
#define RomDisk_NegAcknowledge     IgnoreVoid
#define RomDisk_Select             Select_handler

#endif

#if mouse_supported
PUBLIC  void fn( Mouse_InitServer,      (Conode *));
PUBLIC  void fn( Mouse_TidyServer,      (Conode *));
#define Mouse_Private                   Invalidfn_handler
#if Mouse_Always_Available
#define Mouse_Testfun                   Nullfn
#else                                                                                                                                          
PUBLIC  void fn( Mouse_Testfun,         (bool *));
#endif
PUBLIC  void fn( Mouse_Open,            (Conode *));
#define Mouse_Locate                    Create_handler
#define Mouse_Create                    Create_handler
#define Mouse_Delete                    Invalidfn_handler
#define Mouse_ObjectInfo                Device_ObjectInfo_handler
#define Mouse_ServerInfo                Invalidfn_handler
#define Mouse_Rename                    Invalidfn_handler
#define Mouse_Link                      Invalidfn_handler
#define Mouse_Protect                   Protect_handler
#define Mouse_SetDate                   Invalidfn_handler
#define Mouse_Refine                    Refine_handler
#define Mouse_CloseObj                  Invalidfn_handler

PUBLIC  word fn( Mouse_InitStream,      (Conode *));
PUBLIC  word fn( Mouse_TidyStream,      (Conode *));
#define Mouse_PrivateStream             Invalidfn_handler
#define Mouse_Read                      Invalidfn_handler
#define Mouse_Write                     Invalidfn_handler
PUBLIC  void fn( Mouse_Close,           (Conode *));
#define Mouse_GetSize                   Invalidfn_handler
#define Mouse_SetSize                   Invalidfn_handler
#define Mouse_Seek                      Invalidfn_handler
#define Mouse_GetAttr                   Invalidfn_handler
#define Mouse_SetAttr                   Invalidfn_handler
PUBLIC  void fn( Mouse_EnableEvents,    (Conode *));
PUBLIC  void fn( Mouse_Acknowledge,     (Conode *));
PUBLIC  void fn( Mouse_NegAcknowledge,  (Conode *));
#define Mouse_Select                    Select_handler

PUBLIC  void fn( initialise_mouse,      (void));
PUBLIC  void fn( tidy_mouse,            (void));
PUBLIC  void fn( start_mouse,           (void));
PUBLIC  void fn( stop_mouse,            (void));
#endif

#if keyboard_supported
PUBLIC  void fn( Keyboard_InitServer,     (Conode *));
PUBLIC  void fn( Keyboard_TidyServer,     (Conode *));
#define Keyboard_Private                  Invalidfn_handler
#if Keyboard_Always_Available
#define Keyboard_Testfun                  Nullfn
#else
PUBLIC  void fn( Keyboard_Testfun,        (bool *));
#endif
PUBLIC  void fn( Keyboard_Open,           (Conode *));
#define Keyboard_Locate                   Create_handler
#define Keyboard_Create                   Create_handler
#define Keyboard_Delete                   Invalidfn_handler
#define Keyboard_ObjectInfo               Device_ObjectInfo_handler
#define Keyboard_ServerInfo               Invalidfn_handler
#define Keyboard_Rename                   Invalidfn_handler
#define Keyboard_Link                     Invalidfn_handler
#define Keyboard_Protect                  Protect_handler
#define Keyboard_SetDate                  Invalidfn_handler
#define Keyboard_Refine                   Refine_handler
#define Keyboard_CloseObj                 Invalidfn_handler
PUBLIC  word fn( Keyboard_InitStream,     (Conode *));
PUBLIC  word fn( Keyboard_TidyStream,     (Conode *));
#define Keyboard_PrivateStream            Invalidfn_handler
#define Keyboard_Read                     Invalidfn_handler
#define Keyboard_Write                    Invalidfn_handler
PUBLIC  void fn( Keyboard_Close,          (Conode *));
#define Keyboard_GetSize                  Invalidfn_handler
#define Keyboard_SetSize                  Invalidfn_handler
#define Keyboard_Seek                     Invalidfn_handler
#define Keyboard_GetAttr                  Invalidfn_handler
#define Keyboard_SetAttr                  Invalidfn_handler
PUBLIC  void fn( Keyboard_EnableEvents,   (Conode *));
PUBLIC  void fn( Keyboard_Acknowledge,    (Conode *));
PUBLIC  void fn( Keyboard_NegAcknowledge, (Conode *));
#define Keyboard_Select                   Select_handler

PUBLIC  void fn( initialise_keyboard,     (void));
PUBLIC  void fn( tidy_keyboard,           (void));
PUBLIC  void fn( start_keyboard,          (void));
PUBLIC  void fn( stop_keyboard,           (void));
#endif

#if X_supported
PUBLIC  void fn( X_InitServer, (Conode *));
PUBLIC  void fn( X_TidyServer, (Conode *));
#define X_Private              Invalidfn_handler
PUBLIC  void fn( X_Testfun,    (bool *));
PUBLIC  void fn( X_Open,       (Conode *));
#define X_Locate               Dir_Locate
PUBLIC  void fn( X_Create,     (Conode *));
PUBLIC  void fn( X_Delete,     (Conode *));
#define X_ObjectInfo           Dir_ObjInfo
#define X_ServerInfo           Invalidfn_handler
#define X_Rename               Invalidfn_handler
#define X_Link                 Invalidfn_handler
#define X_Protect              Protect_handler
#define X_SetDate              Invalidfn_handler
#define X_Refine               Refine_handler
#define X_CloseObj             Invalidfn_handler

#define XDir_InitStream        Ignore
#define XDir_TidyStream        Ignore
#define XDir_PrivateStream     Invalidfn_handler
#define XDir_Read              Dir_Read
#define XDir_Write             Invalidfn_handler
#define XDir_Close             IOPROC_Close
#define XDir_GetSize           Dir_GetSize
#define XDir_SetSize           Invalidfn_handler
#define XDir_Seek              Invalidfn_handler
#define XDir_GetAttr           GetDefaultAttr
#define XDir_SetAttr           NullFn
#define XDir_EnableEvents      Invalidfn_handler
#define XDir_Acknowledge       IgnoreVoid
#define XDir_NegAcknowledge    IgnoreVoid
#define XDir_Select            Select_handler

PUBLIC  word fn( X_InitStream,   (Conode *));
PUBLIC  word fn( X_TidyStream,   (Conode *));
#define X_PrivateStream          Invalidfn_handler
PUBLIC  void fn( X_Read,         (Conode *));
PUBLIC  void fn( X_Write,        (Conode *));
PUBLIC  void fn( X_Close,        (Conode *));
#define X_GetSize                Device_GetSize
#define X_SetSize                Invalidfn_handler
#define X_Seek                   Invalidfn_handler
#define X_GetAttr                GetDefaultAttr
#define X_SetAttr                NullFn
#define X_EnableEvents           Invalidfn_handler
#define X_Acknowledge            IgnoreVoid
#define X_NegAcknowledge         IgnoreVoid
#define X_Select                 Select_handler

#endif

#if Network_supported
#define Network_InitServer              IgnoreVoid
#define Network_TidyServer              IgnoreVoid
#define Network_Private                 Invalidfn_handler
#define Network_Testfun                 Nullfn
#define Network_Locate                  Create_handler
PUBLIC  void fn( Network_Open,          (Conode *));
#define Network_Create                  Create_handler
#define Network_Delete                  Invalidfn_handler
#define Network_ObjectInfo              Device_ObjectInfo_handler
#define Network_ServerInfo              Invalidfn_handler
#define Network_Rename                  Invalidfn_handler
#define Network_Link                    Invalidfn_handler
#define Network_Protect                 Protect_handler
#define Network_SetDate                 Invalidfn_handler
#define Network_Refine                  Refine_handler
#define Network_CloseObj                Invalidfn_handler

#define Network_InitStream              Ignore
#define Network_TidyStream              Ignore
PUBLIC  void fn( Network_PrivateStream, (Conode *));
#define Network_Read                    Invalidfn_handler 
#define Network_Write                   Invalidfn_handler
PUBLIC  void fn( Network_Close,         (Conode *));
#define Network_GetSize                 Invalidfn_handler
#define Network_SetSize                 Invalidfn_handler
#define Network_Seek                    Invalidfn_handler
#define Network_GetAttr                 Invalidfn_handler
#define Network_SetAttr                 Invalidfn_handler
#define Network_EnableEvents            Invalidfn_handler
#define Network_Acknowledge             IgnoreVoid
#define Network_NegAcknowledge          IgnoreVoid
#define Network_Select                  Select_handler

#endif /* Network_supported */

#if internet_supported
PUBLIC  void fn( Internet_InitServer, (Conode *));
PUBLIC  void fn( Internet_TidyServer, (Conode *));
#define Internet_Private              Internet_PrivateStream
#define Internet_Testfun              Nullfn
PUBLIC  void fn( Internet_Open,       (Conode *));
#define Internet_Create               Invalidfn_handler
#define Internet_Locate               Dir_Locate
PUBLIC  void fn( Internet_ObjectInfo, (Conode *));
#define Internet_ServerInfo           Invalidfn_handler
#define Internet_Delete               Invalidfn_handler
#define Internet_Rename               Invalidfn_handler
#define Internet_Link                 Invalidfn_handler
#define Internet_Protect              Protect_handler
#define Internet_SetDate              Invalidfn_handler
#define Internet_Refine               Refine_handler
#define Internet_CloseObj             Invalidfn_handler
#define Internet_Revoke               Invalidfn_handler

#define InternetDir_InitStream        Ignore
#define InternetDir_TidyStream        Ignore
#define InternetDir_PrivateStream     Invalidfn_handler
#define InternetDir_Read              Dir_Read
#define InternetDir_Write             Invalidfn_handler
#define InternetDir_Close             IOPROC_Close
#define InternetDir_GetSize           Dir_GetSize
#define InternetDir_SetSize           Invalidfn_handler
#define InternetDir_Seek              Invalidfn_handler
#define InternetDir_GetAttr           GetDefaultAttr
#define InternetDir_SetAttr           NullFn
#define InternetDir_EnableEvents      Invalidfn_handler
#define InternetDir_Acknowledge       IgnoreVoid
#define InternetDir_NegAcknowledge    IgnoreVoid
#define InternetDir_Select            Select_handler

PUBLIC  word fn( Internet_InitStream,   (Conode *));
#define Internet_TidyStream             Ignore
PUBLIC  void fn( Internet_PrivateStream,(Conode *));
PUBLIC  void fn( Internet_Read,         (Conode *));
PUBLIC  void fn( Internet_Write,        (Conode *));
PUBLIC  void fn( Internet_Close,        (Conode *));
PUBLIC  void fn( Internet_GetSize,      (Conode *));
PUBLIC  void fn( Internet_SetSize,      (Conode *));
#define Internet_Seek                   Invalidfn_handler
PUBLIC  void fn( Internet_GetInfo,      (Conode *));
PUBLIC  void fn( Internet_SetInfo,      (Conode *));
#define Internet_EnableEvents           Invalidfn_handler
#define Internet_Acknowledge            IgnoreVoid
#define Internet_NegAcknowledge         IgnoreVoid
PUBLIC  void fn( Internet_Select,       (Conode *));
#endif

/**
*** Explicit declarations of the memory allocation functions
***
*** For most of the hardware on which the server is expected to run malloc()
*** takes an unsigned integer as argument, which may limit it to 64K. The main
*** server sources never need buffers more than 64K, so that is fine.
***
*** If the system's memory allocation is unsatisfactory for one reason or
*** another, I have written my own. See st/stlocal.c and ibm/pclocal.c for
*** details.
**/

#if use_own_memory_management
PUBLIC void  fn(  initialise_memory, (void));
PUBLIC char *fn(  get_mem,           (uint));
PUBLIC void  fn(  free_mem,          (char *));
PUBLIC void  fn(  memory_map,        (void));       /* a debugging facility */
#define malloc(a)  get_mem(a)
#define free(a)    free_mem((char *) (a))

#else

#if !(SOLARIS || SUN3 || SUN4)

/* free/malloc declared in <stdlib.h> */

#if !(PC || MAC || HELIOS || RS6000)     /* The PC's header files declare malloc */
#if !(AMIGA)
PUBLIC char *fn( malloc,      (uint));
/* PUBLIC void fn( free,         (char *)); */	/* now included via <stdlib.h> */
#else
PUBLIC char *fn( malloc,      (uint));
PUBLIC void fn( free,         (void *));
#endif
#endif

#endif /* !SOLARIS */

#endif /* use_own_memory_management */

/**
*** Here are some functions to deal with the multi-tasking support.
*** The only implementation I have so far is for the Sun
**/
#if multi_tasking
/**
*** These routines are called when the Server starts-up and leaves
*** server mode, and during restarts. 
**/
PUBLIC void fn( InitMultiwait,    (void));
PUBLIC void fn( RestartMultiwait, (void));
PUBLIC void fn( TidyMultiwait,    (void));
/**
*** This routine is called from inside the Server's main loop. It should
*** return when one of the I/O's the Server is currently waiting for is
*** possible, or after 1/2 a second to allow the Server to deal correctly
*** with timeouts.
**/
PUBLIC word fn( Multiwait, (void));
/**
*** This routine is called by the Server whenever it is waiting on some
*** form of I/O. The first argument is one of the constants defined in
*** structs.h, specifying the particular form of I/O. The second argument
*** is a pointer to an integer. Whenever the MultiWait is satisfied for the
*** event, that integer should be zapped to the value CoReady. Additional
*** arguments are available if required, e.g. to specify a particular
*** file descriptor.
**/
/**
*** ClearMultiwait() is just the inverse of AddMultiwait()
**/

#ifdef __cplusplus
extern "C"
{
#endif

#if (RS6000 || HP9000 || SCOUNIX)

PUBLIC void fn (AddMultiwait, (WORD, WORD *, ...));
PUBLIC void fn( ClearMultiwait, (WORD, ...));

#else

PUBLIC void fn (AddMultiwait, (long, long *, long, long));
PUBLIC void fn (ClearMultiwait, (long, long, long));

#endif

#ifdef __cplusplus
}
#endif

#endif

/**
*** Bits that had been left out before.
**/
PUBLIC  void fn( initialise_devices,      (void));
PUBLIC  void fn( initialise_files,        (void));
PUBLIC  void fn( restart_devices,         (void));
PUBLIC  void fn( restore_devices,         (void));
PUBLIC  void fn( poll_the_devices,        (void));
PUBLIC  int  fn( read_char_from_keyboard, (word));
PUBLIC  int  fn( init_boot,               (void));
PUBLIC  void fn( boot_processor,          (int));
PUBLIC  void fn( tidy_boot,               (void));
PUBLIC  void fn( init_main_message,       (void));
PUBLIC  void fn( free_main_message,       (void));
PUBLIC  word fn( GetMsg,                  (MCB *));
PUBLIC  word fn( PutMsg,                  (MCB *));
#if debugger_incorporated
PUBLIC  void fn( init_debug,              (void));
PUBLIC  int  fn( debug,                   (void));
PUBLIC  void fn( tidy_debug,              (void));
#endif

#if floppies_available
PUBLIC  word fn( format_floppy, (char *, word, word, word, char *));
#endif

PUBLIC  int  fn( loadimage,               (void));
PUBLIC  void fn( resetlnk,                (void));
PUBLIC  void fn( xpreset,                 (void));
PUBLIC  void fn( xpanalyse,               (void));
PUBLIC  word fn( xpwrbyte,                (word));
PUBLIC  word fn( xpwrrdy,                 (void));
PUBLIC  word fn( xprdrdy,                 (void));
PUBLIC  word fn( xpwrword,                (word));
PUBLIC  word fn( xpwrint,                 (word));
PUBLIC  word fn( xpwrdata,                (byte *, word));
PUBLIC  word fn( xprddata,                (byte *, word));
PUBLIC  word fn( dbwrword,                (word, word));
PUBLIC  word fn( dbwrint,                 (word, word));
PUBLIC  word fn( xprdint,                 (void));
PUBLIC  word fn( xprdword,                (void));
PUBLIC  word fn( xprdbyte,                (void));
PUBLIC  word fn( dbrdword,                (word));
PUBLIC  word fn( dbrdint,                 (word));

#if (PC || SUN3 || SUN4 || ARMBSD)
/* VLSI PID support */
PUBLIC  void fn( vy86pid_set_baudrate,    (word, bool));
PUBLIC  word fn( vy86pid_get_configbaud,  (void));
PUBLIC  void fn( vy86pid_setup_handshake, (void));
#endif

#endif /* Daemon_Module */

/*------------------------------------------------------------------------
--                                                                      --
--          H E L I O S   I N P U T / O U T P U T   S E R V E R         --
--          ---------------------------------------------------         --
--                                                                      --
--             Copyright (C) 1987, Perihelion Software Ltd.             --
--                        All Rights Reserved.                          --
--                                                                      --
--      server.h                                                        --
--                                                                      --
--      Author:  BLV 8/10/87                                            --
--                                                                      --
------------------------------------------------------------------------*/
/* RcsId: $Id: server.h,v 1.13 1994/06/29 13:42:25 tony Exp $ */
/* Copyright (C) 1987, Perihelion Software Ltd.   			*/

#ifndef Daemon_Module

/**
***  The following lines declare all the shared variables in the system. The
***  space for these variables is allocated in module server.c, but all the
***  other modules need to have a declaration of them.
***
***  First come some pointers needed by the coroutine library. These are
***  explained in module server.c. Heliosnode is a pointer to a coroutine
***  list node used to maintain the linked lists in a sensible order.
***
***  mcb is the pointer to the server's message buffer, which is shared by all
***  devices.
***
***  CoCount is used to keep track of coroutine numbering. Each server and
***  stream coroutine has a unique identifier, obtained by incrementing
***  CoCount each time I create something. These identifiers are equivalent
***  to Helios ports, so all ports are unique.
***
***  Time_unit is used to convert host time in units of CLK_TCK per second
***  to Helios time in units of micro seconds. Startup_Time is a Unix time
***  stamp indicating when the system started up, and Now is the current
***  host time. Initial_stamp is a clock_t value corresponding to Startup_time.
***
***  IOname is used to store the result of the name conversion routine, which
***  is applied to all server requests.
***
***  Heliosdir is the name of the special Helios directory specified in
***  the host.con file. The system will try to use /Helios/lib, /Helios/bin
***  etc., and this drive Helios has to be mapped onto one of the local
***  directories identified by Heliosdir.
***
***  Maxdata imposes a limit on the size of the data vector in messsages
***  being passed between the Server and the transputer. It can be set in
***  the configuration file.
***
***  Err_buff is used to output() strings when the Server is running.
***
***  System_image holds a string specifying the system image to be booted
***  into the transputer.
***
***  Exit_jmpbuf is a C jump buffer set up by main() to allow the
***  initialisation routines to exit cleanly. 
***  
***  Special_Reboot is set when the user presses ctrl-shift-F10. Special_Exit
***  is set for ctrl-shift-F9, and Special_Status for ctrl-shift-F8.
***  DebugMode is toggled when the user presses ctrl-shift F7. These flags
***  are checked at regular intervals so that the user can switch systems
***  easily.
***
***  To keep coroutine stack sizes down I provide two miscellaneous buffers of
***  512 bytes each. These are used by e.g. the rename handlers to store file
***  names.
***
***  Next come the arrays for handling devices. All the routines are declared
***  in fundefs.h, and most of them are dummies of one sort or another.
***
***  Floppy_errno, RS232_errno, etc. are error flags, usually set by 
***  interrupt routines when a device error occurs e.g. attempting to
***  write to the floppy drive when there is no disk.
***
***  Device_count is used to keep track of the number of devices in the
***  server. This is used by the memory debugging routine.
***
***  Server_window is the main window, used for Server output in a multiple
***  windowing system and to store the console variables if no multiple
***  windows are available. Server_window_nopop controls the popping-up
***  of this window when pseudo-windows are used.
***
***  real_windows determines whether the windowing system in use involves
***  real multiple windows, e.g. Amiga, Sunview, X, or pseudo-windows,
***  e.g. PC, dumb terminal
***
***  Server_errno is an error variable that may be set by the local routines 
***  to give more accurate error messages. The main server sources always set
***  up a probable error code before any I/O operation, and the local
***  routines can change this error code after failure to reflect the real
***  error.
***
***  number_of_links specifies the number of link adapters which the server
***  has to monitor. The default is 1, to provide upward compatibility with
***  the older versions of the server where there could only ever be one
***  link adapter. current_link identifies the current link, to be used by
***  the link I/O routines. This avoids having to add a new argument to
***  all the link I/O routines to specify the link. link_table is a table of
***  link structures as described in structs.h.
***
***  Multi_nowait is used to indicate that there are coroutines running
***  which MUST be reactivated without a call to Multiwait(), which
***  could suspend the Server for up to half a second according to the
***  spec.
***
***  target_processor is used to indicate what type of processor the server
***  is communicating with.
**/

#ifdef Server_Module
word        target_processor;
List        *WaitingCo, *PollingCo, *SelectCo;
Node        *Heliosnode;
MCB         *mcb;
word        CoCount=1L;             /* counter for coroutine id numbering */
word        time_unit;
time_t      Startup_Time;
clock_t     Now, initial_stamp;
char        IOname[IOCDataMax];
char        *Heliosdir;
word        maxdata;
char        err_buff[256];
char        system_image[80];

#if SOLARIS
jmp_buf *   exit_jmpbuf;
#define	    longjmp_exit	longjmp (*exit_jmpbuf, 1)
#else
jmp_buf     exit_jmpbuf;
#define     longjmp_exit	longjmp (exit_jmpbuf, 1)
#endif

word        Special_Reboot=false, Special_Exit=false, Special_Status=false;
int         DebugMode = 0;
int 	    EnableThatLink = 0;       /* see module tload.c */
UBYTE       *bootstrap = NULL;        /* see module tload.c */
word        bootsize;
byte        misc_buffer1[512], misc_buffer2[512];
int         Device_count;
Window      Server_window;
int         Server_windows_nopop = 0;
#if multiple_windows
DirHeader   Window_List;
int         real_windows = 0;
#endif
word        Server_errno;
int         number_of_links = 1;
int         current_link    = 0;
PRIVATE     Trans_link root_link;
Trans_link  *link_table = &root_link;
int         Multi_nowait = 0;
int         Default_BootLink = 0;
/* C40HalfDuplex flags use of a special protocol to stop HalfDuplex links */
/* blocking.								  */
word       C40HalfDuplex = FALSE;

device_declaration devices[] = 

{ 
  { Type_Directory, DefaultServerName,
     { 
	IOPROC_InitServer, 
	IOPROC_TidyServer,  
	IOPROC_Private,
	(VoidConFnPtr)IOPROC_Testfun,
	IOPROC_Open,       
	IOPROC_Create,      
	IOPROC_Locate,
	IOPROC_ObjectInfo, 
	IOPROC_ServerInfo,  
	IOPROC_Delete,
       	IOPROC_Rename,     
	IOPROC_Link,        
	IOPROC_Protect,
       	IOPROC_SetDate,    
	IOPROC_Refine,      
	IOPROC_CloseObj
      }
  }

#if Romdisk_supported
 ,{ Type_Directory, "romdisk",
     { 
	RomDisk_InitServer,  
	RomDisk_TidyServer,  
	RomDisk_Private,
	(VoidConFnPtr)RomDisk_Testfun,
	RomDisk_Open,        
	RomDisk_Create,      
	RomDisk_Locate,
	RomDisk_ObjectInfo,  
	RomDisk_ServerInfo,  
	RomDisk_Delete,
	RomDisk_Rename,      
	RomDisk_Link,        
	RomDisk_Protect,
	RomDisk_SetDate,     
	RomDisk_Refine,      
	RomDisk_CloseObj
     }
  }
#endif

 ,{ Type_Directory, "helios",
      { 
	Helios_InitServer,  
	Drive_TidyServer,   
	Drive_Private,
	(VoidConFnPtr)Drive_Testfun,
	Drive_Open,         
	Drive_Create,       
	Drive_Locate,
	Drive_ObjectInfo,   
	Drive_ServerInfo,   
	Drive_Delete,    
	Drive_Rename,       
	Drive_Link,         
	Drive_Protect,
	Drive_SetDate,      
	Drive_Refine,       
	Drive_CloseObj
      }
  }

 ,{ Type_File, "logger",
      { 
	Logger_InitServer,  
	Logger_TidyServer,   
	Logger_Private,
	(VoidConFnPtr)Logger_Testfun,
	Logger_Open,        
	Logger_Create,      
	Logger_Locate,
	Logger_ObjectInfo,  
	Logger_ServerInfo,  
	Logger_Delete,    
	Logger_Rename,      
	Logger_Link,        
	Logger_Protect,
	Logger_SetDate,     
	Logger_Refine,      
	Logger_CloseObj
      }
  }

#if !(drives_are_special)
 ,{ Type_Directory, "files",
      { 
	Drive_InitServer,   
	Drive_TidyServer,   
	Drive_Private,
	(VoidConFnPtr)Drive_Testfun,
	Drive_Open,         
	Drive_Create,       
	Drive_Locate,  
	Drive_ObjectInfo,   
	Drive_ServerInfo,   
	Drive_Delete,  
	Drive_Rename,       
	Drive_Link,         
	Drive_Protect,
	Drive_SetDate,      
	Drive_Refine,       
	Drive_CloseObj
      }
  }
#endif

 ,{ Type_File, "console",
     { 
	Console_InitServer,  
	Console_TidyServer,  
	Console_Private,
	(VoidConFnPtr)Console_Testfun,
	Console_Open,        
	Console_Create,      
	Console_Locate,
	Console_ObjectInfo,  
	Console_ServerInfo,  
	Console_Delete,   
	Console_Rename,      
	Console_Link,        
	Console_Protect,
	Console_SetDate,     
	Console_Refine,      
	Console_CloseObj
     }
  }

#if multiple_windows
 ,{ Type_Directory, "window",
     { 
	Window_InitServer,  
	Window_TidyServer,  
	Window_Private,
       	(VoidConFnPtr)Window_Testfun,
	Window_Open,        
	Window_Create,      
	Window_Locate,
	Window_ObjectInfo,  
	Window_ServerInfo,  
	Window_Delete,   
	Window_Rename,      
	Window_Link,        
	Window_Protect,
	Window_SetDate,     
	Window_Refine,      
	Window_CloseObj
     }
  }
#endif

#if Rawdisk_supported
 ,{ Type_Directory, "rawdisk",
     { 
	RawDisk_InitServer,  
	RawDisk_TidyServer,  
	RawDisk_Private,
	(VoidConFnPtr)RawDisk_Testfun,
	RawDisk_Open,        
	RawDisk_Create,      
	RawDisk_Locate,
	RawDisk_ObjectInfo,  
	RawDisk_ServerInfo,  
	RawDisk_Delete,
	RawDisk_Rename,      
	RawDisk_Link,        
	RawDisk_Protect,
	RawDisk_SetDate,     
	RawDisk_Refine,      
	RawDisk_CloseObj
     }
  }
#endif


 ,{ Type_Device, "clock", 
     { 
	Clock_InitServer,  
	Clock_TidyServer,   
	Clock_Private,
	(VoidConFnPtr)Clock_Testfun,
	Clock_Open,        
	Clock_Create,       
	Clock_Locate,
	Clock_ObjectInfo,  
	Clock_ServerInfo,   
	Clock_Delete,
	Clock_Rename,      
	Clock_Link,         
	Clock_Protect,
	Clock_SetDate,     
	Clock_Refine,       
	Clock_CloseObj
     }
  }

#if interaction_supported
 ,{ Type_File, machine_name, 
     { 
	Host_InitServer,  
	Host_TidyServer,   
	Host_Private,
	(VoidConFnPtr)Host_Testfun,
	Host_Open,        
	Host_Create,       
	Host_Locate,
	Host_ObjectInfo,  
	Host_ServerInfo,   
	Host_Delete,
	Host_Rename,      
	Host_Link,         
	Host_Protect,
	Host_SetDate,     
	Host_Refine,       
	Host_CloseObj
     }
  }
#endif

#if RS232_supported

 ,{ 
    Type_Directory, "rs232",
     { 
	RS232_InitServer,  
	RS232_TidyServer,   
	RS232_Private,
	(VoidConFnPtr)RS232_Testfun,
	RS232_Open,        
	RS232_Create,       
	RS232_Locate,
	RS232_ObjectInfo,  
	RS232_ServerInfo,   
	RS232_Delete,
	RS232_Rename,      
	RS232_Link,         
	RS232_Protect,
	RS232_SetDate,     
	RS232_Refine,       
	RS232_CloseObj
     }
  }

#endif

#if Centronics_supported

 ,{
    Type_Directory, "centronics",
     { 
	Centronics_InitServer,  
	Centronics_TidyServer, 
	Centronics_Private,
	(VoidConFnPtr)Centronics_Testfun,
	Centronics_Open,        
	Centronics_Create,     
	Centronics_Locate,
	Centronics_ObjectInfo,  
	Centronics_ServerInfo, 
	Centronics_Delete, 
	Centronics_Rename,      
	Centronics_Link,       
	Centronics_Protect,
	Centronics_SetDate,     
	Centronics_Refine,     
	Centronics_CloseObj
     }
  }

#endif

#if Printer_supported

 ,{ Type_Directory, "printers",
     { 
	Printer_InitServer,     
	Printer_TidyServer,    
	Printer_Private,
	(VoidConFnPtr)Printer_Testfun,
	Printer_Open,           
	Printer_Create,        
	Printer_Locate,
	Printer_ObjectInfo,     
	Printer_ServerInfo,    
	Printer_Delete,
	Printer_Rename,         
	Printer_Link,          
	Printer_Protect,
	Printer_SetDate,        
	Printer_Refine,        
	Printer_CloseObj
     }
  }

#endif

#if Midi_supported

 ,{ Type_Directory, "midi",
     { 
	Midi_InitServer,     
	Midi_TidyServer,    
	Midi_Private,
	(VoidConFnPtr)Midi_Testfun,
	Midi_Open,           
	Midi_Create,        
	Midi_Locate,
	Midi_ObjectInfo,     
	Midi_ServerInfo,    
	Midi_Delete,
	Midi_Rename,         
	Midi_Link,          
	Midi_Protect,
	Midi_SetDate,        
	Midi_Refine,        
	Midi_CloseObj
     }
  }

#endif

#if Ether_supported
  ,{ Type_File, "ether",
      { 
	Ether_InitServer, 
	Ether_TidyServer, 
	Ether_Private,
	(VoidConFnPtr)Ether_Testfun,
	Ether_Open, 
	Ether_Create, 
	Ether_Locate,
	Ether_ObjectInfo, 
	Ether_ServerInfo, 
	Ether_Delete,
	Ether_Rename, 
	Ether_Link, 
	Ether_Protect,
	Ether_SetDate, 
	Ether_Refine, 
	Ether_CloseObj
      }
  }

#endif

#if internet_supported

 ,{ Type_Directory, "internet",
     { 
	Internet_InitServer,    
	Internet_TidyServer,     
	Internet_Private,
	(VoidConFnPtr)Internet_Testfun,
	Internet_Open,          
	Internet_Create,         
	Internet_Locate,
	Internet_ObjectInfo,    
	Internet_ServerInfo,     
	Internet_Delete,
	Internet_Rename,        
	Internet_Link,           
	Internet_Protect,
	Internet_SetDate,       
	Internet_Refine,         
	Internet_CloseObj
     }
  }
#endif

#if mouse_supported

 ,{ Type_File, "mouse",
     { 
	Mouse_InitServer,  
	Mouse_TidyServer,   
	Mouse_Private,
	(VoidConFnPtr)Mouse_Testfun,
	Mouse_Open,        
	Mouse_Create,       
	Mouse_Locate,
	Mouse_ObjectInfo,  
	Mouse_ServerInfo,   
	Mouse_Delete,
	Mouse_Rename,      
	Mouse_Link,         
	Mouse_Protect,
	Mouse_SetDate,     
	Mouse_Refine,       
	Mouse_CloseObj
     }
  }

#endif

#if keyboard_supported

 ,{ Type_File, "keyboard",
     { 
	Keyboard_InitServer,  
	Keyboard_TidyServer, 
	Keyboard_Private,
	(VoidConFnPtr)Keyboard_Testfun,
	Keyboard_Open,        
	Keyboard_Create,     
	Keyboard_Locate,
	Keyboard_ObjectInfo,  
	Keyboard_ServerInfo, 
	Keyboard_Delete,
	Keyboard_Rename,      
	Keyboard_Link,       
	Keyboard_Protect,
	Keyboard_SetDate,     
	Keyboard_Refine,     
	Keyboard_CloseObj
     }
  }

#endif

#if gem_supported

 ,{ Type_File, "gem",  
     { 
	Gem_InitServer,  
	Gem_TidyServer,   
	Gem_Private,
	(VoidConFnPtr)Gem_Testfun,
	Gem_Open,        
	Gem_Create,       
	Gem_Locate,
	Gem_ObjectInfo,  
	Gem_ServerInfo,   
	Gem_Delete,
	Gem_Rename,      
	Gem_Link,         
	Gem_Protect,
	Gem_SetDate,     
	Gem_Refine,       
	Gem_CloseObj
     }
  }
#endif

#if X_supported

 ,{ Type_Directory, "x",
     { 
	X_InitServer,    
	X_TidyServer,     
	X_Private,
	(VoidConFnPtr)X_Testfun,
	X_Open,          
	X_Create,         
	X_Locate,
	X_ObjectInfo,    
	X_ServerInfo,     
	X_Delete,
	X_Rename,        
	X_Link,           
	X_Protect,
	X_SetDate,       
	X_Refine,         
	X_CloseObj
     }
  }
#endif

#if Network_supported

 ,{ Type_File, "NetworkController",
     { 
	Network_InitServer,     
	Network_TidyServer,    
	Network_Private,
	(VoidConFnPtr)Network_Testfun,
	Network_Open,           
	Network_Create,        
	Network_Locate,
	Network_ObjectInfo,     
	Network_ServerInfo,    
	Network_Delete,
	Network_Rename,         
	Network_Link,          
	Network_Protect,
	Network_SetDate,        
	Network_Refine,        
	Network_CloseObj
     }
  }
#endif

#if (MSWINDOWS)
   ,{ Type_Directory, "graphics",
      {
	
	Graph_InitServer,   
	Graph_TidyServer,	
	Graph_Private,
	(VoidConFnPtr)Graph_Testfun,
	Graph_Open,	    
	Graph_Create,	
	Graph_Locate,
	Graph_ObjectInfo,   
	Graph_ServerInfo,	
	Graph_Delete,
	Graph_Rename,	    
	Graph_Link, 	
	Graph_Protect,
	Graph_SetDate,	    
	Graph_Refine,	
	Graph_CloseObj
      }
    }
#endif

 ,{ 0L, NULL,  
     {  (VoidConFnPtr) NULL, (VoidConFnPtr) NULL, (VoidConFnPtr) NULL, (VoidConFnPtr) NULL,
	(VoidConFnPtr) NULL, (VoidConFnPtr) NULL, (VoidConFnPtr) NULL, (VoidConFnPtr) NULL,
	(VoidConFnPtr) NULL, (VoidConFnPtr) NULL, (VoidConFnPtr) NULL, (VoidConFnPtr) NULL,
	(VoidConFnPtr) NULL, (VoidConFnPtr) NULL, (VoidConFnPtr) NULL, (VoidConFnPtr) NULL
      }
  }

};

#if drives_are_special
VoidConFnPtr Drive_Handlers[handler_max] =
      { 
	Drive_InitServer,   
	Drive_TidyServer,   
	Drive_Private,
	(VoidConFnPtr)Drive_Testfun,
	Drive_Open,         
	Drive_Create,       
	Drive_Locate,
	Drive_ObjectInfo,   
	Drive_ServerInfo,   
	Drive_Delete,
	Drive_Rename,       
	Drive_Link,         
	Drive_Protect,
	Drive_SetDate,      
	Drive_Refine,       
	Drive_CloseObj
      };
#endif

VoidConFnPtr IOPROC_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)IOPROC_InitStream, 
	(VoidConFnPtr)IOPROC_TidyStream,
        IOPROC_PrivateStream,
        IOPROC_Read,        
	IOPROC_Write,          
	IOPROC_GetSize,
	IOPROC_SetSize,     
	IOPROC_Close,          
	IOPROC_Seek,
	IOPROC_GetAttr,     
	IOPROC_SetAttr,        
	IOPROC_EnableEvents,
	IOPROC_Acknowledge, 
	IOPROC_NegAcknowledge, 
	IOPROC_Select
      };

VoidConFnPtr Logger_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)Logger_InitStream,  
	(VoidConFnPtr)Logger_TidyStream,
        Logger_PrivateStream,
        Logger_Read,        
	Logger_Write,          
	Logger_GetSize,
	Logger_SetSize,     
	Logger_Close,          
	Logger_Seek,
	Logger_GetAttr,     
	Logger_SetAttr,        
	Logger_EnableEvents,
	Logger_Acknowledge, 
	Logger_NegAcknowledge, 
	Logger_Select
      };


VoidConFnPtr Console_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)Console_InitStream,  
	(VoidConFnPtr)Console_TidyStream,
        Console_PrivateStream,
	Console_Read,        
	Console_Write,          
	Console_GetSize,
	Console_SetSize,     
	Console_Close,          
	Console_Seek,
	Console_GetAttr,     
	Console_SetAttr,        
	Console_EnableEvents,
	Console_Acknowledge, 
	Console_NegAcknowledge, 
	Console_Select
      };

#if multiple_windows
VoidConFnPtr WindowDir_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)WindowDir_InitStream, 
	(VoidConFnPtr)WindowDir_TidyStream,
	WindowDir_PrivateStream,
	WindowDir_Read,         
	WindowDir_Write,        
	WindowDir_GetSize,
	WindowDir_SetSize,      
	WindowDir_Close,        
	WindowDir_Seek,
	WindowDir_GetAttr,      
	WindowDir_SetAttr,      
	WindowDir_EnableEvents,
	WindowDir_Acknowledge,  
	WindowDir_NegAcknowledge, 
	WindowDir_Select
      };
#endif

#if Rawdisk_supported

VoidConFnPtr Rawdisk_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)RawDisk_InitStream,  
	(VoidConFnPtr)RawDisk_TidyStream,
	RawDisk_PrivateStream,
	RawDisk_Read,          
	RawDisk_Write,        
	RawDisk_GetSize,
	RawDisk_SetSize,       
	RawDisk_Close,        
	RawDisk_Seek,
	RawDisk_GetAttr,       
	RawDisk_SetAttr,      
	RawDisk_EnableEvents,
	RawDisk_Acknowledge,   
	RawDisk_NegAcknowledge, 
	RawDisk_Select
      };
#endif
#if Romdisk_supported

VoidConFnPtr Romdisk_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)RomDisk_InitStream,  
	(VoidConFnPtr)RomDisk_TidyStream,
	RomDisk_PrivateStream,
	RomDisk_Read,          
	RomDisk_Write,        
	RomDisk_GetSize,
	RomDisk_SetSize,       
	RomDisk_Close,        
	RomDisk_Seek,
	RomDisk_GetAttr,       
	RomDisk_SetAttr,      
	RomDisk_EnableEvents,
	RomDisk_Acknowledge,   
	RomDisk_NegAcknowledge, 
	RomDisk_Select
      };
#endif

#if gem_supported
VoidConFnPtr Gem_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)Gem_InitStream,  
	(VoidConFnPtr)Gem_TidyStream,
	Gem_PrivateStream,
	Gem_Read,        
	Gem_Write,          
	Gem_GetSize,
	Gem_SetSize,     
	Gem_Close,          
	Gem_Seek,
	Gem_GetAttr,     
	Gem_SetAttr,        
	Gem_EnableEvents,
	Gem_Acknowledge, 
	Gem_NegAcknowledge, 
	Gem_Select
      };
#endif

#if interaction_supported
VoidConFnPtr Host_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)Host_InitStream,  
	(VoidConFnPtr)Host_TidyStream,
	Host_PrivateStream,
	Host_Read,        
	Host_Write,          
	Host_GetSize,
	Host_SetSize,     
	Host_Close,          
	Host_Seek,
	Host_GetAttr,     
	Host_SetAttr,        
	Host_EnableEvents,
	Host_Acknowledge, 
	Host_NegAcknowledge, 
	Host_Select
      };
#endif


#if (Ports_used || Rawdisk_supported)
VoidConFnPtr PortDir_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)PortDir_InitStream, 
	(VoidConFnPtr)PortDir_TidyStream,
	PortDir_PrivateStream,
	PortDir_Read,         
	PortDir_Write,          
	PortDir_GetSize,
	PortDir_SetSize,      
	PortDir_Close,          
	PortDir_Seek,
	PortDir_GetAttr,      
	PortDir_SetAttr,        
	PortDir_EnableEvents,
	PortDir_Acknowledge,  
	PortDir_NegAcknowledge, 
	PortDir_Select
      };
#endif

#if RS232_supported
VoidConFnPtr RS232_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)RS232_InitStream, 
	(VoidConFnPtr)RS232_TidyStream,
	RS232_PrivateStream,
	RS232_Read,         
	RS232_Write,          
	RS232_GetSize,
	RS232_SetSize,      
	RS232_Close,          
	RS232_Seek,
	RS232_GetAttr,      
	RS232_SetAttr,        
	RS232_EnableEvents,
	RS232_Acknowledge,  
	RS232_NegAcknowledge, 
	RS232_Select
      };
#endif

#if Centronics_supported
VoidConFnPtr Centronics_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)Centronics_InitStream, 
	(VoidConFnPtr)Centronics_TidyStream,
	Centronics_PrivateStream,
	Centronics_Read,        
	Centronics_Write,       
	Centronics_GetSize,
	Centronics_SetSize,     
	Centronics_Close,       
	Centronics_Seek,
	Centronics_GetAttr,     
	Centronics_SetAttr,     
	Centronics_EnableEvents,
	Centronics_Acknowledge, 
	Centronics_NegAcknowledge, 
	Centronics_Select
      };
#endif

#if Printer_supported
VoidConFnPtr Printer_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)Printer_InitStream, 
	(VoidConFnPtr)Printer_TidyStream,
	Printer_PrivateStream,
	Printer_Read,           
	Printer_Write,          
	Printer_GetSize,
	Printer_SetSize,        
	Printer_Close,          
	Printer_Seek,
	Printer_GetAttr,        
	Printer_SetAttr,        
	Printer_EnableEvents,
	Printer_Acknowledge,    
	Printer_NegAcknowledge, 
	Printer_Select 
     };
#endif

#if Midi_supported
VoidConFnPtr Midi_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)Midi_InitStream, 
	(VoidConFnPtr)Midi_TidyStream,
	Midi_PrivateStream,
	Midi_Read,           
	Midi_Write,          
	Midi_GetSize,
	Midi_SetSize,        
	Midi_Close,          
	Midi_Seek,
	Midi_GetAttr,        
	Midi_SetAttr,        
	Midi_EnableEvents,
	Midi_Acknowledge,    
	Midi_NegAcknowledge, 
	Midi_Select
      };
#endif

#if Ether_supported
VoidConFnPtr Ether_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)Ether_InitStream, 
	(VoidConFnPtr)Ether_TidyStream,
	Ether_PrivateStream,
	Ether_Read,          
	Ether_Write,          
	Ether_GetSize,
	Ether_SetSize,       
	Ether_Close,          
	Ether_Seek,
	Ether_GetAttr,       
	Ether_SetAttr,        
	Ether_EnableEvents,
	Ether_Acknowledge,   
	Ether_NegAcknowledge, 
	Ether_Select
     };
#endif

#if mouse_supported
VoidConFnPtr Mouse_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)Mouse_InitStream, 
	(VoidConFnPtr)Mouse_TidyStream,
	Mouse_PrivateStream,
	Mouse_Read,         
	Mouse_Write,          
	Mouse_GetSize,
	Mouse_SetSize,      
	Mouse_Close,          
	Mouse_Seek,
	Mouse_GetAttr,      
	Mouse_SetAttr,        
	Mouse_EnableEvents,
	Mouse_Acknowledge,  
	Mouse_NegAcknowledge, 
	Mouse_Select
      };
#endif

#if keyboard_supported
VoidConFnPtr Keyboard_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)Keyboard_InitStream, 
	(VoidConFnPtr)Keyboard_TidyStream,
	Keyboard_PrivateStream,
	Keyboard_Read,        
	Keyboard_Write,        
	Keyboard_GetSize,
	Keyboard_SetSize,     
	Keyboard_Close,        
	Keyboard_Seek,
	Keyboard_GetAttr,     
	Keyboard_SetAttr,      
	Keyboard_EnableEvents,
	Keyboard_Acknowledge, 
	Keyboard_NegAcknowledge, 
	Keyboard_Select
      };
#endif

#if X_supported
VoidConFnPtr XDir_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)XDir_InitStream, 
	(VoidConFnPtr)XDir_TidyStream,
	XDir_PrivateStream,
	XDir_Read,            
	XDir_Write,            
	XDir_GetSize,
	XDir_SetSize,         
	XDir_Close,            
	XDir_Seek,
	XDir_GetAttr,         
	XDir_SetAttr,          
	XDir_EnableEvents,
	XDir_Acknowledge,     
	XDir_NegAcknowledge,   
	XDir_Select
      };

VoidConFnPtr X_Handlers[Stream_max] =
      { (VoidConFnPtr)X_InitStream, 
	(VoidConFnPtr)X_TidyStream,
	X_PrivateStream,
	X_Read,            
	X_Write,            
	X_GetSize,
	X_SetSize,         
	X_Close,            
	X_Seek,
	X_GetAttr,         
	X_SetAttr,          
	X_EnableEvents,
	X_Acknowledge,     
	X_NegAcknowledge,   
	X_Select 
      };

#endif /* X_supported */

#if Network_supported
VoidConFnPtr Network_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)Network_InitStream, 
	(VoidConFnPtr)Network_TidyStream,
	Network_PrivateStream,
	Network_Read,        
	Network_Write,          
	Network_GetSize,
	Network_SetSize,     
	Network_Close,          
	Network_Seek,
	Network_GetAttr,     
	Network_SetAttr,        
	Network_EnableEvents,
	Network_Acknowledge, 
	Network_NegAcknowledge, 
	Network_Select
      };
#endif
                
VoidConFnPtr File_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)File_InitStream,  
	(VoidConFnPtr)File_TidyStream,
	File_PrivateStream,
	File_Read,        
	File_Write,          
	File_GetSize,
	File_SetSize,     
	File_Close,          
	File_Seek,
	File_GetAttr,     
	File_SetAttr,        
	File_EnableEvents,
	File_Acknowledge, 
	File_NegAcknowledge, 
	File_Select
      };

VoidConFnPtr Dir_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)Dir_InitStream, 
	(VoidConFnPtr)Dir_TidyStream,
	Dir_PrivateStream,
	Dir_Read,         
	Dir_Write,          
	Dir_GetSize,
	Dir_SetSize,      
	Dir_Close,          
	Dir_Seek,
	Dir_GetAttr,      
	Dir_SetAttr,        
	Dir_EnableEvents,
	Dir_Acknowledge,  
	Dir_NegAcknowledge, 
	Dir_Select
      };

#if internet_supported
VoidConFnPtr InternetDir_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)InternetDir_InitStream, 
	(VoidConFnPtr)InternetDir_TidyStream,
	InternetDir_PrivateStream,
	InternetDir_Read,        
	InternetDir_Write,         
	InternetDir_GetSize,
	InternetDir_SetSize,     
	InternetDir_Close,         
	InternetDir_Seek,
	InternetDir_GetAttr,     
	InternetDir_SetAttr,  
	InternetDir_EnableEvents,
	InternetDir_Acknowledge,
	InternetDir_NegAcknowledge,
	InternetDir_Select 
      };

VoidConFnPtr Internet_Handlers[Stream_max] =
      { 
	(VoidConFnPtr)Internet_InitStream,  
	(VoidConFnPtr)Internet_TidyStream,
	Internet_PrivateStream,
	Internet_Read,            
	Internet_Write,         
	Internet_GetSize,
	Internet_SetSize,         
	Internet_Close,         
	Internet_Seek,
	Internet_GetInfo,         
	Internet_SetInfo,       
	Internet_EnableEvents,
	Internet_Acknowledge,     
	Internet_NegAcknowledge,
	Internet_Select
      };

#endif /* Internet_supported */

#if (MSWINDOWS)
VoidConFnPtr   Graph_Handlers[Stream_max] =
      {
     	(VoidConFnPtr)Graph_InitStream,   
	(VoidConFnPtr)Graph_TidyStream,
	Graph_PrivateStream,
	Graph_Read,	     
	Graph_Write,	   
	Graph_GetSize,
	Graph_SetSize,	     
	Graph_Close,	   
	Graph_Seek,
	Graph_GetAttr,	     
	Graph_SetAttr,	   
	Graph_EnableEvents,
	Graph_Acknowledge,      
	Graph_NegAcknowledge, 
	Graph_Select
     };
#endif

int Server_Mode = Mode_Normal;

#else       /* declarations for the other modules */

extern word         target_processor;
extern int          Server_Mode;
extern List         *WaitingCo, *PollingCo, *SelectCo;
extern MCB          *mcb;
extern Node         *Heliosnode;
extern word         CoCount;
extern word         time_unit;
extern time_t       Startup_Time;
extern clock_t      Now, initial_stamp;
extern char         IOname[];
extern char         *Heliosdir;
extern char         err_buff[];
extern word         maxdata;
extern char         system_image[];
#if SOLARIS
extern jmp_buf *      	exit_jmpbuf;
#define longjmp_exit	longjmp (*exit_jmpbuf, 1)
#else
extern jmp_buf      	exit_jmpbuf;
#define longjmp_exit	longjmp (exit_jmpbuf, 1)
#endif
extern word         Special_Reboot, Special_Exit, Special_Status;
extern int          DebugMode;
extern int          EnableThatLink;
extern char         *bootstrap;
extern word         bootsize;
extern byte         misc_buffer1[], misc_buffer2[];
extern int          Device_count;
extern Window       Server_window;
extern int          Server_windows_nopop;
#if multiple_windows
extern DirHeader    Window_List;
extern int          real_windows;
#endif
extern word         Server_errno;
extern int          number_of_links, current_link;
extern Trans_link   *link_table;
extern int          Multi_nowait;
extern int          Default_BootLink;
extern word         C40HalfDuplex;

extern VoidConFnPtr Dir_Handlers[];
extern VoidConFnPtr File_Handlers[];
extern VoidConFnPtr IOPROC_Handlers[];
extern VoidConFnPtr Console_Handlers[];
extern VoidConFnPtr Logger_Handlers[];
#if multiple_windows
extern VoidConFnPtr WindowDir_Handlers[];
#endif

#if Rawdisk_supported
extern VoidConFnPtr Rawdisk_Handlers[];
#endif
#if Romdisk_supported
extern VoidConFnPtr Romdisk_Handlers[];
#endif

#if gem_supported
extern VoidConFnPtr Gem_Handlers[];
#endif
#if interaction_supported
extern VoidConFnPtr Host_Handlers[];
#endif
#if (Ports_used || Rawdisk_supported)
extern VoidConFnPtr PortDir_Handlers[];
#endif
#if RS232_supported
extern VoidConFnPtr RS232_Handlers[];
#endif
#if Centronics_supported
extern VoidConFnPtr Centronics_Handlers[];
#endif
#if Printer_supported
extern VoidConFnPtr Printer_Handlers[];
#endif
#if Midi_supported
extern VoidConFnPtr Midi_Handlers[];
#endif
#if Ether_supported
extern VoidConFnPtr Ether_Handlers[];
#endif
#if mouse_supported
extern VoidConFnPtr Mouse_Handlers[];
#endif
#if keyboard_supported
extern VoidConFnPtr Keyboard_Handlers[];
#endif
#if X_supported
extern VoidConFnPtr XDir_Handlers[];
extern VoidConFnPtr X_Handlers[];
#endif
#if Network_supported
extern VoidConFnPtr Network_Handlers[];
#endif
#if internet_supported
extern VoidConFnPtr InternetDir_Handlers[];
extern VoidConFnPtr Internet_Handlers[];
#endif
#if (MSWINDOWS)
extern VoidConFnPtr Graph_Handlers[];
#endif

#endif  /* not Server_Module */

/**
*** Device errors, e.g. writing to a floppy drive that does not contain a
*** disk, have the annoying habit of invoking interrupts rather than
*** producing nice error codes. This gives some problems when I try to
*** handle them in a general way. Roughly, I expect the local routines to
*** set the following error variables to suitable values when a device error
*** occurs, by fair means or foul, and I take care of processing the error.
*** Because they have to be provided by the local routines, I prefer to keep
*** them in the local modules - mainly because of problems with segments on
*** the PC.
**/
#ifndef Local_Module

#if RS232_supported
extern int   RS232_errno;
#endif

#if Centronics_supported
extern int Centronics_errno;
#endif

#if floppies_available
extern int floppy_errno;
#endif

#if Printer_supported
extern int Printer_errno;
#endif

#if Midi_supported
extern int Midi_errno;
#endif

#endif  /* not Local_Module */

#endif /* not daemon module */

