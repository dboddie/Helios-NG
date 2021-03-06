#ifdef USE_NORCROFT_PRAGMAS
#pragma force_top_level
#pragma include_only_once
#endif
/*
 * codebuf.h : target-independent part of object code generation, version 24
 * Copyright (C) Acorn Computers Ltd., 1988.
 * Copyright (C) Codemist Ltd, 1988.
 */

/*
 * RCS $Revision: 1.1 $
 * Checkin $Date: 1992/03/23 15:00:54 $
 * Revising $Author: nickc $
 */

#ifndef _codebuf_LOADED
#define _codebuf_LOADED 1

#ifndef _defs_LOADED
#  include "defs.h"
#endif

extern int32 dataloc;
extern int32 codebase;
extern struct LabelNumber *litlab;
extern int32 litpoolp;
extern int32 mustlitby;
extern int32 codep;

#ifdef TARGET_HAS_BSS
extern int32 bss_size;
#endif

/* xxx/gen.c obj.c and asm.c should now only access 'codeandflagvec'    */
/* via the macros code_xxx_() below.                                    */
#ifdef TARGET_HAS_BYTE_INSTRUCTIONS
#  define CodeFlag_t int32      /* dying?  */
#else
#  define CodeFlag_t unsigned char
#endif
extern struct CodeAndFlag { int32 code[CODEVECSEGSIZE];
                            CodeFlag_t flag[CODEVECSEGSIZE]; }
                   *codeandflagvec[CODEVECSEGMAX];
#define code_inst_(q) (codeandflagvec[(q) >> CODEVECSEGBITS+2])-> \
                                      code[(q)>>2 & CODEVECSEGSIZE-1]
#define code_flag_(q) (codeandflagvec[(q)>>CODEVECSEGBITS+2])-> \
                                      flag[(q)>>2 & CODEVECSEGSIZE-1]
#ifndef NO_ASSEMBLER_OUTPUT     /* i.e. lay off otherwise */
  extern VoidStar (*(codeasmauxvec[CODEVECSEGMAX]))[CODEVECSEGSIZE];
# define code_aux_(q)  (*codeasmauxvec[(q) >> CODEVECSEGBITS+2]) \
                                      [(q)>>2 & CODEVECSEGSIZE-1]
#endif

/* The following macros provide easy access to blocks for xxx/obj.c     */
#define code_instvec_(i)   (&(codeandflagvec[i]->code)[0])
#define code_flagvec_(i)   (&(codeandflagvec[i]->flag)[0])

#ifdef TARGET_HAS_HALFWORD_INSTRUCTIONS
#  define HWORD int32
#  define code_hword_(q) \
     ((unsigned16 *)(codeandflagvec[(q)>>CODEVECSEGBITS+2]->code)) \
                                        [(q)>>1 & 2*CODEVECSEGSIZE-1]
#endif

#ifdef TARGET_HAS_BYTE_INSTRUCTIONS
#define code_byte_(q) \
   ((unsigned char *)(codeandflagvec[(q) >> CODEVECSEGBITS+2]->code)) \
                            [(q) & 4*CODEVECSEGSIZE-1]
/* miserable macro to interpret int32 flags as 4 byte flags.  Hmm.      */
#define flag_byte_(q) \
   ((unsigned char *)(codeandflagvec[(q) >> CODEVECSEGBITS+2]->flag)) \
                            [(q) & 4*CODEVECSEGSIZE-1]
#endif
extern struct LabList *asm_lablist; /* exported to xxxgen.c */

extern struct LabelNumber *nextlabel(void);

extern DataInit *datainitp;     /* exported to xxxobj.c, xxxasm.c */
extern DataInit *datainitq;

extern void labeldata(Symstr *b);
extern int32 trydeletezerodata(DataInit *previous, int32 minsize);

extern void gendcAX(Symstr *sv, int32 offset, int xrflavour);
 /* (possibly external) name + offset, flavour is xr_data or xr_code */
extern void gendc0(int32 nbytes);
extern void gendcI(int32 len, int32 val);
extern void gendcE(int32 len, FloatCon *val);
#ifdef TARGET_CALL_USES_DESCRIPTOR
extern void gendcF(Symstr *sv, int32 offset);
extern int32 genfncon(Symstr* sv);
#else /* TARGET_CALL_USES_DESCRIPTOR */
#  define gendcF(sym, off) gendcAX(sym, off, xr_code)
#endif /* TARGET_CALL_USES_DESCRIPTOR */
#define gendcA(sym, off) gendcAX(sym, off, xr_data)

extern void vg_genstring(StringSegList *s, int32 size, int pad);

extern void padstatic(int32 align);

int32 totargetsex(int32 w, int flag);
/* The following routine is NOT the recommended way to do sex-change.   */
#ifdef REVERSE_OBJECT_BYTE_ORDER
int32 byte_reverse(unsigned32 val, unsigned32 sort);
#endif

extern void codeseg_stringsegs(StringSegList *x, bool incode);
extern void codeseg_flush(Symstr *strlitname);
extern int32 codeseg_function_name(Symstr *name, int32 argn);
extern void show_entry(Symstr *name, int flags);
extern void show_code(Symstr *name);
extern void outcodeword(int32 w, int32 f);
extern void outcodewordaux(int32 w, int32 f, VoidStar aux);
extern int32 codeloc(void);
extern int32 lit_findadcon(Symstr *name, int32 offset, int32 wherefrom);
extern void dumplits2(bool needsjump);
extern int lit_of_count_name(char *s);
extern void dump_count_names(void);
extern int32 lit_findword(int32 w, int32 flavour, Symstr *sym, int32 flag);
extern int32 lit_findwordaux(int32 w, int32 flavour, VoidStar aux, int32 flag);
extern int32 lit_findwordsincurpool(int32 w[], int32 count, int32 flavour);
extern int32 lit_findwordsinprevpools(int32 w[], int32 count,
                                      int32 flavour, int32 earliest);
extern int32 lit_findstringincurpool(StringSegList *s);
extern int32 lit_findstringinprevpools(StringSegList *s, int32 earliest);

extern int32 stringlength(StringSegList *s);

extern int32 addbsssym(Symstr *sym, int32 size, int32 align, bool statik, bool local);

extern void codebuf_init(void);
extern void codebuf_reinit(void);
extern void codebuf_reinit2(void);
extern void codebuf_tidy(void);

#ifdef TARGET_IS_ACW
extern void outbytef(int32 w, int f);
#endif

#endif

/* end of codebuf.h */
