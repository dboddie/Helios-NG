/*

There is a requirement to add lea and ldcon pseudo opcodes to the arm
assembler. This code may help in that it generates eaxctly the right number
of mov/mvn/add/sub instructions to load a constant.
The problem with this code, is that the values to be loaded will not be
known until the second pass (in the case of labels), hence the number of
instructions to be generated cannot be known, hence we cannot know the
value of the lables, ...
A possible solution is to have ldcon pseudo-opcode that can only accept
numeric constants, lea and ldacon pseudo-opcodes being implemented as below.
                             =

Note that labels could be allowed as long as they are not forward references!
This should be the case for all ns_const types handled by the assembler.

The solution is probably to load the constant directly from a table
inserted into the code at some point in the future. each lea/ldcon will
add 1 to the size of the table. The table being inserted either at the end of
the file, where the programmer requests it via a 'consttable' directive, or
automatically inserted (with a warning generated) whenever the 4096 (- tablesize
- 1) byte (ldr) limit is exceeded. a branch over the table is inserted just
before the table.

Most value will however be synthesised in one instruction:
	ldcon:	mov/mvn reg, value shift
	lea	add/sub reg pc, label - 8


The following code is used to implement J_MOVK (ldcon) in the C compiler, for
similar code for J_ADDR (lea), look in ncc/cc349/arm/gen.c.

*/


/* Taken from cmds/cc/ncc/cc450/c40/ops.h */

/* 2-bit binary abbreviations */

#define B_00			0x00U
#define B_01			0x01U
#define B_10			0x02U
#define B_11			0x03U


/* 3-bit binary abbreviations */

#define B_000			0x00U
#define B_001			0x01U
#define B_010			0x02U
#define B_011			0x03U
#define B_100			0x04U
#define B_101			0x05U
#define B_110			0x06U
#define B_111			0x07U


/* 4-bit binary abbreviations */

#define B_0000			0x00U
#define B_0001			0x01U
#define B_0010			0x02U
#define B_0011			0x03U
#define B_0100			0x04U
#define B_0101			0x05U
#define B_0110			0x06U
#define B_0111			0x07U
#define B_1000			0x08U
#define B_1001			0x09U
#define B_1010			0x0aU
#define B_1011			0x0bU
#define B_1100			0x0cU
#define B_1101			0x0dU
#define B_1110			0x0eU
#define B_1111			0x0fU


/* 5-bit binary abbreviations */

#define B_00000			0x00U
#define B_00001			0x01U
#define B_00010			0x02U
#define B_00011			0x03U
#define B_00100			0x04U
#define B_00101			0x05U
#define B_00110			0x06U
#define B_00111			0x07U
#define B_01000			0x08U
#define B_01001			0x09U
#define B_01010			0x0aU
#define B_01011			0x0bU
#define B_01100			0x0cU
#define B_01101			0x0dU
#define B_01110			0x0eU
#define B_01111			0x0fU
#define B_10000			0x10U
#define B_10001			0x11U
#define B_10010			0x12U
#define B_10011			0x13U
#define B_10100			0x14U
#define B_10101			0x15U
#define B_10110			0x16U
#define B_10111			0x17U
#define B_11000			0x18U
#define B_11001			0x19U
#define B_11010			0x1aU
#define B_11011			0x1bU
#define B_11100			0x1cU
#define B_11101			0x1dU
#define B_11110			0x1eU
#define B_11111			0x1fU


/* 6-bit binary abbreviations */

#define B_000000		0x00U
#define B_000001		0x01U
#define B_000010		0x02U
#define B_000011		0x03U
#define B_000100		0x04U
#define B_000101		0x05U
#define B_000110		0x06U
#define B_000111		0x07U
#define B_001000		0x08U
#define B_001001		0x09U
#define B_001010		0x0aU
#define B_001011		0x0bU
#define B_001100		0x0cU
#define B_001101		0x0dU
#define B_001110		0x0eU
#define B_001111		0x0fU
#define B_010000		0x10U
#define B_010001		0x11U
#define B_010010		0x12U
#define B_010011		0x13U
#define B_010100		0x14U
#define B_010101		0x15U
#define B_010110		0x16U
#define B_010111		0x17U
#define B_011000		0x18U
#define B_011001		0x19U
#define B_011010		0x1aU
#define B_011011		0x1bU
#define B_011100		0x1cU
#define B_011101		0x1dU
#define B_011110		0x1eU
#define B_011111		0x1fU
#define B_100000		0x20U
#define B_100001		0x21U
#define B_100010		0x22U
#define B_100011		0x23U
#define B_100100		0x24U
#define B_100101		0x25U
#define B_100110		0x26U
#define B_100111		0x27U
#define B_101000		0x28U
#define B_101001		0x29U
#define B_101010		0x2aU
#define B_101011		0x2bU
#define B_101100		0x2cU
#define B_101101		0x2dU
#define B_101110		0x2eU
#define B_101111		0x2fU
#define B_110000		0x30U
#define B_110001		0x31U
#define B_110010		0x32U
#define B_110011		0x33U
#define B_110100		0x34U
#define B_110101		0x35U
#define B_110110		0x36U
#define B_110111		0x37U
#define B_111000		0x38U
#define B_111001		0x39U
#define B_111010		0x3aU
#define B_111011		0x3bU
#define B_111100		0x3cU
#define B_111101		0x3dU
#define B_111110		0x3eU
#define B_111111		0x3fU

/* (a few( 7-bit binary abbreviations */

#define B_0110000		0x30U

/* (some) 8-bit binary abbreviations */

#define B_01100000		0x60U
#define B_01100001		0x61U
#define B_01100010		0x62U
#define B_01100011		0x63U
#define B_01100100		0x64U
#define B_01100101		0x65U
#define B_01100110		0x66U
#define B_01100111		0x67U
#define B_01101000		0x68U
#define B_01101001		0x69U
#define B_01101010		0x6aU
#define B_01101011		0x6bU
#define B_01101100		0x6cU
#define B_01101101		0x6dU
#define B_01101110		0x6eU
#define B_01101111		0x6fU
#define B_01110000		0x70U
#define B_01110001		0x71U
#define B_01110010		0x72U
#define B_01110011		0x73U
#define B_01110100		0x74U
#define B_01110101		0x75U
#define B_01110110		0x76U
#define B_01110111		0x77U
#define B_01111000		0x78U

/* taken from ncc/cc349/arm/ops.h */

/* Major opcode groups                                                   */

#define OP_RXR      0x00000000L
/*                  0x01000000L     part of RXR                          */

#define OP_RXN      0x02000000L
/*                  0x03000000L     part of RXN                          */



/* Subfields for the data processing opcodes                             */

#define F_ADD       (0x4L<<21L)
#define F_MOV       (0xdL<<21L)
#define F_MVN       (0xfL<<21L)
#define F_SUB       (0x2L<<21L)


#define F_SCC       0x00100000L
#define F_RN(n)     (((int32)(n))<<16L)
#define F_RD(n)     (((int32)(n))<<12L)
#define F_RM(n)     ((int32)(n))

/* The next few are for use with RXR format (and corresponding memory    */
/* access instructions)                                                  */

#define K_NOSHIFT   0L
#define K_LSL(n)    (((int32)(n))<<7L)                  /* 0 to 31       */
#define K_LSR(n)    (((((int32)(n))&0x1fL)<<7L)|0x20L)  /* 1 to 32       */
#define K_ASR(n)    (((((int32)(n))&0x1fL)<<7L)|0x40L)  /* 1 to 32       */
#define K_ROR(n)    ((((int32)(n))<<7L)|0x60L)          /* 1 to 31       */
#define K_RRX       0x60L                  /* one bit with extend        */
#define R_LSL(r)    ((((int32)(r))<<8L)|0x10L)   /* register shift value */
#define R_LSR(r)    ((((int32)(r))<<8L)|0x30L)   /* register shift value */
#define R_ASR(r)    ((((int32)(r))<<8L)|0x50L)   /* register shift value */
#define R_ROR(r)    ((((int32)(r))<<8L)|0x70L)   /* register shift value */


/* subfields for memory reference instructions                           */

#define F_LDR       0x00100000L
#define F_STR       0x00000000L


/*
 * Register names - some defined in target.h.
 */

/* #define R_A1 0x0L      * arg 1 & main result register                 */
#define R_A2    0x1L     /* arg 2                                        */
#define R_A3    0x2L     /* arg 3                                        */
#define R_A4    0x3L     /* arg 4                                        */
/* #define R_V1 0x4L      * register variable 1                          */
#define R_V2    0x5L     /* register variable 2                          */
#define R_V3    0x6L     /* register variable 3                          */
#define R_V4    0x7L     /* register variable 4                          */
#define R_V5    0x8L     /* register variable 5                          */
#define R_V6    0x9L     /* register variable 6                          */
/* NB the next four are NOT real register numbers - they get mapped into
 * real register numbers in armgen.c
 */
#  define R_FP  0xaL     /* Frame pointer                                */
/* #define R_IP 0xbL      * temp + used in call                          */
#  define R_SP  0xcL     /* main stack pointer                           */
#  define R_SL  0xdL     /* stack limit (usually not checked)            */
/* #define R_LR 0xeL      * link address in function calls + workspace   */
#define R_PC    0xfL     /* program counter                              */


/* taken from ncc/cc349/arm/gen.c */

#define ROR(x, n) (((x)<<(32L-(n))) | (((x)>>(n)) & ((1L<<(32L-(n)))-1L)))

static int32 eightbits(int32 n)
{
/* If n fits in an ARM immediate field this function returns a 12-bit    */
/* quantity suitable for use there. Otherwise it returns -1              */
    int32 shift;
    for (shift = 0; shift<0x1000; shift += 0x100)
    {   if ((n&0xffffff00)==0) return(shift|n);
        n = ROR(n, 30);
    }
    return(-1);
}


static bool symmetricload(RealRegister r, int32 n, int32 mask,
                          int32 shift, int32 scc)
{
    int32 c;
    for (c = 0 ; c < shift ; mask <<= 2, c += 2) {
        int32 k;
        for (k = 1-shift ; k < shift ; k++)
            if ((n & mask) == ROR((n & ~mask), shift+k)) {
                load_integer(r, n & mask, 0);
                outinstr(OP_RXR | F_ADD | scc | F_RD(r) | F_RN(r) |
                                  r | K_ROR(32-shift-k));
                return YES;
             }
     }
     return NO;
}


static void load_integer(RealRegister r, int32 n, int32 scc)
/* Set register r to the integer n, setting condition codes on scc       */
{
    int32 op = OP_RXN|F_MOV, packed = eightbits(n);
    if (packed<0) op = OP_RXN|F_MVN, packed = eightbits(~n);
    if (packed>=0) outinstr(op | F_RD(r) | scc | packed);
    else
    {   int32 pos1 = 0, size1 = 0, value1 = -1;
        int32 pos2 = 0, size2 = 0, value2 = -1;
        int32 startsize = 0, startvalue = -1;
        int32 pos = 0, value = -1, start = -1;
        int32 mask = 3;
        /* find the two longest strings of pairs of 00 or 11 (on an even bit
           boundary), setting sizen to the number of bits, posn to the ls bit,
           valuen to 0 for 00, 1 for 11 (n=1 longest)
         */
        for ( ; pos <= 32; mask = mask<<2, pos += 2) {
            int32 newvalue = mask == 0            ? -1 :
                             ((n & mask) == mask) ? 3 :
                             ((n & mask) == 0)    ? 0 :
                                                    -1;
            if (value != newvalue) {
                if (value != -1) {
                    int32 size = pos - start;
                    if (start == 0) {
                        startsize = size;
                        startvalue = value;
                        /* disregard a string in the ls bits if these are the
                           same as the ms bits (it will be included in the
                           string including the ms bits).
                         */
                        if (((n >> 30) & 3) == value) size = -1;
                    }
                    if (pos == 32 && value == startvalue)
                        size += startsize;
                    if (size >= size1) {
                        pos2 = pos1; size2 = size1; value2 = value1;
                        size1 = size; pos1 = start; value1 = value;
                    } else if (size >= size2) {
                        size2 = size; pos2 = start; value2 = value;
                    }
                }
                value = newvalue;
                start = pos;
            }
        }
        {   int32 remainder, k;
            int32 nextpos = pos1 + size1;
            if (nextpos >= 32) nextpos -= 32;
            if ( value1 == 3 && size1 > 8 &&
                 (value2 == 3 || pos1 == 0)) {
            /* both longest strings are 1s, or the longest is 1s and starts
               at the ls bit.  And in any case, the length of the longest
               string is sensible (eg beware 55aa3355).
               Maybe should also look for a (shorter) sensible length for the
               second string.
               Use MVN to construct the 8-bit chunk above the longest string
               (which will give us 1s in the two longest strings for free)
             */
                op = OP_RXN | F_MVN;
                k = (~n) & ROR(255L, (32-nextpos));
                remainder = n - ~k;
            } else if ((value1 == 3 && size1 > 8) ||
                       (value2 == 3 && size2 > 8)) {
            /* one of the two longest strings is a sensible length and of 1s
               (the other is 0s).  Take the 8-bit chunk above the string of 1s
               incremented by 1 in its last place unless that is bit 0 of n.
               (The idea being that we will generate the string of 1s by
                subtracting something)
             */
                if (value2 == 3) {
                    nextpos = pos2 + size2;
                    if (nextpos >= 32) nextpos -= 32;
                }
                op = OP_RXN | F_MOV;
                if (nextpos != 0)
                    k = (n & ROR(255L, (32 - nextpos))) + (1L << nextpos);
                   /* (k has only 8 bits - (n & ...) cannot be 255<<nextpos)
                      or it would have been included in the string of 1s
                    */
                else {
                    if (startvalue == 0) nextpos += startsize;
                    k = n & ROR(255L, (32 - nextpos));
                }
                remainder = n - k;
            } else {
                /* The longest two strings are of 0s.  Take the 8-bit chunk above
                   the longer.
                 */
                op = OP_RXN | F_MOV;
                k = n & ROR(255L, (32 - nextpos));
                remainder = n - k;
            }
            if ( eightbits(remainder) >= 0 ||
                 eightbits(-remainder) >= 0 ||
                 ( !symmetricload(r, n, 0xffff, 16, scc) &&
                   !symmetricload(r, n, 0xff00ff, 8, scc))) {
                outinstr(op | F_RD(r) | eightbits(k));
                add_integer(r, r, remainder, scc);
            }
        }
    }
}

