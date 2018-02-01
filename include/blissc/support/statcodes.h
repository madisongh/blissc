#ifndef statcodes_h__
#define statcodes_h__
/*
 *++
 * statcodes.h - status code and message definitions.
 *
 * Copyright © 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license.  See LICENSE.TXT for details.
 *--
 */
#include <string.h>
#include <stdint.h>
#include <stdarg.h>

// The statcode_t type is represented as an enum in C, but
// it's really a 32-bit integer of which 3 bits are used to
// represent a severity code and 29 bits are used for a
// message number.
#define STC_V_SEVERITY   29
#define STC_S_SEVERITY    3
#define STC_V_MSGNO       0
#define STC_S_MSGNO      29

// Note the pattern here - the high-order bit is
// set for severity levels that should be treated as
// failures, making the OK/FAIL a simple sign check
// on the status code.  The remaining two bits are
// used to differentiate the levels of OK-ness or
// or failure.
#define STC_K_SUCCESS   1U  // 001
#define STC_K_INFO      2U  // 010
#define STC_K_WARN      5U  // 101
#define STC_K_ERROR     6U  // 110
#define STC_K_FATAL     7U  // 111

// Macros to compose status codes by packing the fields into the
// right bit positions.
#define STC_MAKECODE(sev_, msgno_) \
    (((sev_)<<STC_V_SEVERITY)|(((msgno_)&~((~0U)<<STC_S_MSGNO))<<STC_V_MSGNO))
#define STC_CODE_S(m_) STC_MAKECODE(STC_K_SUCCESS,(m_))
#define STC_CODE_I(m_) STC_MAKECODE(STC_K_INFO,(m_))
#define STC_CODE_W(m_) STC_MAKECODE(STC_K_WARN,(m_))
#define STC_CODE_E(m_) STC_MAKECODE(STC_K_ERROR,(m_))
#define STC_CODE_F(m_) STC_MAKECODE(STC_K_FATAL,(m_))

// Factory macros
#undef STATCODE
#define STATCODES \
STATCODE(  0,S,NORMAL,    "normal successful completion") \
STATCODE(  1,W,WARNING,   "unknown warning") \
STATCODE(  2,E,ERROR,     "unknown error") \
STATCODE(  3,F,FATALERR,  "unknown fatal error") \
STATCODE(  4,F,FIOERR,    "file I/O error: !SZ") \
STATCODE(  5,I,MESSAGE,   "!SD") \
STATCODE(  6,I,INFORM,    "!SD") \
STATCODE(  7,W,USRWARN,   "!SD") \
STATCODE(  8,E,USRERR,    "!SD") \
STATCODE(  9,F,EXCFILCNT, "exceeded open file count maximum of !UI") \
STATCODE( 10,F,INTCMPERR, "internal compiler error (!SZ)") \
STATCODE( 11,E,LNTOOLONG, "!SZ line too long, file !SZ") \
STATCODE( 12,F,OUTOFMEM,  "out of memory error (!SZ)") \
STATCODE( 13,E,NUMCNVERR, "numeric conversion error: !SL") \
STATCODE( 14,E,REDECLARE, "name !SL is already declared") \
STATCODE( 15,E,RSVDECL,   "attempt to declare reserved word !SL") \
STATCODE( 16,F,EXCTNCNT,  "too many temporary names") \
STATCODE( 17,E,EOFERR,    "unexpected end of file: !SZ") \
STATCODE( 18,E,LITNOVAL,  "attempt to bind literal !SD with no value") \
STATCODE( 19,W,ATTRNCMPT, "attempt to redeclare name !SD with incompatible attributes") \
STATCODE( 20,W,NUMLITTRC, "numeric literal !IL too large, truncating") \
STATCODE( 21,W,DELIMEXP,  "expected delimiter '!SZ'") \
STATCODE( 22,W,QUOFNCERR, "quote-function used in inappropriate context") \
STATCODE( 23,E,INVSTRLIT, "invalid string literal") \
STATCODE( 24,W,EXPCTCE,   "expected compile-time constant expression") \
STATCODE( 25,E,INVCHRVAL, "invalid character value '!SD'") \
STATCODE( 26,W,INVNAME,   "invalid name '!SD'") \
STATCODE( 27,F,SYNTAXERR, "syntax error") \
STATCODE( 28,W,KWDEXP,    "keyword '!SZ' expected") \
STATCODE( 29,W,UNEXPCOND, "unexpected lexical conditional") \
STATCODE( 30,E,REQFILERR, "error opening REQUIRE file !SD") \
STATCODE( 31,W,CTNAMEXP,  "expecting a COMPILETIME name") \
STATCODE( 32,W,NBITSERR,  "number of bits (!UI) exceeds fullword size (!UI)") \
STATCODE( 33,E,FLDNAMEXP, "field name expected") \
STATCODE( 34,W,FLDEXPERR, "filed expansion error") \
STATCODE( 35,W,SEGNAMEXP, "data segment name expected") \
STATCODE( 36,E,EXPREXP,   "expression expected") \
STATCODE( 37,E,EXPLABEL,  "label expected") \
STATCODE( 38,E,LBLSCPERR, "scope of label '!SD' is not active") \
STATCODE( 39,E,EXITNLOOP, "EXITLOOP encountered not in loop context") \
STATCODE( 40,E,RETNRTN,   "RETURN encountered not in routine context") \
STATCODE( 41,I,RETNOVAL,  "return value specified for NOVALUE routine") \
STATCODE( 42,W,NORETVAL,  "no return value specified for non-NOVALUE routine") \
STATCODE( 43,W,EXPRVALRQ, "expression value required in this context") \
STATCODE( 44,E,INVLABEL,  "invalid label '!SD'") \
STATCODE( 45,W,STRLENERR, "string too long for fullword storage") \
STATCODE( 46,E,DIVBYZERO, "divide by zero error") \
STATCODE( 47,E,BLOCKEXP,  "expected block expression") \
STATCODE( 48,W,BNDINVERT, "low bound greater than high bound") \
STATCODE( 49,E,MULTINRNG, "multiple INRANGE lines in CASE") \
STATCODE( 50,E,MULTOURNG, "multiple OUTRANGE lines in CASE") \
STATCODE( 51,E,NUMAFTINR, "numeric literal case not permitted after INRANGE") \
STATCODE( 52,E,MULNUMCAS, "multiple case lines for same numeric value") \
STATCODE( 53,E,INSUFCASE, "missing actions for some values of CASE range") \
STATCODE( 54,E,SELNOSEL,  "no selectors in SELECT expression") \
STATCODE( 55,W,NAMEEXP,   "name expected") \
STATCODE( 56,E,STRUNMEXP, "structure name expected") \
STATCODE( 57,E,NOMACPRMS, "macro definition contains empty parameter list") \
STATCODE( 58,E,ITERKWMAC, "iteration parameters encountered for KEYWORDMACRO") \
STATCODE( 59,E,ILLMRECUR, "recursive macro call for non-conditional macro") \
STATCODE( 60,E,EXPREMAIN, "expected %REMAINING in recursive macro invocation") \
STATCODE( 61,W,OPEREXP,   "operator expected (!SZ)") \
STATCODE( 62,E,INSFUNARG, "insufficient arguments specified in call to function !SD") \
STATCODE( 63,W,SGNEXTUNS, "sign extension not supported on target machine") \
STATCODE( 64,W,LITRNGERR, "invalid range value (!IL) specified for literal") \
STATCODE( 65,E,STOCLSEXP, "storage class expected") \
STATCODE( 66,W,PSATTRMSM, "psect attributes for !SD do not match previous declaration") \
STATCODE( 67,E,PSNAMEXP,  "psect name expected") \
STATCODE( 68,W,AUSIZERR,  "allocation-unit size !IL larger than fullword size") \
STATCODE( 69,W,INVALIGN,  "invalid alignment value !IL") \
STATCODE( 70,E,FFSNAMEXP, "field or field-set name expected") \
STATCODE( 71,E,LTCEREQD,  "link-time constant expression required") \
STATCODE( 72,W,FLDNOSTRU, "FIELD specified without structure attribute") \
STATCODE( 73,E,LCLNORTN,  "LOCAL or STACKLOCAL declaration occurs outside of routine") \
STATCODE( 74,E,REGNORTN,  "REGISTER declaration occurs outside of routine") \
STATCODE( 75,W,MAPNONDCL, "MAP delcaration of non-existent name !SD") \
STATCODE( 76,E,RTNATTEXP, "routine attribute expected") \
STATCODE( 77,E,STRINGEXP, "string expected") \
STATCODE( 78,E,UNDECRSVD, "attempt to UNDECLARE reserved keyword !SL") \
STATCODE( 79,E,MODDCLEXP, "MODULE declaration expected") \
STATCODE( 80,W,NOTBUILTN, "name !SD declared BUILTIN unknown") \
STATCODE( 81,W,UNDECUND,  "attempt to UNDECLARE unknown name !SD") \
STATCODE( 82,W,FWDNOTDCL, "FORWARD name not declared: !SD") \
STATCODE( 83,F,OPENERR,   "error opening file !SL: !SZ") \
STATCODE( 84,E,INVMACFUN, "macro function encountered outside of macro expansion") \
STATCODE( 85,E,INVEXITER, "%EXITITERATION encountered in non-iterative macro !SD") \
STATCODE( 86,W,SWITCHEXP, "switch name expected") \
STATCODE( 87,W,SWITCHUNS, "switch !SD not supported") \
STATCODE( 88,E,NOMAIN,    "module main routine !SD not declared") \
STATCODE( 89,E,MNTYPERR,  "module main routine !SD not local or global") \
STATCODE( 90,W,UNEXPSWIT, "switch !SD not supported in this context") \
STATCODE( 91,W,SWITCHDUP, "switch !SD already set") \
STATCODE( 92,F,RDOUTFILE, "file_readline called on output file !SZ") \
STATCODE( 93,F,WRTINFILE, "file_writeline called on input file !SZ") \
STATCODE( 94,W,LSTOPTEXP, "listing option keyword expected") \
STATCODE( 95,E,NOCASES,   "CASE bounds result in no case actions") \
STATCODE( 96,E,MULOTHERW, "multiple OTHERWISE labels in select expression") \
STATCODE( 97,E,MULALWAYS, "multiple ALWAYS labels in select expression") \
STATCODE( 98,E,INVTOKEN,  "invalid token encountered") \
STATCODE( 99,W,EXCFUNARGS,"too many arguments passed to executable function !SD") \
STATCODE(100,E,CSNOTCONST,"character size is not a compile-time constant") \
STATCODE(101,E,INVCSIZE,  "invalid character size !UI") \
STATCODE(102,W,INITSZERR, "initializer too large for !SD") \
STATCODE(103,E,PROFNCTCE, "non-constant field reference found in PRESET") \
STATCODE(104,E,PRBADEXPR, "unexpected offset expression found in PRESET") \
STATCODE(105,E,EXCPRLIMIT,"exceeded compiler limit on PRESET expressions") \
STATCODE(106,E,PRBADVALUE,"non-constant PRESET value expression") \
STATCODE(107,E,PRINVADSZ, "PRESET field size invalid for address initializer") \
STATCODE(108,E,PROVERLAP, "overlapping initializer fields found in PRESET") \
STATCODE(109,E,EXCRTNPARS,"routine !SD exceeds parameter list size limit") \
STATCODE(110,E,ADDRVALRQ, "address expression required in this context") \
STATCODE(111,E,EXCCALPARS,"routine call exceeds parameter list size limit") \
STATCODE(112,E,INSFPARS,  "insufficient number of parameters in routine call") \
STATCODE(113,W,INVOPTLVL, "invalid OPTLEVEL setting") \
STATCODE(114,W,NAMETOOLON,"name \"!SD\" too long, truncating to !UI characters") \
STATCODE(115,E,UNDCLLABEL,"label !SD used but not declared") \
STATCODE(116,E,UNDCLNAME, "name !SD used but not declared") \
STATCODE(117,E,INVLIBDCL, "declaration not allowed in library generation") \
STATCODE(118,E,INVLIBHDR, "invalid header in library !SZ") \
STATCODE(119,E,LIBVERMISM,"compiler version mismatch for library !SZ") \
STATCODE(120,E,LIBHSTMISM,"host mismatch for library !SZ") \
STATCODE(121,E,LIBTRGMISM,"target mismatch for library !SZ") \
STATCODE(122,E,LIBRDERR,  "error reading library file !SZ")

typedef unsigned int statcode_t;
#ifdef STATCODES_INSTANTIATE
#define STATCODE(msg__,typ__,nam__,txt__) const statcode_t STC__##nam__ =  STC_CODE_##typ__(msg__);
#else
#define STATCODE(msg__,typ__,nam__,txt__) extern const statcode_t __attribute__((unused)) STC__##nam__;
#endif
STATCODES
#undef STATCODE

static inline __attribute__((unused)) unsigned int stc_severity (statcode_t s) {
    return (s >> STC_V_SEVERITY) & ~((~0U) << STC_S_SEVERITY); }
static inline __attribute__((unused)) unsigned int stc_msgno (statcode_t s) {
    return (s >> STC_V_MSGNO) & ~((~0U) << STC_S_MSGNO); }
static inline __attribute__((unused)) int stc_success (statcode_t s) { return (int) s >= 0; }
static inline __attribute__((unused)) int stc_fail (statcode_t s) { return (int) s < 0; }

int stc_msg_format(statcode_t statcode, char *buf, size_t bufsiz, ...);
int stc_msg_vformat(statcode_t statcode, char *buf, size_t bufsiz, va_list ap);

#endif /* statcodes_h__ */
