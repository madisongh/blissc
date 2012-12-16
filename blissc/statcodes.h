//
//  statcodes.h
//  blissc
//
//  Created by Matthew Madison on 12/16/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_statcodes_h
#define blissc_statcodes_h

#include <string.h>
#include <stdint.h>
#include <stdarg.h>

#define STC_V_SEVERITY   29
#define STC_S_SEVERITY    3
#define STC_V_MSGNO       0
#define STC_S_MSGNO      29

#define STC_K_SUCCESS   1  // 001
#define STC_K_INFO      2  // 010
#define STC_K_WARN      5  // 101
#define STC_K_ERROR     6  // 110
#define STC_K_FATAL     7  // 111

#define STC_MAKECODE(sev_, msgno_) \
    (((sev_)<<STC_V_SEVERITY)|(((msgno_)&~((-1)<<STC_S_MSGNO))<<STC_V_MSGNO))
#define STC_CODE_S(m_) STC_MAKECODE(STC_K_SUCCESS,(m_))
#define STC_CODE_I(m_) STC_MAKECODE(STC_K_INFO,(m_))
#define STC_CODE_W(m_) STC_MAKECODE(STC_K_WARN,(m_))
#define STC_CODE_E(m_) STC_MAKECODE(STC_K_ERROR,(m_))
#define STC_CODE_F(m_) STC_MAKECODE(STC_K_FATAL,(m_))

#define STC_ENUMERATE(mno_,type_,name_) STC__##name_ = STC_CODE_##type_((mno_)),

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
STATCODE( 11,E,LNTOOLONG, "input line too long in file !SZ") \
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
STATCODE( 27,E,SYNTAXERR, "syntax error") \
STATCODE( 28,W,KWDEXP,    "keyword '!SZ' expected") \
STATCODE( 29,W,UNEXPCOND, "unexpected lexical conditional") \
STATCODE( 30,E,REQFILERR, "error opening REQUIRE file !SD") \
STATCODE( 31,W,CTNAMEXP,  "expecting a COMPILETIME name") \
STATCODE( 32,W,NBITSERR,  "number of bits (!UI) exceeds fullword size (!UI)") \
STATCODE( 33,E,FLDNAMEXP, "field name expected") \
STATCODE( 34,W,FLDEXPERR, "filed expansion error") \
STATCODE( 35,W,SEGNAMEXP, "data segment name expected") \

#define STATCODE(msg,typ,nam,txt) STC_ENUMERATE(msg,typ,nam)
typedef enum {
STATCODES
} statcode_t;
#undef STATCODE

static inline __unused unsigned int stc_severity (statcode_t s) { return (s >> STC_V_SEVERITY) & ~((-1) << STC_S_SEVERITY); }
static inline __unused unsigned int stc_msgno (statcode_t s) { return (s >> STC_V_MSGNO) & ~((-1) << STC_S_MSGNO); }
static inline __unused int stc_success (statcode_t s) { return (int) s >= 0; }
static inline __unused int stc_fail (statcode_t s) { return (int) s < 0; }

int stc_msg_format(statcode_t statcode, char *buf, size_t bufsiz, ...);
int stc_msg_vformat(statcode_t statcode, char *buf, size_t bufsiz, va_list ap);

#endif
