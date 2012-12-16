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

// The following is used here just to get the size of an enum.
enum statcode_e__ {
    STC_UNUSED___
};

#define STC_V_SEVERITY   (sizeof(enum statcode_e__)-3)
#define STC_S_SEVERITY   3
#define STC_V_MSGNO      0
#define STC_S_MSGNO      (sizeof(enum statcode_e__)-3)

#define STC_K_SUCCESS   1  // 001
#define STC_K_INFO      2  // 010
#define STC_K_WARN      5  // 101
#define STC_K_ERROR     6  // 110
#define STC_K_FATAL     7  // 111

#define STC_MAKECODE(sev_, msgno_) \
    (((sev_)<<STC_V_SEVERITY)|(((msgno_)&((-1)<<STC_S_MSGNO))<<STC_V_MSGNO))
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
STATCODE( 11,E,LNTOOLONG, "input line too long in file !SZ")


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
