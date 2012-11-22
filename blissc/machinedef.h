//
//  machinedef.h
//  blissc
//
//  Created by Matthew Madison on 11/18/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_machinedef_h
#define blissc_machinedef_h

struct machinedef_s {
    int     bpunit, bpval, bpaddr;
    int     addr_signed; // true if addresses are sign-extended
};

typedef struct machinedef_s machinedef_t;

#endif
