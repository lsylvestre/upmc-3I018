/* UPMC -- licence informatique
 * (C) 2009-2011 Equipe enseignante
 * LI223: Initiation à la Compilation et aux Machines Virtuelles
 *
 * Redistribution possible sous licence GPL v2.0 ou ultérieure
 */

/** \file prim.h
 * Implémentation des primitives
 ******/

#ifndef _PRIM_H_
#define _PRIM_H_

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#include "vm.h"
#include "constants.h"

void execute_prim(vm_t * vm, varray_t *stack, int prim, int n);

#endif
