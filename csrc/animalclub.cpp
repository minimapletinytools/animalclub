#include <stdlib.h>
#include <iostream>
#include <cstdint>

#include "HsFFI.h"
#include "animalclub.h"
#include "AnimalClub/ForeignBindings_stub.h"

void init(void){
  int argc = 2;
  char *argv[] = { (char *)"+RTS", (char *)"-A32m", NULL };
  char **pargv = argv;

  // Initialize Haskell runtime
  hs_init(&argc, &pargv);
}

void exit(void){
  hs_exit();
}

// TODO wrap exported haskell functions to be friendlier to call

void breed(uint32_t seed, uint32_t* dna1, uint32_t* dna2, uint32_t* outdna, uint32_t size) {
  breed_hs(seed, dna1, dna2, outdna, size);
}
