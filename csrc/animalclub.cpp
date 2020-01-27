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

void breed(uint32_t seed, char* dna1, char* dna2, char* outdna, uint32_t size) {
  breed_hs(seed, dna1, dna2, outdna, size);
}

GoatPtr random_goat(uint32_t length) {
  return (GoatPtr)random_goat_hs(length);
}
void free_goat(GoatPtr goat) {
  return free_goat_hs((HsStablePtr)goat);
}
