#include <stdlib.h>
#include <iostream>
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


void test()
{
}
