#include <stdlib.h>
#include <iostream>
#include <cstdint>

#include "animalclub.h"


bool test_breed_hs() {
  const char dnasize = 8;
  uint32_t dna1[dnasize] = {0,0,0,0,0,0,0,0};
  uint32_t dna2[dnasize] = {0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF};
  uint32_t expected[dnasize] = {0xF0F0F0F0,0xF0F0F0F0,0xF0F0F0F0,0xF0F0F0F0,0xF0F0F0F0,0xF0F0F0F0,0xF0F0F0F0,0xF0F0F0F0};
  uint32_t outdna[dnasize];
  breed(0,dna1, dna2,outdna,dnasize);
  for(int i = 0; i < dnasize; i++){
    std::cout << outdna[i] << " " << expected[i] << std::endl;
    if(expected[i]!=outdna[i])
      return false;
  }
  return true;
}

int main(int argc, char *argv[])
{
  init();
  assert(test_breed_hs());
  exit();
  return 0;
}
