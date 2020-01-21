#include <stdlib.h>
#include <iostream>
#include <cstdint>

#include "animalclub.h"

#define CATCH_CONFIG_RUNNER
#include "catch.hpp"

using namespace std;

bool test_breed_hs() {
  const char dnasize = 8;
  unsigned char dna1[dnasize] = {0};
  unsigned char dna2[dnasize] = {0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF};
  unsigned char expected[dnasize] = {0b01010101,0b01010101,0b01010101,0b01010101,0b01010101,0b01010101,0b01010101,0b01010101};
  unsigned char outdna[dnasize] = {0};
  breed(0,reinterpret_cast<char*>(dna1),reinterpret_cast<char*>(dna2),reinterpret_cast<char*>(outdna),dnasize);
  for(int i = 0; i < dnasize; i++){
    //printf("0x%x\n", dna2[0]);
    //printf("expected: 0x%x, actual: 0x%x, match=%i\n", expected[i], outdna[i], expected[i]==outdna[i]);
    //std::cout << (uint8_t)outdna[i] << " " << (uint8_t)expected[i] << std::endl;
    if(expected[i]!=outdna[i])
      return false;
  }
  return true;
}

TEST_CASE( "basic breed", "genetics" ) {
    REQUIRE( test_breed_hs() );
}

int main( int argc, char* argv[] ) {
  // global setup...
  init();

  int result = Catch::Session().run( argc, argv );

  // global clean-up...
  exit();

  return result;
}