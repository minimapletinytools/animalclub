#include <stdlib.h>
#include <iostream>
#include <sstream>
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

std::string printMesh(Mesh m) {
  std::ostringstream r;
  for(int i = 0; i < m.vertex_count/3; i++) {
    r << "v " << m.vertices[i*3 + 0] << " " << m.vertices[i*3 + 1] << " " << m.vertices[i*3 + 2] << std::endl;
  }
  for(int i = 0; i < m.face_count/3; i++) {
    r << "f " << m.faces[i*3 + 0]+1 << " " << m.faces[i*3 + 1]+1 << " " << m.faces[i*3 + 2]+1 << std::endl;
  }
  return r.str();
}


void breed(uint32_t seed, char* dna1, char* dna2, char* outdna, uint32_t size) {
  breed_hs(seed, dna1, dna2, outdna, size);
}

GoatSpecimenPtr random_goat() {
  return (GoatSpecimenPtr)random_goat_hs();
}
void free_goat(GoatSpecimenPtr goat) {
  return free_goat_hs((HsStablePtr)goat);
}

GoatSpecimenPtr breed_goat(GoatSpecimenPtr g1, GoatSpecimenPtr g2) {
  return breed_goat_hs(g1,g2);
}

Mesh* goat_mesh(GoatSpecimenPtr ptr) {
  HsPtr r = goat_mesh_hs(ptr);
  return (Mesh*)r;
}
void free_goat_mesh(Mesh* ptr) {
  free_goat_mesh_hs((HsPtr)ptr);
}

void dump_goat(GoatSpecimenPtr ptr) {
  dump_goat_hs(ptr);
}
