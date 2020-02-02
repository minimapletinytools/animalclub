#pragma once

typedef void* GoatSpecimenPtr;

struct Mesh {
  float* vertices;
  uint32_t vertex_count;
  uint32_t* faces;
  uint32_t face_count;
};

std::string printMesh(Mesh);

extern "C" void my_init(void);

extern "C" void my_exit(void);

extern "C" void breed(uint32_t seed, char* dna1, char* dna2, char* outdna, uint32_t size);

extern "C" GoatSpecimenPtr random_goat();
extern "C" void free_goat(GoatSpecimenPtr goat);
extern "C" GoatSpecimenPtr breed_goat(GoatSpecimenPtr, GoatSpecimenPtr);
extern "C" Mesh* goat_mesh(GoatSpecimenPtr);
extern "C" void free_goat_mesh(Mesh*);
extern "C" void dump_goat(GoatSpecimenPtr);
