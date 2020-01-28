#pragma once

typedef void* GoatSpecimenPtr;

struct Mesh {
  float* vertices;
  uint32_t vertex_count;
  uint32_t* faces;
  uint32_t face_count;
};

std::string printMesh(Mesh);

void init(void);

void exit(void);

void breed(uint32_t seed, char* dna1, char* dna2, char* outdna, uint32_t size);

GoatSpecimenPtr random_goat();
void free_goat(GoatSpecimenPtr goat);
GoatSpecimenPtr breed_goat(GoatSpecimenPtr, GoatSpecimenPtr);
Mesh* goat_mesh(GoatSpecimenPtr);
void free_goat_mesh(Mesh*);
