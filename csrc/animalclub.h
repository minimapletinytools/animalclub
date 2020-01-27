#pragma once

typedef void* GoatPtr;

void init(void);

void exit(void);

void breed(uint32_t seed, char* dna1, char* dna2, char* outdna, uint32_t size);

GoatPtr random_goat(uint32_t length);
void free_goat(GoatPtr goat);
