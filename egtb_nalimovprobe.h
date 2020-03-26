#ifndef EGTB_NALIMOV_PROBE_H_INCLUDED
#define EGTB_NALIMOV_PROBE_H_INCLUDED

#include <string>

void Nalimov_init(const std::string& path);
void Nalimov_setCache(unsigned long size, bool verbose);

#endif