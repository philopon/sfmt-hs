#include <SFMT.h>

uint32_t wrap_genrand_uint32(sfmt_t* sfmt) {
  return sfmt_genrand_uint32(sfmt);
}

uint64_t wrap_genrand_uint64(sfmt_t* sfmt) {
  return sfmt_genrand_uint64(sfmt);
}

double wrap_genrand_real2(sfmt_t* sfmt) {
  return sfmt_genrand_real2(sfmt);
}

double wrap_genrand_res53(sfmt_t* sfmt) {
  return sfmt_genrand_res53(sfmt);
}
