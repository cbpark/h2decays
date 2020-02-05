#ifndef ALAPHAS_C_H_
#define ALAPHAS_C_H_

#ifdef __cplusplus
extern "C" {
#endif

typedef struct CAlphaS CAlphaS;

CAlphaS *mkAlphaS(double mt, double mz, double alpha);

double alphasQ(CAlphaS *as, double q);

#ifdef __cplusplus
}
#endif

#endif  // ALAPHAS_C_H_
