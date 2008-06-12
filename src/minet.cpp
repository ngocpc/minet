#include "minet.h"

R_CallMethodDef callMethods[] = {
      {"discEF",(void*(*)()) &discEF, 4},
      {"discEW",(void*(*)()) &discEW, 4},
      {"buildMIMshrink", (void*(*)()) &buildMIMshrink,3},
      {"buildMIMempirical",(void*(*)())&buildMIMempirical,3},
      {"buildMIMmillermadow",(void*(*)())&buildMIMmillermadow,3},
      {"buildMIMdirichlet",(void*(*)())&buildMIMdirichlet,3},
      {"mrnet", (void*(*)()) &mrnet,  2},
      {"aracne",(void*(*)()) &aracne, 3},
      {"clr", (void*(*)()) &clr, 2},
      {"symmetrize",(void*(*)())&symmetrize,2},
      {"validate",(void*(*)())&validate, 6},
      { NULL, NULL, 0}
};
void R_init_minet(DllInfo *info) 
{
      R_registerRoutines( info ,NULL, callMethods, NULL, NULL ); 
}

