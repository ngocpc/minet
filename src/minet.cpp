#include "minet.h"

R_CallMethodDef callMethods[] = {
      {"mrnet", (void*(*)()) &mrnet,  2},
	  {"mrnetb", (void*(*)()) &mrnet,  2},
      {"aracne",(void*(*)()) &aracne, 3},
      {"clr", (void*(*)()) &clr, 2},
      {"validate",(void*(*)())&validate, 6},
      { NULL, NULL, 0}
};
void R_init_minet(DllInfo *info) 
{
      R_registerRoutines( info ,NULL, callMethods, NULL, NULL ); 
}

