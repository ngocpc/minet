/*
This file belong to
minet: Mutual Information NETworks, <http://minet.meyerp.com>
a package that implements various algorithms for inferring mutual information networks from data.

Copyright (Creative Commons by-nc-sa) July 2010  Patrick Emmanuel Meyer <software@meyerp.com>
<License full notice: at the root of the package 
and at http://creativecommons.org/licenses/by-nc-sa/3.0/legalcode> 
*/

#include "minet.h"

R_CallMethodDef callMethods[] = {
      {"mrnet", (void*(*)()) &mrnet,  2},
	  {"mrnetb", (void*(*)()) &mrnet,  2},
      {"aracne",(void*(*)()) &aracne, 3},
      {"clr", (void*(*)()) &clr, 3},
      {"validate",(void*(*)())&validate, 6},
      { NULL, NULL, 0}
};
void R_init_minet(DllInfo *info) 
{
      R_registerRoutines( info ,NULL, callMethods, NULL, NULL ); 
}

