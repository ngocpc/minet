#include "minet.h"
SEXP discEF( SEXP Rdata, SEXP Rnrows, SEXP Rncols, SEXP Rnbins )
{
    const double *data;
    const int *nrows, *ncols, *nbins;
    int *res;
    double *spl, *col;
    SEXP Rres, Rspl, Rcol;
    PROTECT(Rdata=AS_NUMERIC(Rdata));
    PROTECT(Rnrows=AS_INTEGER(Rnrows));
    PROTECT(Rncols=AS_INTEGER(Rncols));
    PROTECT(Rnbins=AS_INTEGER(Rnbins));
    data = NUMERIC_POINTER(Rdata);
    nrows = INTEGER_POINTER(Rnrows);
    ncols = INTEGER_POINTER(Rncols);
    nbins = INTEGER_POINTER(Rnbins);
    PROTECT(Rres=NEW_INTEGER((*nrows)*(*ncols)));
    PROTECT(Rspl=NEW_NUMERIC(*nbins));
    PROTECT(Rcol=NEW_NUMERIC(*nrows));
    spl = NUMERIC_POINTER(Rspl);
    col = NUMERIC_POINTER(Rcol);
    res = INTEGER_POINTER(Rres);
    for( int i=0; i<(*ncols)*(*nrows); ++i ) res[i]=0;      
    const double epsilon = 0.01;
    for( int v=0; v<(*ncols); ++v )
    {
        int N=(*nrows);
        for( int s=0; s<N; ++s )
            col[s] = data[v*N+s];
            sort(col,col+N);
        for( int j=N-1; col[j]==NA; --j ) N--;
        int freq = N/(*nbins), mod = N%(*nbins);
        int splitpoint=freq-1;
        for( int i=0; i<(*nbins)-1; ++i ) {
              if( mod>0 ) {spl[i] = col[splitpoint+1]; mod--;}
              else spl[i]=col[splitpoint];
              splitpoint += freq;
         }
         spl[(*nbins)-1] = col[N-1]+epsilon;
         for( int s=0; s<(*nrows); ++s )
            if( data[s+v*(*nrows)]!=NA )
            {
                int bin = -1;
                for( int k=0; bin==-1 && k<(*nbins); ++k )
                   if( data[s+v*(*nrows)] <= spl[k] ) bin=k;
                res[s+v*(*nrows)] = bin;
            }
            else res[s+v*(*nrows)]=NA;
     }
     UNPROTECT(7);
     return Rres;
}
SEXP discEW( SEXP Rdata, SEXP Rnrows, SEXP Rncols, SEXP Rnbins )
{
    const double *data;
    const int *nrows, *ncols, *nbins;
    int *res;
    double *spl, *col;
    SEXP Rres, Rspl, Rcol;
    PROTECT(Rdata=AS_NUMERIC(Rdata));
    PROTECT(Rnrows=AS_INTEGER(Rnrows));
    PROTECT(Rncols=AS_INTEGER(Rncols));
    PROTECT(Rnbins=AS_INTEGER(Rnbins));
    data = NUMERIC_POINTER(Rdata);
    nrows = INTEGER_POINTER(Rnrows);
    ncols = INTEGER_POINTER(Rncols);
    nbins = INTEGER_POINTER(Rnbins);
    PROTECT(Rres=NEW_INTEGER((*nrows)*(*ncols)));
    PROTECT(Rspl=NEW_NUMERIC(*nbins));
    PROTECT(Rcol=NEW_NUMERIC(*nrows));
    spl = NUMERIC_POINTER(Rspl);
    col = NUMERIC_POINTER(Rcol);
    res = INTEGER_POINTER(Rres);
    int N=(*nrows), n=(*ncols);
    for( int v=0; v<n; ++v ) {
          double max=double(INT_MIN), min=double(INT_MAX);
          for( int i=0; i<N; ++i )
            if( data[v*N+i] != NA ) {
                if( data[v*N+i] > max ) max=data[v*N+i];
                if( data[v*N+i] < min ) min=data[v*N+i];
          }
	      double binsize = (max-min)/(*nbins);
          for( int s=0; s<N; ++s ) {
                int b=0;
                if( data[v*N+s]==NA ) b=NA;
                else while(binsize!=0 && ! ( ( min+b*binsize)<=data[v*N+s] && 
                           data[v*N+s]<(min+(b+1)*binsize) ) ) ++b;
                if( b==(*nbins) ) 
                      b=(*nbins)-1;              
                res[v*N+s]=b;
          }
    }
    UNPROTECT(7);
    return Rres;
}
