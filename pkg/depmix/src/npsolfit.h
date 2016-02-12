
#ifndef NPSOLFIT
#define NPSOLFIT 1

#include <stdio.h>
#include <stdlib.h>
/* #include <fstream.h> */

#include <R.h>
#include <Rmath.h>

extern "C" {

//globals needed in npsolc and funobj	
int derivatives;
int tdc; 
int *fixed;
double *fixedvalues;
int npars;


/************************************************************/
/*															*/
/*	FUNCTION DECLARATIONS									*/
/*															*/
/************************************************************/

//should this be pointers to functions
void funobj(int *mode, int *n, double *x, double *f, double *g, int *nstate);
void funcon(int *mode, int *ncnln, int *n, int *ldJ, int *needc, double *x, double *c, double *cJac, int *nstate);

// declare objective function to be optimized by npsol such that it can be called by Fortran
void F77_SUB (funobj) (int *mode, int *n, double *x, double *f, double *g, int *nstate);

// declare non-linear constraint function such that it can be called by Fortran
void F77_SUB (funcon) (int *mode, int * ncnln, int *n, int *ldJ, int *needc, double *x, double *c, double *cJac);

//declare npsol for use in C
void F77_NAME (npsol) (int *n, int *nclin, int *ncnln, int *ldA, int *ldJu, int *ldR, 
				double *A, double *bl, double *bu, 
				void funcon(int *mode, int *ncnln, int *n, int *ldJ, int *needc, double *x, double *c, double *cJac, int *nstate), 
				void funobj(int *mode, int *n, double *x, double *f, double *g, int *nstate), 
				int *inform, int *iter, int *istate, double *c, double *cJacu, double *clamda, 
				double *objf, double *gradu, double *R, double *x, 
				int *iw, int *leniw, double *w, int *lenw);

 
// npsol options read from file with logical unit ioptns, inform will return 0 if all is okay
void F77_NAME (npfile) (int *ioptns, int *inform);
void F77_NAME (npoptn) (char *option); //char *option
 
// set, open  and close file "npsoloptions" on logical unit 33
void F77_NAME (opoptf)();
void F77_NAME (cloptf)();

//this is the C-wrapper for npsol which is called from R
void npsolc(int *n, int *nclin, int *ncnln, int *ldA, int *ldJu, int *ldR, 
		double *A, double *bl, double *bu,
		int *inform, int *iter, int *istate, double *c, double *cJacu, double *clamda, 
		double *objf, double *gradu, double *R, double *x,
		int *totMem, int *maxnpcalls, int *optfile, 
		int *print, int *derivatives, int *tdcov, int *fixedlogical, double *fixedvals, int *nrpars);

} //end extern "C"

#endif