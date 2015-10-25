void sumsqr(double *sumsqr, double *x, int *n)
{
  int i;
  
  sumsqr[0] = 0.0;
  
  for (i=0; i<n[0]; i++) {
    sumsqr[0] += x[i] * x[i];
  }
}
