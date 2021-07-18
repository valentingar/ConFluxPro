#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

NumericVector  cumsumcpp(NumericVector x){
  double sum = 0;
  int n = x.size();
  for (int i=0;i<=n;i++){
    x[i] = sum + x[i];
    sum = x[i];
  }
  return x;
}

NumericVector prod_mod_cpp(Rcpp::NumericVector prod,
                          Rcpp::NumericVector height,
                          Rcpp::NumericVector DS,
                          double F0,
                          double C0){

  NumericVector flux = ((prod)*height);
  flux = cumsumcpp(flux);
  flux = flux + F0;
  flux.push_front(0);
  flux.erase(flux.size());
  NumericVector conc = -((prod)/(2*DS)) * pow((height),2)  - flux/DS *(height);
  conc = cumsumcpp(conc);
  conc = conc + C0;
  return conc;
}

//' @useDynLib ConFluxPro
//' @importFrom Rcpp sourceCpp
// [[Rcpp::export]]
double prod_optim_cpp(NumericVector X,
                             NumericVector height,
                             NumericVector DS,
                             double C0,
                             NumericVector pmap,
                             NumericVector cmap,
                             NumericVector conc,
                             NumericVector dstor,
                             bool zero_flux,
                             double F0,
                             double known_flux,
                             double known_flux_factor,
                             bool Ds_optim,
                             NumericVector layer_couple,
                             NumericVector wmap
) {
  NumericVector prod = X[pmap-1];
  prod += dstor;
  NumericVector conc_mod = prod_mod_cpp(prod,
                                        height,
                                        DS,
                                        F0,
                                        C0);
 conc_mod = conc_mod[cmap-1];
 NumericVector k = pow((conc-conc_mod), 2);
 k = k * wmap;
 k = k[(!is_na(k)) & (!is_nan(k)) & (!is_infinite(k))];
 conc = conc[(!is_na(conc)) & (!is_nan(conc)) & (!is_infinite(conc))];
 double RMSE = sqrt(sum(k)/k.size())/(sum(conc)/conc.size());

 NumericVector X_f =  X;
 X_f.erase(X_f.size());
 NumericVector X_b = X;
 X_b.erase(X_b.begin());

 double prod_penal = ((sum(abs((X_f-X_b) * layer_couple)) / (X.size()-1)));
 RMSE += prod_penal;
 return RMSE;
}
