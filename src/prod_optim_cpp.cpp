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

// [[Rcpp::export]]
NumericVector cumsumcpp(NumericVector x){
  double sum = 0;
  int n = x.size();
  NumericVector out(n);

  for (int i=0; i< n; i++){
    sum += x[i];
    out[i] = sum;
  }

  return out;
}

// [[Rcpp::export]]
NumericVector prod_mod_cpp(Rcpp::NumericVector prod,
                          Rcpp::NumericVector height,
                          Rcpp::NumericVector DS,
                          double Fnull,
                          double Cnull){

  NumericVector flux = ((prod)*height);
  flux = cumsumcpp(flux);
  flux = flux + Fnull;
  flux.push_front(0);
  flux.erase(flux.size());
  NumericVector conc = -((prod)/(2*DS)) * pow((height), 2)  - flux/DS *(height);
  conc = cumsumcpp(conc);
  conc = conc + Cnull;
  return conc;
}



// [[Rcpp::export]]
NumericVector prod_conc(Rcpp::NumericVector prod,
                           Rcpp::NumericVector height,
                           Rcpp::NumericVector DS,
                           double Fnull,
                           double Cnull){

  NumericVector flux = ((prod)*height);
  flux = cumsumcpp(flux);
  flux = flux + Fnull;
  flux.push_front(0);
  flux.erase(flux.size());
  NumericVector conc = -height * (((prod)/(2*DS)) * height  - flux/DS );
  conc = cumsumcpp(conc);
  conc = conc + Cnull;
  return conc;
}
