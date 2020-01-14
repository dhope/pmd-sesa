#include <Rcpp.h>
#include <iostream>
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
double intVec(NumericVector x, NumericVector y, String method = "trapezoid") {
  int n = x.size();
  // std::cout << n << std::endl;
  double total = 0;
  if (method == "rectangle"){
    for(int i = 1; i < n; ++i) {
      //std::cout << i << std::endl;
      total += (x[i] - x[i-1]) * y[i-1];
    }
  } else if(method == "upperstep"){
    for(int i = 1; i < n; ++i) {
      //std::cout << i << std::endl;
      total += (x[i] - x[i-1]) * y[i];
    }
  } else if(method == "trapezoid"){
      for(int i = 1; i < n; ++i) {
        //std::cout << i << std::endl;
        total += 0.5 * (x[i] * (y[i] + y[i-1]) - x[i-1] * (y[i] + y[i-1]) );
      }
  } else if(method == "intermediate"){
    for(int i = 1; i < n; ++i) {
      //std::cout << i << std::endl;
      total += 0.25 * ( (x[i]-x[i-1]) * (y[i] - y[i-1]) ) + ( y[i-1] * ( x[i]-x[i-1] ) );
    } 
  }else total = 999;
  return total;
  }

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*/*** R
x <- seq(1,100,5)
y <- seq(0,1, length.out = length(x))
intVec(x, y)
intVec(x,y, method = 'rectangle')
*/
