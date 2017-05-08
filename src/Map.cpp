#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

std::map<double, int> tableC(NumericVector x) {

  std::map<double, int> counts;

  int n = x.size();
  for (int i = 0; i < n; i++) {
    counts[x[i]]++;
  }

  return counts;
}

/*** R

x <- sample(x = c(1, 3, 5), size = 50, replace = TRUE)

y <- tableC(x)

*/
