
#include <Rcpp.h>
#include <radix.h>
using namespace Rcpp;

//[[Rcpp::depends(triebeard)]]
// [[Rcpp::export]]


std::string Radix() {

  radix_tree<std::string, std::string> radix;
  radix["turnin"] = "entry the first";
  radix["turin"] = "entry the second";

  std::vector<radix_tree<std::string, std::string>::iterator> vec;
  std::vector<radix_tree<std::string, std::string>::iterator>::iterator it;

  it = radix.prefix_match("tur");

  if(it == vec.end()){
    printf("No match was found :(");
  } else {
    for (it = vec.begin(); it != vec.end(); ++it) {
      std::string result = "Key of a prefix match: " + it->first + " , value of a prefix match: " + it->second;
    }
  }

  return result;

}
// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
x <- Radix()
*/
