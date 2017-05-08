#include <Rcpp.h>
#include <radix.h>
using namespace Rcpp;


// [[Rcpp::depends(triebeard)]]
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
radix_tree<std::string, std::string> radix;


radix["turnin"] = "entry the first";
radix["turin"] = "entry the second";

// radix_tree<std::string, std::string>::iterator it;
//
// it = radix.longest_match("turing");
//
// if(it = radix.end()){
//   printf("No match was found :(");
// } else {
//   std::string result = "Key of longest match: " + it->first + " , value of longest match: " + it->second;
// }

/*** R

*/
