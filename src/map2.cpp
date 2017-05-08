#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List multimap_string_generic(){

  typedef std::pair<std::string, std::vector<int> > _pair ;
  std::multimap< std::string, std::vector<int> > m ;

  std::vector<int> b ; b.push_back(1) ; b.push_back(2) ;
  m.insert( _pair("b", b) );

  std::vector<int> a ; a.push_back(1) ; a.push_back(2) ; a.push_back(2) ;
  m.insert( _pair("a", a) );

  std::vector<int> c ; c.push_back(1) ; c.push_back(2) ; c.push_back(2) ; c.push_back(2) ;
  m.insert( _pair("c",  c) );

  return wrap(m);

}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
c <- multimap_string_generic()
*/
