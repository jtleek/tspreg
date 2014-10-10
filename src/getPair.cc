library(RcppArmadillo)
library(inline)
library(genefilter)
library(Rcpp)


code2 <- '
  arma::mat datc = Rcpp::as<arma::mat>(dat);
  arma::mat modc = Rcpp::as<arma::mat>(mod);
  arma::mat mod0c = Rcpp::as<arma::mat>(mod0);
  int n = datc.n_rows; int m = datc.n_cols;
  int df = modc.n_cols; int df0 = mod0c.n_cols;

  arma::vec fstats = arma::zeros<arma::vec>(m);
  arma::vec res = arma::zeros<arma::vec>(n);
  arma::vec res0 = arma::zeros<arma::vec>(n);
  arma::vec pair = arma::zeros<arma::vec>(n);

  double ss,ss0; 

  NumericVector x ;
  NumericVector y ; 

  for(int i=0; i < m; i++){
    for(int j=0; j < i; j++){
    //  pair = ifelse(datc.col(i) < datc.col(j),1,0);
      //res = pair - modc*arma::solve(modc,pair);
      //res0 = pair - mod0c*arma::solve(mod0c,pair);
      //ss = arma::as_scalar(arma::trans(res)*res0);
      //ss0 = arma::as_scalar( arma::trans(res0)*res0);
      //fstats(i) = ((ss0 - ss)/(df-df0))/(ss/(n-df));
    }
  }
  return Rcpp::wrap(ifelse(datc.col(1) < datc.col(2),1,0));'

rcppFstats <- cxxfunction(signature(dat="numeric",mod="numeric",mod0="numeric"),
                                               code2,plugin="RcppArmadillo",verbose=TRUE)




cppFunction('
  NumericVector tsPair(NumericMatrix X, NumericMatrix mod, NumericMatrix mod0 )
')
