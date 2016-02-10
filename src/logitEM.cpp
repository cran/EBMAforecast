#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List emLogit(Rcpp::NumericVector outcome,
  		     Rcpp::NumericMatrix prediction, Rcpp::NumericVector W, double tol, int maxIter,
			     double wisdom) {
    double LL = 0.00;
    double LL_old = 0.00;
    int leng = prediction.nrow();
    int width = prediction.ncol();
    int iter = 0 ;
    double improv = 1.1;

while((improv > tol) & (iter < maxIter)){
    Rcpp::NumericMatrix  znumerator(leng,width);
    Rcpp::NumericMatrix Zs(leng,width); 
    Rcpp::NumericVector zdenom(leng);
    Rcpp::NumericVector unnormalizedW(width);
    Rcpp::NumericVector w_old(W);
    Rcpp::NumericVector missZ(leng);
    Rcpp::NumericVector adjustConst(leng);
    for (int r = 0; r < leng; r++) {; 
      if(outcome(r)==1){;
        for (int c = 0; c < width; c++) {
          znumerator(r,c) = prediction(r,c)*w_old(c);
        }
     } 
     if(outcome(r)==0){;
       for (int c = 0; c < width; c++) {
       znumerator(r,c) = (1-prediction(r,c))*(w_old(c));
      }
    }
      LogicalVector na_test = is_na(znumerator(r,_));
      for (int c = 0; c < width; c++) {;
        if(na_test(c)==TRUE or znumerator(r,c)< 1e-4 ){;
        znumerator(r,c)=0;
        }
      }
    zdenom(r) = sum(znumerator(r,_));
  }
     
  for(int c = 0; c<width; c++){;
    for(int r=0; r<leng; r++){
      Zs(r,c) = znumerator(r,c)/zdenom(r);
    }
    LogicalVector na_testz = is_na(Zs(_,c));
    for (int r = 0; r < leng; r++) {;
      if(na_testz(r)==TRUE or Zs(r,c)< 1e-4){;
        Zs(r,c)=0;
      }
    }
  }
  for(int r = 0; r<leng; r++){;
    missZ(r) = 0;
    LogicalVector na_test = is_na(Zs(r,_));
    for(int c=0; c<width; c++){
      if(na_test(c)==FALSE)
        missZ(r) +=1; 
      }
    adjustConst(r) = (wisdom*1.0)/missZ(r);
  }
  for(int r = 0; r<leng; r++){;
    for(int c=0; c<width; c++){
      Zs(r,c) = adjustConst(r) + ((1.0-wisdom)*Zs(r,c));
    }
  }  
  for(int r = 0; r<width; r++){;
    unnormalizedW[r] = (sum(Zs(_,r)));
  }
  W = unnormalizedW / double(sum(unnormalizedW));
  for(int r = 0; r<width; r++){;
    if(W(r)<1e-4){;
      W(r) = 0;
    }
  }
  LL = sum(log(zdenom));
  double top = double(fabs(LL_old-LL));
  double bot = (1.0+fabs(LL));
  improv  = top/bot;
  LL_old = LL;
  iter +=1;
}
  return Rcpp::List::create(Rcpp::Named("LL") = LL,Rcpp::Named("W") = W,Rcpp::Named("Iterations") = iter, Rcpp::Named("improv") = improv);
}






