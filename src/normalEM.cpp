#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List emNorm(Rcpp::NumericVector outcome,
  		     Rcpp::NumericMatrix prediction, Rcpp::NumericMatrix RSQ, Rcpp::NumericVector W, double tol, int maxIter
			     ,double wisdom, double sigma2) {
    double LL = 0.00;
    double LL_old = 0.00;
    int leng = prediction.nrow();
    int width = prediction.ncol();
    int iter = 0 ;
    double improv = 1.1;

while((improv > tol) & (iter < maxIter)){
    Rcpp::NumericMatrix  znumerator(leng,width);
    Rcpp::NumericVector w_old(W);
    Rcpp::NumericVector zdenom(leng);
    Rcpp::NumericVector unnormalizedW(width);
    Rcpp::NumericMatrix Zs(leng,width); 
    Rcpp::NumericMatrix RZ(leng,width); 
    Rcpp::NumericMatrix  g(leng,width);
    Rcpp::NumericVector missZ(leng);
    Rcpp::NumericVector adjustConst(leng);
    for(int r = 0; r < leng; r++) {
      for (int c = 0; c < width; c++) {
         g(r,c) = R::dnorm(outcome[r],prediction(r,c),sqrt(sigma2),0);
        }
      for (int c = 0; c < width; c++) {
        znumerator(r,c) = g(r,c)*w_old(c);
        if(R_IsNA(znumerator(r,c))==TRUE){;
                  znumerator(r,c) = 0;
          }
        }
      zdenom(r) = sum(znumerator(r,_));    
     } 
    for(int r = 0; r<leng; r++){;
       missZ(r) = 0;
      for(int c=0; c<width; c++){
        Zs(r,c) = znumerator(r,c)/zdenom(r);
        if(Zs(r,c)<1e-4){;
          Zs(r,c) = 0;
        }
      }
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
      LogicalVector na_testZ = is_na(Zs(r,_));
      for (int c = 0; c < width; c++) {;
          if(na_testZ(c)==TRUE){;
            Zs(r,c)=0;
          }
      }
    }
  for(int c = 0; c<width; c++){;
    unnormalizedW[c] = (sum(Zs(_,c)));
    for(int r = 0; r < leng; r++){
        RZ(r,c) = Zs(r,c)*RSQ(r,c);
    }
  }
  for(int r = 0; r < leng; r++){
    LogicalVector na_testRZ = is_na(RZ(r,_));
        for (int c = 0; c < width; c++) {;
          if(na_testRZ(c)==TRUE){;
            RZ(r,c)=0;
          } 
        }
  }
  sigma2 = sum(RZ)/sum(Zs);
  
  W = unnormalizedW / double(sum(unnormalizedW));
  for(int c = 0; c<width; c++){;
     if(W(c)<1e-4){;
       W(c) = 0;
     }
  }
  LL = sum(log(zdenom));
  double top = double(fabs(LL_old-LL));
  double bot = (1.0+fabs(LL));
  improv  = top/bot;
  LL_old = LL;
 iter +=1;
} 
   
   
   
   
  return Rcpp::List::create(Rcpp::Named("LL") = LL,Rcpp::Named("W") = W,Rcpp::Named("Sigma2") = sigma2,Rcpp::Named("Iterations") = iter, Rcpp::Named("improv") = improv);
}






