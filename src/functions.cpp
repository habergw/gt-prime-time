#include <Rcpp.h>
#include <vector>

using namespace Rcpp;
using namespace std;

inline double get_ET(double p, double k, double Se, double Sp)
{
    if (k == 1)
        return 1;
    else
        return Se - pow(1 - p, k) * (Se + Sp - 1) + 1 / k;
}

NumericVector get_test(int ind1, int ind2, NumericVector &x, 
                       double Se, double Sp)
{
   double res = 0.0;
   double sum = 0.0;

   for (int i = ind1; i < ind2 + 1; i++) {
    if (x[i] == 1) {
        sum = 1;
        break;
    }
   }

   if (sum > 0) {
    res = R::runif(0, 1) < Se;
   } else {
    res = R::runif(0, 1) > Sp;
   }
   
   return NumericVector::create(sum, res);
}
// [[Rcpp::export]]
NumericVector get_k_cpp(NumericVector x, int k, double p, double Se, double Sp)
{
  int j = 0;
  int i = -1;

  int T = 0;
  double est_1 = 0.0;
  double est_0 = 0.0;
  double val_1 = 0.0;
  double val_0 = 0.0;
  int n = x.length();

  while(i != n - 1) {
    j = i + 1;
    i = j + k - 1;
    if (i >= n) {
      j = n - k;
      i = n - 1;
    }

    double res = 0.0;
    double sum = 0.0;

   for (int k = j; k < i + 1; k++) {
     if (x[k] == 1) {
        sum = 1;
        break;
    }
   }

    if (sum > 0) {
      val_1 += 1;
      res = R::runif(0, 1) < Se;
      if (res > 0) {
        est_1 += 1;
      }
    } else {
      val_0 += 1;
      res = R::runif(0, 1) > Sp;
      if (res == 0) {
        est_0 += 1;
      }
    }
    
    T += 1;
  }

  est_1 /= val_1;
  est_0 /= val_0;

  double ET_est = get_ET(p, k, est_1, est_0);
  double ET_true = get_ET(p, k, Se, Sp);

  return NumericVector::create(est_1, est_0, T, ET_est, ET_true);
}

NumericMatrix get_N(int N, double p, int k_max, 
                    NumericVector Se, 
                    NumericVector Sp)
{
    NumericVector x = rbinom(N, 1, p);

    NumericMatrix res(k_max, 5);

    for (int i = 0; i < k_max; i++) {
        res(i,_) = get_k_cpp(x, i + 1, p, Se[i], Sp[i]);
    }

    return res;
}

NumericVector get_opt_k(int N, double p, int k_max, 
                        NumericVector Se, 
                        NumericVector Sp,
                        double Se_thresh,
                        double Sp_thresh)
{
    int opt_ind = 0;
    double T = 0;

    NumericMatrix k_mat = get_N(N, p, k_max, Se, Sp);

    for (int i = 0; i < k_mat.nrow(); i++) {
        T += k_mat(i, 2);
        if (k_mat(i, 0) > Se_thresh && k_mat(i, 1) > Sp_thresh) {
            opt_ind = i;
            break;
        }

    }

    for (int i = opt_ind + 1; i < k_mat.nrow(); i++) {
        T += k_mat(i, 2);
        if (k_mat(i, 0) > Se_thresh && 
                k_mat(i, 1) > Sp_thresh && 
                k_mat(i, 3) < k_mat(opt_ind, 3)) {
            opt_ind = i;
        }
    }

    NumericVector res = {(double) opt_ind + 1, T, k_mat(opt_ind, 0), 
                         k_mat(opt_ind, 1), Se[opt_ind], Sp[opt_ind],
                         k_mat(opt_ind, 3), k_mat(opt_ind, 4)};

    return res;
}


// [[Rcpp::export]]
NumericMatrix get_N_sim(int N, double p, int k_max, 
                    NumericVector Se, 
                    NumericVector Sp, 
                    double Se_thresh,
                    double Sp_thresh,
                    int M)
{
    NumericMatrix res(M, 8);

    for (int i = 0; i < M; i++) {
        res(i, _) = get_opt_k(N, p, k_max, Se, Sp, Se_thresh, Sp_thresh);
    }

    return res;
}
