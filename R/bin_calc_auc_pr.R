#' Calculate AUC-PR
#'
#' @description
#' Calculates Area under the precision-recall curve
#'
#' @param actual original data values - binary.
#' @param predicted predicted values in probabilities or binary.
#' @return Returns the area under the curve
#'
#' @export
#'

# Code to calculate area under the PR curve copied from PRROC package
# https://github.com/cran/PRROC/blob/master/R/PRROC.R
bin_calc_auc_pr <- function(actual, predicted) {

  fg <- predicted[actual == 1]
  bg <- predicted[actual == 0]

  o0<-order(fg);
  sorted.scores.class0<-fg[o0];

  o1<-order(bg);
  sorted.scores.class1<-bg[o1];

  weights.class0<-c(rep(1,length(sorted.scores.class0)),rep(0,length(sorted.scores.class1)));
  sorted.scores.class0<-c(sorted.scores.class0,sorted.scores.class1);
  o0<-order(sorted.scores.class0);
  sorted.scores.class0<-sorted.scores.class0[o0];
  weights.class0<-weights.class0[o0];
  weights.class1<-1-weights.class0;
  sorted.scores.class1<-sorted.scores.class0;

  all.scores<-sorted.scores.class0;
  all.weights.pos<-weights.class0;
  all.weights.neg<-weights.class1;

  o<-order(all.scores,decreasing = T);
  all.scores<-all.scores[o]
  all.weights.pos<-all.weights.pos[o];
  all.weights.neg<-all.weights.neg[o];

  cum.weights.pos<-cumsum(all.weights.pos);
  cum.weights.neg<-cumsum(all.weights.neg);
  cum.use<-c(all.scores[-length(all.scores)]!=all.scores[-1],TRUE)

  all.scores<-all.scores[cum.use]
  cum.weights.pos<-cum.weights.pos[cum.use];
  cum.weights.neg<-cum.weights.neg[cum.use];


  r.fg<-sum(all.weights.pos);
  tp<-cum.weights.pos
  fp<-cum.weights.neg;
  tp.prev<-c(0,cum.weights.pos[ -length(cum.weights.pos) ])
  fp.prev<-c(0,cum.weights.neg[ -length(cum.weights.neg) ])

  h<-(fp-fp.prev)/(tp-tp.prev);
  a<-1+h;
  b<-(fp.prev-h*tp.prev)/r.fg;
  h[tp==tp.prev]<-1;
  a[tp==tp.prev]<-1;
  b[tp==tp.prev]<-0;

  v<-( tp/r.fg - tp.prev/r.fg - b / a * ( log( a * tp/r.fg + b ) - log( a * tp.prev/r.fg + b ) ) ) / a;
  v2<-( tp/r.fg - tp.prev/r.fg ) / a;
  v[b==0]<-v2[b==0]

  vals<-v
  auc_pr<-sum(vals)

  return(auc_pr)
}
