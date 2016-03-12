---
layout: post
title: compute Area Under the Precision-Recall Curve 
category: programming
tags: R programming
keywords: 
description: 
---

Here is the C code: compute_PR_AUC.c
```
#include <stdlib.h>

/* 
  prec_recall: Computes precision at all possible recall levels
  INPUT:
  precision: pointer to the precision vector 
  recall: pointer to the recall vector
  order: pointer to the vector storing the order of labels
  labels: pointer to the labels vector
  n : pointer to the number of elements of the above vectors
  OUTPUT
  precision and recall vectors
*/
void  prec_recall(double * precision, double * recall, int * order, int * labels, int * n){
  register int i;
  int TP = 0;
  int TN = 0;
  int FP = 0;
  int FN = 0;
  int np = 0;
  int m;
  
  
  m = *n;
  for (i=0; i < m; i++)
    if (labels[i] == 1) np++;  
  TP = np;
  TN = 0;
  FP = m - np;
  FN = 0;
  
  if((TP + FP) > 0)		
	  precision[m-1] = TP/((double)(TP + FP));
  else
	  precision[m-1] = 0;	
  if((TP + FN) > 0)		
	  recall[m-1] = TP/((double)(TP + FN));
  else
	  recall[m-1] = 0;
  	  
  for (i=(m-2); i >=0; i--) {
    if(labels[order[i+1]-1] == 1 ){
		TP--;
		FN++;			
	} else {
		TN++;
		FP--;
	}
    if(TP + FP != 0)
		precision[i] = TP/((double)(TP + FP));
	else
		precision[i] = 0;	
	if(TP + FN != 0)
		recall[i] = TP/((double)(TP + FN));
	else
		recall[i] = 0;	
  } 
}

/* 
  trap_rule: Computes the integral according to the trapezoidal rule
  INPUT:
  x: pointer to double vector: its values must be in increasing order
  y: pointer to double vector: its values correspond to f(x)
  n : pointer to the number of elements of the above vectors
  value : pointer to the integral value computed by the function
  OUTPUT
  precision and recall vectors
*/
void  trap_rule(double * x, double * y,  int * n, double * value){
  register int i;
  int m = *n;
  register double integral_value = 0;
  
  for (i=1; i<m; i++) 
    integral_value += ((x[i] - x[i-1]) * (y[i] + y[i-1]) / 2);
    
  *value = integral_value;
}

```

Here is the R wrapper
```
# load c functions
c_file <- "compute_PR_AUC.c"
command_line <- paste0(" R CMD SHLIB ", c_file)
system(command_line)
dyn.load("compute_PR_AUC.so")

is.loaded("prec_recall")
is.loaded("trap_rule")


# Function that implements the trapezoidal rule for integration
# Input:
# x : abscissa values in increasing order
# y : ordinate values
# Output:
# value of the integral
trap.rule.integral <- function (x,y){
  if (length(x) != length(y))
    stop("trap.rule.integral: length of x and y vectors must match");
  integral_value = 0.0;
  integral_value <- .C("trap_rule", as.double(x), as.double(y), as.integer(length(x)), 
                       as.double(integral_value));
  return (integral_value[[4]]);
}


# Function to compute the precision at all recall levels  for a single class
# Input:
# scores : vector of the predicted scores in [0,1], which will be also used as thresholds
# labels : 0/1 vector of the true labels
# Output: 
# a list with 3 elements:
# precision : precision at different thresholds
# recall : recall at different thresholds
# AUC
precision_recall_AUC <- function(scores, labels){
  n<-length(scores); 
  if (n!=length(labels))
    stop("precision.at.recall.level: length of labels and scores does not match");
  if(length(which(labels > 0)) == 0)
    return(list(res=0,precision=rep(0,n),recall=rep(0,n)));
  scores.ordered <- order(scores, decreasing=TRUE);	
  precision <- recall <- rep(0, n);
  res <- .C("prec_recall", as.double(precision), as.double(recall), as.integer(scores.ordered), 
            as.integer(labels), as.integer(n));
  
  precision <- res[[1]];
  recall <- res[[2]];
  
  AUC_PR <- trap.rule.integral(recall, precision)
  
  return(list(precision=precision, recall=recall, AUC_PR=AUC_PR))  
}

```
