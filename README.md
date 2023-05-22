# order-md
The order-md algorithm is an adjustment of the order-m algorithm for estimating efficiency scores of decision making units (firms). 

## Required R Packages
dplyr is the only required package.

## What is nonparametric-frontier analysis?
Efficiency analyses seek to answer questions about the frontier of an industry and understand the efficiency of firms within them. The efficiency of firms could be simply stated to be the distance between a firm and its frontier. Nonparametric efficiency analyses, namely methods based around Data Envelopment Analysis (DEA) have become widely used to determine the allocative efficiency of industries (Bădin, Daraio, and Simar, 2014). A major strength of nonparametric methods is the lack of a priori assumptions about technology are opposed. Since the introduction of envelopment-based estimators in the 1950s, various improvements have been made to reduce the assumptions of these algorithms and provide robustness against various challenges (Simar and Wilson, 2015).

Nonparametric estimation is traditionally based around linear programs. The order-m algorithm introduced a bootstrap approach (Cazals, Florens, and Simar, 2002).

## What is this?
This algorithm was introduced to provide a method robust against the bias inherent to the order-m algorithm. The order-m algorithm appears to handle certain situations poorly, such as industries with a non-normal distributions firm sizes.

## How does this algorithm work?
This algorithm samples quadrants 2 and 4 in relation to the chosen point instead of y >= yi. This means the algorithm only samples firm we know strictly dominate the firm or are dominated by the firm.

## Notes
Paper forthcoming.

## Citations

(Bădin, Daraio, and Simar, 2014) Bădin, L., C. Daraio, and L. Simar. 2014. Explaining Inefficiency in Nonparametric Production Models: the State of the Art. Annals of Operations Research, 214(1), pp.5-30.

(Cazals, Florens, and Simar, 2002) Cazals, C., Florens, J.P. and Simar, L., 2002. Nonparametric frontier estimation: a robust approach. Journal of econometrics, 106(1), pp.1-25.

(Daraio and Simar, 2005) Daraio, C. and Simar, L., 2005. Introducing environmental variables in nonparametric frontier models: a probabilistic approach. Journal of productivity analysis, 24(1), pp.93-121.

(Daraio and Simar, 2007) Daraio, C. and L. Simar. 2007b. Conditional Nonparametric Frontier Models for Convex and Nonconvex Technologies: a Unifying Approach. Journal of Productivity Analysis, 28(1), pp.13-32.

(Daraio, Simar, and Wilson, 2020) Daraio, C., Simar, L. and Wilson, P.W., 2020. Fast and efficient computation of directional distance estimators. Annals of Operations Research, 288(2), pp.805-835.

(Daouia, 2012) Daouia, A., Laurent, T. and Laurent, M.T., 2012. Package ‘frontiles’.

(Daouia, Florens, and Simar, 2012) Daouia, A., Florens, J.P. and Simar, L., 2012. Regularization of nonparametric frontier estimators. Journal of Econometrics, 168(2), pp.285-299.

(Wilson, 2008) Wilson, P.W. 2008. FEAR: A software package for frontier efficiency analysis with R. Socio-economic planning sciences, 42(4), pp.247-254.

(Oh, 2013) Oh, D.H.. 2013. Package ‘nonparaeff’.

(Simar and Wilson, 2015) Simar, L. and P.W. Wilson. 2015. Statistical Approaches for Non‐parametric Frontier Models: a Guided Tour. International Statistical Review, 83(1), pp.77-110.
