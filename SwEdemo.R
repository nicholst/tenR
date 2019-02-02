# SwEdemo code
# Attempting to find minimum run time for mass-univariate application of sandwich estimator
#
# T. Nichols 1 Feb 2019

require(Rfast)

# Assuming the 'block' is school...
Nschool = 200
Nstud   = 40      # Number of students pers school
Nelm    = 10000   # Number of vertices/voxels
rho     = 0.95    # Intraschool correlation... maxed out to verify SwE is working


P       = 10      # Number of predictors... all fake/simulated
N       = Nschool*Nstud

# Design: intercept, one between school variable, rest within school
X = cbind(rep(1,N),
          rep(runif(Nschool),each=Nstud),
          matrix(runif((P-2)*N),ncol=P-2))

# Simulate repeated measures data, N x Nelm, with intraschool correlation rho
Y = sqrt(1-rho)*matrix(rnorm(N*Nelm),ncol=Nelm) + 
    sqrt(rho)  *matrix(rep(rnorm(Nschool*Nelm),each=Nstud),ncol=Nelm)

start_time <- Sys.time()

# OLS fit
fit     = lmfit(X, Y)
res     = fit$residuals
SE.ols  = sqrt(matrix(diag(solve(t(X)%*%X))) %*% t(apply(res^2,2,sum))/(N-P))
T.ols   = fit$be/SE.ols

cat("OLS: ");Sys.time()-start_time


# Computation of SwE standard errors
start_time <- Sys.time()

SE.swe = matrix(0,nrow=P,ncol=Nelm)

Ischool= rep(1:Nschool,each=Nstud)
Bread  = solve(t(X)%*%X)
# There *must* be a way to vectorise this, at least for each school... but I can't 
# figure it out
for (i in 1:Nelm) {
    Meat=matrix(0,nrow=P,ncol=P)
    for (s in 1:Nschool) {
        I=(s==Ischool)
        Meat=Meat+(t(X[I,])%*%res[I,i])%*%(t(res[I,i])%*%X[I,])
    }
    S = Bread%*%Meat%*%Bread
    SE.swe[,i]=sqrt(diag(S))
}
T.swe = fit$be/SE.swe

cat("SwE: ");Sys.time()-start_time

T.sd=c(ols=sd(T.ols[2,]),swe=sd(T.swe[2,]))

cat("Standard deviation of T scores for between school covariate... should be 1.0\n")
print(T.sd)

