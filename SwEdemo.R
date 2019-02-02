# SwEdemo code
# Attempting to find minimum run time for mass-univariate application of sandwich estimator
#
# T. Nichols 1 Feb 2019

require(Rfast)

# Assuming the 'block' is school...
Nschool = 200
Nstud   = 40      # Number of students pers school
Nelm    = 32492   # Number of vertices/voxels
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
BreadX = Bread%*%t(X)
S      = array(0,dim=c(P,P,Nelm))
S0     = array(0,dim=c(1,P,Nelm))
for (s in 1:Nschool) {
    I=(s==Ischool)
    Ns=sum(I)
    # half of Meat times t(BreadX)
    e = array(res[I,],c(1,Ns,Nelm))
    S0[] = apply(e,3,function(x)x%*%t(BreadX[,I]))
    # Full `Bread*Meat*Bread' contribution for school s
    S = S + array(apply(S0,3,function(x)t(x)%*%x),dim=c(P,P,Nelm))
}

SE.swe[]=sqrt(apply(S,3,diag))
T.swe = fit$be/SE.swe

cat("SwE vectorised: ");Sys.time()-start_time

T.sd=c(ols=sd(T.ols[2,]),swe=sd(T.swe[2,]))

cat("Standard deviation of T scores for between school covariate... should be 1.0\n")
print(T.sd)


## # Computation of SwE standard errors
## start_time <- Sys.time()

## SE.swe.old = matrix(0,nrow=P,ncol=Nelm)

## # Compare to old way
## for (i in 1:Nelm) {
##     Meat=matrix(0,nrow=P,ncol=P)
##     S.old0=matrix(0,nrow=P,ncol=P)
##     for (s in 1:Nschool) {
##         I=(s==Ischool)
##         Meat=Meat+(t(X[I,])%*%res[I,i])%*%(t(res[I,i])%*%X[I,])
##        ## S.old0=S.old0+BreadX[,I]%*%res[I,i]%*%t(res[I,i])%*%t(BreadX[,I])
##        ##  a0=BreadX[,I]%*%res[I,i]%*%t(res[I,i])%*%t(BreadX[,I])
##        ##  a11=t(res[I,i])%*%t(BreadX[,I])
##        ##  a1=t(a11)%*%a11
##     }
##     S.old = Bread%*%Meat%*%Bread
##     SE.swe.old[,i]=sqrt(diag(S.old))
##  }
## T.swe.old = fit$be/SE.swe.old
 
## cat("SwE old: ");Sys.time()-start_time



