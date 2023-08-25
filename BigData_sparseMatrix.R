rm(list = ls())
library(Matrix)
library(MASS)
library(spcov)
#genertae positive definite covariance matrix
set.seed(1)
n<-100
p<-50
S<-GenerateCliquesCovariance(5,p/5,1)
S=S$Sigma
dim(S)
X<-mvrnorm(n,rep(0,p),S)
empS<-var(X)
V<-spcov(S,empS, lambda =0.06*matrix(1,p,p),step.size=100)
s=10
S_new<-V$Sigma
dim(S_new)
set.seed(2)
x=sample(1:50,10)
y=sample(1:50,10)
U<-matrix(0,nrow=50,ncol=50)
for (i in 1:50){
  U[i,i]=S_new[i,i]
  print(U[i,i])
}
for (i in 1:10){
  U[x[i],y[i]]=S_new[x[i],y[i]]
}
length(which (U==0))
m<-which(U==0)
m[1]
for (i in 1:6){
  U[m[i],m[i+1]]<-10**-8
}
eig<-eigen(U)
val_eige<-eig$values
val_eige>0# U is positive definitie
U
U[1:10,1:10]
U1<-forceSymmetric(U)
U1[1:10,1:10]
length(which(U1==0))
length(which(U==0))

