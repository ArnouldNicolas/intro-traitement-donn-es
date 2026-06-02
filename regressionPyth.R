dat = read.csv("C:/Users/choco/Downloads/insurance.csv", header = TRUE)

sexNormalized=as.numeric(gsub("male", 1, gsub("female", 2, dat$sex)))

regionNormalized=as.numeric(gsub("southwest", 1, gsub("northwest", 2, gsub("northeast",3,gsub("southeast",4,dat$region)))))

smokerNormalized=as.numeric(gsub("yes", 1, gsub("no", 2, dat$smoker)))


X1=dat$age
X2=sexNormalized
X3=dat$bmi
X4=dat$children
X5=smokerNormalized
X6=regionNormalized
Y=dat$charges

library(data.table)
datNormalized=data.table(
  age=X1,
  sex=X2,
  bmi=X3,
  children=X4,
  smoker=X5,
  region=X6,
  charges=Y
)



library(MASS)

l=stepAIC(lm(Y~1,data=datNormalized),direction="forward",scope=list(lower=lm(Y~1,data=datNormalized),upper=lm(Y~X1+X2+X3+X4+X5+X6,data=datNormalized))) #forward
coeff=l$coefficients

t(c(32,31,0,1)) %*% coeff
