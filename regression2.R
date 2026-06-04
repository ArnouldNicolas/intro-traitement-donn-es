dat = read.csv("C:/Users/choco/Downloads/classe4_autres_bas1.csv", header = TRUE)

sexNormalized=as.numeric(gsub("male", 0, gsub("female", 1, dat$sex)))

regionNormalized=as.numeric(gsub("southwest", 1, gsub("northwest", 2, gsub("northeast",3,gsub("southeast",0,dat$region)))))

smokerNormalized=as.numeric(gsub("yes", 1, gsub("no", 0, dat$smoker)))


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
datNormalized=datNormalized[children<=3]


X1=datNormalized$age
X2=datNormalized$sex
X3=datNormalized$bmi
X4=datNormalized$children
X5=datNormalized$smoker
X6=datNormalized$region
Y=datNormalized$charges


library(MASS)

l=stepAIC(lm(Y~1,data=datNormalized),direction="both",scope=list(lower=lm(Y~1,data=datNormalized),upper=lm(Y~X1+X2+X3+X4+X5+X6,data=datNormalized))) #forward
coeff=l$coefficients

# 1 X5 X1 X3 X4 smoker age bmi children 

profil=t(c(1,42,1,1,1))
profil %*% coeff



#Distinction fumeurs pas fumeurs 

#non fumeurs
datNormalizedSmokersFree=datNormalized[smoker==0]


X1=datNormalizedSmokersFree$age
X2=datNormalizedSmokersFree$sex
X3=datNormalizedSmokersFree$bmi
X4=datNormalizedSmokersFree$children
#X5=datNormalizedSmokersFree$smoker
X6=datNormalizedSmokersFree$region
Y=datNormalizedSmokersFree$charges

l2=stepAIC(lm(Y~1,data=datNormalizedSmokersFree),direction="both",scope=list(lower=lm(Y~1,data=datNormalizedSmokersFree),upper=lm(Y~X1+X2+X3+X4+X6,data=datNormalizedSmokersFree))) #forward
coeff2=l2$coefficients
s2=summary(l2)

Yesti2=cbind(1,X1,X4,X6,X2)%*%coeff2


plot(X1,Yesti2,xlab="Âge",ylab="Estimation de la charge",main="Estimation de la charge en fonction de l'âge chez les non fumeurs", col="red")
plot(X2,Yesti2)
plot(X4,Yesti2)

