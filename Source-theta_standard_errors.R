#source('~/GenDat-S1-Rev2-3.R')
#source('~/GenDat-S1-Rev2-5.R')
library(MASS)
new.dat2<-datwdeath[datwdeath$quarter!=1,]
new.dat2$A[new.dat2$ran==1]<-0
new.dat3<-new.dat2

source("OP-Search-7YCat.R")
	
OP<-ff$OP
r= as.numeric(new.dat2$A1c<7.00)
r[new.dat2$A1c>7.00&new.dat2$D==1]<- -2
r[new.dat2$death==1]<- -10


r2<-r

phiS<-genBasVec(new.dat3$ran,new.dat3$step.d,new.dat3$age,new.dat3$weight,new.dat3$D, new.dat3$Astar,new.dat3$A1c,OP)
phiS<-t(phiS)


phi<-genBasVec(new.dat3$ran,new.dat3$step.past,new.dat3$age.past,new.dat3$weight.past,new.dat3$D.past, new.dat3$Astar.past,new.dat3$A1c.past,new.dat3$A) ; dim(phi)
phi<-t(phi)

idset<-unique(new.dat2$ID)
n2<-length(idset)

W<-(phi)%*%t(phi)/n2; dim(W)

eps<-r2+gam*t(as.matrix(thet))%*%phiS-t(as.matrix(thet))%*%phi

W2<-cov<-phiphis<-matrix(0,ncol=dim(phi)[1],nrow=dim(phi)[1])

for(t in 2:(16)){  # this is identical to the loop for n
		
#	tep2<-(phi[,new.dat3$quarter==t])%*%t(phiS[,new.dat3$quarter==t])
	tep2<-(phiS[,new.dat3$quarter==t])%*%t(phi[,new.dat3$quarter==t])
	phiphis<-tep2+phiphis
		}

tep<-matrix(0, nrow =dim(phi)[1],ncol=n2)

for(t in 2:(16)){
	
temp<-t(t(phi[,new.dat3$quarter==t])-gam*t(phi[,new.dat3$quarter==t])%*%ginv(W)%*%(phiphis/n2))%*%diag(eps[new.dat3$quarter==t])

nzeroc<-n2-dim(temp)[2]
if(nzeroc>0) {zerm<-matrix(0,nrow=dim(phi)[1],ncol= nzeroc); temp<-cbind(temp, zerm)
} 


	tep<-tep+temp

if(t%%8==0) print(t)
}
temp3<-ginv((W+1/2*gam^2*(phiphis/n2)%*%ginv(W)%*%t(phiphis/n2) +1/2*gam^2*t(phiphis/n2)%*%ginv(W)%*%(phiphis/n2) -1*gam*t(phiphis/n2)-1*gam*(phiphis/n2)))

cov<-tep%*%t(tep)

#covThet<-(temp3)%*%t(temp5)%*%(cov)%*%(temp5) %*%t(temp3)
covThet<-t(temp3)%*%(cov) %*%(temp3)

sd<-sqrt(diag(covThet)/n2/n2)



