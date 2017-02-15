

#I think this works the best... This is what I used for the current results

rm(list=ls())
library(MASS)


load("Rev2-IMP-GGQ-75-2K-S1-300-400itr-gam6-45-r2-GenDat6.RData")
Big.thet<-thet.m[1:100,]


thet0<-apply(Big.thet,2,mean)
#thet0<-thet0[-27]


library(MASS)
set.seed(302)

N=5*1000
source('~/Generate_Data.R', chdir = TRUE)
gam<-0.6


q1<-6
q2<-7
q3<-8
q4<-9



qa1<-b.a[2]
qa2<-b.a[3]
qa3<-b.a[4]

qw1<-b.w[2]
qw2<-b.w[3]
qw3<-b.w[4]

source('~/GGQ-R-Code/Feature_functions.R', chdir = TRUE)

thet0<-thet 
Q<-Norm<-optcd<-J<-Norm1<-NULL
Norm[1]<-1
maxk<-100
#source("/Users/Ashkan/Dropbox/Susan/Sandeep/MyTTT3.R")
#source('~/Dropbox/Susan/My Code on Diab./DiabGen8-#.R', chdir = TRUE)

new.dat2<-datwdeath[datwdeath$quarter!=1,]
new.dat2$A[new.dat2$ran==1]<-0


K<-dim(new.dat2)[1]

#q1<-quantile(new.dat2$A1c)[2]
#q2<-quantile(new.dat2$A1c)[3]
#q3<-quantile(new.dat2$A1c)[4]




r= as.numeric(new.dat2$A1c<7.00)
r[new.dat2$A1c>7.00&new.dat2$D==1]<- -2
r[new.dat2$death==1]<- -10


   # Reward function
#phi<-genBasVec(new.dat2$A1c.past,new.dat2$A.past,new.dat2$Astar.past) # generastes the feature functions
phi<-genBasVec(new.dat2$ran,new.dat2$step.past,new.dat2$age.past,new.dat2$weight.past,new.dat2$D.past, new.dat2$Astar.past,new.dat2$A1c.past,new.dat2$A) # generastes the feature functions
#phi<-genBasVec(new.dat2$A1c,new.dat2$A,new.dat2$Astar) # generastes the feature functions
#cbind(new.dat2$A==1,(new.dat2$A==1)*(new.dat2$Astar.past==0))

dimbet<-dim(phi)[2]

test<-solve(t(phi)%*%phi)
#solve((phi)%*%t(phi))
theta<-matrix(0,ncol=dimbet,nrow=maxk)
w<-matrix(0,ncol=dimbet,nrow=maxk)

mu<-rep(0,dimbet)
mu<-rep(0,dimbet)
Sigma<-diag(rep(0.05,dimbet))
#thet <-theta[1,]<-mvrnorm(1,mu,Sigma) #thet0+mvrnorm(1,mu,Sigma)
thet <-theta[1,]<-thet0+mvrnorm(1,mu,Sigma)
#######################################
	phihat<-genphihat(new.dat2$ranf,new.dat2$step.d,new.dat2$age,new.dat2$weight,new.dat2$D,new.dat2$A1c,thet,1,new.dat2$Astar)

	delta<-r+gam*(phihat)%*%thet-phi%*%thet



w[1,]<-wl<-as.array( (t(delta)%*%phi)%*%test )

######################################


k<-2
conv<-0
     thet<-as.array(theta[k-1,] )
     wl<-as.array(w[k-1,])  
     
     
while( conv==0 & k<maxk){
	
	
	alpha<-1/(k)  #Sc. II
	beta<-1/(k)^(3/4) #Sc. II
	alpha<-0.025
    beta<-0.025
#	beta<-1
#	alpha<-1

       alpha=1/((k)*log(k)) #Sc. I
        beta=1/(k)   #Sc. I
#        alpha=1/((500)*log(500));
#        beta=1/(500);


EthInc<-EwInc<-0
Norm1[1]<-10


     thet<-as.array(theta[k-1,] )
     wl<-as.array(w[k-1,]) 
idset<-unique(new.dat2$ID)
for (l in idset)	{


	
#	       alpha=1/((l)*log(l+k+1)) #Sc. I
#        beta=1/(l+k) 
	sel<-new.dat2$ID==l
	phihat<-genphihat(new.dat2$ranf[sel],new.dat2$step.d[sel],new.dat2$age[sel],new.dat2$weight[sel],new.dat2$D[sel],new.dat2$A1c[sel],thet,1,new.dat2$Astar[sel])
#	phihat<-genphihat(cd,thet,K)
	delta<-r[sel]+gam*(phihat)%*%thet-phi[sel,]%*%thet
#	delta<-r[l]+gam*(phihat[l,])%*%thet-phi[l,]%*%thet
#	delta<-r[k]+gam*(phihat(h1[k-1,],theta[k-1,])%*%theta[k-1,])-phi[k-1,]%*%theta[k-1,]
	
#	t(phihat)%*%(phi[sel,]%*%as.matrix(wl))
#	t(phi[sel,]%*%as.matrix(wl))%*%phihat
	
	if(sum(sel)==1){	thetaINC<-alpha*(t(delta)%*%phi[sel,]-gam*(t(as.matrix(wl))*t(phi[sel,]))*phihat) } else{
	thetaINC<-alpha*(t(delta)%*%phi[sel,]-gam*(t(as.matrix(wl))%*%t(phi[sel,]))%*%phihat)	}
	
	wINC<-beta*(t(delta)%*%phi[sel,]- t(phi[sel,]%*%as.matrix(wl))%*%phi[sel,])
	
	
	thet<-(thet)+as.vector(thetaINC)/25
	wl<-(wl)+as.vector(wINC)/20
	
#	l=l+1
#	thet
#	thetaINC
#	print(sum(thet))
}
    	
     theta[k,]<-thet 
     w[k,]<-wl 	

	
	
	Q[k]<-phi[1,]%*%theta[k,]
	Norm[k]<-sqrt(sum((theta[k-1,]-theta[k,])^2))
	
	

#########J(theta) & NORM1 CALCULATION ####################################
     
Antheta<-rep(0, dimbet)
Bn<-matrix(0,ncol= dimbet,nrow= dimbet)
Resid<-matrix(0,ncol= dimbet,nrow=K)

	phihat<-genphihat(new.dat2$ranf,new.dat2$step.d,new.dat2$age,new.dat2$weight,new.dat2$D, new.dat2$A1c,theta[k,],K,new.dat2$Astar)

	delta<-r+gam*(phihat%*%theta[k,])-phi%*%theta[k,]
    ResHat<-(t(delta)%*%phi) 
    Norm1[k]<-sqrt(sum(ResHat^2))
    Bn<-t(phi)%*%(phi)

    	J[k]<-ResHat%*%solve(Bn)%*%t(ResHat)/(K)



	if(sqrt(sum((theta[k-1,]-theta[k,])^2))<0.05) conv<-1
#	if(abs(Norm1[k]-Norm1[k-1])<.05) conv<-1
print(sqrt(sum((theta[k-1,]-theta[k,])^2)))

	k<-k+1
	
#	print(c(delta,theta[k,],w[k,]))
print(k)

#	k<-k+1
}     
gam
     
     par(mfrow=c(1,3))
#plot(Q[1:(k-1)],type="o",ylim=c(min(Q),max(Q)),ylab="State-Value")
#plot(Q[1:(k-1)],type="l",ylim=c(min(Q),max(Q)),ylab="State-Value",xlab="iteration")
#plot(Norm[1:(k-1)],type="l",ylim=c(min(Norm),max(Norm)),ylab="Norm",xlab="iteration")
#save(nvisits,thet, k, J , norm,N,sig,tr.effect , genBasVec, genphihat ,file="Rev2-IMP-GGQ-75-5K-S1-433-gam6-3.RData")

#par(mfrow=c(2,1))


#plot(w[1:K,1],type="l")
#plot(w[1:K,2],type="l")
#plot(w[1:K,3],type="l")
#plot(w[1:K,4],type="l")
#plot(w[1:K,5],type="l")
#theta[1:(k-1),]     
#save(list=ls(),file="Rev-GGQ-thet72-JASA-N10K-gam3.RData")          

#divided by 25 and 25. 

