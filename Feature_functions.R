

# A sample feature"

genBasVec<-function(r,st,a,w,d,as,res,action){
	
#1:7
	phi1<-(action==0)*(st==0)* cbind(1,exp(-0.5*(res-6.5)^2),exp(-0.5*(res-7.5)^2) , exp(-0.5*(a-qa1)^2),exp(-0.5*(a-qa3)^2),exp(-0.5*(w-qw1)^2),exp(-0.5*(w-qw3)^2))
#8:15	
    phi2<-(action==0)*(st==1)* cbind(1,exp(-0.5*(res-q1)^2),exp(-0.5*(res-q2)^2),exp(-0.5*(res-q3)^2),d,exp(-0.5*(a-qa1)^2),exp(-0.5*(a-qa3)^2),exp(-0.5*(w-qw1)^2),exp(-0.5*(w-qw3)^2))
#16:23    
    phi3<-(action==0)*(st==2)* cbind(1,exp(-0.5*(res-5.5)^2),exp(-0.5*(res-6.5)^2),d,exp(-0.5*(a-qa1)^2),exp(-0.5*(a-qa3)^2),exp(-0.5*(w-qw1)^2),exp(-0.5*(w-qw3)^2))
#25:31    
    phi4<-(action==0)*(st==3)* cbind(1,exp(-0.5*(res-5.5)^2),exp(-0.5*(res-6.5)^2),d,exp(-0.5*(a-qa1)^2),exp(-0.5*(a-qa3)^2),exp(-0.5*(w-qw1)^2),exp(-0.5*(w-qw3)^2))
#32:39    
    phi5<-(action==0)*(st==4)* cbind(1,exp(-0.5*(res-q1)^2),exp(-0.5*(res-q3)^2),d,exp(-0.5*(a-qa1)^2),exp(-0.5*(a-qa3)^2),exp(-0.5*(w-qw1)^2),exp(-0.5*(w-qw3)^2))

    #40:46
	phi6<-(action==1)*(st==0)* cbind(1,exp(-0.5*(res-q2)^2),exp(-0.5*(res-q4)^2) ,exp(-0.5*(a-qa1)^2),exp(-0.5*(a-qa3)^2),exp(-0.5*(w-qw1)^2),exp(-0.5*(w-qw3)^2))
	
    #47:54
    phi7<-(action==2)*(st==1)* cbind(1,exp(-0.5*(res-q2)^2),exp(-0.5*(res-q3)^2),exp(-0.5*(res-q4)^2),d,exp(-0.5*(a-qa1)^2),exp(-0.5*(a-qa3)^2),exp(-0.5*(w-qw1)^2),exp(-0.5*(w-qw3)^2))
    
    
    #55:62
    phi8<-(action==3)*(st==2)* cbind(1,exp(-0.5*(res-q2)^2),exp(-0.5*(res-q3)^2),d,exp(-0.5*(a-qa1)^2),exp(-0.5*(a-qa3)^2),exp(-0.5*(w-qw1)^2),exp(-0.5*(w-qw3)^2))
    
    
 #  63:70
    phi9<-(action==4)*(st==3)* cbind(1,exp(-0.5*(res-q2)^2),exp(-0.5*(res-q3)^2),d,exp(-0.5*(a-qa1)^2),exp(-0.5*(a-qa3)^2),exp(-0.5*(w-qw1)^2),exp(-0.5*(w-qw3)^2))
    
 


#    BasVec<-cbind(phi1,phi2,phi3,phi4,phi5,phi6,phi7,phi8, phi9,phi15,phi16,phi17,phi11,phi12,phi13,phi14)    


    BasVec<-cbind(phi1,phi2,phi3,phi4,phi5,phi6,phi7,phi8,phi9)    

 #   BasVec<-cbind(phi15)    

       return(BasVec)
	
	}

new.dat2<-datwdeath[datwdeath$quarter!=1,]
#new.dat2<-new.dat2[1:400000,]

K<-dim(new.dat2)[1]

phi<-genBasVec(new.dat2$ran,new.dat2$step.past,new.dat2$age.past,new.dat2$weight.past,new.dat2$D.past, new.dat2$Astar.past,new.dat2$A1c.past,new.dat2$A) # generastes the feature functions
test<-t(phi)%*%phi

which(apply(test,2,sum)==0)

test<-solve(test)
sum(test<0)
#solve((phi)%*%t(phi))




b1<-7.02
b2<-7.98

genphihat<-function(r,st,a,w,d,res,theta,K,as){
	

phihat<-matrix(0,ncol=dimbet,nrow=K)

Q1<- as.vector(genBasVec(r,st,a,w,d,as,res,0)%*%(theta))
Q2<- as.vector(genBasVec(r,st,a,w,d,as,res,1)%*%(theta))
Q3<- as.vector(genBasVec(r,st,a,w,d,as,res,2)%*%(theta))
Q4<- as.vector(genBasVec(r,st,a,w,d,as,res,3)%*%(theta))
Q5<- as.vector(genBasVec(r,st,a,w,d,as,res,4)%*%(theta))
#Q6<- genBasVec(response[i],6,d[i],as[i])%*%(theta)




PiS1<-rep(0,length(res))

temp1<-cbind(Q1,-100,-100,-100,Q5)
PiS1[st==3 & res>b1&res<b2]<-(apply(temp1,1,which.max)-1)[st==3 & res>b1&res<b2]
temp2<-cbind(Q1,-100,-100,Q4,-100)
PiS1[st==2 & res>b1&res<b2]<-(apply(temp2,1,which.max)-1)[st==2 & res>b1&res<b2]
temp3<-cbind(Q1,-100,Q3,-100,-100)
PiS1[st==1 & res>b1&res<b2]<-(apply(temp3,1,which.max)-1)[st==1 & res>b1&res<b2]
temp4<-cbind(Q1,Q2,-100,-100,-100)
PiS1[st==0 & res>b1&res<b2]<-(apply(temp4,1,which.max)-1)[st==0 & res>b1&res<b2]


temp5<-cbind(-100,-100,-100,-100,Q5)
PiS1[st==3 &res>b2]<-(apply(temp5,1,which.max)-1)[st==3 &res>b2]
temp6<-cbind(-100,-100,-100,Q4,-100)
PiS1[st==2 &res>b2]<-(apply(temp6,1,which.max)-1)[st==2 &res>b2]
temp7<-cbind(-100,-100,Q3,-100,-100)
PiS1[st==1 &res>b2]<-(apply(temp7,1,which.max)-1)[st==1 &res>b2]
temp8<-cbind(-100,Q2,-100,-100,-100)
PiS1[st==0 &res>b2]<-(apply(temp8,1,which.max)-1)[st==0 &res>b2]



PiS1[st==4]<-0
PiS1[st==4 | res<b1]<-0



phihat<-genBasVec(r,st,a,w,d,as,res,PiS1)


return(phihat)
}

#r<-new.dat2$ranf
#st<-new.dat2$step.d
#a<-new.dat2$age
#w<-new.dat2$weight
#d<-new.dat2$D
#as<-new.dat2$Astar
#res<-new.dat2$A1c
#theta<-theta[k-1,]
