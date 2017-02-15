

b1<-7.01
b2<-7.98


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

genphihat<-function(r,st,a,w,d,res,theta,K,as){
	

value<-NULL; PiS.vec<-NULL
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



value<-genBasVec(r,st,a,w,d,as,res,PiS1)%*%theta
PiS.vec<-PiS1

return(list(OP=PiS.vec,val=value))
}



	ff<-genphihat(new.dat2$ran,new.dat2$step.d,new.dat2$age,new.dat2$weight,new.dat2$D, new.dat2$A1c,thet,K,new.dat2$Astar)


