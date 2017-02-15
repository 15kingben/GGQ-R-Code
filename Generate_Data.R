
expit<-function(x){
	exp(x)/(1+exp(x))
	}


sig<-1.0
sigep<-rep(.5,N)



source("~/MyCode-Parm.R") # reads the parameter values
goal=7

bl.treatment<-sample(c(0,2,3,6),N,replace=TRUE) #1=None 2=Sulf 3=Ins 4=Sulf+Ins
bl.treatment<-sample(c(0),N,replace=TRUE) #1=None 2=Sulf 3=Ins 4=Sulf+Ins


ID<-1:N
bl.a1c<-rnorm(N,7.7,sig)
bl.age<-round(rnorm(N,0,1),2)
gender<-rbinom(N,1,0.5)
race<-rbinom(N,1,0.7)
bl.weight<-round(rnorm(N,0,1),2)



bl.a1c.none<-rnorm(sum(bl.treatment==0),9.0,sig)
bl.a1c.Sulf<-rnorm(sum(bl.treatment==2),9.2,sig)
bl.a1c.Ins<-rnorm(sum(bl.treatment==3),9.4,sig)
bl.a1c.SulfIns<-rnorm(sum(bl.treatment==6),9.7,sig)

bl.a1c[bl.treatment==0]<-bl.a1c.none
bl.a1c[bl.treatment==2]<-bl.a1c.Sulf
bl.a1c[bl.treatment==3]<-bl.a1c.Ins
bl.a1c[bl.treatment==6]<-bl.a1c.SulfIns


        
        a1c.type<-rep(0,N)
        a1c.type[bl.treatment==0]<-1
        a1c.type[bl.treatment==2]<-2
        a1c.type[bl.treatment==3]<-3
        a1c.type[bl.treatment==6]<-4
        
        
a1c.order=cbind(none=c(1:4,7),sulf.only=c(6,1,3,4,7),ins.only=c(1,3,5,7,7),sulf.ins=c(1,3,5,7,7))
a1c.numtx=cbind(none=c(1,1,1,1,0),sulf.only=c(1,1,1,1,0),ins.only=c(1,1,1,0,0),sulf.ins=c(1,1,1,0,0))




nh3<-data.frame(ID,bl.treatment,bl.a1c)

S=17
v=v.values[S]
d=d.values[S]
a=a.values[S]
u=u.values[S]
q=q.values[S]

A=du.conv.oral[u];  B=du.conv.ins[u];  C=du.safe[u]; D=du.sulf.hypo[u]; E=du.sulf.weig[u]; F=du.ins.hypo[u]; G=du.ins.weig[u]; H=du.ins2.hypo[u];
	a1c.du.bl =c(0,A+C+D+E,B+C+F+G,A+B+C+C+D+E+F+G)  	#based on ldl.type
	a1c.incdu=cbind(none=c(A+C,A+C+D+E,B+C+F+G,H),
		     sulf.only=c(A+C,B+C+F+G,H,0),
			ins.only=c(A+C,H,0,0),
			sulf.ins=c(A+C,H,0,0))

# A1c: We assume patients on sulf-only at bl get metf, ins, ins2;
		       # patients on ins-only at bl get metf, ins2;
               # patients on sulf+ins at bl get metf, ins2;

	##### mu matrix ####################
	tr.effect<-c(.15,.20,0.02,.14,.07,.10,.0)

	mu<-matrix(0,ncol=4,nrow=6)
	mu[1,]<-c(9,9.2,9.4,9.7)
	for (i in 2:5){
		
		for (j in 1:4) {
			
			mu[i,j]<- mu[i-1,j]- mu[i-1,j]*tr.effect[a1c.order[i-1,j]]
			
			
		}
	}
	mu[6,]<-mu[5,]
	
#####################################	
			
	
	###########DuDc should be added here
	source("~/MyCode-dudc.R")
    ##################################### 
	
		
	reachgoal0=ifelse(bl.a1c<=goal,1,0);
	nh3$goalat=ifelse(reachgoal0==1,0,99)

	nh3$post.a1c=bl.a1c
    nh3$visit.muP<-rep(9.7,N)
	nh3$visit.muP[bl.treatment==0]<-9
	nh3$visit.muP[bl.treatment==2]<-9.2
	nh3$visit.muP[bl.treatment==3]<-9.4
	nh3$num.intens=0	
	nh3$du=a1c.du.bl[a1c.type]+ifelse(bl.treatment==2,sim.du.sulf.all,0)			 #allowing baseline du;
	aug=rep(0,length(bl.a1c))	
	
	nh3$visit.mu<-mu[1,a1c.type]
	nh3$dc<-rep(0,N)
		
		head(nh3[,-c(4:15)])
		
		mean.pctreduc<-c(.15,.20,0.02,.14,.07,.10,.0)

		max.step<-c(4,4,3,3)[a1c.type]
		
	#----------------------------------------------------------------------;
nvisits<-15
mu.visit<-pctreduc.m<-dc.m<-r<-visit.postF<-step.m<-visit.tx<-asstx<-pctreduc.m2<-visit.muP.m<-age.m<-weight.m<-matrix(0,ncol=nvisits,nrow=N);
step=rep(0,N)
visit=1
for( visit in 1:nvisits) {

if(visit==1){ age.m[,visit]<-round( (bl.age+rnorm(N,0,.5))/sqrt(1+.5^2),2)} else{age.m[,visit]<-round((age.m[,visit-1]+rnorm(N,0.0,.5))/sqrt(1+.5^2),2)}


if(visit==1){ weight.m[,visit]<-round((bl.weight+rnorm(N,0,.5))/sqrt(1+.5^2),2)} else{weight.m[,visit]<-round((weight.m[,visit-1]+rnorm(N,0.0,.5))/sqrt(1+.5^2),2)}

 visit.pctreduc=rep(0,N);		 dc.vector=rep(0,N); ae.vector=rep(0,N); past.ae.vector=rep(0,N); visit.mu=rep(0,N); visit.muP=rep(0,N);     a1c.numtxt<-asstxt<-a1c.orderi<-rep(0,N); 
 
post.a1c<- visit.post<-visit.muA<-reachgoal<-num.intens<-du<-goalat<-rep(0,N);


temp<-NULL
for (i in 1:N){	

	if(visit==1){temp<-as.vector(nh3$num.intens[i]+1+nh3$dc[i]<6); visit.pctreduc[i][temp]<-mean.pctreduc[(a1c.order[nh3$num.intens[i][temp]+1+nh3$dc[i][temp],a1c.type[i][temp]])]} else{temp<-as.vector(nh3$num.intens[i]+1+nh3$dc[i]+r[i,visit-1]<6); visit.pctreduc[i][temp]<-mean.pctreduc[(a1c.order[step[i]+(1),a1c.type[i][temp]])]}



dc.m[i,visit]<-dc.vector[i]<-dc.matrix[i,a1c.order[step[i]+1,a1c.type[i]]]
mu.visit[i,visit]<-visit.muP[i]<- mu[nh3$num.intens[i]+1,a1c.type[i]] # mu in the previous visit
a1c.numtxt[i]<-(a1c.numtx[nh3$num.intens[i]+1,a1c.type[i]])
asstx[i,visit]<-a1c.orderi[i]<-(a1c.order[step[i]+1,a1c.type[i]])
}

pctreduc.m[,visit]<-visit.pctreduc
temp<-NULL


		indic.dc<-rep(0,N)
	if(visit>1){	indic.dc<-(visit.tx[,visit-1]==4 | visit.tx[,visit-1]==5); dc.vector[indic.dc]=0}
		
		

		
	elge<-nh3$post.a1c>8
	
	if(visit==1){visit.muP[elge]<-ifelse(dc.m[elge,1]!=1,mu.visit[elge,visit]*(1-visit.pctreduc[elge]),nh3$visit.muP[elge])} else{treat.eff<-visit.pctreduc[elge]; visit.muP[elge]<-ifelse((dc.m[elge,visit]!=1)*(asstx[elge,visit]!=asstx[elge,visit-1])*(asstx[elge,visit-1]!=7), nh3$visit.muP[elge]*(1-treat.eff),nh3$visit.muP[elge])}


	
		true.a1c=(nh3$post.a1c[elge]-	nh3$visit.muP[elge]+sig*rnorm(sum(elge),0,sigep[elge]))/sqrt(1+sigep[elge]^2)+ nh3$visit.muP[elge]*(1-visit.pctreduc[elge])*1

		temp<-(nh3$post.a1c[elge]-	nh3$visit.muP[elge]+sig*rnorm(sum(elge),0,sigep[elge]))/sqrt(1+sigep[elge]^2)+ nh3$visit.muP[elge]*1

		a1c.vec=cbind(true.a1c,1)
		
		
     if(visit==1){indic=rep(1,sum(elge))}  else{ indic<-visit.tx[elge,visit-1]!=5 & visit.tx[elge,visit-1]!=4& visit.tx[elge,visit-1]!=7}
		
		post.a1c[elge]=ifelse(nh3$post.a1c[elge]>goal&dc.vector[elge]!=1&indic==1,a1c.vec[,1],temp); 
    	visit.post[elge]=ifelse(nh3$post.a1c[elge]>goal&dc.vector[elge]!=1&indic==1,a1c.vec[,1],temp);



      visit.muA[elge]<-ifelse(dc.vector[elge]!=1&indic==1, visit.mu[elge] , visit.muP[elge]);



	reachgoal=ifelse(post.a1c[elge]<=goal&nh3$goalat[elge]>(visit-1),1,0);
	goalat[elge]=ifelse(reachgoal==1,visit,nh3$goalat[elge])		
	du[elge]   =ifelse(goalat[elge]>=1&dc.vector[elge]!=1,du[elge]+ae.vector[elge],du[elge])

	num.intens[elge]=ifelse(nh3$goalat[elge]>(visit-1)&dc.vector[elge]!=1&nh3$num.intens[elge]<5&indic==1 ,nh3$num.intens[elge]+ a1c.numtxt[elge],nh3$num.intens[elge])

	#	nh3.2=nh3.1
  ########################################

 ################### nh3.0$post.a1c>7 & nh3.0$post.a1c<8


   elge<-(  nh3$post.a1c<8)  # LOOK!!!
	n11<-sum( nh3$post.a1c<8)
#	r[elge,visit]<-rbinom(N,1,1/2)[elge]

##############Confounding######################
if(visit==1){r[elge,visit]<-rbinom(N,1,expit(1-.2*bl.a1c))[elge]} else{r[elge,visit]<-rbinom(N,1,expit(0-.2*visit.postF[,visit-1]+0.5*step.m[,visit-1] + 0.5* dc.m[,visit-1]))[elge]}

###############################################


	r[elge & nh3$post.a1c<7 ,visit]<-0

     if(visit==1){indic=rep(1,length(nh3$post.a1c))}  else{ indic<-asstx[,visit-1]!=5 & asstx[,visit-1]!=4& asstx[,visit-1]!=7}

    r[elge==TRUE & indic==0 ,visit]<-0

	
	r1<-r[elge,visit]
	
	dc.vector[elge][r1==1]<-0
	dc.vector[nh3$post.a1c<7]<-0
	dc.vector11<-dc.vector[elge]

if(visit==1){asstx[elge,visit]<-ifelse(r[elge,visit]==1 | bl.a1c[elge]<7,bl.treatment[elge],asstx[elge,visit])} else{	asstx[elge,visit]<-ifelse(r[elge,visit]==1 | nh3$post.a1c[elge]<7,asstx[elge,visit-1],asstx[elge,visit])}

if(visit==1){visit.muP[elge]<-ifelse(r1!=1&dc.m[elge,1]!=1,mu.visit[elge,visit]*(1-visit.pctreduc[elge]),nh3$visit.muP[elge])} else{treat.eff<-visit.pctreduc[elge]; visit.muP[elge]<-ifelse((asstx[elge,visit]!=asstx[elge,visit-1])*(dc.m[elge,visit]!=1), nh3$visit.muP[elge]*(1-treat.eff),nh3$visit.muP[elge])}



        visit.muA[elge]<-ifelse(r1!=1&dc.vector[elge]!=1, visit.mu[elge] , visit.muP[elge]);
		
		true.a1c=((nh3$post.a1c[elge]-	nh3$visit.muP[elge]+sig*rnorm(sum(elge),0,sigep[elge]))/sqrt(1+sigep[elge]^2)+ nh3$visit.muP[elge]*(1-visit.pctreduc[elge]))*(1-r1)+ ((nh3$post.a1c[elge]-nh3$visit.muP[elge]+sig*rnorm(sum(elge),0,sigep[elge]))/sqrt(1+sigep[elge]^2)+nh3$visit.muP[elge])*r1
		

		
		a1c.vec=cbind(true.a1c,1)

		temp<-(nh3$post.a1c[elge]-	nh3$visit.muP[elge]+sig*rnorm(sum(elge),0,sigep[elge]))/sqrt(1+sigep[elge]^2)+ nh3$visit.muP[elge]*1
		
        
     if(visit==1){indic=rep(1,sum(elge))}  else{ indic<-visit.tx[elge,visit-1]!=5 & visit.tx[elge,visit-1]!=4& visit.tx[elge,visit-1]!=7}

		post.a1c[elge]=ifelse(nh3$post.a1c[elge]>goal&dc.vector11!=1&r1!=1&indic==1,a1c.vec[,1],temp); 
    	visit.post[elge]=ifelse(nh3$post.a1c[elge]>goal&dc.vector11!=1&r1!=1& indic==1, a1c.vec[,1],temp);	
	
	reachgoal=ifelse(post.a1c[elge]<=goal&nh3$goalat[elge]>(visit-1),1,0);
	goalat[elge]=ifelse(reachgoal==1,visit,nh3$goalat[elge])		
	du[elge]   =ifelse(goalat[elge]>=1&dc.vector11!=1,du[elge]+ae.vector[elge],du[elge])

    num.intens[elge]=ifelse(nh3$post.a1c[elge]>goal&dc.vector11!=1&r1!=1&indic==1,nh3$num.intens[elge]+a1c.numtxt[elge] ,nh3$num.intens[elge])
    
    
    
    

		
if(visit==1) {
	visit.tx[,visit]<-ifelse(nh3$post.a1c>goal,a1c.order[1,a1c.type],bl.treatment)
	visit.tx[dc.vector==1,visit]<-bl.treatment[dc.vector==1]
	visit.tx[elge,visit][ r1==1]<-bl.treatment[elge][r1==1]
	step=step+1*(visit.tx[,visit]!=bl.treatment)+(dc.vector==1)
	} else{
    grched<-nh3$post.a1c<7
	visit.tx[,visit]<-ifelse(nh3$post.a1c>goal,a1c.orderi,visit.tx[,visit-1])
	visit.tx[elge,visit][r[elge,visit]==1]<-visit.tx[elge,visit-1][r[elge,visit]==1]
	visit.tx[dc.vector==1,visit]<-visit.tx[dc.vector==1,visit-1]
    visit.tx[grched,visit] <- visit.tx[grched,visit-1]
#	step<-step+1*(visit.tx[,visit]!=visit.tx[,visit-1])+(dc.vector==1)*(visit.tx[,visit]!=visit.tx[,visit-1])
		step<-step+1*(visit.tx[,visit]!=visit.tx[,visit-1])*(visit.tx[,visit]!=7)+(dc.vector==1)*as.numeric(step<(max.step+1))*(visit.tx[,visit]==visit.tx[,visit-1])*(1-grched)

	}
			
	step.m[,visit]<-step
	visit.postF[,visit]<-visit.post
	dc.m[,visit]<-dc.vector
	visit.muP.m[,visit]<-visit.muP
	nh3$goalat<-goalat
    nh3$visit.muA<-visit.muA
    nh3$visit.muP<-visit.muP
    nh3$post.a1c<-post.a1c
    nh3$visit.post<-visit.post
    nh3$dc<-dc.vector
    nh3$du<-du
    nh3$num.intens<-num.intens
#    print(cbind(visit,step))
}  # visit loop


#visit.postF[1:100,]
visit.tx[49,]
visit.postF[49,]
visit.muP.m[49,]
dc.m[49,]
r[49,]
bl.treatment[49]
mean.pctreduc
mu
a1c.order




##########################################################
###################DATA FRAME#######################################
##########################################################
A1c<-A<-D<-Astar<-ran<-bl.treat<-step.d<-age<-weight<-NULL

for(i in 1:N){
	temp1<-c(bl.a1c[i],visit.postF[i,])
	temp2<-c(bl.treatment[i], visit.tx[i,])
	temp3<-c(0, dc.m[i,])
	temp4<-c(bl.treatment[i],asstx[i,])
	temp5<-c(0,r[i,])
#	temp6<-rep(bl.treatment[i],41)
	temp6<-rep(bl.treatment[i],nvisits+1)
	temp7<-c(0,step.m[i,])
	temp8<-c(bl.age[i],age.m[i,])
	temp9<-c(bl.weight[i],weight.m[i,])

    A1c<-c(A1c,temp1)
	Astar<-c(Astar,temp2)
	D<-c(D,temp3)
	A<-c(A,temp4)
	ran<-c(ran,temp5)
	bl.treat<-c(bl.treat,temp6)
	step.d<-c(step.d,temp7)
	age<-c(age,temp8)
	weight<-c(weight,temp9)
	
if(i%%1000==0) print( i)
}

A1c.past<-c(0,A1c[-(1+visit)*N])
Astar.past<-c(0,Astar[-(1+visit)*N])
A.past<-c(0,A[-(1+visit)*N])
D.past<-c(0,D[-(1+visit)*N])
step.past<-c(0,step.d[-(1+visit)*N])
ran.past<-c(0,ran[-(1+visit)*N])
age.past<-c(0,age[-(1+visit)*N])
weight.past<-c(0,weight[-(1+visit)*N])
ranf<-rbinom(length(A1c),1,0.5)
ranf[A1c>8]<-0
pavis=1:((visit+1)*N)
ID<-gl(N,visit+1,(visit+1)*N)
quarter<-gl(visit+1,1,(visit+1)*N)
A1c.past[quarter==1]<-0
Astar.past[quarter==1]<-0
step.past[quarter==1]<-0
D.past[quarter==1]<-0



new.dat.0<-data.frame(cbind(ID,quarter,age.past,weight.past,ran.past,A.past,D.past,Astar.past,step.past,A1c.past,catA1c.past=round(A1c.past,0),age,weight,ran,A,D,Astar,step.d,A1c,catA1c=round(A1c,0),ranf,bl.treat))

new.dat.0$A1c.past[new.dat.0$quarter==1]<-rnorm(sum(new.dat.0$quarter==1),9.4,1.5)


new.dat.01<-new.dat.0[new.dat.0$bl.treat==0,]

head(new.dat.01)

#new.dat.01[1:500,-c(3:7)]
new.dat2<-new.dat.nao<-na.omit(new.dat.01)
#hist(new.dat.01$A1c[new.dat.01$quarter==1])
#hist(new.dat.nao$catA1c)



#b<-round(quantile(new.dat.nao$A1c.past,prob=c(0,0.1,0.2,0.32,0.4,0.5,0.59,0.65,0.7,0.75,0.8,0.85,0.9,1)),10)
#### NEW TXT#####
		b<-round(c(-1,5.9,6.64,7.01,7.5,7.98,8.5,9.4,19),10)
		b<-round(c(-1,5.9,6.64,7.01,7.2,7.5,7.7,7.98,8.5,9.4,19),10)
		b<-round(c(-1,7.01,7.2,7.5,7.7,7.98,9.4,19),10)



b.a<-round(quantile(new.dat.nao$age.past, prob=c(0,0.30,0.5,0.8,1)),10)
b.a<-c(b.a[1]-0.001,b.a[-c(1,length(b.a))],b.a[length(b.a)]+0.001)

b.w<-round(quantile(new.dat.nao$weight.past, prob=c(0,0.30,0.5,0.8,1)),10)
b.w<-c(b.w[1]-0.001,b.w[-c(1,length(b.w))],b.w[length(b.w)]+0.001)



catA1c<-cut(new.dat.nao$A1c,breaks=b,label=FALSE)
catA1c.past<-cut(new.dat.nao$A1c.past,breaks=b,label=FALSE)

catage<-cut(new.dat.nao$age,breaks=b.a,label=FALSE)
catage.past<-cut(new.dat.nao$age.past,breaks=b.a,label=FALSE)

catweight<-cut(new.dat.nao$weight,breaks=b.w,label=FALSE)
catweight.past<-cut(new.dat.nao$weight.past,breaks=b.w,label=FALSE)


new.dat.nao$catage<-catage
new.dat.nao$catage.past<-catage.past

new.dat.nao$catA1c<-catA1c
new.dat.nao$catA1c.past<-catA1c.past

new.dat.nao$catweight<-catweight
new.dat.nao$catweight.past<-catweight.past





sum(catA1c)
sum(catA1c.past)


new.dat.nao$A[new.dat.nao$A1c.past<7&new.dat.nao$A1c.past>.1]<-7
new.dat2$A[new.dat2$A1c.past<7&new.dat2$A1c.past>.1]<-0
new.dat2$Astar[new.dat2$A1c.past<7& new.dat2$A1c.past>.1]<-0

new.dat.nao$A[new.dat.nao$ran==1]<-7


new.dat.nao$A[new.dat.nao$A==7]<-new.dat.nao$Astar[new.dat.nao$Astar==7]<-0
new.dat.nao$A[new.dat.nao$A1c.past<7&new.dat.nao$A1c.past>.1]<-0
new.dat.nao$Astar[new.dat.nao$A1c.past<7&new.dat.nao$A1c.past>.1]<-0
new.dat.nao$Astar[new.dat.nao$ran==1 | new.dat.nao$D==1]<-0
new.dat2$Astar[new.dat2$ran==1 | new.dat2$D==1]<-0



new.dat.nao$Astar.past<-c(0,new.dat.nao$Astar[-length(new.dat.nao$Astar)])
new.dat.nao$A.past<-c(0,new.dat.nao$A[-length(new.dat.nao$Astar)])

new.dat2$Astar.past<-c(0, new.dat2$Astar[-length(new.dat.nao$Astar)])
new.dat2$A.past<-c(0, new.dat2$A[-length(new.dat.nao$Astar)])
new.dat2$A[new.dat2$A==7]<-0
new.dat2$Astar[new.dat2$Astar==7]<-0


tn<-sum((new.dat.nao$A==0)*(new.dat.nao$A1c.past>7))
sum((new.dat.nao$A==0)*(new.dat.nao$A1c.past>7)*(new.dat.nao$A1c<7))/tn

tn<-sum((new.dat.nao$A==1)*(new.dat.nao$A1c.past>7))
sum((new.dat.nao$A==1)*(new.dat.nao$A1c.past>7)*(new.dat.nao$A1c<7))/tn

tn<-sum((new.dat.nao$A==2)*(new.dat.nao$A1c.past>7))
sum((new.dat.nao$A==2)*(new.dat.nao$A1c.past>7)*(new.dat.nao$A1c<7))/tn


tn<-sum((new.dat.nao$A==3)*(new.dat.nao$A1c.past>7))
sum((new.dat.nao$A==3)*(new.dat.nao$A1c.past>7)*(new.dat.nao$A1c<7))/tn

tn<-sum((new.dat.nao$A==4)*(new.dat.nao$A1c.past>7))
sum((new.dat.nao$A==4)*(new.dat.nao$A1c.past>7)*(new.dat.nao$A1c<7))/tn

mean.pctreduc

#new.dat.nao[1:100,]
#new.dat.01[1:1000,]
	temp.n3<-sum((new.dat.nao$A==(2-1))) # D dinominator
sum((new.dat.nao$D==(2-1))*(new.dat.nao$A==(2-1)))/temp.n3

	temp.n3<-sum((new.dat.nao$A==(3-1))) # D dinominator
sum((new.dat.nao$D==(2-1))*(new.dat.nao$A==(3-1)))/temp.n3

	temp.n3<-sum((new.dat.nao$A==(4-1))) # D dinominator
sum((new.dat.nao$D==(2-1))*(new.dat.nao$A==(4-1)))/temp.n3

	temp.n3<-sum((new.dat.nao$A==(5-1))) # D dinominator
sum((new.dat.nao$D==(2-1))*(new.dat.nao$A==(5-1)))/temp.n3

1-mean(new.dat.nao$A1c[new.dat.nao$Astar==3]/new.dat.nao$A1c.past[new.dat.nao$Astar==3])




head(new.dat.nao)
mean(new.dat.nao$catA1c.past)


temp<- I(new.dat.nao$A1c-7>0)*(new.dat.nao$A1c)^2
##new.dat.nao$death<-rbinom(dim(new.dat.nao)[1],1,expit(-10+0.7*new.dat.nao$A1c+0.5*new.dat.nao$step.d))
new.dat.nao$death<-rbinom(dim(new.dat.nao)[1],1,expit(-10+0.08*temp+0.5*new.dat.nao$step.d))

temp<-aggregate(death ~ID,data= new.dat.nao,FUN= sum)
sum(temp$death==0)/N # This clculates the percentage of death


dim(aggregate(death ~ID,data= new.dat.nao,FUN= cumsum))
#save(list=ls(),file="GenDat-S1-10000-7YCat-nonreg-Sig1.RData")
agr<-aggregate(death ~ID,data= new.dat.nao,FUN= cumsum)

cs<-NULL
for(i in 1:N){
	
	temp<-cumsum(agr[i,-1])
	cs<-c(cs,temp)
	
}
length(new.dat.nao$step.past)
length(cs)


datwdeath<-new.dat.nao[cs<2,]
