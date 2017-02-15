# disutilities and discontinioutions


#################################################################
#################################################################
#################################################################

# simulate metformin disutilities;
#	elig=which(contr_metf!=1)		# restrict to patients who aren't contraindicated
	du1=runif(N,0,1); du2=runif(N,0,1); du3=runif(N,0,1);      du4=runif(N,0,1);     
	 # q indexes side effect probabilities
	 # u indexes utility estimates  
	sim.du.metf.abdp	=rep(0,N); sim.du.metf.abdp	=ifelse(du1<=pdu.metf.abdp[q],du.metf.abdp[u],0);
	sim.du.metf.naus	=rep(0,N); sim.du.metf.naus	=ifelse(du2<=pdu.metf.naus[q],du.metf.naus[u],0);
	sim.du.metf.diar	=rep(0,N); sim.du.metf.diar	=ifelse(du3<=pdu.metf.diar[q],du.metf.diar[u],0);
	sim.du.metf.dysp	=rep(0,N); sim.du.metf.dysp	=ifelse(du4<=pdu.metf.dysp[q],du.metf.dysp[u],0);
	had.metf.du=ifelse(sim.du.metf.abdp!=0|sim.du.metf.naus!=0|sim.du.metf.diar!=0|sim.du.metf.dysp!=0,1,0)
	pct.ge1.metf.du=mean(had.metf.du)
	sim.du.metf.all=sim.du.metf.abdp+sim.du.metf.naus+sim.du.metf.diar+sim.du.metf.dysp

### diar, naus and ... are side effects 


# simulate sulfonylurea disutilities;
	du1=runif(N,0,1); du2=runif(N,0,1); du3=runif(N,0,1); du4=runif(N,0,1);
	sim.du.sulf.abdp	=rep(0,N); sim.du.sulf.abdp	=ifelse(du1<=pdu.sulf.abdp[q],du.sulf.abdp[u],0);
	sim.du.sulf.naus	=rep(0,N); sim.du.sulf.naus	=ifelse(du2<=pdu.sulf.naus[q],du.sulf.naus[u],0);
	sim.du.sulf.diar	=rep(0,N); sim.du.sulf.diar	=ifelse(du3<=pdu.sulf.diar[q],du.sulf.diar[u],0);
	sim.du.sulf.dizz	=rep(0,N); sim.du.sulf.dizz	=ifelse(du4<=pdu.sulf.dizz[q],du.sulf.dizz[u],0);
	had.sulf.du=ifelse(sim.du.sulf.abdp!=0|sim.du.sulf.naus!=0|sim.du.sulf.diar!=0|sim.du.sulf.dizz!=0,1,0)
	pct.ge1.sulf.du=mean(had.sulf.du)
	sim.du.sulf.all=sim.du.sulf.abdp+sim.du.sulf.naus+sim.du.sulf.diar+sim.du.sulf.dizz

# simulate insulin disutilities;
	#all subjects have du for hypoglycemia and risk of serious adverse events
	had.bl.du.any=ifelse(bl.treatment==2&had.sulf.du==1,1,0)

 
#################################################################
#################################################################
#################################################################


#################################################################
#################################################################
#################################################################

# simulate metformin discontinuation;
	elig=which(had.metf.du==1)		#excluding patients with metformin contraindications
	sim=runif(length(elig),0,1);                 #d indexes the discontinuation rate  
	dc.metf.ae1=rep(0,N); dc.metf.ae1[elig]=ifelse(sim<=pdiscon.metf[d]*.5/pct.ge1.metf.du,1,0);  #pct.ge1.metf.du=mean(had.metf.du[elig])

	elig=which(had.metf.du==0)
	sim=runif(length(elig),0,1);
	dc.metf.ae0=rep(0,N); dc.metf.ae0[elig]=ifelse(sim<=pdiscon.metf[d]*.5/(1-pct.ge1.metf.du),1,0);
	nh3$discon.metf=dc.metf.ae0+dc.metf.ae1
	#if (S<9){
	#	elig=which(contr_metf!=1)			#Paper 1 analysis	
	#	sim=runif(length(elig),0,1);
	#	nh3$discon.metf=0; nh3$discon.metf[elig]=ifelse(sim<=pdiscon.metf[d],1,0);
	#	}


# simulate sulfonylurea discontinuation;			#note: all patients have weight gain on sulf, so all at risk for discon;
	elig=which(bl.treatment!=2)					#excluding patients who are on sulf at baseline
#	elig=which(bl.treatment==2)					#excluding patients who are on sulf at baseline
	sim=runif(length(elig),0,1);
	nh3$discon.sulf=0; nh3$discon.sulf[elig]=ifelse(sim<=pdiscon.sulf[d],1,0);

# simulate TZD discontinuation;			#note: all patients have weight gain on sulf, so all at risk for discon;
	elig=which(bl.treatment!=6)					#excluding patients who are on sulf at baseline
#	elig=which(bl.treatment==3)					#excluding patients who are on sulf at baseline
	sim=runif(length(elig),0,1);
	nh3$discon.tzd=0; nh3$discon.tzd[elig]=ifelse(sim<=pdiscon.tzd[d],1,0);



# simulate insulin discontinuation;				#note: all patients have weight gain on ins, so all at risk for discon;
	elig=which(bl.treatment!=3|bl.treatment!=4)
	sim=runif(length(elig),0,1);
	nh3$discon.ins=0; nh3$discon.ins[elig]=ifelse(sim<=pdiscon.ins[d],1,0);

# simulate high-dose insulin discontinuation;  # NOT excluding patients who are on ins at baseline.  Using incremental dc rate
	sim=runif(N,0,1);				# Note: we have set dc on high dose to zero via input parameters.
	nh3$discon.ins.hi=0; nh3$discon.ins.hi=ifelse(sim<=(pdiscon.ins.hi[d]-pdiscon.ins[d]),1,0);  




#nh3$discon.tzd<-0
nh3$discon.sulf2<-0

sim.du.sulf2.all=0
sim.du.tzd.all=0

#dc.matrix=cbind(dc.inel.metf=nh3$discon.metf,dc.sulf=nh3$discon.tzd,dc.sulf=nh3$discon.tzd,dc.ins=nh3$discon.ins,dc.ins2=ifelse(nh3$discon.ins==1|nh3$discon.ins.hi==1,1,0),dc.sulf2=nh3$discon.sulf2,max.step=0)

dc.matrix=cbind(dc.inel.metf=nh3$discon.metf,dc.sulf=nh3$discon.sulf,dc.tzd=nh3$discon.tzd,dc.ins=nh3$discon.ins,dc.ins2=ifelse(nh3$discon.ins==1|nh3$discon.ins.hi==1,1,0),dc.sulf2=nh3$discon.sulf2,max.step=0)


ae.matrix=cbind(du.ae.metf=sim.du.metf.all,du.ae.sulf=sim.du.sulf.all,du.ae.tzd=sim.du.tzd.all,du.ae.ins=0,du.ae.ins2=0,du.ae.sulf2=sim.du.sulf2.all,max=0)
	# ae.matrix will add disutilities based on simulated side effects to disutilities that apply to everyone (weight gain, hypog)

#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
   

