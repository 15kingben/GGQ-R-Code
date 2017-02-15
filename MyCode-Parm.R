metf.mean		=.1535
sulf.mean		=.1984
sulf2.mean		=.10
ins.mean		=.1465
ins2.mean		=.0733
tzd.mean		=.01

metf.se	 =.0179
sulf.se	 =.0153
ins.se	 =.0150
ins2.se	 =.0075
tzd.se	 =.005

cv.metf=	c(0.746, 0.820, 0.895, 1.119, 1.119, 0)
cv.sulf=	c(0.992, 1.091, 1.190, 1.488, 1.488, 0)
cv.sulf2=	c(0.992, 1.091, 1.190, 1.488, 1.488, 0)
cv.ins=	c(0.925, 1.018, 1.110, 1.388, 1.388, 0)
cv.ins2=	c(0.925, 1.018, 1.110, 1.388, 1.388, 0)
cv.tzd=	c(0.746, 0.820, 0.895, 1.119, 1.119, 0)


pdu.metf.abdp	=c(0,.031,.031,.031)
pdu.metf.naus	=c(0,.067,.067,.067)
pdu.metf.diar	=c(0,.106,.106,.106)
pdu.metf.dysp	=c(0,.013,.013,.013)

pdu.sulf.abdp	=c(0,.011,.011,.011)
pdu.sulf.naus	=c(0,.012,.012,.012)
pdu.sulf.diar	=c(0,.013,.013,.013)
pdu.sulf.dizz	=c(0,.013,.013,.013)

du.metf.abdp	=c(0,.005,.0025,.0075)
du.metf.naus	=c(0,.005,.0025,.0075)
du.metf.diar	=c(0,.005,.0025,.0075)
du.metf.dysp	=c(0,.005,.0025,.0075)

du.sulf.abdp	=c(0,.005,.0025,.0075)
du.sulf.naus	=c(0,.005,.0025,.0075)
du.sulf.diar	=c(0,.005,.0025,.0075)
du.sulf.dizz	=c(0,.005,.0025,.0075)
du.sulf.weig	=c(0,.04,.02,.06)
du.sulf.hypo	=c(0,.005,.0025,.0075)	

du.ins.hypo		=c(0,.005,.0025,.0075)
du.ins.weig		=c(0,.06,.03,.09)
du.ins2.hypo	=c(0,.01,.005,.015)

du.conv.oral	=c(0,.002,.001,.003)
du.conv.ins 	=c(0,.010,.005,.015)
du.safe		=c(0,.002,.001,.003)


pdiscon.metf   =c(.1145,      .1645,	.2145)			# UKPDS-13
pdiscon.sulf   =c(.1002,      .1502,	.2002)			# UKPDS-13
pdiscon.ins    =c(.2663,      .3163,	.3663)			# UKPDS-13
pdiscon.ins.hi =c(.2663*0,    .3163*0,	.3663*0)			# 

pdiscon.metf   =c(.1145,      .2145,	.2145)			# UKPDS-13
#pdiscon.metf   =c(.1145,      .1645,	.6145)			# UKPDS-13
#pdiscon.sulf   =c(.1002,      .1502*1,	.7002)			# UKPDS-13
pdiscon.sulf   =c(.1002,      0*.202,	.2002)			# UKPDS-13
##pdiscon.tzd   =c(.1002,      .1502*1,	.5002)			# UKPDS-13
pdiscon.tzd   =c(.1002,     0* .05602,	.2002)			# UKPDS-13
#pdiscon.ins    =c(.2663*0,      .1163*1,	.7663)			# UKPDS-13
pdiscon.ins    =c(.2663,      .3663,	.3663)			# UKPDS-13
pdiscon.ins.hi =c(.2663*0,    .3163*0,	.5663*1)			# 







v.values=c(1, 2, 3, 4, 1, 1, 1, 4,    	1, 4, 1, 1, 1, 1, 1, 4, 4)	# v indexes variance estimate
d.values=c(1, 1, 1, 1, 2, 3, 1, 3, 		1, 1, 2, 1, 1, 1, 1, 2, 3)	# d indexes discontinuation rate
a.values=c(1, 1, 1, 1, 1, 1,.8,.8, 		1, 1, 1,.8, 1, 1, 1, .4,.4)	# a is the adherence rate
u.values=c(1, 1, 1, 1, 1, 1, 1, 1, 		2, 2, 2, 2, 3, 4, 1, 4, 4)	# u indexes utility estimates
q.values=c(1, 1, 1, 1, 1, 1, 1, 1, 		2, 2, 2, 2, 2, 2, 3, 4, 4)	# q indexes side effect probabilities







