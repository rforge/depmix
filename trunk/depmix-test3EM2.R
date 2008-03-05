setwd("/Users/ivisser/Documents/projects/depmixProject/depmixNew/rforge/depmix/trunk/")

source("depmixS4.R")
source("classes.R")
source("hmModel.R")
source("fithmModel.R")
source("lystig.R")
source("fb.R")
source("trGLM.r")
source("rVGLM.r")

source("EM.R")

maxit=100
tol=1e-5

load("data/speed.Rda")

rModels <- list(
  list(
	rModel(formula=rt~1,data=speed,family=gaussian(),pstart=c(5.5,.2))),
  list(
	rModel(formula=rt~1,data=speed,family=gaussian(),pstart=c(6.3,.2)))
)

trstart=c(0.8,0.2,0.1,0.9)
instart=c(0,1)


mod <- depmix(rModels=rModels,data=speed,transition=~1,trstart=trstart,instart=instart)

logLik(mod)
source("EM.R")
fmod <- em(mod,verbose=T)

# 
# OPTIMIZE USING RDONLP
# 

fixed <- getpars(mod,"fixed")
fixed[2] <- TRUE
fdmod <- fit(mod,fixed=fixed)


# final loglike
# [1] -84.3424

# pars:   
# init: 	0.00        20.58             
# trans: 	0.00 		-2.13            
# 			0.00        2.39
#         
# obser:	5.51     	0.19    
# 			6.38        0.24




		