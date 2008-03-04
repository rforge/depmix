#
# Started by Ingmar Visser, March 4, 2008
# 

# 
# BALANCE SCALE data example with age as covariate on class membership
# 

setwd("/Users/ivisser/Documents/projects/depmixProject/depmixNew/rforge/depmix/trunk/data/")

balance=read.table("balance.txt",head=T)

save(balance,file="balance.rda")

# the data set consists of four distance items on a balance scale task
# the data frame contains 11 columns with covariates sex age in days and age in years,
# and two versions of the four items; t1-t4 are trichotomous items with 1, 2, and 3 
# meaning left, balance, and right; d1-d4 are dichotomously scored items with 0=incorrect
# and 1=correct




# 
# SPEED ACCURACY DATA
# 

# FIX ME!!!!!

# data specs rudy
rudy=read.table("hrudy", header=TRUE)
rudy=rudy[,c("rt","corr","Pacc")]
rudy[,2]=replace(rudy[,2],which(rudy[,2]==-1),0)
rudy[,1]=log(rudy[,1])
rudy=markovdata(dat=rudy,item=c("con","cat","cov"),ntimes=c(168,134,137),xm=-999,dname="Rudy")
rudy[,"Pacc"]=rudy[,"Pacc"]/max(rudy[,"Pacc"])
save(rudy,file="rudy.rda",ascii=TRUE)



# 1948-1983 rainfall data
sno=matrix(scan("rain"),36,365,byrow=T)
sno90=matrix(sno[,1:90],36,90)
snorep=replace(sno90,list=which(sno90>0),1)
snorep=t(snorep)
rain=markovdata(dat=snorep,ntimes=rep(90,36),itemt="cat",dname="rain")
save(rain,file="rain.rda",ascii=TRUE)

# all together
ntimes=c(11,17,18,11,25,15,14,10,13,32,10,10,11,10,12,15,14,10,15,23,10,
13,15,17,9,11,26,20,18,13,9,10,11,9,9,18,20,22,45,13,25,11,
11,15,14,12,23,11,47,27,16,10,12,15,10,14,18,17,10,16,11,23,11,
18,12,10,11,16,10,9,11,10,10,9,13,11,10,10,11,11,10,10,37,9,
11,10,10,10,9,10,10,10,10,10,16,13,10,12,13,9,11,9,15,11,10,
13,12,9,12,14,22,14,15,10,10,13,10,9,12,11,9,10,10,11,10,10,
12,9,9,16,12,10,11,11,12,10,9,14,10,11,10,10,9,10,10,10,10,
10,14,11,12,10,10,9,11,10,14,25,23,39,29,29,26,23,21,29,28,24,
33,27,23,41,20,22,31,29,48,22,44,42,19,30,35,39,40,43,38,17,27,
22,47,42)

discrimination=markovdata(dat=scan("mdall"),item="cat",ntimes=ntimes,dname="discrimination",itemn="correct")
save(discrimination,file="discrimination.rda",ascii=TRUE)



# ...Bentley data, 2 component mixture

mbd <- markovdata(dat=scan("mbd"),itemtypes="cont",ntimes=rep(1,220))
save(mbd,file="mbd.rda",ascii=TRUE)
