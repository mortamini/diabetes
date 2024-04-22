glucose = read.csv(file.choose()) #-----out2.csv -----
head(glucose)
n = nrow(glucose)

cntr = 0
newglucosedate = newglucosecase = newglucose33 =
newglucose34 = newglucose58 = newglucose60 = newglucose62 = c()
for(case in 1:69){
dates = unique(glucose$date[glucose$case == case])
newglucosedate = c(newglucosedate, dates)
newglucosecase = c(newglucosecase, rep(case,length(dates)))
for(date in dates){
cntr = cntr + 1
newglucose33[cntr] = mean(glucose$Code_33[glucose$case == case & glucose$date == date],na.rm = T)
newglucose34[cntr] = mean(glucose$Code_34[glucose$case == case & glucose$date == date],na.rm = T)
newglucose58[cntr] = mean(glucose$Code_58[glucose$case == case & glucose$date == date],na.rm = T)
newglucose60[cntr] = mean(glucose$Code_60[glucose$case == case & glucose$date == date],na.rm = T)
newglucose62[cntr] = mean(glucose$Code_62[glucose$case == case & glucose$date == date],na.rm = T)
}
}
glucose = data.frame(date = newglucosedate, case = newglucosecase,
Reg_ins = newglucose33, NPH_ins = newglucose34, Pr_brkfst_glu = newglucose58,
Pr_lnch_glu = newglucose60, Pr_sup_glu = newglucose62)
head(glucose)
n = nrow(glucose)
data = as.matrix(glucose[,-(1:2)])
allmiss = which(apply(data,1,function(t) all(is.na(t)|is.nan(t))))
notallmiss = which(!apply(data,1,function(t) all(is.na(t)|is.nan(t))))
for(ii in allmiss){
neigh = notallmiss[order(abs(ii-notallmiss))[1:2]]
data[ii,] = (data[neigh[1],]+data[neigh[2],])/2
}
if(ncol(data)>1) data = complete(mice(data,printFlag=FALSE))
data = as.matrix(data)
glucose3 = cbind(glucose[,1:2],data)
head(glucose3)