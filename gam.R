library('nlme')
library('mgcv')


updowns <- function(seq2){
	sign(apply(seq2,2,diff))
}

abnormal <- function(seq2){
	1*(seq2[,2] > 180)
}

if(!("diabetes" %in% ls())){
	diabetes = read.csv(file.choose()) #finally_diabetes_data.csv
	head(diabetes)
	numdates = as.numeric(as.Date(diabetes$date,format = "%m/%d/%Y"))
	nas = which(is.na(numdates))
	for(ind in nas){
		mnd = (numdates[ind-1]+numdates[ind+1])/2
		if(trunc(mnd) == mnd){
			numdates[ind] = mnd 
		}else{
			diabetes = diabetes[-ind,]
		}
	}
	diabetes$date = numdates[which(!is.na(numdates))]
	for(case in 1:69){
		mindate =  min(diabetes$date[diabetes$case == case])
		diabetes$date[diabetes$case == case] = diabetes$date[diabetes$case == case] - 
		mindate + 1 
	}
	head(diabetes)
	#Lag
	diabetes_lag <- diabetes
	diabetes_lag $ Reg_ins_lag <- c(diabetes $ Reg_ins[-1] , 0)
	diabetes_lag $ NPH_ins_lag <- c(diabetes $ NPH_ins[-1] , 0)
	diabetes_lag $ min_glu_lag <- c(diabetes $ min_glu[-1] , 0)
	diabetes_lag $ max_glu_lag <- c(diabetes $ max_glu[-1] , 0)
	diabetes_lag <- diabetes_lag[-nrow(diabetes_lag),]
	head(diabetes_lag)
	tail(diabetes_lag)
	#3-fold cv
	test_diabetes = train_diabetes = 
	test_diabetes_lag = train_diabetes_lag = list()
	for(fold in 1:3){
		sam = (fold - 1)*23 + 1:23
		test_diabetes[[fold]] = diabetes[diabetes$case %in% sam,]
		train_diabetes[[fold]] = diabetes[! diabetes$case %in% sam,]
		test_diabetes_lag[[fold]] = diabetes_lag[diabetes_lag$case %in% sam,]
		train_diabetes_lag[[fold]] = diabetes_lag[! diabetes_lag$case %in% sam,]
	}
}
CV00 = CV01 = CV10 = CV11 = 0 
CVRS00 = CVRS01 = CVRS10 = CVRS11 = 0
CVAB00 = CVAB01 = CVAB10 = CVAB11 = 0
for(fold in 1:3){
n = nrow(train_diabetes[[fold]])
nt = nrow(test_diabetes[[fold]])
###-===================================================
###-===================================================
###-===================================================
###-===================================================
###-===================================================
# Without lag & With Out random effects

df = 3

L0R0 <- gam(list(
min_glu ~ s(Reg_ins,k=df) + s(NPH_ins,k=df),
max_glu ~ s(Reg_ins,k=df) + s(NPH_ins,k=df)), 
data = train_diabetes[[fold]], family = mvn(d = 2))

pred00 = predict(L0R0,test_diabetes[[fold]])
y00 = cbind(test_diabetes[[fold]]$min_glu,
test_diabetes[[fold]]$max_glu)
res00 = pred00 - y00

CV00 = CV00 + sum(res00^2,na.rm = T)/nt/2/3
CVRS00 = CVRS00 + 
sum(updowns(pred00) == updowns(y00))/prod(dim(y00))/3

CVAB00 = CVAB00 +
sum(abnormal(pred00) == abnormal(y00))/sum(y00[,2] > 180)/3

###-===================================================
###-===================================================
###-===================================================
###-===================================================
###-===================================================
# with lag & Without  random effects

df = 3

L1R0 <- gam(list(
min_glu_lag ~ s(Reg_ins,k=df) + s(NPH_ins,k=df)+
s(min_glu,k=df) + s(max_glu,k=df),
max_glu_lag ~ s(Reg_ins,k=df) + s(NPH_ins,k=df)+
s(min_glu,k=df) + s(max_glu,k=df)), 
data = train_diabetes_lag[[fold]], family = mvn(d = 2))


pred10 = predict(L1R0,test_diabetes_lag[[fold]])
y10 = cbind(test_diabetes_lag[[fold]]$min_glu_lag,
test_diabetes_lag[[fold]]$max_glu_lag)
res10 = pred10 - y10

CV10 = CV10 + sum(res10^2,na.rm = T)/nt/2/3

CVRS10 = CVRS10 + 
sum(updowns(pred10) == updowns(y10))/prod(dim(y10))/3

CVAB10 = CVAB10 +
sum(abnormal(pred10) == 1 & abnormal(y10) == 1)/sum(y10[,2] > 180)/3




###-===================================================
###-===================================================
###-===================================================
###-===================================================
###-===================================================
###-===================================================
# Without  lag & With random effects

df = 3

L0R1 <- gam(list(
min_glu ~ s(date,k=df) + 
s(date, by=case,k=df) + s(Reg_ins,k=df) 
+ s(NPH_ins,k=df),
max_glu ~ s(date,k=df) + 
s(date, by=case,k=df) + s(Reg_ins,k=df) 
+ s(NPH_ins,k=df)), 
data = train_diabetes[[fold]], family = mvn(d = 2), 
method = "REML")

pred01 = predict(L0R1,test_diabetes[[fold]])
y01 = cbind(test_diabetes[[fold]]$min_glu,
test_diabetes[[fold]]$max_glu)
res01 = pred01 - y01

CV01 = CV01 + sum(res01^2,na.rm = T)/nt/2/3


CVRS01 = CVRS01 + 
sum(updowns(pred01) == updowns(y01))/prod(dim(y01))/3


CVAB01 = CVAB01 +
sum(abnormal(pred01) == 1 & abnormal(y01) == 1)/sum(y01[,2] > 180)/3



###-===================================================
###-===================================================
###-===================================================
###-===================================================
###-===================================================
###-===================================================
# with lag & With random effects

df = 3

L1R1 <- gam(list(
min_glu_lag ~ s(date,k=df)+
s(date, by=case,k=df) + 
s(Reg_ins,k=df) + s(NPH_ins,k=df)+
s(min_glu,k=df) + s(max_glu,k=df),
max_glu_lag ~ s(date,k=df)+ s(date, by=case,k=df) + 
s(Reg_ins,k=df) + s(NPH_ins,k=df)+
s(min_glu,k=df) + s(max_glu,k=df)), 
data = train_diabetes_lag[[fold]], family = mvn(d = 2), 
method = "REML")

pred11 = predict(L1R1,test_diabetes_lag[[fold]])
y11 = cbind(test_diabetes_lag[[fold]]$min_glu_lag,
test_diabetes_lag[[fold]]$max_glu_lag)
res11 = pred11 - y11 

CV11 = CV11 + sum(res11^2,na.rm = T)/nt/2/3


CVRS11 = CVRS11 + 
sum(updowns(pred11) == updowns(y11))/prod(dim(y11))/3


CVAB11 = CVAB11 +
sum(abnormal(pred11) == 1 & abnormal(y11) == 1)/sum(y11[,2] > 180)/3

}
CV00
CV01
CV10
CV11

CVRS00 
CVRS01 
CVRS10 
CVRS11


CVAB00 
CVAB01 
CVAB10 
CVAB11

###-===================================================
###-===================================================
###-===================================================
###-===================================================
###-===================================================

n = nrow(train_diabetes[[1]])
nt = nrow(test_diabetes[[1]])

rand_start = sample(1:(nt-51),1)
rand_range = rand_start:(rand_start+50)


L0R0 <- gam(list(
min_glu ~ s(Reg_ins,k=df) + s(NPH_ins,k=df),
max_glu ~ s(Reg_ins,k=df) + s(NPH_ins,k=df)), 
data = train_diabetes[[1]], family = mvn(d = 2))

L1R0 <- gam(list(
min_glu_lag ~ s(Reg_ins,k=df) + s(NPH_ins,k=df)+
s(min_glu,k=df) + s(max_glu,k=df),
max_glu_lag ~ s(Reg_ins,k=df) + s(NPH_ins,k=df)+
s(min_glu,k=df) + s(max_glu,k=df)), 
data = train_diabetes_lag[[1]], family = mvn(d = 2))


L0R1 <- gam(list(
min_glu ~ s(date,k=df) + 
s(date, by=case,k=df) + s(Reg_ins,k=df) 
+ s(NPH_ins,k=df),
max_glu ~ s(date,k=df) + 
s(date, by=case,k=df) + s(Reg_ins,k=df) 
+ s(NPH_ins,k=df)), 
data = train_diabetes[[1]], family = mvn(d = 2), 
method = "REML")

L1R1 <- gam(list(
min_glu_lag ~ s(date,k=df)+
s(date, by=case,k=df) + 
s(Reg_ins,k=df) + s(NPH_ins,k=df)+
s(min_glu,k=df) + s(max_glu,k=df),
max_glu_lag ~ s(date,k=df)+ s(date, by=case,k=df) + 
s(Reg_ins,k=df) + s(NPH_ins,k=df)+
s(min_glu,k=df) + s(max_glu,k=df)), 
data = train_diabetes_lag[[1]], family = mvn(d = 2), 
method = "REML")



dev.new(width=100, height=50)
par(mfrow=c(2,2))

ts.plot(test_diabetes[[1]]$min_glu[rand_range],
ylim = c(min(test_diabetes[[1]]$min_glu[rand_range]),
max(test_diabetes[[1]]$max_glu[rand_range])),
ylab = "min and max glucose",
main="without lag & without random effects",
xlab = "random range of time of length 50")
lines(predict(L0R0,test_diabetes[[1]])[rand_range,1],col="red")
lines(test_diabetes[[1]]$max_glu[rand_range])
lines(predict(L0R0,test_diabetes[[1]])[rand_range,2],col="blue")


ts.plot(test_diabetes_lag[[1]]$min_glu_lag[rand_range],
ylim = c(min(test_diabetes_lag[[1]]$min_glu_lag[rand_range]),
max(test_diabetes_lag[[1]]$max_glu_lag[rand_range])),
ylab = "min and max future glucose",
main="with lag & without random effects",
xlab = "random range of time of length 50")
lines(predict(L1R0,test_diabetes_lag[[1]])[rand_range,1],col="red")
lines(test_diabetes_lag[[1]]$max_glu_lag[1:100])
lines(predict(L1R0,test_diabetes_lag[[1]])[rand_range,2],col="blue")


ts.plot(test_diabetes[[1]]$min_glu[rand_range],
ylim = c(min(test_diabetes[[1]]$min_glu[rand_range]),
max(test_diabetes[[1]]$max_glu[rand_range])),
ylab = "min and max glucose",
main="without lag & With random effects",
xlab = "random range of time of length 50")
lines(predict(L0R1,test_diabetes[[1]])[rand_range,1],col="red")
lines(test_diabetes[[1]]$max_glu[rand_range])
lines(predict(L0R1,test_diabetes[[1]])[rand_range,2],col="blue")


ts.plot(test_diabetes_lag[[1]]$min_glu_lag[rand_range],
ylim = c(min(test_diabetes_lag[[1]]$min_glu_lag[rand_range]),
max(test_diabetes_lag[[1]]$max_glu_lag[rand_range])),
ylab = "min and max future glucose",
main="with lag & With random effects",
xlab = "random range of time of length 50")
lines(predict(L1R1,test_diabetes_lag[[1]])[rand_range,1],col="red")
lines(test_diabetes_lag[[1]]$max_glu_lag[rand_range])
lines(predict(L1R1,test_diabetes_lag[[1]])[rand_range,2],col="blue")


