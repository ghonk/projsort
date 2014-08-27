# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # PROJECT SORT YEAR TWO IMPUTATION AND ANALYSIS SCRIPT  # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # common filepaths
# # # #
# # # gh win
#setwd("C:\\Users\\garrett\\Dropbox\\aa projects\\materials\\SORT\\yr 2 data")
# # # gh linux
#setwd("~/Dropbox/aa projects/materials/SORT/yr 2 data")
# # # Linux ~ Skynet
#setwd("/home/garrett/projectsort")
# # # #
# # # type this to run script
# source("prsort.R")
# # # #
# # # # clear workspace, load libraries
rm(list = ls())
library(psych)
library(stringr)
library(MASS)
library(mi)

# # TODOOOOS # #
# # need to change log and gaus function to take a list of conditions, zelig generally busted
# # full data splitter bug
# # sort accuracy as predictor
# # hi/lo unit scores as split variable 
# # performance on prior exps as a predictor 
	
# # # # define some functions
# # # #

# # # # separate analysis output; prints clean divisions and spaces
# # how do you print a clean line without numbers ie [1]
printspace <- function(numspaces,printline){
	if (printline==1)
		{
		print(rep("___", 28))
		}
	cat(rep("\n",numspaces))
	}
	
# # # # iterable imputation func
iterimp <- function(datatoimpute,numberimps,expname){
	# # get/set imputation info
	info = mi.info(datatoimpute)
	info = update(info, "type", list("period" = "unordered-categorical"))
	print(info)
	# # impute dat data once
	imp = mi(datatoimpute, info, n.imp = numberimps, n.iter = 60, R.hat = 1.1, 
		max.minutes = 90, rand.imp.method = "bootstrap", 
		run.past.convergence = FALSE, seed = NA, 
		check.coef.convergence = TRUE, add.noise = noise.control())
	# # impute dat data til *data* convergence
	looptoconverge = FALSE
	if ((converged(imp, check = "data")) == TRUE){
		looptoconverge = TRUE
		}
	while(looptoconverge == FALSE){
		imp = mi(imp)
		looptoconverge = converged(imp, check = "data")
		print(paste0("data converged: ",converged(imp, check = "data")))
		print(paste0("coefs converged: ",converged(imp, check = "coefs")))
		}
	# # save imputed datasets
	curdir = getwd()
	subdir = expname
	if (file.exists(subdir)){
		setwd(file.path(curdir, subdir))
		write.mi(imp)
	} else {
		dir.create(file.path(curdir, subdir))
		setwd(file.path(curdir, subdir))
		write.mi(imp)
	}
	setwd(curdir)	
	return(imp)
	}

# # # # iterable cumulative link mixed model regression function	
# # clmm function
iterclmm <- function(experiment, variables, expdata, conditions){		
	for (vars in variables){
		# # create list for each model
		varname = paste0(vars,"_modlist")
		condcount = 1
		for (condition in conditions){
			listname = paste0(varname,condcount)
			eval(parse(text=paste0(listname," = list()")))
			templist = list()
			count = 1
			# # iterate through each dataset and run model
			for (dataset in expdata){
				modelname = paste0(vars,"_ds",count)
				# # set up  clmm regression
				tempcom = paste0(modelname,
					' <- clmm(as.factor(',vars,') ~ as.factor(',condition,
					')+as.numeric(Zunit1Score)+(1|teacher/class),data = dataset)')
				# # run it
				eval(parse(text=tempcom))
				# # save it, print model summary
				eval(parse(text=paste0("templist[[",count,"]] = ",modelname)))
				count = count + 1
				}
			eval(parse(text=paste0(listname,"<<- templist")))	
		# #	# # create pooled coefficient table
			#eval(parse(text=paste0("modellist = ", templist)))
			modellist = templist
			modelrownames = rownames(coefficients(summary(modellist[[1]])))
			# # these vars find the target factors in the regression
			tablelength = length(modelrownames)
			factorrowlength = length(modelrownames[substr(modelrownames, 1, 2) == "as"])
			startrow = length(modelrownames) - (factorrowlength - 1) 
			coeflist = list()
			# # HACK TO MAKE LIST WORK - NEED BETTER SOLUTION FOR ADDING TO EMPTY LIST
			coeflist[[1]] = 0
			# # make a list of target coefficients
			for (model in modellist){
				coeflist[[length(coeflist)+1]] = 
					coefficients(summary(model))[startrow:tablelength,]			
				}
			coeflist = coeflist[2:length(coeflist)]
			# # prints coefficient table for every imputed dataset
			#print(coeflist)
			# # transform coefficient list
			coefest = list(); stderrors = list() 
			zvals = list(); pvals = list()
			count = 1
			# # assign vals 
			for (coeftable in coeflist){
				temptable = t(coeftable)
				coefest[[count]] = temptable[1,]; stderrors[[count]] = temptable[2,]
				zvals[[count]] = temptable[3,]; pvals[[count]] = temptable[4,]
				count = count + 1
				}
			tempvals = data.frame(cbind(coefest, stderrors, zvals, pvals))
			eval(parse(text=paste0(vars, "_regvals <<- tempvals")))
			# # create regression result table
			print(vars)
			print(condition)
			coefsandstderror = mi.pooled(coefest, stderrors)
			coef.est = coefsandstderror$coefficients; std.error = coefsandstderror$se
			zvalues = coef.est / std.error; pvalues = 2 * (1- pnorm(abs(zvalues)))
			pooltable = data.frame(cbind(coef.est,std.error,zvalues,pvalues))
			eval(parse(text=paste0(vars, "_sum <<- pooltable")))
			print(pooltable)
			printspace(2,0)
			condcount = condcount + 1
			}		
		printspace(4,0)
		}
	printspace(4,0)
	}

# # # # function to initialize zelig mi data object	
zeligmidata <- function(experiment, impdata){
	tempdata = paste0("impdata_e",experiment)
	x = ""
	for (impnum in 1:length(impdata)){
		y = paste0(tempdata,"[[",impnum,"]],")
		x = paste0(x, y)
		}
	x = substr(x, 1, nchar(x)-1)
	eval(parse(text=paste0("tempdata = mi(",x,")"))) 	
	return(tempdata)
	}
	
# # # # iterable zelig hierarchical regression modelling function
iterzelig <- function(experiment, variables, modeltype, conditions){
	# # # # analyze each variable of the target type
	for (vars in variables){
		modelname = paste0(vars,"Xe",experiment)
		modelsumname = paste0(modelname, "_sum")
		# # # # run model
		tempcom = paste0(modelname, ' <<- zelig(',vars,' ~ as.factor(',conditions[1],
							')+as.numeric(Zunit1Score)+tag(1|teacher/class),model = \"',
							modeltype,'\",data = tempdata,cite=FALSE)')
		eval(parse(text=tempcom))
		print(eval(parse(text=paste0(modelsumname, " <<- summary(", modelname,")"))))			
		printspace(3,1)
		}
	}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # setup output file and load data # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
printspace(25,0)	
sink(file = "projectSortAnalysis.txt", append = FALSE, 
			type = c("output", "message"),split = TRUE)
rundate = Sys.time()
print(rundate)
printspace(1,0)

# # # # get data
data_full = read.csv("sort y2 data final.csv")
data_e1 = read.csv("sort y2 data final e1.csv")
data_e2 = read.csv("sort y2 data final e2.csv")
data_e3 = read.csv("sort y2 data final e3.csv")
data_e4 = read.csv("sort y2 data final e4.csv")
datalist = list(data_e1, data_e2, data_e3, data_e4) 

# # # # setup data
data_full <- subset(data_full, subset=(sped == 0))

# # ? want to include SPED (1) ? if not (0)
includesped = 0
if(includesped==0){
	for(expdata in 1:length(datalist)){
		datalist[[expdata]] = subset(datalist[[expdata]], 
			subset=(datalist[[expdata]]$sped == 0))
		}
	}
## ERROR HERE - SHOULD INCLUDE FULL DATA UP TO CURRENT EXP 
# # ? use only participants with complete data(1) or exp specific(0) ?
includecomplete = 0
if(includecomplete==1){
	for(expdata in 1:length(datalist)){
		datalist[[expdata]] = subset(datalist[[expdata]],
			subset=(datalist[[expdata]]$allData == 1))
		}
	}
		
# # # # print general descriptives for full dataset/all exps
print(describe(data_full))
readline("Press <return> to start imputation") 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # conduct imputation  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # Strip unneeded vars for imputation
impdatalist = list()
impdatalist[[1]] = datalist[[1]][,4:18]
impdatalist[[2]] = datalist[[2]][,6:19]
impdatalist[[3]] = datalist[[3]][,5:16]
impdatalist[[4]] = datalist[[4]][,16:27]

# # # # iterate through datasets and impute values 
for(dataset in 1:length(impdatalist)){
	# # create labels
	objlabel = paste0("impobj_e",dataset)
	datasetlabel = paste0("impdata_e",dataset)
	# # run imp and save imputation objects
	tempcom = paste0("impobj_e",dataset," = iterimp(impdatalist[[dataset]],
						40,datasetlabel)")
	eval(parse(text=tempcom))
	# # save datasets as active dataframes
	tempcom = paste0(datasetlabel, " = mi.completed(", objlabel, ")")
	eval(parse(text=tempcom))
	}
save.image("imputeddata.RData")
readline("All datasets imputed. Press <return>")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # build variables for each imputed dataset for each exp # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # #
#   E1  #
# # # # #

for(dataset in 1:length(impdata_e1)){
	# # free response transfer var
	a = impdata_e1[[dataset]]$e1_1_pingsolo_tri/max(impdata_e1[[dataset]]$e1_1_pingsolo_tri)
	b = impdata_e1[[dataset]]$e1_2_pingteam_tri/max(impdata_e1[[dataset]]$e1_2_pingteam_tri)
	z = cbind(a,b)
	impdata_e1[[dataset]]$frtrans = apply(z, 1, mean)
	# # total transfer
	z = cbind(impdata_e1[[dataset]]$trans,impdata_e1[[dataset]]$e1_1_mc,
		impdata_e1[[dataset]]$e1_2_mc)
	impdata_e1[[dataset]]$tottrans = apply(z, 1, mean)
	# # free response mastery
	a = impdata_e1[[dataset]]$e1_4_mammals/max(impdata_e1[[dataset]]$e1_4_mammals)
	b = impdata_e1[[dataset]]$e1_5_beaks_tri/max(impdata_e1[[dataset]]$e1_5_beaks_tri)
	z = cbind(a,b,impdata_e1[[dataset]]$e1_3_lizards_bi)
	impdata_e1[[dataset]]$frmast = apply(z, 1, mean)
	# # total mastery
	z = cbind(impdata_e1[[dataset]]$frmast,impdata_e1[[dataset]]$e1_3_mc,
			  impdata_e1[[dataset]]$e1_5_mc)
	impdata_e1[[dataset]]$totmast = apply(z, 1, mean)
	# # total performance
	z = cbind(impdata_e1[[dataset]]$totmast,impdata_e1[[dataset]]$tottrans)
			impdata_e1[[dataset]]$totperf = apply(z, 1, mean)
	}

# # # # #
#   E2  #
# # # # #

for(dataset in 1:length(impdata_e2)){
	# # transfer var
	a = impdata_e2[[dataset]]$e2_1_bakery_1_tri/max(impdata_e2[[dataset]]$e2_1_bakery_1_tri)
	b = impdata_e2[[dataset]]$e2_2_grocery_2_quad/max(impdata_e2[[dataset]]$e2_2_grocery_2_quad)
	z = cbind(a,b,impdata_e2[[dataset]]$e2_1_bakery_2_bi,impdata_e2[[dataset]]$e2_2_grocery_1_bi)
	impdata_e2[[dataset]]$trans = apply(z, 1, mean)
	# # free response mastery
	a = impdata_e2[[dataset]]$e2_3_invasive_tri/max(impdata_e2[[dataset]]$e2_3_invasive_tri)
	b = impdata_e2[[dataset]]$e2_4_herbicide_tri/max(impdata_e2[[dataset]]$e2_4_herbicide_tri)
	z = cbind(a,b)
	impdata_e2[[dataset]]$frmast = apply(z, 1, mean)
	# # total mastery
	z = cbind(impdata_e2[[dataset]]$frmast,impdata_e2[[dataset]]$e2_5_mc,
			  impdata_e2[[dataset]]$e2_6_mc)
	impdata_e2[[dataset]]$totmast = apply(z, 1, mean)
	# # total performance
	z = cbind(impdata_e2[[dataset]]$totmast,impdata_e2[[dataset]]$trans)
	impdata_e2[[dataset]]$totperf = apply(z, 1, mean)
	}

# # # # #
#   E3  #
# # # # #

for(dataset in 1:length(impdata_e3)){
	# # transfer var
	a = impdata_e3[[dataset]]$e3_1_hotpot_tri/max(impdata_e3[[dataset]]$e3_1_hotpot_tri)
	b = impdata_e3[[dataset]]$e3_2_music_tri/max(impdata_e3[[dataset]]$e3_2_music_tri)
	z = cbind(a,b)
	impdata_e3[[dataset]]$trans = apply(z, 1, mean)
	# # free response mastery
	a = impdata_e3[[dataset]]$e3_3_monarchs_1_tri/max(impdata_e3[[dataset]]$e3_3_monarchs_1_tri)
	b = impdata_e3[[dataset]]$e3_3_monarchs_2_tri/max(impdata_e3[[dataset]]$e3_3_monarchs_2_tri)
	z = cbind(a,b)
	impdata_e3[[dataset]]$frmast = apply(z, 1, mean)
	# # total mastery
	z = cbind(impdata_e3[[dataset]]$frmast,impdata_e3[[dataset]]$e3_4_mc,
			  impdata_e3[[dataset]]$e3_5_mc)
	impdata_e3[[dataset]]$totmast = apply(z, 1, mean)
	# # total performance
	z = cbind(impdata_e3[[dataset]]$totmast,impdata_e3[[dataset]]$trans)
	impdata_e3[[dataset]]$totperf = apply(z, 1, mean)
	}  

# # # # #
#   FA  #
# # # # #

for(dataset in 1:length(impdata_e4)){
	# # transfer var
	a = impdata_e4[[dataset]]$fa_1_tech/max(impdata_e4[[dataset]]$fa_1_tech)
	b = impdata_e4[[dataset]]$fa_2_tulips/max(impdata_e4[[dataset]]$fa_2_tulips)
	z = cbind(a,b)
	impdata_e4[[dataset]]$trans = apply(z, 1, mean)
	# # free response mastery
	z = cbind(impdata_e4[[dataset]]$fa_3_bears_1_bi,impdata_e4[[dataset]]$fa_3_bears_2_bi)
	impdata_e4[[dataset]]$frmast = apply(z, 1, mean)
	# # total mastery
	z = cbind(impdata_e4[[dataset]]$frmast,impdata_e4[[dataset]]$fa_mc_1,
			  impdata_e4[[dataset]]$fa_mc_2,impdata_e4[[dataset]]$fa_mc_3)
	impdata_e4[[dataset]]$totmast = apply(z, 1, mean)
	# # total performance
	z = cbind(impdata_e4[[dataset]]$totmast,impdata_e4[[dataset]]$trans)
	impdata_e4[[dataset]]$totperf = apply(z, 1, mean)
	}  

# # # # make var and condition lists


# # # # # # 
# E1 vars #
# # # # # # 
e1gausvars   <- colnames(impdata_e1[[1]][,16:20])
e1gausvars[length(e1gausvars)+1] = "ZevoScore"
e1ordvars    <- colnames(impdata_e1[[1]][,7:10])
e1bivars     <- colnames(impdata_e1[[1]][,11:15])
# # add condition dummy codes
for (dataset in 1:length(impdata_e1)){
	impdata_e1[[dataset]]$cond_E1_noFB = datalist[[1]]$cond_E1_noFB
	impdata_e1[[dataset]]$cond_E1_robFB = datalist[[1]]$cond_E1_robFB
	impdata_e1[[dataset]]$sortacc = datalist[[1]]$e1sort_acc
	meanunitscore = mean(impdata_e1[[dataset]]$Zunit1Score)
	impdata_e1[[dataset]]$hilounitscore = rep(0,length(impdata_e1[[dataset]]$Zunit1Score))
	impdata_e1[[dataset]]$hilounitscore[impdata_e1[[dataset]]$Zunit1Score >= meanunitscore] = 1
	# # uncomment for sort accuracy analyses
	#impdata_e1[[dataset]] = subset(impdata_e1[[dataset]],
	#	subset=(impdata_e1[[dataset]]$cond_E1_names != "bl"))
	}
e1condset = list()
e1condset = 
colnames(impdata_e1[[1]])[substr(colnames(impdata_e1[[1]]), 1, 4) == "cond"]

# # # # # # 
# E2 vars #
# # # # # # 
e2gausvars   <- colnames(impdata_e2[[1]][,15:18])
e2gausvars[length(e2gausvars)+1] = "ZevoScore"
e2ordvars    <- colnames(impdata_e2[[1]][,7:10])
e2bivars     <- colnames(impdata_e2[[1]][,11:14])
# # add condition dummy codes
for (dataset in 1:length(impdata_e2)){
	impdata_e2[[dataset]]$cond_E2_SL = datalist[[2]]$cond_E2_SL
	impdata_e2[[dataset]]$cond_E2_LS = datalist[[2]]$cond_E2_LS
	impdata_e2[[dataset]]$sortacc = datalist[[2]]$e2sort_acc
	meanunitscore = mean(impdata_e2[[dataset]]$Zunit1Score)
	# # uncomment for sort accuracy analyses
	#impdata_e2[[dataset]] = subset(impdata_e2[[dataset]],
	#	subset=(impdata_e2[[dataset]]$cond_E2_LDS != "d_BL"))
	
	}
e2condset = list()
e2condset = 
colnames(impdata_e2[[1]])[substr(colnames(impdata_e2[[1]]), 1, 4) == "cond"]

# # # # # # 
# E3 vars #
# # # # # # 
e3gausvars   <- colnames(impdata_e3[[1]][,13:16])
e3gausvars[length(e3gausvars)+1] = "ZevoScore"
e3ordvars    <- colnames(impdata_e3[[1]][,7:10])
e3bivars     <- colnames(impdata_e3[[1]][,11:12])
# # add condition dummy codes
for (dataset in 1:length(impdata_e3)){
	impdata_e3[[dataset]]$cond_E3_Single = datalist[[3]]$cond_E3_S
	impdata_e3[[dataset]]$cond_E3_Spaced = datalist[[3]]$cond_E3_SC
	impdata_e3[[dataset]]$sortacc = datalist[[3]]$e3sortPropCorr
	meanunitscore = mean(impdata_e3[[dataset]]$Zunit1Score)
	# # uncomment for sort accuracy analyses
	#impdata_e3[[dataset]] = subset(impdata_e3[[dataset]],
	#	subset=(impdata_e3[[dataset]]$cond_E3_M != "d_BL"))
	# # 
	}
e3condset = list()
e3condset = 
colnames(impdata_e3[[1]])[substr(colnames(impdata_e3[[1]]), 1, 4) == "cond"]

# # # # # # 
# FA vars #
# # # # # # 
e4gausvars   <- colnames(impdata_e4[[1]][,13:16])
e4gausvars[length(e4gausvars)+1] = "ZevoScore"
e4ordvars    <- colnames(impdata_e4[[1]][,6:7])
e4bivars     <- colnames(impdata_e4[[1]][,8:12])
# # condition list
e4condset    <- colnames(impdata_e4[[1]][,1:2])	
	

	
# # Pause
save.image("imputationwithvars.rdata")
readline("Data is ready to go. Press <return> to start analysis") 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # Begin  Ordinal Analysis # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(ordinal)
options(warn=1)
# # # # analyze all ordinal variables
for (experiment in 1:3){
	# # # # analyze e1
	if (experiment == 1){
		iterclmm(experiment, e1ordvars, impdata_e1, e1condset)
		}
	if (experiment == 2){
		iterclmm(experiment, e2ordvars, impdata_e2, e2condset)
		}
	if (experiment == 3){
		iterclmm(experiment, e3ordvars, impdata_e3, e3condset)
		}
	}
	
save.image("ordinaldata.rdata")
readline("Ordinal analyses complete. <return>") 

# # don't load on skynet
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # Begin Logistic and Gaussian Analysis  # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
options(warn=1)
library(Zelig)
library(ZeligMultilevel)
source("summaryFunc.r")
#library(ZeligChoice)

# # # # analyze each experiment: binomial and gaussian vars
for (experiment in 1:3){
	# # # # analyze e1
	if (experiment == 1){
		tempdata = zeligmidata(experiment, impdata_e1)
		e1condset[[1]] = "cond_E1_BL"
		iterzelig(experiment, e1gausvars, "ls.mixed", e1condset)
		iterzelig(experiment, e1bivars, "logit.mixed", e1condset)
		e1condset[[1]] = "cond_E1_noFB"
		iterzelig(experiment, e1gausvars, "ls.mixed", e1condset)
		iterzelig(experiment, e1bivars, "logit.mixed", e1condset)
		}
	if (experiment == 2){
		e2condset[[1]] = "cond_E2_LDS"
		tempdata = zeligmidata(experiment, impdata_e2)
		iterzelig(experiment, e2gausvars, "ls.mixed", e2condset)
		iterzelig(experiment, e2bivars, "logit.mixed", e2condset)
		e2condset[[1]] = "cond_E2_SL"
		iterzelig(experiment, e2gausvars, "ls.mixed", e2condset)
		iterzelig(experiment, e2bivars, "logit.mixed", e2condset)
		e2condset[[1]] = "cond_E2_LS"
		iterzelig(experiment, e2gausvars, "ls.mixed", e2condset)
		iterzelig(experiment, e2bivars, "logit.mixed", e2condset)
		}
	if (experiment == 3){
		e3condset[[1]] = "cond_E3_M"
		tempdata = zeligmidata(experiment, impdata_e3)
		iterzelig(experiment, e3gausvars, "ls.mixed", e3condset)
		iterzelig(experiment, e3bivars, "logit.mixed", e3condset)
		e3condset[[1]] = "cond_E3_Single"
		iterzelig(experiment, e3gausvars, "ls.mixed", e3condset)
		iterzelig(experiment, e3bivars, "logit.mixed", e3condset)
		e3condset[[1]] = "cond_E3_Spaced"
		iterzelig(experiment, e3gausvars, "ls.mixed", e3condset)
		iterzelig(experiment, e3bivars, "logit.mixed", e3condset)
		}
	}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # Begin Aptitude Split Analysis # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


sink()	
closeAllConnections()
