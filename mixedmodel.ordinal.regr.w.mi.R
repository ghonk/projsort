#Do you know of an approach/package that facilitates mixed model regression of 
#ordinal dependent variables on multiply imputed datasets in R?

#Ideally, the function takes:

#a list of multiply imputed datasets

#a list of target variables (dependent variables, one or more)

#a list of factors (independent variables, one or more)

#a list of dummy coded conditions (for analysis of multifactor IVs)

#and returns a table similar to the result of other regressions in r

#After extensive searching, I had to create such a function using CLMM in the 
#ordinal package. 

#If you can't answer the first question perhaps you can advise me wrt code 
#adaptations, statistical appropriateness of my approach, efficiency (it takes a
#LONG time with many imputed datasets), etc

#here's some data that mirrors mine

numimpdatasets = 3; N = 170; datalist = list()
for (datasetnum in 1:numimpdatasets){
    dvone = sample(1:5, N, replace=T)   
    dvtwo = sample(1:5, N, replace=T)
    teacher = c('Tom','Dick','Harry')[sample(1:3, N, replace=T)]
    studentclass = c('Class1','Class2','Class3','Class4','Class5',
                     'Class6')[sample(1:6, N, replace=T)]
    aptitude = runif(N, -3.5, 3.5)
    randord = sample(1:3, N, replace=T)
    conddummycode1 = c('a_cond1', 'b_cond2', 'c_cond3')[randord]
    conddummycode2 = c('c_cond1', 'b_cond2', 'a_cond3')[randord]
    datalist[[datasetnum]] = data.frame(cbind(dvone, dvtwo,teacher,
            studentclass, aptitude, conddummycode1, conddummycode2))
    }
dvs = colnames(datalist[[1]])[1:2]
conditions = colnames(datalist[[1]])[6:7]
ivs = c("+as.numeric(aptitude)","+(1|teacher/studentclass)")


# here is my approach. I use the coefficient/stderror pooling from the mi package
# and then calculate the p-value for each factor

# Right now I only have the summary table displaying the factors in the 
# regression. The ideal approach would output a table identical to that of 
# summary(clmmModel) but I haven't done the research/found the math for pooling
# the other output components


require(ordinal)
require(mi)

iterclmm <- function(dvs, ivs, datalist, conditions){       
    for (dv in dvs){
        # # create name for list of target variable models
        varname = paste0(dv,"_modlist")
        ivlist = ""
        for (iv in ivs){ivlist = paste0(ivlist,iv)}
        # # roll through each dummy-coded condition designation
        for (condition in conditions){
            templist = list()
            # # this count used to create a list of models/dataset
            count = 1
            # # iterate through each dataset and run model
            for (dataset in datalist){
                # # set up  clmm regression
                tempcom = paste0('model <- clmm(as.factor(',dv,') ~ as.factor(',condition,')',ivlist,',data = dataset)')
                # # run it
                eval(parse(text=tempcom))
                # # save it, print model summary
                eval(parse(text=paste0("templist[[",count,"]] = model")))
                count = count + 1
                }   
        # # # # create pooled coefficient table
            modelrownames = rownames(coefficients(summary(templist[[1]])))
            coeflist = list()
            # # HACK TO MAKE LIST WORK - NEED BETTER SOLUTION FOR ADDING TO EMPTY LIST
            coeflist[[1]] = 0
            # # make a list of coefficients from all models
            for (model in templist){
                coeflist[[length(coeflist)+1]] = coefficients(summary(model))           
                }
            coeflist = coeflist[2:length(coeflist)]
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
            eval(parse(text=paste0(dv, "_regvals <<- tempvals")))
            # # create regression results table for target variables
            print(dv)
            print(condition)
            coefsandstderror = mi.pooled(coefest, stderrors)
            coef.est = coefsandstderror$coefficients; std.error = coefsandstderror$se
            zvalues = coef.est / std.error; pvalues = 2 * (1- pnorm(abs(zvalues)))
            pooltable = data.frame(cbind(coef.est,std.error,zvalues,pvalues))
            eval(parse(text=paste0(dv, "_sum <<- pooltable")))
            print(pooltable)
            cat(rep("\n",2))
            }       
        cat(rep("\n",4))
        }
    }