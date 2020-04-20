

###TEAM :XUAN DONG, HUXUAN MING, LINDSAY NEUFER , Manalo Iscel,D

rm(list = ls())
getwd()
data <- read.csv(file="Final_Data_vf_2.csv", header=TRUE, sep=",")
table(data$Krannert.Funding)


data$Teamwork.Leadership
#####data preproessing 
#******************************************************************************
#Clean-up "Degree"
#******************************************************************************
sapply(data, function(x) sum(is.na(x)))
data$Krannert.Funding<-as.character(data$Krannert.Funding)
data$Krannert.Funding

data$Krannert.Funding[data$Krannert.Funding=='No Funding']<-0
data$Krannert.Funding[data$Krannert.Funding=='Cash only']<-1

data$Krannert.Funding[data$Krannert.Funding==""]<-NA
data$Krannert.Funding
data<-droplevels(data[-which(data$Status..Y.=="Defer to next year"),])

data$Krannert.Funding<-as.factor(data$Krannert.Funding)

str(data)



table(data$Krannert.Funding)
str(data$Krannert.Funding)

data$Degree[data$Degree == "Master of Science"] <- "MS"
data$Degree[data$Degree == "Bachelor of Arts"] <- "BA"
data$Degree[data$Degree == "Bachelor of Commerce"] <- "B"
data$Degree[data$Degree == "Bachelor of Science"] <- "BS"

#clean-up
data$Degree<-droplevels(data$Degree)



table(data$Degree)




#Look into "Bachelors Degree" observations
data$Major[data$Degree == "Bachelors Degree"]

#Manage exceoptions (Bachelors Degree: Telecomuncations and Construction Mgmt.)
data$Degree[data$Major == "Telecommunication"] <- "MS"
data$Degree[data$Major == "Construction Management"] <- "B"

#final clean-up
data$Degree<-droplevels(data$Degree)
table(data$Degree)

#******************************************************************************
#Clean-up "LiveIn"
#******************************************************************************

table(data$LiveIn)

data$LiveIn[data$LiveIn == "United States of America"] <- "USA"
data$LiveIn[data$LiveIn == "Taiwan, Republic of China"] <- "Taiwan"

#clean-up
data$LiveIn<-droplevels(data$LiveIn)
table(data$LiveIn)

#******************************************************************************
#Clean-up "Citizenship"
#******************************************************************************

table(data$Citizenship)

data$Citizenship[data$Citizenship == "United States of America"] <- "USA"
data$Citizenship[data$Citizenship == "Taiwan, Republic of China"] <- "Taiwan"
table(data$Citizenship)
#clean-up
data$Citizenship<-droplevels(data$Citizenship)
table(data$Citizenship)

#******************************************************************************
#Clean-up "Ethnicity"
#******************************************************************************
table(data$Ethnicity)

table(data$Citizenship)
#data$Ethnicity[data$Ethnicity== "Asian"] <- "International"
data$Ethnicity[data$Ethnicity == "Two or more"] <- "International"

#Clean-up
data$Ethnicity<-droplevels(data$Ethnicity)
table(data$Ethnicity)



#####drop insuffcient col from data set(state, city,provience, zipcode,country)
data$Major<-NULL
data$City<-NULL
data$Province<-NULL
data$ZipCode<-NULL
data$Country<-NULL
data$State<-NULL

###Impute NA using other column value(interview decision,program.fit, teamleadership)
# Calculate Program Fit Measure (average of measures)
data$Program.Fit[is.na(data$Program.Fit)]<-rowMeans(data[is.na(data$Program.Fit),
                                                         c("Actively.seeking.GA",
                                                           "Would.they.be.good.GA",
                                                           "English.Proficiency",
                                                           "Quality.of.work.experience",
                                                           "Will.leave.good.impression.with.recruiters",
                                                           "Employment.goal.reasonable",
                                                           "Desire.to.attend",
                                                           "Program.align.with.career.goals",
                                                           "Would.I.hire.him.her",
                                                           "Comfort.with.Interview",
                                                           "Asked.appropriate.question",
                                                           "Prepare.for.interview"
                                                         )]
                                                    ,na.rm = T)
data$Program.Fit<-round(data$Program.Fit)
data$Program.Fit

#######replace Nan to 0 
is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))

data[is.nan(data)] <- 0






names(data)
data$InterviewDecision[is.na(data$InterviewDecision)]<-rowMeans(data[is.na(data$InterviewDecision),
                                                          c("Profesional.Appearance","Well.thought.out.ideas.avoided.fillers",
                                                            "Prepare.for.interview","Asked.appropriate.question","Comfort.with.Interview",
                                                            "Exhibits.honesty.confidence.Integrity","Maturity..Self.awareness"
                                                         
                                                            
                                                            )]
                                                          
                                                    ,na.rm = T)
data$Program.Fit<-round(data$Program.Fit)

names(data)
data$Want.to.be.teammate.with[data$Want.to.be.teammate.with=="\\"]<-NA
data$Want.to.be.teammate.with<-as.integer(data$Want.to.be.teammate.with)

#Clean-up leadership potential (/) inserted as value
data$Leadership.potential[data$Leadership.potential=="\\"]<-NA
data$Leadership.potential<-as.integer(data$Leadership.potential)

data$Teamwork.Leadership[is.na(data$Teamwork.Leadership)]<-rowMeans(data[is.na(data$Teamwork.Leadership),
                                                                     c(
                                                                       "Want.to.be.teammate.with","Leadership.potential",
                                                                       "Contribute.to.group.discussion","Basic.conversion.skills"
                                                                     
                                                                       
                                                                     )]
                                                                
                                                                ,na.rm = T)
data$Program.Fit<-round(data$Program.Fit)



###remove combined columns 


data[,c("Actively.seeking.GA",
  "Would.they.be.good.GA",
  "English.Proficiency",
  "Quality.of.work.experience",
  "Will.leave.good.impression.with.recruiters",
  "Employment.goal.reasonable",
  "Desire.to.attend",
  "Program.align.with.career.goals",
  "Would.I.hire.him.her",
  "Comfort.with.Interview",
  "Asked.appropriate.question",
  "Prepare.for.interview"
)]<-NULL

data[,c("Profesional.Appearance","Well.thought.out.ideas.avoided.fillers",

           "Exhibits.honesty.confidence.Integrity","Maturity..Self.awareness")]<-NULL


data[,c("Want.to.be.teammate.with","Leadership.potential",
        "Contribute.to.group.discussion","Basic.conversion.skills","Maturity..Self.awareness","ID")]<-NULL
class(data$Interview.Recommendation)
   
#######interview recomm clean

data$Interview.Recommendation<-as.character(data$Interview.Recommendation)

data$Interview.Recommendation[data$Interview.Recommendation == "Admit 3.5" ] <- "Admit"
data$Interview.Recommendation[data$Interview.Recommendation == "Admit3.5" ] <- "Admit"
data$Interview.Recommendation [data$Interview.Recommendation=="Waitlist(3.5)"] <- "Waitlist"
data$Interview.Recommendation<-as.factor(data$Interview.Recommendation)
###converting categorical val 

str(data)
data$Engineering <- as.factor(data$Engineering)  
data$CS_CE <- as.factor(data$CS_CE)

data$Business <-as.factor(data$Business)
data$WorldUniversityRanking <- as.factor(data$WorldUniversityRanking)
data$BRICSRanking <- as.factor(data$BRICSRanking)
data$UStop100 <- as.factor(data$UStop100)
data$AsiaRanked <-as.factor(data$AsiaRanked)
data$EmpConsulting <- as.factor(data$EmpConsulting)  
data$EmpAnalytics <-as.factor(data$EmpAnalytics)
data$EmpFinServices <-as.factor(data$EmpFinServices)
data$EmpEducation <- as.factor(data$EmpEducation)
data$EmpPrimarilyOverseas <- as.factor(data$EmpPrimarilyOverseas)
data$EmpUSAbased <- as.factor(data$EmpUSAbased)
data$EmpWorldwide <- as.factor(data$EmpWorldwide)
data$Will.get.stay.involved.as.student.alumnus<-as.factor(data$Will.get.stay.involved.as.student.alumnus)
data$Essay.Responses<-as.factor(data$Essay.Responses)
data$Kira.Responses<-as.factor(data$Kira.Responses)
data$Program.Fit<-as.factor(data$Program.Fit)
data$InterviewDecision<-as.factor(data$InterviewDecision)
data$Teamwork.Leadership<-as.factor(data$Teamwork.Leadership)
data$Stats<-as.factor(data$Stats)

class(data)
#### replace all 0s with NA for score column
data$GMAT[data$GMAT==0]<-NA
data$Quant[data$Quant==0]<-NA
data$Verbal[data$Verbal==0]<-NA
data$IBT_W[data$IBT_W==0]<-NA
data$IBT_L[data$IBT_L==0]<-NA
data$IBT_R[data$IBT_R==0]<-NA
data$IBT_S[data$IBT_S==0]<-NA
data$IBT_Total[data$IBT_Total==0]<-NA
data$GPA[data$GPA==0]<-NA
DataQualityReport(data)
data$GradYear<-NULL
data$Instit_Codes<-NULL
data$Class<-NULL

data$Quant<-NULL
data$Verbal<-NULL

data$Will.get.stay.involved.as.student.alumnus<-NULL
#### impute missing value 
#install.packages('mice')
library(mice)
?mice
names(data)
OG<-data
imputedValues <- mice(data=data # just using some features
                      , seed=1234    # keep to replicate results
                      , method="cart" # model you want to use
                      , m=1           # Number of multiple imputations
                      , maxit = 1     # number of iterations
)

Imp <- complete(imputedValues,1)

data$GMAT<-Imp$GMAT

data$GPA<-Imp$GPA
data$IBT_W<-Imp$IBT_W
data$IBT_L<-Imp$IBT_L
data$IBT_R<-Imp$IBT_R
data$IBT_S<-Imp$IBT_S
data$IBT_Total<-Imp$IBT_Total

data$MW_Pre<-Imp$MW_Pre
data$MW_Post<-Imp$MW_Post
data$EmpConsulting<-Imp$EmpConsulting
data$EmpPrimarilyOverseas<-Imp$EmpPrimarilyOverseasin
data$EmpWorldwide<-Imp$EmpWorldwide
data$EmpUSAbased<-Imp$EmpUSAbased
data$EmpAnalytics<-Imp$EmpAnalytics
data$EmpFinServices<-Imp$EmpFinServices
data$EmpEducation<-Imp$EmpEducation
data$InterviewDecision<-Imp$InterviewDecision

data$Essay.Responses<-Imp$Essay.Responses
data$Kira.Responses<-Imp$Kira.Responses
data$Program.Fit<-Imp$Program.Fit
data$Krannert.Funding<-Imp$Krannert.Funding


DataQualityReport(data)

DataQualityReportOverall(data)





########EDA for degree, 
#install.packages('ggpubr')
library(ggplot2)

library(ggpubr)

dev.off()

p1<-ggplot(data, aes(factor(Degree)),main='Degree') + geom_bar(fill='black')
p2<-ggplot(data, aes(factor(Engineering)),main='Engineering') + geom_bar(fill='yellow')
p3<-ggplot(data, aes(factor(Stats)),main='stats') + geom_bar(fill='blue')
p4<-ggplot(data, aes(factor(Business)),main='Business') + geom_bar(fill='darksalmon')

p5<-ggplot(data, aes(factor(CS_CE)),main='CS_CE') + geom_bar(fill='pink')
p6<-ggplot(data, aes(factor(Experience_months)),main='Experience_months') + geom_bar(fill='orange')
p7<-ggplot(data, aes(factor(EmpConsulting)),main='EmpConsulting') + geom_bar(fill='blue')
p8<-ggplot(data, aes(factor(EmpWorldwide)),main='EmpWorldwide') + geom_bar(fill='lightgreen')
p9<-ggplot(data, aes(factor(EmpAnalytics)),main='EmpAnalytics') + geom_bar(fill='green')
p10<-ggplot(data, aes(factor(EmpFinServices)),main='EmpFinServices') + geom_bar(fill='lightblue')
p11<-ggplot(data, aes(factor(EmpEducation)),main='EmpEducation') + geom_bar(fill='goldenrod1')


figure <- ggarrange(p1, p2, p3,p4,p5,p6,p7,p8,p9,p10,p11,
                    labels = c("Degree",'Engineering' ,'stats','Business','CS_CE', 'Experience_months',
                               'EmpConsulting', 'EmpWorldwide','EmpAnalytics','EmpFinServices',
                               'EmpEducation'),
                    ncol = 3 ,nrow = 4)
figure 
### check the dataset again to see if there is any missing value 
data$Essay.Responses
sapply(data, function(x) sum(is.na(x)))




### drop more columns 



table(data$Krannert.Funding)
###create dummies for the categorical dataset 
####the one using one hot coding (Ethnicity)
colnames(data)[colnames(data) == 'Status..Y.'] <- 'y'

class(data$InterviewDecision)

data[is.nan(data)] <- 0

DataQualityReportOverall(data)










####the one using frequency encoding(GradYear,livin, citizenship)
freqcode<-data[ ,c('LiveIn','Citizenship')]
#install.packages('cattonum')
library(cattonum)
freqcode<-as.data.frame(freqcode)

freqcode<-catto_freq(freqcode,1:2,verbose=TURE)

data$Essay.Responses
freqcode

data<-cbind(data,freqcode)
#### the one using labeling coding (interviewdecision, essay response, Kira response, program.fit, teamwork.leadership)
#install.packages('CatEncoders')


################################################################################
## drop OG var related to freq and label encoding 
################################################################################

data[c('LiveIn','Citizenship')]<-NULL
data$Interview.Recommendation<-as.factor(data$Interview.Recommendation)
table(data$Interview.Recommendation)
################################################################################
## Creating Dummy Variables one hot coding 
################################################################################



#install.packages("caret")
library(caret)
dummies <- dummyVars(y ~ ., data = data)            # create dummyes for Xs
ex <- data.frame(predict(dummies, newdata = data))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
data <- cbind(data$y, ex)                              # combine your target variable with Xs
names(data)[1] <- "y"                               # make target variable called 'Y'
rm(dummies, ex)                                  # delete temporary things we no longer need
str(data)



#######################################################################################
#check missing value 
#######################################################################################

DataQualityReportOverall(data) # percent complete data 
DataQualityReport(data)        # data quality by feature



data[is.nan(data)] <- 0

#########################################################################################
#subset data into two group ( with protected info and without protected info )
#protected info" age gender ethnicity, 

#########################################################################################
datap<-data[,-c(3:8)]

#########################################################################################
#########cleaning parttwo zeroco, muticolinearity standardization for dataset without protected info

#########################################################################################

# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(data[,2:ncol(data)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .80) # num Xs with cor > t
summary(descrCor[upper.tri(descrCor)])                    # summarize the cors

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff. Find them and remove them
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.80)
filteredDescr <- data[,2:ncol(data)][,-highlyCorDescr] # remove those specific columns
descrCor2 <- cor(filteredDescr)                  # calculate a new cor matrix
# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])

# update dataset by removing those filtered vars that were highly correlated
data<- cbind(data$y, filteredDescr)
names(data)[1] <- "y"

rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)  # clean up



################################################################################
# Identifying linear dependencies and remove them
################################################################################
set.seed(1234) # set a seed so you can replicate your results
# Find if any linear combinations exist and which column combos they are.
# Below I add a vector of 1s at the beginning of the dataset. This helps ensure
# the same features are identified and removed.
library(caret)
# first save response
y <- data$y
# create a column of 1s. This will help identify all the right linear combos
data <- cbind(rep(1, nrow(data)), data[2:ncol(data)])
names(data)[1] <- "ones"
# identify the columns that are linear combos
comboInfo <- findLinearCombos(data)
comboInfo
# remove columns identified that led to linear combos
data <- data[, -comboInfo$remove]
# remove the "ones" column in the first column
data <- data[, c(2:ncol(data))]
rm(comboInfo)

# Add the target variable back to our data.frame
data <- cbind(y, data)

nrow(y)

################################################################################
## Zero- and Near Zero-Variance Predictors
################################################################################
# In some situations, the data generating mechanism can create predictors that 
# only have a single unique value (i.e. a "zero-variance predictor"). For many 
# models (excluding tree-based models), this may cause the model to crash or the 
# fit to be unstable.
# Similarly, predictors might have only a handful of unique values that occur 
# with very low frequencies. 

# identify columns that are "near zero"
nzv <- nearZeroVar(data[,2:ncol(data)])
# remove those columns from your dataset
d_filtered <- data[,2:ncol(data)][, -nzv]
# dimension of your filtered dataset
dim(d_filtered)
# updated dataset 
data <- cbind(data$y, d_filtered)
names(data)[1] <- "y"
# remove filtered dataset
rm(d_filtered)


#******************************************************************************
# Z-score standardization & YeoJohnson
#******************************************************************************
#names(d)
#create a dataset that keeps unstandarized variables
d_uns <- data[,c(2:ncol(data), 1)] 

# Keep dummy variables aside so that those are not standarized
# dCats => contains the 0/1 variable, dNums => contains numeric features 
numcols <- apply(X=data, MARGIN=2, function(c) sum(c==0 | c==1)) != nrow(data)
catcols <- apply(X=data, MARGIN=2, function(c) sum(c==0 | c==1)) == nrow(data)
dNums <- data[,numcols]
dCats <- data[,catcols]

# Z-score: This will make all the numeric features centered at 0 and have a standard
# deviation of 1. method = c("center", "scale")

#YeoJohnson: makes the distribution of features more bell-shaped c("YeoJohnson)

# Identify the means, standard deviations, other parameters, etc. for transformation
preProcValues <- preProcess(dNums[,2:ncol(dNums)], method = c("center","scale", "YeoJohnson"))

# #save preprocess values for shinny app development
# saveRDS(preProcValues, file = 'preprocessvalues_shinny_app.rds')

# Transforma variables using the predict() function
dNums <- predict(preProcValues, dNums)

# combine the standardized numeric features with the dummy vars
data <- cbind(dNums, dCats)

#Clean-up the environment
rm(preProcValues, numcols, catcols, dNums, dCats)  # clean up






#########################################################################################
#########cleaning parttwo zeroco, muticolinearity standardization for dataset with protected info

#########################################################################################

# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(datap[,2:ncol(datap)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .80) # num Xs with cor > t
summary(descrCor[upper.tri(descrCor)])                    # summarize the cors

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff. Find them and remove them
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.80)
filteredDescr <- datap[,2:ncol(datap)][,-highlyCorDescr] # remove those specific columns
descrCor2 <- cor(filteredDescr)                  # calculate a new cor matrix
# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])

# update dataset by removing those filtered vars that were highly correlated
datap<- cbind(datap$y, filteredDescr)
names(datap)[1] <- "y"

rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)  # clean up



################################################################################
# Identifying linear dependencies and remove them
################################################################################
set.seed(1234) # set a seed so you can replicate your results
# Find if any linear combinations exist and which column combos they are.
# Below I add a vector of 1s at the beginning of the dataset. This helps ensure
# the same features are identified and removed.
library(caret)
# first save response
y <- datap$y
# create a column of 1s. This will help identify all the right linear combos
datap <- cbind(rep(1, nrow(datap)), datap[2:ncol(datap)])
names(datap)[1] <- "ones"
# identify the columns that are linear combos
comboInfo <- findLinearCombos(datap)
comboInfo
# remove columns identified that led to linear combos
datap <- datap[, -comboInfo$remove]
# remove the "ones" column in the first column
datap <- datap[, c(2:ncol(datap))]
rm(comboInfo)

# Add the target variable back to our data.frame
datap <- cbind(y, datap)



################################################################################
## Zero- and Near Zero-Variance Predictors
################################################################################
# In some situations, the data generating mechanism can create predictors that 
# only have a single unique value (i.e. a "zero-variance predictor"). For many 
# models (excluding tree-based models), this may cause the model to crash or the 
# fit to be unstable.
# Similarly, predictors might have only a handful of unique values that occur 
# with very low frequencies. 

# identify columns that are "near zero"
nzv <- nearZeroVar(datap[,2:ncol(datap)])
# remove those columns from your dataset
d_filtered <- datap[,2:ncol(datap)][, -nzv]
# dimension of your filtered dataset
dim(d_filtered)
# updated dataset 
datap <- cbind(datap$y, d_filtered)
names(datap)[1] <- "y"
# remove filtered dataset
rm(d_filtered)


#******************************************************************************
# Z-score standardization & YeoJohnson
#******************************************************************************
#names(d)
#create a dataset that keeps unstandarized variables
d_uns <- datap[,c(2:ncol(datap), 1)] 

# Keep dummy variables aside so that those are not standarized
# dCats => contains the 0/1 variable, dNums => contains numeric features 
numcols <- apply(X=datap, MARGIN=2, function(c) sum(c==0 | c==1)) != nrow(datap)
catcols <- apply(X=datap, MARGIN=2, function(c) sum(c==0 | c==1)) == nrow(datap)
dNums <- datap[,numcols]
dCats <- datap[,catcols]

# Z-score: This will make all the numeric features centered at 0 and have a standard
# deviation of 1. method = c("center", "scale")

#YeoJohnson: makes the distribution of features more bell-shaped c("YeoJohnson)

# Identify the means, standard deviations, other parameters, etc. for transformation
preProcValues <- preProcess(dNums[,2:ncol(dNums)], method = c("center","scale", "YeoJohnson"))

# #save preprocess values for shinny app development
# saveRDS(preProcValues, file = 'preprocessvalues_shinny_app.rds')

# Transforma variables using the predict() function
dNums <- predict(preProcValues, dNums)

# combine the standardized numeric features with the dummy vars
datap <- cbind(dNums, dCats)

#Clean-up the environment
rm(preProcValues, numcols, catcols, dNums, dCats)  # clean up

####relevel response var for protected dataset

data$y<-as.factor(ifelse(data$y=='Confirmed',1,0))
levels(data$y)<-make.names(levels(factor(data$y)))
levels(data$y)
data$y<-relevel(data$y,'X1')








####relevel response var for dataset without protected dataset

datap$y<-as.factor(ifelse(datap$y=='Confirmed',1,0))
levels(datap$y)<-make.names(levels(factor(datap$y)))
levels(datap$y)
datap$y<-relevel(datap$y,'X1')






#******************************************************************************
#                                 EDA
#******************************************************************************

#******************************************************************************
#                              y is unbalanced 
#******************************************************************************
y<-as.numeric(ifelse(data$y=='X1',1,0))

hist(y,main='reponse variable',col='blue',margin=FALSE)



#******************************************************************************
#                          indenpendet var  test score
#******************************************************************************
par(mfrow=c(3,4))


hist(d_uns$GMAT,main='GMAT',col='yellow',margin=FALSE)
hist(d_uns$Quant,main='Quant',col='yellow',margin=FALSE)
hist(d_uns$Verbal,main='Verbal',col='yellow',margin=FALSE)
hist(d_uns$IBT_W,main='IBTW',col='yellow',margin=FALSE)
hist(d_uns$IBT_L,main='IBTL',col='yellow',margin=FALSE)
hist(d_uns$IBT_R,main='IBTR',col='yellow',margin=FALSE)
hist(d_uns$IBT_S,main='IBTS',col='yellow',margin=FALSE)
hist(d_uns$IBT_Total,main='IBTtoal',col='yellow',margin=FALSE)



#################################################################
#check the distribution of var before and after imputation  
#################################################################

par(mfrow=c(1,2))


hist(d_uns$GMAT,col='pink',main='OG distribution')
hist(data$GMAT,col='gold',main='model-based')

#################################################################
#Model building part one for dataset with everything 
#################################################################

#################################################################
# data partition 
#################################################################
inTrain <- createDataPartition(data$y
                               , p=.85
                               , list=F)
trainW <-data[inTrain,]
testW<- data[-inTrain,]

#################################################################
# upsample becasue OG is unbalanced and we have limited number of data
#################################################################



#install.packages('DMwR')
library(DMwR)

upTrainW <- upSample(x=trainW[,2:ncol(trainW)], y = trainW$y, yname= "y")
# 
# 
# 
upTrainW <- upTrainW[,c(ncol(upTrainW),1:ncol(upTrainW)-1)]



library(DMwR)


upTrainW <-SMOTE(y~.,data =trainW, perc.over = 100,k=3) 

afterup<-ggplot(upTrainW, aes(factor(y)),main='y') + geom_bar(fill='blue')
dev.off()
afterup
seq(0,1,by=0.1)
##lasso and ridge ###

set.seed(1234)
#install.packages('e1071')
ctrl <- trainControl('LOOCV',classProbs = T,summaryFunction =twoClassSummary)
tuneGrid=expand.grid(.alpha=seq(0,1,by=0.1),.lambda=seq(0,30,by=0.1))
lmFit <- train( y~.,
               data = upTrainW,
               method = "glmnet",
               trControl = ctrl,
               family='binomial',
               metric = "AUC",
               grid=tuneGrid)


rm1_trp <- predict(lmFit, newdata=upTrainW, se.fit= FALSE,type='prob')[,1]
rm1_trc <- predict(lmFit, newdata=upTrainW)
rm1_tep <- predict(lmFit, newdata=testW, type='prob')[,1]
rm1_tec <- predict(lmFit, newdata=testW)


cmrf <- confusionMatrix(data=rm1_trc, upTrainW$y)
testcmrf <- confusionMatrix(data=rm1_tec, testW$y)
cmrf
testcmrf
varImp(lmFit)

dev.off()
plot(varImp(lmFit,scale=F))


pROC1 <- roc(testW$y,rm1_tep)
pROC1

dev.off()
plot(pROC1)

coef(lmFit$finalModel, lmFit$finalModel$lambdaOpt)
###### dataset without sensitive information 

########spliting data 
#################################################################
inTrain <- createDataPartition(datap$y
                               , p=.85
                               , list=F)
trainWo <-datap[inTrain,]
testWo<- datap[-inTrain,]


##################upsample unbalanced data 



#install.packages('DMwR')
library(DMwR)
upTrainWo <-SMOTE(y~.,data = trainWo, perc.over = 100,k=3) 

afterup<-ggplot(upTrainWo, aes(factor(y)),main='y') + geom_bar(fill='blue')
afterup



#################glmnet################## 



set.seed(1234)
#install.packages('e1071')
ctrl <- trainControl('LOOCV',classProbs = T,summaryFunction =twoClassSummary)
tuneGrid=expand.grid(.alpha=c(0.3,0.5,0.8),.lambda=seq(0,30,by=0.1))
lmFitwo <- train( y~.,
                data = upTrainWo,
                method = "glmnet",
                trControl = ctrl,
                family='binomial',
                metric = "AUC",
                grid=tuneGrid)


rm1_trpwo <- predict(lmFitwo, newdata=upTrainWo, se.fit= FALSE,type='prob')[,1]
rm1_trcwo <- predict(lmFitwo, newdata=upTrainWo)
rm1_tepwo <- predict(lmFitwo, newdata=testWo, type='prob')[,1]
rm1_tecwo <- predict(lmFitwo, newdata=testWo)


cmrfwo <- confusionMatrix(data=rm1_trcwo, upTrainWo$y)
testcmrfwo <- confusionMatrix(data=rm1_tecwo, testWo$y)
cmrfwo
testcmrfwo

varImp(lmFitwo)

dev.off()
plot(varImp(lmFitwo,scale=F))

#install.packages('pROC')
library(pROC)

pROC0 <- roc(testWo$y,rm1_tepwo)
pROC0
plot(pROC0)


coef(lmFitwo$finalModel, lmFitwo$finalModel$lambdaOpt)


























