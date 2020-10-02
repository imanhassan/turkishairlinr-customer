library(readxl)
DATA <- read_excel("DATA.xlsx")
attach(DATA)

library(ggplot2)
library(tidyverse)

#Distribuation

  #Gender
qplot(Gender, fill=BR1 )
qplot(Gender, fill=Route )
qplot(Gender, fill=FlyingClass)
qplot(Gender, fill=`Age Range`)

  #Route
qplot(Route, fill=Gender)
qplot(Route, fill=FlyingClass)
qplot(Route, fill=`Age Range`)

  #Age Range   
qplot(`Age Range`, fill=Gender)
qplot(`Age Range`, fill=BR1)
qplot(`Age Range`, fill=FlyingClass)
qplot(`Age Range`, fill=Route)+ scale_fill_manual(values=c( "#cd9575", "#915c83"))
qplot(BR1,fill=FlyingClass)
  #Flying Class
qplot(FlyingClass, fill=Gender)+ scale_fill_manual(values=c( "#87a96b", "#ffbf00")) #IMP
qplot(FlyingClass, fill=`Age Range`)
qplot(FlyingClass, fill=BR1)

  #Frist reason to choose Turkish airline
FirstReasonsforChose <- DATA$`TS[{First}]. Single Response Question`
qplot(FirstReasonsforChose, fill= Gender)+theme(axis.text.x = element_text(angle = 60,hjust = 1))+ scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
qplot(FirstReasonsforChose, fill= FlyingClass)+theme(axis.text.x = element_text(angle = 60,hjust = 1))+ scale_fill_manual(values=c( "#ffbf00", "#e32636")) #IMP

# is there is difference in mean values of first reason to choose TA in term of GM1?

Gm1Fst<- aov(GM1~FirstReasonsforChose)
summary(Gm1Fst)

Gm1Fsts<- glm(GM1~FirstReasonsforChose)
summary(Gm1Fsts)
FirstReasonsforChose[FirstReasonsforChose=="NoOther" ]=NA;FirstReasonsforChose

Gm1Fst<- aov(GM1~FirstReasonsforChose)
summary(Gm1Fst)
Gm1Fsts<- glm(GM1~FirstReasonsforChose)
summary(Gm1Fsts)
GM1FstReas<- t.test(GM1~Reason)
GM1FstReas

boxplot(GM1~FirstReasonsforChose)+theme(axis.text.x = element_text(angle = 60,hjust = 1))
qplot(FirstReasonsforChose)+theme(axis.text.x = element_text(angle = 60,hjust = 1))

attach(DATA)
DATA$Reason<-ifelse(DATA$`TS[{First}]. Single Response Question`=='Flightsafety'|DATA$`TS[{First}]. Single Response Question`=='Directflight',1,0)
DATA$Reason<-ifelse(is.na(DATA$Reason),0,DATA$Reason)



# Is there is a significant difference among the satisfaction level in term of different reservation ways?
Reservationtype1<-aov(GM1~BR1)
summary(Reservationtype1)

# Are international passangers more satisfied than domestics?
attach(DATA)
factor(Route)

routeGm1<- t.test(GM1~Route, var.equal=TRUE)
routeGm1

boxplot(GM1~Route)

# Are Females more satisfied than males?
attach(DATA)
factor(Gender)
GendarGm1<- lm(GM1~Gender)
summary(GendarGm1)
DATA$Male<-ifelse(Gender=="M",1,0)
gend<- lm(GM1~Male)
summary(gend)

# Are level of education diff in satisfaction level ?
factor(Dem1)
Dem1Gm1<- aov(GM1~Dem1)
summary.aov(Dem1Gm1)
boxplot(GM1~Dem1)

# Are Residence country diff in satisfaction level ?
factor(`Residence Country`)
ResCouGm1<- aov(GM1~`Residence Country`)
summary.aov(ResCouGm1) 
ResCouLGm1<- glm(GM1~`Residence Country`)
summary(ResCouLGm1)
boxplot(GM1~`Residence Country`)


# Is there is a significant difference among Age range in term of General sarisfaction?
attach(DATA)
AgeRangeRe<- glm(GM1~`Age Range`)
summary(AgeRangeRe)
boxplot(GM1~`Age Range`, col=rainbow(7))

# Is there is a significant difference among irregualities in term of General sarisfaction?
attach(DATA)
install.packages("tidyverse")
library(tidyverse)
factor(`EH1x (1/7)`)
irregulaityGM<- aov(GM1~ `EH1x (1/7)`)
summary(irregulaityGM)
boxplot(GM1~ `EH1x (1/7)`)


irregulaAffect<- glm(GM1~`EH1x (1/7)`)
summary(irregulaAffect)

EH1log<-glm(formula = GM1 ~ `EH1x (1/7)`)
DATA$issue<- ifelse(`EH1x (1/7)`=='Delay'|`EH1x (1/7)`=='MissingBaggage'|`EH1x (1/7)`=='OtherPleaseState',1,0)
DATA$issue<-ifelse(is.na(DATA$issue),0,DATA$issue)

summary(aov(GM1~issue))
summary(glm(GM1~issue))

boxplot(GM1~issue)

# Final Model (How Pre-flight experiance , on-board, post_flight affect the overall experiance?)
attach(DATA)
set.seed(1234)
ind <- sample(2, nrow(DATA), replace = TRUE, prob = c(0.7, 0.3))
train.data <- DATA[ind == 1, ]
test.data <- DATA[ind == 2, ]

overaalregressionN<-glm(GM1~UO1+UD1+issue+US1, data = train.data)

overaalregressionN<-lm(GM1~UO1+UD1+UD2+UD10+issue, data = train.data)
summary(overaalregressionN)
factor(UO1)
factor(UD1)
factor(BR1)

# correlation

library(corrplot)
factor(`I1 SO1 C1`)

cor(GM1,UD1)
cor(GM1,US1)
cor(GM1,`I1 SO1 C1`)
cor(GM1,UD10)
cor(GM1,UO1)
cor(GM1,UD2)
cor(GM1,Reason)
cor(GM1,issue)



  #Logistic Regression
overaalregression<-glm(GM1~UO1+UD1+issue+UD2+UD10, data = train.data, family= 'ordinal')
summary(overaalregression)
anova(overaalregression)
plot(overaalregression)
predLR<-predict(overaalregression,test.data)
str(predLR)
library(gmodels)
CrossTable(test.data$GM1, predLR)
tabLR <- table(Predicted =predLR, Actual = test.data$GM1)
1-sum(diag(tabLR))/sum(tabLR)


  
#MULTICOURINULTY TEST
library(car)
vif(overaalregression)

  # NN
attach(DATA)
install.packages("neuralnet")
require(neuralnet)
library(MASS)
library(grid)
library(neuralnet)
library(nnet)

nn <- nnet(GM1~UO1+UD1+issue+UD2+UD10+Reason,data = train.data, size=5)
predNN<- predict(nn, test.data)
nn$weights
table(predict(nn,train.data),train.data$GM1)
table(predNN,test.data$GM1)
testPred <- predict(nn,test.data)
NN<-table(testPred,test.data$GM1)
mean(predNN == test.data$GM1)
plot(nn)
1-sum(diag(NN))/sum(NN)

  #Decision Tree
library(sandwich)
library(party)
library(zoo)
attach(DATA)
PreFlightExp<- DATA$UO1
OnboardExp<- UD1
Cabin<- UD2
InflightCatering<- UD10

myFormula <- GM1~PreFlightExp+OnboardExp+issue+Cabin+InflightCatering+Reason
Satisf_ctree <- ctree(myFormula, data = train.data)
table(predict(Satisf_ctree), train.data$GM1)

print(Satisf_ctree)
plot(Satisf_ctree)
plot(Satisf_ctree, type = "simple")
testPredDT <- predict(Satisf_ctree, newdata = test.data)
DT<-table(testPredDT, test.data$GM1)
mean(testPred == test.data$GM1)
1-sum(diag(DT))/sum(DT)

    #UO1 PRE-FLIGHT EXPERIENCES

# which one of pre-flight experiance affect the most the (pre-flight satis + overall satis)?
PreflightW<- aov(UO1~UO3+UO2+UO6+UO8+UO9f)
summary(PreflightW)

boxplot(UO1~UO3)

PreflightWo<- glm(UO1~UO3+UO2+UO6+UO8+UO9f)
summary(PreflightWo)

    #UO3 Check-in

# what is the most important variable affecting the Check-in - Counter & CIP lounge (Uo3)
PreFlightUo3CC<- aov(UO3~`UO4 [{UO4b}]. Single Response Question`+`UO4 [{UO4c}]. Single Response Question`+`UO4 [{UO4d}]. Single Response Question`+`UO4 [{UO4e}]. Single Response Question`+`UO4 [{UO4f}]. Single Response Question`+`UO4 [{UO4g}]. Single Response Question`)
summary(PreFlightUo3CC)
#b,c,g


# what is the most important variable affecting the Check-in - All & have handbage (Uo3)
PreFlightUo3All<- aov(UO3~`UO4 [{UO4a}]. Single Response Question`+`UO4 [{UO4h}]. Single Response Question`+`UO4 [{UO4i}]. Single Response Question`)
summary(PreFlightUo3All)
#a,h,i


    #UO3 Check-in (H) All & have handbage - Check-in service time


# is there is a difference in satisfaction level for each Gender in term of Staff welcome
PreFlightUo4HGender<- t.test(`UO4 [{UO4h}]. Single Response Question`~Gender)
PreFlightUo4HGender
boxplot(`UO4 [{UO4h}]. Single Response Question`~Gender)
#no

# is there is a difference in satisfaction level for each Flying type in term of Staff welcome
PreFlightUo4hFlyingC<- t.test(`UO4 [{UO4h}]. Single Response Question`~FlyingClass)
PreFlightUo4hFlyingC
boxplot(`UO4 [{UO4h}]. Single Response Question`~FlyingClass)
#no

# is there is a difference in satisfaction level for each plane type in term of Staff welcome
PreFlightUo4hPlane<- t.test(`UO4 [{UO4h}]. Single Response Question`~PlaneType)
PreFlightUo4hPlane
boxplot(`UO4 [{UO4h}]. Single Response Question`~PlaneType)
#no

# is there is a difference in satisfaction level for each Route in term of Staff welcome
PreFlightUo4HRoute<- t.test(`UO4 [{UO4h}]. Single Response Question`~Route)
PreFlightUo4HRoute
boxplot(`UO4 [{UO4h}]. Single Response Question`~Route)
#NO

# is there is a difference in satisfaction level for each Age range in term of Staff welcome
onboardU11HAgeRange<- aov(`UO4 [{UO4h}]. Single Response Question`~`Age Range`)
summary(onboardU11HAgeRange)
boxplot(`UO4 [{UO4b}]. Single Response Question`~`Age Range`)
#no


  #UO3 Check-in (a) All & have handbage - Queue Time


# is there is a difference in satisfaction level for each Gender in term of Staff welcome
PreFlightUo4aGender<- t.test(`UO4 [{UO4a}]. Single Response Question`~Gender)
PreFlightUo4aGender
boxplot(`UO4 [{UO4a}]. Single Response Question`~Gender)
#no

# is there is a difference in satisfaction level for each Flying type in term of Staff welcome
PreFlightUo4aFlyingC<- t.test(`UO4 [{UO4a}]. Single Response Question`~FlyingClass)
PreFlightUo4aFlyingC
boxplot(`UO4 [{UO4a}]. Single Response Question`~FlyingClass)
#no

# is there is a difference in satisfaction level for each plane type in term of Staff welcome
PreFlightUo4aPlane<- t.test(`UO4 [{UO4a}]. Single Response Question`~PlaneType)
PreFlightUo4aPlane
boxplot(`UO4 [{UO4a}]. Single Response Question`~PlaneType)
#no

# is there is a difference in satisfaction level for each Route in term of Staff welcome
PreFlightUo4aRoute<- t.test(`UO4 [{UO4a}]. Single Response Question`~Route)
PreFlightUo4aRoute
boxplot(`UO4 [{UO4a}]. Single Response Question`~Route)
#NO

# is there is a difference in satisfaction level for each Age range in term of Staff welcome
onboardo4aAgeRange<- aov(`UO4 [{UO4a}]. Single Response Question`~`Age Range`)
summary(onboardo4aAgeRange)
boxplot(`UO4 [{UO4b}]. Single Response Question`~`Age Range`)
#no

  #UO3 Check-in (a) All & have handbage - Queue Time


# is there is a difference in satisfaction level for each Gender in term of Staff welcome
PreFlightUo4aGender<- t.test(`UO4 [{UO4a}]. Single Response Question`~Gender)
PreFlightUo4aGender
boxplot(`UO4 [{UO4a}]. Single Response Question`~Gender)
#no

# is there is a difference in satisfaction level for each Flying type in term of Staff welcome
PreFlightUo4aFlyingC<- t.test(`UO4 [{UO4a}]. Single Response Question`~FlyingClass)
PreFlightUo4aFlyingC
boxplot(`UO4 [{UO4a}]. Single Response Question`~FlyingClass)
#no

# is there is a difference in satisfaction level for each plane type in term of Staff welcome
PreFlightUo4aPlane<- t.test(`UO4 [{UO4a}]. Single Response Question`~PlaneType)
PreFlightUo4aPlane
boxplot(`UO4 [{UO4a}]. Single Response Question`~PlaneType)
#no

# is there is a difference in satisfaction level for each Route in term of Staff welcome
PreFlightUo4aRoute<- t.test(`UO4 [{UO4a}]. Single Response Question`~Route)
PreFlightUo4aRoute
boxplot(`UO4 [{UO4a}]. Single Response Question`~Route)
#NO

# is there is a difference in satisfaction level for each Age range in term of Staff welcome
onboardo4aAgeRange<- aov(`UO4 [{UO4a}]. Single Response Question`~`Age Range`)
summary(onboardo4aAgeRange)
boxplot(`UO4 [{UO4b}]. Single Response Question`~`Age Range`)
#no

    #UO3 Check-in (i) All & have handbage - Language Skills of Check-In Staff


# is there is a difference in satisfaction level for each Gender in term of Staff welcome
PreFlightUo4iGender<- t.test(`UO4 [{UO4i}]. Single Response Question`~Gender)
PreFlightUo4iGender
boxplot(`UO4 [{UO4i}]. Single Response Question`~Gender)
#no

# is there is a difference in satisfaction level for each Flying type in term of Staff welcome
PreFlightUo4iFlyingC<- t.test(`UO4 [{UO4i}]. Single Response Question`~FlyingClass)
PreFlightUo4iFlyingC
boxplot(`UO4 [{UO4i}]. Single Response Question`~FlyingClass)
#no

# is there is a difference in satisfaction level for each plane type in term of Staff welcome
PreFlightUo4iPlane<- t.test(`UO4 [{UO4i}]. Single Response Question`~PlaneType)
PreFlightUo4iPlane
boxplot(`UO4 [{UO4i}]. Single Response Question`~PlaneType)
#no

# is there is a difference in satisfaction level for each Route in term of Staff welcome
PreFlightUo4iRoute<- t.test(`UO4 [{UO4i}]. Single Response Question`~Route)
PreFlightUo4iRoute
boxplot(`UO4 [{UO4i}]. Single Response Question`~Route)
#NO

# is there is a difference in satisfaction level for each Age range in term of Staff welcome
onboardo4iAgeRange<- aov(`UO4 [{UO4i}]. Single Response Question`~`Age Range`)
summary(onboardo4iAgeRange)
boxplot(`UO4 [{UO4i}]. Single Response Question`~`Age Range`)
#no

      #UO3 Check-in (b) Counter & CIP lounge- Staff welcome

# is there is a difference in satisfaction level for each Gender in term of Staff welcome
PreFlightUo4bGender<- t.test(`UO4 [{UO4b}]. Single Response Question`~Gender)
PreFlightUo4bGender
boxplot(`UO4 [{UO4b}]. Single Response Question`~Gender)
#no

# is there is a difference in satisfaction level for each Flying type in term of Staff welcome
PreFlightUo4bFlyingC<- t.test(`UO4 [{UO4b}]. Single Response Question`~FlyingClass)
PreFlightUo4bFlyingC
boxplot(`UO4 [{UO4b}]. Single Response Question`~FlyingClass)
#no

# is there is a difference in satisfaction level for each plane type in term of Staff welcome
PreFlightUo4bPlane<- t.test(`UO4 [{UO4b}]. Single Response Question`~PlaneType)
PreFlightUo4bPlane
boxplot(`UO4 [{UO4b}]. Single Response Question`~PlaneType)
#no

# is there is a difference in satisfaction level for each Route in term of Staff welcome
PreFlightUo4bRoute<- t.test(`UO4 [{UO4b}]. Single Response Question`~Route)
PreFlightUo4bRoute
boxplot(`UO4 [{UO4}]. Single Response Question`~Route)
#no

# is there is a difference in satisfaction level for each Age range in term of Staff welcome
onboardU11bAgeRange<- aov(`UD11 [{UD11b}]. Single Response Question`~`Age Range`)
summary(onboardU11bAgeRange)
boxplot(`UD11 [{UD11b}]. Single Response Question`~`Age Range`)
#yes


    #UO3 Check-in (b) Counter & CIP lounge- Staff welcome


# is there is a difference in satisfaction level for each Gender in term of Staff welcome
PreFlightUo4bGender<- t.test(`UO4 [{UO4b}]. Single Response Question`~Gender)
PreFlightUo4bGender
boxplot(`UO4 [{UO4b}]. Single Response Question`~Gender)
#no

# is there is a difference in satisfaction level for each Flying type in term of Staff welcome
PreFlightUo4bFlyingC<- t.test(`UO4 [{UO4b}]. Single Response Question`~FlyingClass)
PreFlightUo4bFlyingC
boxplot(`UO4 [{UO4b}]. Single Response Question`~FlyingClass)
#no

# is there is a difference in satisfaction level for each plane type in term of Staff welcome
PreFlightUo4bPlane<- t.test(`UO4 [{UO4b}]. Single Response Question`~PlaneType)
PreFlightUo4bPlane
boxplot(`UO4 [{UO4b}]. Single Response Question`~PlaneType)
#no

# is there is a difference in satisfaction level for each Route in term of Staff welcome
PreFlightUo4bRoute<- t.test(`UO4 [{UO4b}]. Single Response Question`~Route)
PreFlightUo4bRoute
boxplot(`UO4 [{UO4}]. Single Response Question`~Route)
#no

# is there is a difference in satisfaction level for each Age range in term of Staff welcome
onboardU11bAgeRange<- aov(`UD11 [{UD11b}]. Single Response Question`~`Age Range`)
summary(onboardU11bAgeRange)
boxplot(`UD11 [{UD11b}]. Single Response Question`~`Age Range`)
#yes


    
    #UO3 Check-in (c) Counter & CIP lounge- Interaction of check-in staff


# is there is a difference in satisfaction level for each Gender in term of Staff welcome
PreFlightUo4cGender<- t.test(`UO4 [{UO4c}]. Single Response Question`~Gender)
PreFlightUo4cGender
boxplot(`UO4 [{UO4c}]. Single Response Question`~Gender)
#no

# is there is a difference in satisfaction level for each Flying type in term of Staff welcome
PreFlightUo4cFlyingC<- t.test(`UO4 [{UO4c}]. Single Response Question`~FlyingClass)
PreFlightUo4cFlyingC
boxplot(`UO4 [{UO4c}]. Single Response Question`~FlyingClass)
#no

# is there is a difference in satisfaction level for each plane type in term of Staff welcome
PreFlightUo4cPlane<- t.test(`UO4 [{UO4c}]. Single Response Question`~PlaneType)
PreFlightUo4cPlane
boxplot(`UO4 [{UO4c}]. Single Response Question`~PlaneType)
#no

# is there is a difference in satisfaction level for each Route in term of Staff welcome
PreFlightUo4cRoute<- t.test(`UO4 [{UO4c}]. Single Response Question`~Route)
PreFlightUo4cRoute
boxplot(`UO4 [{UO4c}]. Single Response Question`~Route)
#no

# is there is a difference in satisfaction level for each Age range in term of Staff welcome
onboardUO4cAgeRange<- aov(`UO4 [{UO4c}]. Single Response Question`~`Age Range`)
summary(onboardUO4cAgeRange)
boxplot(`UO4 [{UO4c}]. Single Response Question`~`Age Range`)
#no

      #UO3 Check-in (G) Counter & CIP lounge- Appearance of the counter


# is there is a difference in satisfaction level for each Gender in term of Staff welcome
PreFlightUo4GGender<- t.test(`UO4 [{UO4g}]. Single Response Question`~Gender)
PreFlightUo4GGender
boxplot(`UO4 [{UO4g}]. Single Response Question`~Gender)
#yes

# is there is a difference in satisfaction level for each Flying type in term of Staff welcome
PreFlightUo4gFlyingC<- t.test(`UO4 [{UO4g}]. Single Response Question`~FlyingClass)
PreFlightUo4gFlyingC
boxplot(`UO4 [{UO4g}]. Single Response Question`~FlyingClass)
#no

# is there is a difference in satisfaction level for each plane type in term of Staff welcome
PreFlightUo4gPlane<- t.test(`UO4 [{UO4g}]. Single Response Question`~PlaneType)
PreFlightUo4gPlane
boxplot(`UO4 [{UO4g}]. Single Response Question`~PlaneType)
#no

# is there is a difference in satisfaction level for each Route in term of Staff welcome
PreFlightUo4gRoute<- t.test(`UO4 [{UO4g}]. Single Response Question`~Route)
PreFlightUo4gRoute
boxplot(`UO4 [{UO4g}]. Single Response Question`~Route)
#no

# is there is a difference in satisfaction level for each Age range in term of Staff welcome
onboardUO4gAgeRange<- aov(`UO4 [{UO4g}]. Single Response Question`~`Age Range`)
summary(onboardUO4gAgeRange)
boxplot(`UO4 [{UO4g}]. Single Response Question`~`Age Range`)
#no



        #UD1 On-board Experiance

# which one of onboard experiance affect the most the (onboard satis + overall satis)?
OnboardW<- aov(UD1~UD2+UD4+UD6+UD8+UD10)
summary(OnboardW)

OnboardWO<- glm(UD1~UD2+UD4+UD6+UD8+UD10)
summary(OnboardWO)

# what is the most important variable affecting the Cabin (UD2)
OnboardCabin<- aov(UD2~ `UD3 [{UD3a}]. Single Response Question`+`UD3 [{UD3b}]. Single Response Question`+`UD3 [{UD3c}]. Single Response Question`+`UD3 [{UD3d}]. Single Response Question`+`UD3 [{UD3e}]. Single Response Question`+`UD3 [{UD3f}]. Single Response Question`)
summary(OnboardCabin)

boxplot(UD2~ `UD3 [{UD3a}]. Single Response Question`+`UD3 [{UD3b}]. Single Response Question`+`UD3 [{UD3c}]. Single Response Question`+`UD3 [{UD3d}]. Single Response Question`+`UD3 [{UD3e}]. Single Response Question`+`UD3 [{UD3f}]. Single Response Question`)

OnboardCabin<- glm(UD2~ `UD3 [{UD3a}]. Single Response Question`+`UD3 [{UD3b}]. Single Response Question`+`UD3 [{UD3c}]. Single Response Question`+`UD3 [{UD3d}]. Single Response Question`+`UD3 [{UD3e}]. Single Response Question`+`UD3 [{UD3f}]. Single Response Question`)
summary(OnboardCabin)

    # UD2 (a) Sufficient room in the overhead cabinets


# is there is a difference in satisfaction level for each gender in term of Sufficient room in the overhead cabinets
onboardU3aGendar<- t.test(`UD3 [{UD3a}]. Single Response Question`~Gender)
onboardU3aGendar
boxplot(`UD3 [{UD3a}]. Single Response Question`~Gender)
#no 

# is there is a difference in satisfaction level for each Flying class in term of Sufficient room in the overhead cabinets
onboardU3aFlyingc<- t.test(`UD3 [{UD3a}]. Single Response Question`~FlyingClass)
onboardU3aFlyingc
boxplot(`UD3 [{UD3a}]. Single Response Question`~FlyingClass)
#no 

# is there is a difference in satisfaction level for each plane type in term of Sufficient room in the overhead cabinets
onboardU3aplane<- t.test(`UD3 [{UD3a}]. Single Response Question`~PlaneType)
onboardU3aplane
boxplot(`UD3 [{UD3a}]. Single Response Question`~PlaneType)
#no

# is there is a difference in satisfaction level for each Route in term of Sufficient room in the overhead cabinets
onboardU3aRoute<- t.test(`UD3 [{UD3a}]. Single Response Question`~Route)
onboardU3aRoute
boxplot(`UD3 [{UD3a}]. Single Response Question`~Route)
#no

# is there is a difference in satisfaction level for each Age range in term of Sufficient room in the overhead cabinets
onboardU3aAgeRange<- glm(`UD3 [{UD3a}]. Single Response Question`~`Age Range`)
summary(onboardU3aAgeRange)
boxplot(`UD3 [{UD3a}]. Single Response Question`~`Age Range`)

    #UD2 (c) Cabin design 

# is there is a difference in satisfaction level for each gender in term of Cabin design 
onboardU3cGendar<- t.test(`UD3 [{UD3c}]. Single Response Question`~Gender)
onboardU3cGendar
boxplot(`UD3 [{UD3c}]. Single Response Question`~Gender)
#no 

# is there is a difference in satisfaction level for each plane type in term of Cabin design 
onboardU3cplane<- t.test(`UD3 [{UD3c}]. Single Response Question`~PlaneType)
onboardU3cplane
boxplot(`UD3 [{UD3c}]. Single Response Question`~PlaneType)
#no

# is there is a difference in satisfaction level for each Fling class in term of Cabin design 
onboardU3cFC<- t.test(`UD3 [{UD3c}]. Single Response Question`~FlyingClass)
onboardU3cFC
boxplot(`UD3 [{UD3c}]. Single Response Question`~FlyingClass)
#no

# is there is a difference in satisfaction level for each Route in term of Cabin design 
onboardU3cRoute<- t.test(`UD3 [{UD3c}]. Single Response Question`~Route)
onboardU3cRoute
boxplot(`UD3 [{UD3c}]. Single Response Question`~Route)
#yes

# is there is a difference in satisfaction level for each Age range in term of Cabin design 
onboardU3cAgeRange<- aov(`UD3 [{UD3c}]. Single Response Question`~`Age Range`)
summary(onboardU3cAgeRange)
boxplot(`UD3 [{UD3c}]. Single Response Question`~`Age Range`)
#no

    #UD3  (e) Cleanliness of the cabin


# is there is a difference in satisfaction level for each gender in term of Cleanliness of the cabin 
onboardU3eGendar<- t.test(`UD3 [{UD3e}]. Single Response Question`~Gender)
onboardU3eGendar
boxplot(`UD3 [{UD3e}]. Single Response Question`~Gender)
#no 

# is there is a difference in satisfaction level for each plane type in term of Cleanliness of the cabin 
onboardU3eplane<- t.test(`UD3 [{UD3e}]. Single Response Question`~PlaneType)
onboardU3eplane
boxplot(`UD3 [{UD3e}]. Single Response Question`~PlaneType)
#no

# is there is a difference in satisfaction level for flying class type in term of Cleanliness of the cabin 
onboardU3eFC<- t.test(`UD3 [{UD3e}]. Single Response Question`~FlyingClass)
onboardU3eFC
boxplot(`UD3 [{UD3e}]. Single Response Question`~FlyingClass)
#no

# is there is a difference in satisfaction level for each Route in term of Cleanliness of the cabin 
onboardU3eRoute<- t.test(`UD3 [{UD3e}]. Single Response Question`~Route)
onboardU3eRoute
boxplot(`UD3 [{UD3e}]. Single Response Question`~Route)
#no

# is there is a difference in satisfaction level for each Age range in term of Cleanliness of the cabin 
onboardU3eAgeRange<- aov(`UD3 [{UD3e}]. Single Response Question`~`Age Range`)
summary(onboardU3eAgeRange)
boxplot(`UD3 [{UD3e}]. Single Response Question`~`Age Range`)
#no

   #UD10

# what is the most important variable affecting the inflight catering (UD10)

OnboardWD10<- aov(UD10~ `UD11 [{UD11a}]. Single Response Question`+`UD11 [{UD11b}]. Single Response Question`+`UD11 [{UD11c}]. Single Response Question`+`UD11 [{UD11d}]. Single Response Question`+`UD11 [{UD11e}]. Single Response Question`+`UD11 [{UD11f}]. Single Response Question`)
summary(OnboardWD10)

OnboardWD10<- glm(UD10~ `UD11 [{UD11a}]. Single Response Question`+`UD11 [{UD11b}]. Single Response Question`+`UD11 [{UD11c}]. Single Response Question`+`UD11 [{UD11d}]. Single Response Question`+`UD11 [{UD11e}]. Single Response Question`+`UD11 [{UD11f}]. Single Response Question`)
summary(OnboardWD10)

      #UD11  (a)  Flavour of food & beverage

# is there is a difference in satisfaction level for each gender in term of Flavour of food & beverage
onboardU11aGendar<- t.test(`UD11 [{UD11a}]. Single Response Question`~Gender)
onboardGendar
boxplot(`UD11 [{UD11a}]. Single Response Question`~Gender)
#no 

# is there is a difference in satisfaction level for each plane type in term of Flavour of food & beverage
onboardU11aplane<- t.test(`UD11 [{UD11a}]. Single Response Question`~PlaneType)
onboardU11aplane
boxplot(`UD11 [{UD11a}]. Single Response Question`~PlaneType)
#no

# is there is a difference in satisfaction level for each Flying class in term of Flavour of food & beverage
onboardU11aFC<- t.test(`UD11 [{UD11a}]. Single Response Question`~FlyingClass)
onboardU11aFC
boxplot(`UD11 [{UD11a}]. Single Response Question`~FlyingClass)
#no

# is there is a difference in satisfaction level for each Route in term of Flavour of food & beverage
onboardU11aRoute<- t.test(`UD11 [{UD11a}]. Single Response Question`~Route)
onboardU11aRoute
boxplot(`UD11 [{UD11a}]. Single Response Question`~Route)
#no

# is there is a difference in satisfaction level for each Age range in term of Flavour of food & beverage
onboardU11aAgeRange<- aov(`UD11 [{UD11a}]. Single Response Question`~`Age Range`)
summary(onboardU11aAgeRange)
boxplot(`UD11 [{UD11a}]. Single Response Question`~`Age Range`)
#no

      #UD11  (b)  Quantity of food & beverage


# is there is a difference in satisfaction level for each Gender in term of Flavour of Quantity of food & beverage
onboardU11bGender<- t.test(`UD11 [{UD11b}]. Single Response Question`~Gender)
onboardU11bGender
boxplot(`UD11 [{UD11b}]. Single Response Question`~Gender)
#no

# is there is a difference in satisfaction level for each plane type in term of Flavour of Quantity of food & beverage
onboardU11bPlane<- t.test(`UD11 [{UD11b}]. Single Response Question`~PlaneType)
onboardU11bPlane
boxplot(`UD11 [{UD11b}]. Single Response Question`~PlaneType)
#yes

# is there is a difference in satisfaction level for each flying class in term of Flavour of Quantity of food & beverage
onboardU11bFC<- t.test(`UD11 [{UD11b}]. Single Response Question`~FlyingClass)
onboardU11bFC
boxplot(`UD11 [{UD11b}]. Single Response Question`~FlyingClass)
#yes

# is there is a difference in satisfaction level for each Route in term of Flavour of Quantity of food & beverage
onboardU11broute<- t.test(`UD11 [{UD11b}]. Single Response Question`~Route)
onboardU11broute
boxplot(`UD11 [{UD11b}]. Single Response Question`~Route)
#yes

# is there is a difference in satisfaction level for each Age range in term of Flavour of Quantity of food & beverage
onboardU11bAgeRange<- aov(`UD11 [{UD11b}]. Single Response Question`~`Age Range`)
summary(onboardU11bAgeRange)
boxplot(`UD11 [{UD11b}]. Single Response Question`~`Age Range`)
#yes 

          #UD11  (e)  Beverage variety


# is there is a difference in satisfaction level for each Gender in term of Beverage variety
onboardU11eGender<- t.test(`UD11 [{UD11e}]. Single Response Question`~Gender)
onboardU11eGender
boxplot(`UD11 [{UD11e}]. Single Response Question`~Gender)
#no

# is there is a difference in satisfaction level for each plane type in term of Beverage variety
onboardU11ePlane<- t.test(`UD11 [{UD11e}]. Single Response Question`~PlaneType)
onboardU11ePlane
boxplot(`UD11 [{UD11e}]. Single Response Question`~PlaneType)
#yes

# is there is a difference in satisfaction level for each Flying class in term of Beverage variety
onboardU11eFC<- t.test(`UD11 [{UD11e}]. Single Response Question`~FlyingClass)
onboardU11eFC
boxplot(`UD11 [{UD11e}]. Single Response Question`~FlyingClass)
#yes

# is there is a difference in satisfaction level for each Route in term of Beverage variety
onboardU11eroute<- t.test(`UD11 [{UD11e}]. Single Response Question`~Route)
onboardU11broute
boxplot(`UD11 [{UD11e}]. Single Response Question`~Route)
#yes

# is there is a difference in satisfaction level for each Age range in term of Beverage variety
onboardU11eAgeRange<- aov(`UD11 [{UD11e}]. Single Response Question`~`Age Range`)
summary(onboardU11eAgeRange)
boxplot(`UD11 [{UD11e}]. Single Response Question`~`Age Range`)
#no

        #UD11  (f)  Presentation quality


# is there is a difference in satisfaction level for each Gender in term of Flavour of Presentation quality
onboardU11fGender<- t.test(`UD11 [{UD11f}]. Single Response Question`~Gender)
onboardU11fGender
boxplot(`UD11 [{UD11f}]. Single Response Question`~Gender)
#no

# is there is a difference in satisfaction level for each Route r in term of Flavour of Presentation quality
onboardU11fRoute<- t.test(`UD11 [{UD11f}]. Single Response Question`~Route)
onboardU11fRoute
boxplot(`UD11 [{UD11f}]. Single Response Question`~Route)
#no

# is there is a difference in satisfaction level for each Plane type r in term of Flavour of Presentation quality
onboardU11fPlane<- t.test(`UD11 [{UD11f}]. Single Response Question`~PlaneType)
onboardU11fPlane
boxplot(`UD11 [{UD11f}]. Single Response Question`~PlaneType)
#no

# is there is a difference in satisfaction level for each Flying Class r in term of Flavour of Presentation quality
onboardU11fFC<- t.test(`UD11 [{UD11f}]. Single Response Question`~FlyingClass)
onboardU11fFC
boxplot(`UD11 [{UD11f}]. Single Response Question`~FlyingClass)
#yes

# is there is a difference in satisfaction level for each Age range in term of Flavour of Presentation quality
onboardU11fAgeRange<- aov(`UD11 [{UD11f}]. Single Response Question`~`Age Range`)
summary(onboardU11fAgeRange)
boxplot(`UD11 [{UD11f}]. Single Response Question`~`Age Range`)
#yes

