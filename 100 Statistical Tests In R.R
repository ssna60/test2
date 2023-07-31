library(psych)
library(compOverlapCorr)

#100 Statistical Tests In R
#-------------------------------------------------------------------------------

#Test 1 Pearson's product moment correlation coefficient t-test
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
cor.test(x,y,method="pearson",alternative="two.sided",conf.level = 0.95)

#-------------------------------------------------------------------------------

#Test 2 Spearman rank correlation test
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
cor.test(x,y,method="spearman",alternative="two.sided")
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
spearman.test(x,y,alternative="two.sided",approximation ="exact")
cor.test(x,y,method="spearman",alternative="two.sided",approximation ="exact")

#-------------------------------------------------------------------------------

#Test 3 Kendall's tau correlation coefficient test
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
cor.test(x,y,method="kendal",alternative="two.sided")


#-------------------------------------------------------------------------------

#Test 4 Z Test of the difference between independent correlations
paired.r(0.54,0.89,NULL,17,26,twotailed=TRUE)


paired.r(0.69,0.77,NULL,594,420,twotailed=TRUE)


#-------------------------------------------------------------------------------

#Test 5 Difference between two overlapping correlation coefficients
compOverlapCorr(123, r13=-0.2, r23=-0.28, r12=0.30)


paired.r(xy=-0.2,xz=-0.28,yz=0.30, 123,twotailed=TRUE)


r.test(123,r12=-.2,r34=-.28,r23=.3,twotailed = TRUE)

#-------------------------------------------------------------------------------library(psych)
library(compOverlapCorr)
#-------------------------------------------------------------------------------

#Test 6 Difference between two non-overlapping dependent correla?on coefficients
r.test(187, r12 = -0.11, r34 = 0.06, r23 = 0.41)

#-------------------------------------------------------------------------------

#Test 7 Bartlett's Test of sphericity
set.seed(1234) 
n=1000
y1 <- rnorm(n)
y2<- rnorm(n)
y3<-y1+y2
data<-matrix(c(y1,y2,y3) , nrow = n, ncol=3, byrow=TRUE,)
correlation.matrix <- cor(data)
cortest.bartlett(correlation.matrix,n)

#-------------------------------------------------------------------------------

#Test 8 Jennrich Test of the equality of two matrices
set.seed(1234)  
n1 =1000
n2=1000
sample.1 <- matrix(rnorm(n1),ncol=10)
sample.2 <- matrix(rnorm(n2),ncol=10)
cortest.jennrich(sample.1, sample.2)

#-------------------------------------------------------------------------------

#Test 9 Granger causality test
sample1=c(3083,3140,3218,3239,3295,3374,3475,3569,3597,3725,3794,3959,4043)
sample2=c(75,78,80,82,84,88,93,97,99,104,109,115,120,127)
sample3=c(5,8,0,2,4,8,3,7,9,10,10,15,12,12)
data1=cbind(sample1,sample2,sample3)

#-------------------------------------------------------------------------------
#BOOK
#Test 10 Durbin-Watson autocorrelation test
dependent.variable=c(3083,3140,3218,3239,3295,3374,3475,3569,3597,3725)
independent.variable=c(75,78,80,82,84,88,93,97,99,104,109,115,120,127)

#-------------------------------------------------------------------------------library(psych)
library(compOverlapCorr)
library(bgtest)

library(UsingR)
library(BSDA)


#Test 11 Breusch-Godfrey autocorrelation test
dependent.variable=c(3083,3140,3218,3239,3295,3374,3475,3569,3597,3725)
independent.variable=c(75,78,80,82,84,88,93,97,99,104,109,115,120,127)
bgtest(lm(dependent.variable ~ independent.variable),order=1)

#-------------------------------------------------------------------------------

#Test 12 One sample t-Test for a hypothesized mean
x <- c(59.3,14.2,32.9,69.1,23.1,79.3,51.9,39.2,41.8)
t.test(x,mu=40, alternative = "two.sided", conf.level = 0.95)



x <- c(59.3,14.2,32.9,69.1,23.1,79.3,51.9,39.2,41.8)
t.test(x,mu=30, alternative = "greater", conf.level = 0.95)

#-------------------------------------------------------------------------------

#Test 13 One sample Wilcoxon signed rank test
x <- c(59.3,14.2,32.9,69.1,23.1,79.3,51.9,39.2,41.8)
wilcox.test (x,mu=40, alternative = "two.sided")



x <- c(59.3,14.2,32.9,69.1,23.1,79.3,51.9,39.2,41.8)
wilcox.test (x,mu=30, alternative = "greater")

#-------------------------------------------------------------------------------

#Test 14 Sign Test for a hypothesized median
x<-c(12,2,17,25,52,8,1,12)
simple.median.test(x, median = 20)


x<-c(12,2,17,25,52,8,1,12)
SIGN.test(x,md=20, alternative = "two.sided", conf.level = 0.95)

#-------------------------------------------------------------------------------

#Test 15 Two sample t-Test for the difference in sample means
x<-c(0.795,.864,.841,.683,.777,.720)
y<-c(.765,.735,1.003,.778,.647,.740,.612)
t.test(x,y, alternative = "two.sided", var.equal=TRUE)

#-------------------------------------------------------------------------------
#Test 16 Pairwise t-Test for the difference in sample means
sample_1 <- c(2.9, 3.5, 2.8, 2.6, 3.7)
sample_2 <- c(3.9, 2.5, 4.3, 2.7)    
sample_3<- c(2.9, 2.4, 3.8, 1.2, 2.0)
sample <- c(sample_1, sample_2, sample_3)
g <- factor(rep(1:3, c(5, 4, 5)),
            labels = c("sample_1",
                       " sample_2",
                       " sample_3"))
pairwise.t.test(sample, g, p.adjust.method ="holm", pool.sd = FALSE,alternative = "two.sided")

#-------------------------------------------------------------------------------

#Test 17 Pairwise t-Test for the difference in sample means with common variance
sample_1 <- c(2.9, 3.5, 2.8, 2.6, 3.7)
sample_2 <- c(3.9, 2.5, 4.3, 2.7)    
sample_3<- c(2.9, 2.4, 3.8, 1.2, 2.0)
sample <- c(sample_1, sample_2, sample_3)
g <- factor(rep(1:3, c(5, 4, 5)),
            labels = c("sample_1",
                       " sample_2",
                       " sample_3"))
pairwise.t.test(sample, g, p.adjust.method ="holm", pool.sd = TRUE,alternative = "two.sided")

#-------------------------------------------------------------------------------

#Test 18 Welch t-Test for the difference in sample means
sample1<-c(0.795,.864,.841,.683,.777,.720)
sample2<-c(.765,.735,1.003,.778,.647,.740,.612)
t.test(sample1,sample2, alternative = "two.sided", var.equal=FALSE)



sample1<-c(0.795,0.864,0.841,0.683,0.777,0.720)
sample2<-c(0.765,0.735,1.003,0.778,0.647,0.740,0.612)
t.test(sample1,sample2, alternative = "greater", var.equal=FALSE)

#-------------------------------------------------------------------------------

#Test 19 Paired t-Test for the difference in sample means
initial_value <- c(16,20,21,22,23,22,27,25,27,28)
final_value <- c(19,22,24,24,25,25,26,26,28,32)
t.test(final_value,initial_value, alternative = "two.sided", paired =TRUE)


initial_value <- c(16,20,21,22,23,22,27,25,27,28)
final_value <- c(19,22,24,24,25,25,26,26,28,32)
t.test(final_value,initial_value, alternative = "greater", paired =TRUE)


#-------------------------------------------------------------------------------

#Test 20 Matched pairs Wilcoxon test

initial_value <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
final_value <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wilcox.test(initial_value, final_value, paired = TRUE, alternative ="two.sided")

initial_value <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
final_value <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wilcox.test(initial_value, final_value, paired = TRUE, alternative ="greater")
#-------------------------------------------------------------------------------
#Test 16 Pairwise t-Test for the difference in sample means
sample_1 <- c(2.9, 3.5, 2.8, 2.6, 3.7)
sample_2 <- c(3.9, 2.5, 4.3, 2.7)    
sample_3<- c(2.9, 2.4, 3.8, 1.2, 2.0)
sample <- c(sample_1, sample_2, sample_3)
g <- factor(rep(1:3, c(5, 4, 5)),
            labels = c("sample_1",
                       " sample_2",
                       " sample_3"))
pairwise.t.test(sample, g, p.adjust.method ="holm", pool.sd = FALSE,alternative = "two.sided")

#-------------------------------------------------------------------------------

#Test 17 Pairwise t-Test for the difference in sample means with common variance
sample_1 <- c(2.9, 3.5, 2.8, 2.6, 3.7)
sample_2 <- c(3.9, 2.5, 4.3, 2.7)    
sample_3<- c(2.9, 2.4, 3.8, 1.2, 2.0)
sample <- c(sample_1, sample_2, sample_3)
g <- factor(rep(1:3, c(5, 4, 5)),
            labels = c("sample_1",
                       " sample_2",
                       " sample_3"))
pairwise.t.test(sample, g, p.adjust.method ="holm", pool.sd = TRUE,alternative = "two.sided")

#-------------------------------------------------------------------------------

#Test 18 Welch t-Test for the difference in sample means
sample1<-c(0.795,.864,.841,.683,.777,.720)
sample2<-c(.765,.735,1.003,.778,.647,.740,.612)
t.test(sample1,sample2, alternative = "two.sided", var.equal=FALSE)



sample1<-c(0.795,0.864,0.841,0.683,0.777,0.720)
sample2<-c(0.765,0.735,1.003,0.778,0.647,0.740,0.612)
t.test(sample1,sample2, alternative = "greater", var.equal=FALSE)

#-------------------------------------------------------------------------------

#Test 19 Paired t-Test for the difference in sample means
initial_value <- c(16,20,21,22,23,22,27,25,27,28)
final_value <- c(19,22,24,24,25,25,26,26,28,32)
t.test(final_value,initial_value, alternative = "two.sided", paired =TRUE)


initial_value <- c(16,20,21,22,23,22,27,25,27,28)
final_value <- c(19,22,24,24,25,25,26,26,28,32)
t.test(final_value,initial_value, alternative = "greater", paired =TRUE)


#-------------------------------------------------------------------------------

#Test 20 Matched pairs Wilcoxon test

initial_value <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
final_value <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wilcox.test(initial_value, final_value, paired = TRUE, alternative ="two.sided")

initial_value <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
final_value <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wilcox.test(initial_value, final_value, paired = TRUE, alternative ="greater")
#-------------------------------------------------------------------------------
remove(list = ls())
library(psych)
library(compOverlapCorr)
library(bgtest)

library(randtests)

library(UsingR)
library(BSDA)
library(DescTools)
library(trend)
library(tseries)
remove(list = ls())

#Test 21 Pairwise paired t-Test for the difference in sample means
sample_1 <- c(2.9, 3.5, 2.8, 2.6, 3.7,4.0)
sample_2 <- c(3.9, 2.5, 4.3, 2.7,2.6,3.0)    
sample_3<- c(2.9, 2.4, 3.8, 1.2, 2.0,1.97)
sample <- c(sample_1, sample_2, sample_3)
g <- factor(rep(1:3, c(6, 6, 6)),
            labels = c("sample_1",
                       " sample_2",
                       " sample_3"))
pairwise.t.test(sample, g, p.adjust.method ="holm", paired=TRUE,alternative = "two.sided")

#-------------------------------------------------------------------------------

#Test 22 Pairwise Wilcox Test for the difference in sample means
sample_1 <- c(2.9, 3.5, 2.8, 2.6, 3.7,4.0)
sample_2 <- c(3.9, 2.5, 4.3, 2.7,2.6,3.0)    
sample_3<- c(2.9, 2.4, 3.8, 1.2, 2.0,1.97)
sample <- c(sample_1, sample_2, sample_3)
g <- factor(rep(1:3, c(6, 6, 6)),
            labels = c("sample_1",
                       " sample_2",
                       " sample_3"))
pairwise.wilcox.test(sample, g, p.adjust.method ="holm", paired=
                       TRUE,alternative = "two.sided")

#-------------------------------------------------------------------------------

#Test 23 Two sample dependent sign rank Test for difference in medians
initial_value <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
final_value <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
SIGN.test(initial_value,final_value, alternative = "two.sided")



initial_value <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
final_value <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
SIGN.test(initial_value,final_value, alternative = "greater")

#-------------------------------------------------------------------------------

#Test 24 Wilcoxon rank sum Test for the difference in medians
x<-c(0.795,0.864,0.841,0.683,0.777,0.720)
y<-c(0.765,0.735,1.003,0.778,0.647,0.740,0.612)
wilcox.test(x,y, alternative = "two.sided")

x<-c(0.795,0.864,0.841,0.683,0.777,0.720)
y<-c(0.765,0.735,1.003,0.778,0.647,0.740,0.612)
wilcox.test(x,y, alternative = "greater")

#-------------------------------------------------------------------------------

#Test 25 Wald-Wolfowitz runs Test for dichotomous data
binary_factor<-factor(c(1,0,0,0,0,0,0,0,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,0,0,0))
RunsTest(binary_factor, alternative="two.sided")
runs.test(binary_factor,alternative ="two.sided")

binary_factor<-factor(c(1,0,0,0,0,0,0,0,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,0,0,0))
RunsTest(binary_factor, alternative="less")
runs.test(binary_factor,alternative ="less")

#-------------------------------------------------------------------------------

#Test 26 Wald-Wolfowitz runs Test for continuous data
y=c(1.8,2.3,3.5,4,5.5,6.3,7.2,8.9,9.1)

runs.test(y, alternative = "two.sided")

y=c(1.8,2.3,3.5,4,5.5,6.3,7.2,8.9,9.1)
runs.test(y, alternative = "positive.correlated")

#-------------------------------------------------------------------------------

#Test 27 Bartels Test of randomness in a sample
y<-c(-82.29,-31.14,136.58,85.42,42.96,-122.72,0.59,55.77,117.62,-10.95,211.38,-304.02,30.72,238.19,140.98,18.88,-48.21,-63.7)
bartels.test(y,alternative="two.sided")


y<-c(-82.29,-31.14,136.58,85.42,42.96,-122.72,0.59,55.77,117.62,-10.95,211.38,-304.02,30.72,238.19,140.98,18.88,-48.21,-63.7)
bartels.test(y,alternative ="positive.correlated")

#-------------------------------------------------------------------------------

#Test 28 Ljung-Box Test
y<-c(-82.29,-31.14,136.58,85.42,42.96,-122.72,0.59,55.77,117.62,-10.95,211.38,-304.02,30.72,238.19,140.98,18.88,-48.21,-63.7)
Box.test (y, lag = 3,type = "Ljung-Box")

#-------------------------------------------------------------------------------

#Test 29 Box-Pierce test
y<-c(-82.29,-31.14,136.58,85.42,42.96,-122.72,0.59,55.77,117.62,-10.95,211.38,-304.02,30.72,238.19,140.98,18.88,-48.21,-63.7)
Box.test (y, lag = 3,type = "Box-Pierce")

#-------------------------------------------------------------------------------

#Test 30 BDS test
set.seed(1234)
x <- rnorm(5000)
bds.test(x,m=6)

DAX<-EuStockMarkets[,1]
diff_DAX = diff(DAX,1)
bds.test(diff_DAX, m=6)

#-------------------------------------------------------------------------------
remove(list = ls())
library(psych)
library(compOverlapCorr)
library(bgtest)

library(randtests)

library(UsingR)
library(BSDA)
library(DescTools)
library(trend)
library(tseries)
library(fBasics)
library(PairedData)

#-------------------------------------------------------------------------------

#Test 31 Wald-Wolfowitz two sample run test
combined_sample<-factor(c(0,0,0,0,1,1,1,1,0,1,0,1,1))
runs.test(combined_sample)

combined_sample<-factor(c(0,0,0,0,1,1,1,1,0,1,0,1,1))
runs.test(combined_sample,alternative ="less")

#-------------------------------------------------------------------------------

#Test 32 Mood's test
sample_1 <-c(3.84,2.6,1.19,2)
sample_2<-c(3.97,2.5,2.7,3.36,2.3)
mood.test (sample_1, sample_2, alternative ="two.sided")

sample_1 <-c(3.84,2.6,1.19,2)
sample_2<-c(3.97,2.5,2.7,3.36,2.3)
scaleTest(sample_1,sample_2,method = "mood")

#-------------------------------------------------------------------------------

#Test 33 F-Test of equality of variances
machine.1=c(10.8,11.0,10.4,10.3,11.3)
machine.2=c (10.8,10.6,11,10.9,10.9,10.7,1.8)
var.test(machine.1,machine.2, rao =1, alternave="two.sided",conf.level= 0.95)

#-------------------------------------------------------------------------------

#Test 34 Pitman-Morgan test
machine.am=c(10.8,11.0,10.4,10.3,11.3,10.2,11.1)
machine.pm=c (10.8,10.6,11,10.9,10.9,10.7,1.8)
pitman.morgan.test(machine.am,machine.pm)

#-------------------------------------------------------------------------------

#Test 35 Ansari-Bradley test
sample_1 <-c(3.84,2.6,1.19,2)
sample_2<-c(3.97,2.5,2.7,3.36,2.3)
ansari.test(sample_1, sample_2, alternative ="two.sided")

sample_1 <-c(3.84,2.6,1.19,2)
sample_2<-c(3.97,2.5,2.7,3.36,2.3)
ansari.exact(sample_1, sample_2, alternative ="two.sided")

sample_1 <-c(3.84,2.6,1.19,2)
sample_2<-c(3.97,2.5,2.7,3.36,2.3)
scaleTest(sample_1,sample_2,method = "ansari")

#-------------------------------------------------------------------------------

#Test 36 Bartlett Test for homogeneity of variance
count_data<-c(250,260,230,270,310,330,280,360,250,230,220,260,340,270,300,320,250,240)
sample<-c("A","A","A","A","B","B","B","B","C","C","C","C","D","D","D","D","E","E","E","E")
data<-data.frame((list(count= count_data, sample=sample)))
bartlett.test(data$count, data$sample)

bartlett.test(count ~ sample, data = data)

#-------------------------------------------------------------------------------

#Test 37 Fligner-Killeen test
count_data<-c(250,260,230,270,310,330,280,360,250,230,220,260,340,270,300,320,250,240)
sample<-c("A","A","A","A","B","B","B","B","C","C","C","C","D","D","D","D","E","E","E","E")
data<-data.frame((list(count= count_data, sample=sample)))
fligner.test (data$count, data$sample)

fligner.test (count ~ sample, data = data)

#-------------------------------------------------------------------------------

#Test 38 Levene's Test of equality of variance
count_data<-c(250,260,230,270,310,330,280,360,250,230,220,260,340,270,300,320,250,240)
sample<-c("A","A","A","A","B","B","B","B","C","C","C","C","D","D","D","D","E","E","E","E")
data<-data.frame((list(count= count_data, sample=sample)))
leveneTest (data$count, data$sample)

#-------------------------------------------------------------------------------

#Test 39 Cochran C Test for inlying or outlying variance
count_data<-c(250,260,230,270,310,330,280,360,250,230,220,260,340,270,300,320,250,240)
sample<-c("A","A","A","A","B","B","B","B","C","C","C","C","D","D","D","D","E","E","E","E")
data<-data.frame((list(count= count_data, sample=sample)))
cochran.test(count~sample,data,inlying=FALSE)

C.test(lm(count~sample,data =data))

cochran.test(count~sample,data,inlying=TRUE)

#-------------------------------------------------------------------------------

#Test 40 Brown-Forsythe Levene-type test
count_data<-c(250,260,230,270,310,330,280,360,250,230,220,260,340,270,300,320,250,240)
sample<-c("A","A","A","A","B","B","B","B","C","C","C","C","D","D","D","D","E","E","E","E")
data<-data.frame((list(count= count_data, sample=sample)))
levene.test (data$count, data$sample, locaon="median", correction.method="zero.correction")

#-------------------------------------------------------------------------------
remove(list = ls())
library(psych)
library(compOverlapCorr)
library(bgtest)

library(randtests)

library(UsingR)
library(BSDA)
library(DescTools)
library(trend)
library(tseries)
library(fBasics)
library(PairedData)
library(moments)

#-------------------------------------------------------------------------------

#Test 41 Mauchly's sphericity test
dependent_variable <- c (-5, -10, -5, 0, -3, -3, -5, -7, -2, 4, -1, -5, -4, -8, -4,-5,12,-7)
mlm <- matrix (dependent_variable, nrow = 6, byrow = TRUE)
mauchly.test (lm (mlm ~ 1), X = ~1)

#-------------------------------------------------------------------------------

#Test 42 Binominal test
binom.test(x = 25, n= 30, p = 0.5, alternave = "two.sided", conf.level = 0.95)
binom.test(x=25, n=30, p = 0.5,
           alternative = c("two.sided"),
           conf.level = 0.95)

binom.test(x = 25, n= 30, p = 0.5, alternative = "greater", conf.level = 0.95)
binom.test(x=25, n=30, p = 0.5,
           alternative = c( "greater"),
           conf.level = 0.95)

#-------------------------------------------------------------------------------

#Test 43 One sample proportions test
prop.test(52,100,p=0.5, alternative = "two.sided", conf.level = 0.95)

#-------------------------------------------------------------------------------

#Test 44 One sample Poisson test
poisson.test(6,6.22,alternative="two.sided",conf.level=0.95)

#-------------------------------------------------------------------------------

#Test 45 Pairwise comparison of proportions test
sample<-rbind(s1=c(95,106),s2=c(181,137),s3=c(76,85),s4=c(13,29),s5=c(11,26),s6=c(201,179))
colnames(sample) <-c("treat1","trea2")
pairwise.prop.test(sample, p.adjust.method ="holm" )

pairwise.prop.test(sample, p.adjust.method ="BY" )

#-------------------------------------------------------------------------------

#Test 46 Two sample Poisson test
poisson.test(c(10,2),c(20000,17877),alternative="two.sided",conf.level=0.95)

#-------------------------------------------------------------------------------

#Test 47 Multiple sample proportions test
prop.test(tier_1_or_2, total, alternative = "two.sided", conf.level = 0.95)

#-------------------------------------------------------------------------------

#Test 48 Chi-squared Test for linear trend
infected.swimmers  <- c( 5,5,33)
all.swimmers <- c( 13,8,37)
prop.trend.test(infected.swimmers, all.swimmers)

#-------------------------------------------------------------------------------

#Test 49 Pearson's paired chi-squared test
Table_data<- as.table(rbind(c(20, 30), c(30,20)))
hisq.test(Table_data , correct = FALSE )

chisq.test(Table_data , correct = TRUE)

#-------------------------------------------------------------------------------

#Test 50 Fishers exact test 
Table_data<- as.table(rbind(c(20, 30), c(30,20)))
dimnames(Table_data) <- list(gender=c("Male","Female"), party=c("Labour", "Conservative"))
fisher.test(Table_data, alternative = "two.sided", conf.level = 0.95)

fisher.test(Table_data, alternative = "less", conf.level = 0.95)
#-------------------------------------------------------------------------------

#Test 51 Cochran-Mantel-Haenszel test
Data <-array(c(12, 16, 7, 19,16, 11, 5, 20), dim = c(2, 2, 2),
             dimnames = list(Treatment = c("Drug", " Placebo"),        
                             Response = c("Improved", "No Change"),
                             Sex = c("Male", "Female")))
mantelhaen.test(Data, alternave = "two.sided", correct = FALSE, exact = FALSE, conf.level = 0.95)
#-------------------------------------------------------------------------------

#Test 52 McNemar's test
data<-matrix(c(59, 4, 128, 20),
             nrow = 2,
             dimnames = list("chest radiography" = c("positive", "negative"),
                             "sputum culture" = c("positive", "negative")))
mcnemar.test(data)

#-------------------------------------------------------------------------------

#Test 53 Equal means in a one-way layout with equal variances
Value <- c(2.9, 3.5, 2.8, 2.6, 3.7, 3.9, 2.5, 4.3, 2.7, 2.9, 2.4, 3.8, 1.2, 2.0)
Sample_Group <- factor(c(rep(1,5),rep(2,4),rep(3,5)))
data <- data.frame(Sample_Group, Value)
oneway.test(Value~ Sample_Group, data = data, var.equal = TRUE)

#-------------------------------------------------------------------------------

#Test 54 Welch-Test for more than two samples
Value <- c(2.9, 3.5, 2.8, 2.6, 3.7, 3.9, 2.5, 4.3, 2.7, 2.9, 2.4, 3.8, 1.2, 2.0)
Sample_Group <- factor(c(rep(1,5),rep(2,4),rep(3,5)))
data <- data.frame(Sample_Group, Value)

oneway.test(Value~ Sample_Group, data = data, var.equal = FALSE)       

#-------------------------------------------------------------------------------

#Test 55 Kruskal Wallis rank sum test
Value <- c(2.9, 3.5, 2.8, 2.6, 3.7, 3.9, 2.5, 4.3, 2.7, 2.9, 2.4, 3.8, 1.2, 2.0)
Sample_Group <- factor(c(rep(1,5),rep(2,4),rep(3,5)))
data <- data.frame(Sample_Group, Value)
kruskal.test(Value ~ Sample_Group, data=data)

sample_1 <- c(2.9, 3.5, 2.8, 2.6, 3.7)
sample_2 <- c(3.9, 2.5, 4.3, 2.7)    
sample_3<- c(2.9, 2.4, 3.8, 1.2, 2.0)
kruskal.test(list(sample_1, sample_2, sample_3))
sample <- c(sample_1, sample_2, sample_3)
g <- factor(rep(1:3, c(5, 4, 5)),
            labels = c("sample_1",
                       " sample_2",
                       " sample_3"))
kruskal.test(sample, g)

#-------------------------------------------------------------------------------

#Test 56 Friedman's test
Diet_data<-matrix(c(8, 8, 7,
                    7, 6, 6,
                    6, 8, 6,
                    8, 9, 7,
                    5, 8, 5,
                    9, 7, 7,
                    7, 7, 7,
                    8, 7, 7,
                    8, 6, 8,
                    7, 6, 6,
                    7, 8, 6,
                    9, 9, 6),
                  nrow = 12,
                  byrow = TRUE,
                  dimnames = list(1 : 12, c("Healthy Balanced", "Low Fat", "Low Carb")))
friedman.test(Diet_data)

#-------------------------------------------------------------------------------

#Test 57 Quade test
Diet_data<-matrix(c(8, 8, 7,
                    7, 6, 6,
                    6, 8, 6,
                    8, 9, 7,
                    5, 8, 5,
                    9, 7, 7,
                    7, 7, 7,
                    8, 7, 7,
                    8, 6, 8,
                    7, 6, 6,
                    7, 8, 6,
                    9, 9, 6),
                  nrow = 12,
                  byrow = TRUE,
                  dimnames = list(1 : 12, c("Healthy Balanced", "Low Fat", "Low Carb")))
quade.test(Diet_data)

#-------------------------------------------------------------------------------

#Test 58 D' Agostino Test of skewness
sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,1.44,0.58,0.16)
agostino.test(sample, alternative = "two.sided")         

#-------------------------------------------------------------------------------

#Test 59 Anscombe-Glynn Test of kurtosis
sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,1.44,0.58,0.16)
anscombe.test (sample, alternative = "two.sided" )

#-------------------------------------------------------------------------------

#Test 60 Bonett-Seier Test of kurtosis
sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,1.44,0.58,0.16)
bonett.test (sample, alternative = "two.sided" )

#-------------------------------------------------------------------------------
remove(list = ls())
library(psych)
library(compOverlapCorr)
library(bgtest)

library(randtests)

library(UsingR)
library(BSDA)
library(DescTools)
library(trend)
library(tseries)
library(fBasics)
library(PairedData)
library(moments)

library(nortest)
library(lawstat)
library(outliers)

library(outlierTest)
library(lmtest)
library(car)
#Test 61 Shapiro-Wilk test
sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,1.44,0.58,0.16)
shapiro.test (sample)

#-------------------------------------------------------------------------------

#Test 62 Kolmogorov-Smirnov Test of normality
sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,1.44,0.58,0.16)
ksnormTest(sample)

#-------------------------------------------------------------------------------

#Test 63 Jarque-Bera test
sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,1.44,0.58,0.16)
jarqueberaTest(sample)

#-------------------------------------------------------------------------------

#Test 64 D' Agostino test
sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,1.44,0.58,0.16)
dagoTest(sample)

#-------------------------------------------------------------------------------

#Test 65 Anderson-Darling Test of normality
sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,1.44,0.58,0.16)
ad.test(sample)

#-------------------------------------------------------------------------------

#Test 66 Cramer-von Mises test
sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,1.44,0.58,0.16)
cvm.test(sample)

#-------------------------------------------------------------------------------

#Test 67 Lilliefors test
sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,1.44,0.58,0.16)
lillieTest(sample)

#-------------------------------------------------------------------------------

#Test 68 Shapiro-Francia test
sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,1.44,0.58,0.16)
sfTest (sample)

#-------------------------------------------------------------------------------

#Test 69 Mardia's Test of multivariate normality
diff =diff(EuStockMarkets,1)#calculate daily difference
mardia(diff)

#-------------------------------------------------------------------------------

#Test 70 Kolomogorov - Smirnov Test for goodness of fit
#Beta R-code = pbeta
#Lognormal R-code = plnorm
#Binomial pbinom R-code =
#Negative Binomial R-code = pnbinom
#Cauchy R-code = pcauchy
#Normal R-code = pnorm
#Chisquare R-code = pchisq
#Poisson R-code = ppois
#Exponential R-code = pexp
#Student t R-code = pt
#F R-code = pf
#Uniform R-code = punif
#Gamma R-code = pgamma
#Tukey R-code = ptukey
#Geometric R-code = pgeom
#Weibull R-code = pweib
#Hypergeometric R-code = phyper
#Wilcoxon R-code = pwilcox
#Logistic R-code = plogis



sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,1.44,0.58,0.16)
ks.test(sample,"pnorm")

#-------------------------------------------------------------------------------

#Test 71 Anderson-Darling goodness of fit test
#Beta R-code = pbeta
#Lognormal R-code = plnorm
#Binomial pbinom R-code =
#Negative Binomial R-code = pnbinom
#Cauchy R-code = pcauchy
#Normal R-code = pnorm
#Chisquare R-code = pchisq
#Poisson R-code = ppois
#Exponential R-code = pexp
#Student t R-code = pt
#F R-code = pf
#Uniform R-code = punif
#Gamma R-code = pgamma
#Tukey R-code = ptukey
#Geometric R-code = pgeom
#Weibull R-code = pweib
#Hypergeometric R-code = phyper
#Wilcoxon R-code = pwilcox
#Logistic R-code = plogis

sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,1.44,0.58,0.16)
ad.test(sample,"plnorm")

#-------------------------------------------------------------------------------

#Test 72 Two-sample Kolmogorov-Smirnov test
sample1<- c(-2.12, 0.08, -1.59, -0.15, 0.9, -0.7, -0.22, -0.66, -2.14, 0.65, 1.38, 0.27, 3.33, 0.09, 1.45, 2.43, -0.55, -0.68, -0.62, -1.91, 1.11, 0.43, 0.42, 0.09, 0.76)
sample2<- c(0.91, 0.89, 0.6, -1.31, 1.07, -0.11, -1.1, -0.83, 0.8, -0.53, 0.3, 1.05, 0.35, 1.73, 0.09, -0.51, -0.95, -0.29, 1.35, 0.51, 0.66, -0.56, -0.04, 1.03, 1.47)
ks.test(sample1,sample2,alternative="two.sided")

ks2Test(sample1,sample2)


#-------------------------------------------------------------------------------

#Test 73 Anderson-Darling multiple sample goodness of fit test
DAX <-diff(EuStockMarkets[,1],1)
SMI <- diff(EuStockMarkets[,2],1)
CAC <- diff(EuStockMarkets[,3],1)
FTSE <- diff(EuStockMarkets[,4],1)
ad.test(DAX,SMI,CAC,FTSE)

#-------------------------------------------------------------------------------

#Test 74 Brunner-Munzel generalized Wilcoxon Test
ordinal.score1<-c(2,2,4,1,1,4,1,3,1,5,2,4,1,1)
ordinal.score2<-c(3,3,4,3,1,2,3,3,1,5,4)

brunner.munzel.test(ordinal.score1, ordinal.score2, alternave="two.sided", alpha=0.05)

#-------------------------------------------------------------------------------

#Test 75 Dixon's Q test
sample<-c(0.189,0.167,0.187,0.183,0.186,0.182,0.181,0.184,0.177)
dixon.test(sample)

#-------------------------------------------------------------------------------

#Test 76 Chi-squared Test for outliers
dependent.variable=c(3083,3140,3218,3239,3295,3374,3475,3569,3597,3725,3794)
independent.variable=c(75,78,80,82,84,88,93,97,65,104,109,115,120,127)
regression.model <- lm(dependent.variable ~ independent.variable)
residual <-rstudent(regression.model)
regression.model <- lm(dependent.variable ~ independent.variable)
residual <-rstudent(regression.model)
chisq.out.test(residual, variance=1)

#-------------------------------------------------------------------------------

#Test 77 Bonferroni outlier test

dependent.variable=c(3083,3140,3218,3239,3295,3374,3475,3569,3597,3725)
independent.variable=c(75,78,80,82,84,88,93,97,65,104,109,115,120,127)
outlierTest (lm(dependent.variable ~ independent.variable))


regression.model <- lm(dependent.variable ~ independent.variable)
residual <-rstandard(regression.model)
outlier(residual)

residual <-rstudent(regression.model)
outlier(residual)

#-------------------------------------------------------------------------------

#Test 78 Grubbs test
sample<-c(0.189,0.167,0.187,0.183,0.186,0.182,0.181,0.184,0.177)
grubbs.test(sample, type = 10, opposite = FALSE, two.sided = TRUE)

#-------------------------------------------------------------------------------

#Test 79 Goldfeld-Quandt Test for heteroscedasticity
dependent.variable=c(3083,3140,3218,3239,3295,3374,3475,3569,3597,3725)
independent.variable=c(75,78,80,82,84,88,93,97,99,104,109,115,120,127)
gqtest (lm(dependent.variable ~ independent.variable))

#-------------------------------------------------------------------------------

#Test 80 Breusch-Pagan Test for heteroscedasticity
dependent.variable=c(3083,3140,3218,3239,3295,3374,3475,3569,3597,3725,3794,39)
independent.variable=c(75,78,80,82,84,88,93,97,99,104,109,115,120,127)
ncvTest (lm(dependent.variable ~ independent.variable))

bptest (lm(dependent.variable ~ independent.variable),studenze = FALSE)

bptest (lm(dependent.variable ~ independent.variable),studenze = TRUE)

#-------------------------------------------------------------------------------
remove(list = ls())
library(psych)
library(compOverlapCorr)
library(bgtest)

library(randtests)

library(UsingR)
library(BSDA)
library(DescTools)
library(trend)
library(tseries)
library(fBasics)
library(PairedData)
library(moments)

library(nortest)
library(lawstat)
library(outliers)

library(outlierTest)
library(lmtest)
library(car)
library(circular)

#Test 81 Harrison-McCabe Test for heteroskedasticity
dependent.variable=c(3083,3140,3218,3239,3295,3374,3475,3569,3597,3725,37)
independent.variable=c(75,78,80,82,84,88,93,97,99,104,109)
hmctest(lm(dependent.variable ~ independent.variable))

#-------------------------------------------------------------------------------

#Test 82 Harvey-Collier Test for linearity
dependent.variable=c(3083,3140,3218,3239,3295,3374,3475,3569,3597,3725,37)
independent.variable=c(75,78,80,82,84,88,93,97,99,104,109)
harvtest(dependent.variable~independent.variable)

#-------------------------------------------------------------------------------

#Test 83 Ramsey Reset test
dependent.variable=c(3083,3140,3218,3239,3295,3374,3475,3569,3597,3725,37)
independent.variable=c(75,78,80,82,84,88,93,97,99,104,109)
ind.2=c(5,8,0,2,4,8,3,7,9,10,10,15,12,12)
model <- lm(dep~ind.1+ind.2) 
resettest(model, power=2:3, type="regressor")

#-------------------------------------------------------------------------------

#Test 84 White neural network test
set.seed(1234)
white.test(diff(EuStockMarkets[,1],1)) # test DAX

data:  diff(EuStockMarkets[, 1], 1)
white.test(diff(EuStockMarkets[,1],1)) # test SMI
data:  diff(EuStockMarkets[, 1], 1)
white.test(diff(EuStockMarkets[,1],1)) # test CAC
data:  diff(EuStockMarkets[, 1], 1)
white.test(diff(EuStockMarkets[,1],1)) # test FTSE

#-------------------------------------------------------------------------------

#Test 85 Augmented Dickey-Fuller test

set.seed(1234)
data <- cumsum(rnorm(10000)) # contains a unit root
adf.test(data)

adf.test(data,alternative="explosive",k=10)

summary(ur.df(sunspots, type = "none",selectlags = "AIC") )

#-------------------------------------------------------------------------------

#Test 86 Phillips-Perron test
set.seed(1234)
data <- cumsum(rnorm(10000)) # contains a unit root
PP.test(data)

set.seed(1234)
data <- cumsum(rnorm(10000))
diff_data = diff(data,1) # unit root removed
PP.test(diff_data)

#-------------------------------------------------------------------------------

#Test 87 Phillips-Ouliaris test
po.test(diff(log(EuStockMarkets),1),demean = TRUE)

data:  diff(log(EuStockMarkets), 1)

#-------------------------------------------------------------------------------

#Test 88 Kwiatkowski-Phillips-Schmidt-Shin test
set.seed(1234)
x <- rnorm(7000)
kpss.test(x, null = "Level")

data:  x

kpss.test(x, null = "Trend")

DAX<-EuStockMarkets[,1]
diff_DAX = diff(DAX,1)
kpss.test(diff_DAX, null = "Trend")
data:  diff_DAX

kpss.test(diff_DAX, null = "Level")

#-------------------------------------------------------------------------------

#Test 89 Elliott, Rothenberg & Stock test
set.seed(1234)
x <- rnorm(7000)
summary(ur.ers(x, type="DF-GLS", model= "trend", lag.max=4))

lm(formula = dfgls.form, data = data.dfgls)

#-------------------------------------------------------------------------------

#Test 90 Schmidt - Phillips test
summary(ur.sp(sunspots, type="tau", pol.deg=1, signif=0.05))

lm(formula = sp.data)

#-------------------------------------------------------------------------------

#Test 91 Zivot and Andrews test
data(USeconomic)
m1<- diff(USeconomic[,1],1)
summary(ur.za(m1, model="both", lag=3))

#-------------------------------------------------------------------------------

#Test 92 Grambsch-Therneau Test of proportionality
sample <- list(time=c(3,3,3,4,3,1,1,2,2,3,3,4),
               status=c(0,0,1,1,1,1,0,1,1,0,0,1),
               factor.1=c(2,2,1,0,2,1,1,1,0,0,0,0),
               factor.2=c(1,0,0,0,0,0,0,1,1,1,1,1))
fit <- coxph(Surv(time, status) ~ factor.1 + factor.2, sample)
cox.zph(fit)

#-------------------------------------------------------------------------------

#Test 93 Mantel-Haenszel log-rank test
time <- c(13, 18, 28, 26, 21, 22, 24, 25, 10, 13, 15, 16, 17, 19, 25, 32)#monthstatus <- c(1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1)
treatment.group <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
sex <- c(1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 2, 1, 1, 1, 2, 2)# 1 = male
survdiff(Surv(time, status) ~ treatment.group, rho=0)
survdiff(formula = Surv(time, status) ~ treatment.group, rho = 0)

#-------------------------------------------------------------------------------

#Test 94 Peto and Peto test
install.packages('survival')
require(survival)
time <- c(13, 18, 28, 26, 21, 22, 24, 25, 10, 13, 15, 16, 17, 19, 25, 32)#months
status <- c(1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1)
treatment.group <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
sex <- c(1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 2, 1, 1, 1, 2, 2)# 1 = male
survdiff(Surv(time, status) ~ treatment.group, rho=1)
survdiff(formula = Surv(time, status) ~ treatment.group, rho = 1)
#-------------------------------------------------------------------------------

#Test 95 Kuiper's Test of uniformity
turtles_radan<- 0.0174532925*turtles[,2] # convert degrees to radans
kuiper(turtles_radan)

kuiper.test(turtles_radan)

#-------------------------------------------------------------------------------

#Test 96 Rao's spacing Test of uniformity
rao.spacing.test(turtles[,2])

rao.spacing.test(turtles_radan)

rao.spacing.test(turtles_radan)

#-------------------------------------------------------------------------------

#Test 97 Rayleigh Test of uniformity
ants<- as.numeric (fisherB10[[1]])
r.test(ants,degree=TRUE)$r.bar

ants_radians<-0.0174532925*circular(ants)
rayleigh.test(ants_radians)

#-------------------------------------------------------------------------------

#Test 98 Watson's goodness of fit test
ants<- as.numeric (fisherB10[[1]])
ants_radians<-0.0174532925*circular(ants)
watson(ants,dist='vm')

#-------------------------------------------------------------------------------

#Test 99 Watson's two-sample Test of homogeneity
sample <- split(swallows$heading, swallows$treatment)
treatment<-circular(as.numeric (sample[[2]]) *0.0174532925)
control<-circular(as.numeric (sample[[1]]) *0.0174532925)
watson.two(control,treatment, plot=FALSE)

watson.two.test(control,treatment)

#-------------------------------------------------------------------------------

#Test 100 Rao's Test for homogeneity
set.seed(1234)
w <- list(rvonmises(300, circular(0), kappa=10))
x <- list(rvonmises(300, circular(0), kappa=20))
y <- list(rvonmises(300, circular(0), kappa=10))
z <- list(rvonmises(300, circular(0), kappa=10))
sample<-c(w,x,y,z)
rao.homogeneity(sample)

rao.test(w,x,y,z)

#-------------------------------------------------------------------------------

#Test 101 Pearson Chi square test
sample <-c(-1.441,-0.642,0.243,0.154,-0.325,-0.316,0.337,-0.028,1.359,-
             1.67,-0.42,1.02,-1.15,0.69,-1.18,2.22,1,-1.83,0.01,-0.77,-0.75,-1.55,-
             1.44,0.58,0.16)
pchiTest (sample)

pearson.test(sample)



#-------------------------------------------------------------------------------

