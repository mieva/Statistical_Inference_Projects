# Statistical Inference analysis script
# 1. SIMULATION

nosim <- 1000
#n <- 40

# The exponential distribution can be simulated in R with rexp(n, lambda) 
# where lambda is the rate parameter.
# The mean of exponential distribution is 1/lambda 
# and the standard deviation is also 1/lambda. 
# Set lambda = 0.2 for all of the simulations

lambda = 0.2
meanexp = 1/lambda
sigmaexp = 1/lambda

# CCL function calculation
cfunc <- function(x, n) sqrt(n) * (mean(x) - meanexp) / sigmaexp

# simulate
#set.seed(123) # bad!! disturd the simulation
dat <- data.frame(
        x = apply(matrix(rexp(40, lambda), nosim), 1, cfunc, 40)
)

# Open device
png(file = "StatisticalInferenceProject/plot_test.png")

#qplot(dat$x, geom="histogram",
#      binwidth = 5,  
#      main = "Histogram for mean", 
#      xlab = "mean values",  
#      fill=I("light blue"),
#      col=I("black"))

#g1 <- ggplot(data=dat, aes(dat$x)) + geom_histogram()
#g1 <- g1 + stat_function(fun = dnorm, size = 1)
ggplot(data=dat, aes(dat$x)) + 
        geom_histogram(aes(y = ..density..),
                       breaks=seq(-20, 20, by = 2),  
                       col="red", 
                       fill="green", 
                       alpha = .2) +
        
#        geom_density(col=2) + # follows the plot
        stat_function(fun = dnorm, size = 1) +
        
        labs(title = "Histogram for mean") +
        labs(x = "means", y="Count")

# Close the device
dev.off()

dat2 <- data.frame(
        x = c(apply(matrix(rexp(40, lambda), nosim), 1, cfunc, 40),
              apply(matrix(rexp(100, lambda), nosim), 1, cfunc, 100),
              apply(matrix(rexp(500, lambda), nosim), 1, cfunc, 500)
        ),
        size = factor(rep(c(40, 100, 500), rep(nosim, 3)))
)

# Open device
png(file = "StatisticalInferenceProject/plot2.png", width=800, height=400)

# plot
g <- ggplot(data=dat2, aes(dat2$x, fill = size)) + geom_histogram(binwidth=5, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)

# Close the device
dev.off()

######################################################## NON FUNGE!! LE DISTRIBUZIONI NON SONO GAUSSIANE
## pROVIAMO DIVERSAMENTE
nosim <- 1000
n <- 40

# The exponential distribution can be simulated in R with rexp(n, lambda) 
# where lambda is the rate parameter.
# The mean of exponential distribution is 1/lambda 
# and the standard deviation is also 1/lambda. 
# Set lambda = 0.2 for all of the simulations

lambda = 0.2
meanexp = 1/lambda
sigmaexp = 1/lambda

## Setting the seed of simulation
set.seed(1234)

dat <- replicate(nosim, mean(rexp(n, lambda)))
str(dat, 10)

h = hist(dat, prob=TRUE, main = "Histogram for simulated means", xlab = "Means of each sample")
curve(dnorm(x, mean=mean(dat), sd=sd(dat)), add=TRUE, col = "green")
curve(dnorm(x, mean=mean_theor, sd=sd_theor), add=TRUE, col = "red")

# mean of sample
mean_dat <- mean(dat)
mean_dat

## As known the theoretical mean of the distribution is supposed to be `1/lambda`
mean_theor<-1/lambda
mean_theor

# standard deviation
sd_dat <- sd(dat)
sd_dat
var_dat <- var(dat)
var_dat

## Calculation for the theoretical sd and variance for sampling data
sd_theor <- (1/lambda)/sqrt(40)
var_theor <- theor_sd^2

# CI
mean_dat + c(-1, 1)*qnorm(0.975)*sd_dat/sqrt(n)

## Compare sd vs theoretical NON TORNA!!!
dat2 <- replicate(nosim, sd(rexp(n, lambda)))
str(dat2, 10)
h2 = hist(dat2, prob=TRUE, main = "Histogram for simulated sd", xlab = "Standard deviation of each sample")
mean_dat2 <- mean(dat2)
mean_dat2
sd_dat2 <- sd(dat2)
sd_dat2
curve(dnorm(x, mean=sd(dat2), sd=sd(dat2)), add=TRUE, col = "green")

mean_dat2 + c(-1, 1)*qnorm(0.975)*sd_dat2/sqrt(n)

MSE <- (2/(n-1))*var_dat^4


# 2. Data analysis

library(datasets)
data(ToothGrowth)

str(ToothGrowth)
summary(ToothGrowth)

#attach(ToothGrowth) #??
#names(ToothGrowth) #non serve

# The response is the length of odontoblasts (teeth) in each of 10 guinea pigs 
# at each of three dose levels of Vitamin C (0.5, 1, and 2 mg) 
# with each of two delivery methods (orange juice or ascorbic acid).


plot(supp, len, main = "ToothGrowth data overview", xlab = "Type of delivery", ylab = "Lenght")
plot(factor(dose), len, main = "ToothGrowth data overview", xlab = "Dosage (mg)", ylab = "Lenght")

ggplot(data=ToothGrowth, aes(x=factor(dose), y=len))+geom_boxplot()+facet_grid(~supp)+ggtitle("Analyzing ToothGrowth data")

ggplot(data=ToothGrowth, aes(x=factor(dose), y=len))+geom_point()+facet_grid(~supp)+ggtitle("Analyzing ToothGrowth data")
# or
ggplot(data=ToothGrowth, aes(x=factor(dose), y=len))+geom_point() + geom_smooth(method = "lm", aes(group = 1))+facet_grid(~supp) + ggtitle("Analysis of Tooth Growth")+labs(x = "Vitamin C dose (mg)", y="Lenght")


## dummy variable for plot
#ToothGrowth$factor_supp_1 <- factor ( with ( ToothGrowth, ifelse ( ( supp == "OJ" ), 1 , 0 ) ) ) #??

## subset
#subdata <-subset(ToothGrowth, select = c(1,3,4)) #??

## plot
#with(ToothGrowth, plot(dose, len, col = supp)

sampleOJ_1<-subset(ToothGrowth, (dose==0.5 & supp == "OJ"))
sampleOJ_2<-subset(ToothGrowth, (dose==1.0 & supp == "OJ"))
sampleOJ_3<-subset(ToothGrowth, (dose==2.0 & supp == "OJ"))

sampleVC_1<-subset(ToothGrowth, (dose==0.5 & supp == "VC"))
sampleVC_2<-subset(ToothGrowth, (dose==1.0 & supp == "VC"))
sampleOJ_3<-subset(ToothGrowth, (dose==2.0 & supp == "VC"))

t.test(sampleOJ_2$len, sampleOJ_1$len, paired = FALSE, var.equal = FALSE)$conf
t.test(sampleOJ_3$len, sampleOJ_1$len, paired = FALSE, var.equal = FALSE)$conf
t.test(sampleOJ_3$len, sampleOJ_2$len, paired = FALSE, var.equal = FALSE)$conf

t.test(sampleVC_2$len, sampleVC_1$len, paired = FALSE, var.equal = FALSE)$conf
t.test(sampleVC_3$len, sampleVC_1$len, paired = FALSE, var.equal = FALSE)$conf
t.test(sampleVC_3$len, sampleVC_2$len, paired = FALSE, var.equal = FALSE)$conf

t.test(sampleOJ_1$len, sampleVC_1$len, paired = FALSE, var.equal = FALSE)$conf
t.test(sampleOJ_2$len, sampleVC_2$len, paired = FALSE, var.equal = FALSE)$conf
t.test(sampleOJ_3$len, sampleOJ_3$len, paired = FALSE, var.equal = FALSE)$conf






