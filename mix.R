data <- read_excel("final-project-yuance-yiyi-group-project/5361 Table.xlsx")
x <- data$charges
n <- 574
tau1 <- tau2 <- rep(0,574)
pi1 <- pi2 <- 0.5
mu1<-7200
mu2<-29109
sigma1<-sqrt(26893073)
sigma2<-sqrt(74741505)
for (i in 1:1000){
  for (j in 1:574) {
    tau1[j] <- pi1*dnorm(x[j],mean=mu1,sd=sigma1)/(pi1*dnorm(x[j],mean=mu1,sd=sigma1)+pi2*dnorm(x[j],mean=mu2,sd=sigma2))
    tau2[j] <- 1-tau1[j]
  }
  pi1.new<-sum(tau1)/574
  pi2.new<-sum(tau2)/574
  
  if (abs(pi1.new - pi1) < 0.0001){
    pi <- c(pi1.new, pi2.new)
    iter <- i
    return(list(pi,iter))
  }
  pi1 <- pi1.new
  pi2 <- pi2.new
}
