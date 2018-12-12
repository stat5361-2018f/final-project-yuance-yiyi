

fnorm <- function(x,theta1,theta2){dnorm(x,theta1[1],theta1[1])-dnorm(x,theta2[2],theta2[2])}
cut <- function(x,theta1,theta2) uniroot(fnorm,interval = c(theta[1],theta[2]))$root
llk <- function(x,theta){
  mu <- theta[1]
  sigma <- theta[2]
  sum(log(dnorm(x,mu,sigma)))
}
theta1 <- c(2500,50)
theta2 <- c(5000,100)
data <- read.table(file.choose())
datans <- data$charges(<=cut)
datas <- data$charges(>cut)
llk.optimns <- optim(theta1,llk)$par
