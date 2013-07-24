
rm(list=ls())

# Read the data  
x = as.matrix(read.table("ex2x.dat",header=FALSE,sep="\n"))
y = as.matrix(read.table("ex2y.dat"))

# Visualizing the data 
plot(x,y)

# Augment the feature with a bias term
x = cbind(rep(1,dim(x)[1]),x)

# initialize the weights
theta = c(0,0)

# flag to stop looping
flag = TRUE
 
theta_old = theta
alpha = 0.07
counter = 1

update_term = NULL

while(flag)
{
  # theta' * x 
  first_term = ((theta)%*%t(x)) 
  # Removing the ingleton dimensions from the matrix
  first_term = first_term[,]
  
  update_term[1] = sum((first_term - y)*x[,1])*alpha/dim(x)[1]
  update_term[2] = sum((first_term - y)*x[,2])*alpha/dim(x)[1]
  
  theta = theta_old - update_term
  
  # To observe the number of iterations
  counter = counter+1
  
  cat(counter)
  cat("\n")
  
  # Check for convergence
  conv = norm(as.matrix(theta - theta_old)/(theta+0.001))
  theta_old = theta
  
  if(conv < 1e-10)
  {
    flag = FALSE
  }  
}

abline(theta)



