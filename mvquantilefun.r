set.seed(1)
n = 100
x = seq(-10,10,length=n)
y = seq(-10,10,length=n)

# exponential centers
centers = NULL
centers = rbind (centers,  cbind (3, 3))
centers = rbind (centers,  cbind (-5, 0))
centers = rbind (centers,  cbind (0, -5))

# PDF function
threeExp = function (x,y)
{
  rez <- 0
  rez <- rez + exp(-((x - centers[1,1])^2 + (y - centers[1,2])^2)*0.75)
  rez <- rez + exp(-((x - centers[2,1])^2 + (y - centers[2,2])^2)*0.5)*0.75
  rez <- rez + exp(-((x - centers[3,1])^2 + (y - centers[3,2])^2)*0.25)*0.5
  rez
}

# PDF
persp( outer( x, y, Vectorize(threeExp) ) )

# discretization
pdf <- matrix(nrow=n, ncol=n) 
for(i in 1:n)
{
  for(j in 1:n)
    pdf[i,j] <- threeExp(x[i],y[j])
}

cdf <- matrix(nrow=n, ncol=n)
for(i in 1:n)
  cdf[i,] <- cumsum(pdf[i,])

col <- cdf[,ncol(cdf)]