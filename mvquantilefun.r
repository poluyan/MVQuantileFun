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

col = col/sum(col)

col <- c(0, cumsum(col))

#write.csv(col)

row <- matrix(nrow=n, ncol=n + 1) 
for(i in 1:n)
{
  t <- pdf[i,]
  t <- t/sum(t)
  t <- cumsum(t)

  row[i,1] <- 0
  for(j in 2:(n+1))
    row[i,j] <- t[j-1]
}


n = 1000 # number of sample
random = NULL
sample = NULL
for(i in 1:n)
{
  r1 <- runif(1, 0, 1)
  r2 <- runif(1, 0, 1)

  # linear interpolation
  index1 <- which(col == min(col[col>r1])) - 1
  x0 <- x[index1]
  y0 <- col[index1]
  x1 <- x[index1 + 1]
  y1 <- col[index1 + 1];
  fx <- x0 + (r1 - y0) * (x1 - x0) / (y1 - y0);

  index2 <- which(row[index1,] == min(row[index1,][row[index1,]>r2])) - 1
  x0 <- y[index2]
  y0 <- row[index1,index2]
  x1 <- y[index2 + 1]
  y1 <- row[index1,index2 + 1]
  fy <- x0 + (r2 - y0) * (x1 - x0) / (y1 - y0)
	
  random = rbind (random,  cbind (r1, r2))
  sample = rbind (sample,  cbind (fx, fy))
}
plot(random)
plot(sample)