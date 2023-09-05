systematic_sampling = function(x , N = length(x) , n , r = sample(1:N/n))
{
  k = N/n
  samp = c()
  for(j in 0:(n-1))
  {
    samp = c(samp , x[r + j*k])
  }
  final = list(samp , mean(samp))
  return(final)
}
all_posible_systematic_sampling = function(x , N = length(x) , n)
{
  k = N/n
  f = c()
  for(r in 1:k)
  {
    z = systematic_sampling(x , n = n , r = r)
    cat("r = ",r," => ")
    f = rbind(f , z[[1]])
    cat(z[[1]])
    cat(" => mean = ", mean(z[[1]]) , " => s2 ", var(z[[1]]) , " => sigma2 ", (n-1)*var(z[[1]])/n)
    cat("\n")
  }
  return(f)
}
systematic_correlation = function(x , N = length(x) , n)
{
  result = all_posible_systematic_sampling(x , N = length(x) , n)
  result = as.matrix(result)
  mat = result - mean(x)
  print(mat)
  k = N/n
  s = 0
  for(r in 1:k)
  {
    for(i in 1:n)
    {
      for(j in 1:n)
      {
        if(i != j)
        {
            s = s + mat[r,i]*mat[r,j]
        }
      }
    }
  }
  ps = s / ((n-1)*(N-1)*var(x))
  cat("Ps is ",ps)
}
systematic_correlation_estimate_sample = function(x)
{
  A = x - mean(x)
  print(A)
  s = 0
  n = length(x)
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      if(i!=j)
      {
        s = s + A[i]*A[j]
      }
    }
  }
  rs = s / (n*(n-1)*var(x))
  cat("rs is ",rs)
}
x = c(3,4,1,8,5,2,4,7,6,12,3,5,10,11,12,18,2,17)
systematic_sampling(x , n = 6 , r = 2)
systematic_sampling(x , n = 6 , r = 1)
systematic_sampling(x , n = 6 , r = 3)
mean(x)
y = 1:30
systematic_sampling(y , n = 5 , r = 4)
all_posible_systematic_sampling(x , n=6)
x = c(1,2,3,0,2,4,3,4,2,1,0,2)
all_posible_systematic_sampling(x , n = 4)
systematic_correlation(x , n = 4)
y = c(1,0,3,1)
systematic_correlation_estimate_sample(y)
x = c(1,2,5,7,9,3,8,1,4,5,9,6)
all_posible_systematic_sampling(x , n = 4)
systematic_correlation(x , n = 4)
x = c(1,3,5,2,9)
systematic_correlation_estimate_sample(x)
var(x)
