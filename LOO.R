euclidian_dist <- function(u, v)
{
  return (sqrt(sum((u - v)^2)))
}


sort_by_dist <- function(data, u)
{
  row <- dim(data)[1]
  col <- dim(data)[2]
  dst <- matrix(NA, row, 2)
  for(i in 1:row)
  {
    curr_dist <- euclidian_dist(data[i, 1:col-1], u)
    dst[i, ] <- c(i, curr_dist)  
  }
  ordered_data <- data[order(dst[, 2]), ]
  return (ordered_data)
}


find_loo <- function(data)
{
  row <- dim(data)[1]
  col <- dim(data)[2]
  loo <- matrix(0, row - 1, 1)
  
  for(i in 1:row)
  {
    new_data <- data[-i, ]
    new_data <- sort_data_by_dist(new_data, data[i, 1:col-1])
    
    for(k in 1:(row-1))
    {
      classes <- new_data[1:k, col]
      counts <- table(classes)
      answer <- names(which.max(counts))
      
      if(data[i, 3] != answer)
        loo[k] = loo[k] + 1
    }
  }
  
  return (loo)
}


main <- function() 
{
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  train <- iris[,3:5]
  loo <- find_loo(train)
  plot(loo, type="l")
  print(which.min(loo))
}


main()
