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


knn <- function(data, u, k=1)
{
  # This function gets the data and a point and returns
  # the class of the nearest one.
  row <- dim(data)[1]
  col <- dim(data)[2]
  data <- sort_by_dist(data, u)
  classes <- data[1:k, col]
  counts <- table(classes)
  answer <- names(which.max(counts))
  return (answer)
}


main <- function()
{
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  train <- iris[,3:5]
  
  # drawing a plot of petal parameters
  plot(train[1:2], pch = 21, bg = colors[iris$Species],
      col = colors[iris$Species], asp = 1)
  
  # generate 10 tests
  test <- cbind(runif(10, min=0.1, max=6.9),
                runif(10, min=0.1, max=2.4))
  # testing knn for k=1
  k <- 1
  for(i in 1:10)
  {
    answer <- knn(train, c(test[i, 1], test[i, 2]), k)
    if(answer == -1)
    {
      print('There are no neigbors')
      break
    }
    points(test[i, 1], test[i, 2], pch = 22, bg = colors[answer], asp = 1)
  }
}


main()