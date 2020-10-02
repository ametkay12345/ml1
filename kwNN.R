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


get_weight_arr <- function(k)
{
    w <- c(1, k)
    for(i in 1:k)
        w[i] <- ((k + 1 - i) / k)
    return (w)
}


kwnn <- function(data, u, k=1)
{
    row <- dim(data)[1]
    col <- dim(data)[2] 
    w <- get_weight_arr(k)
    # number of classes
    classes <- unique(data[, col])
    return (0)
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
    w <- get_weight_arr(8)
    print())
}


main()