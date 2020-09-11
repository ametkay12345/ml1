euclidian_dist <- function(u, v)
{
    return (sqrt(sum((u - v)^2)))
}


nn <- function(data, u)
{
    # This function gets the data and a point and returns
    # the class of the nearest one.
    row <- dim(data)[1]
    col <- dim(data)[2]
    min_dist <- Inf
    nearest <- -1
    for(i in 1:row)
    {
        curr_dist <- euclidian_dist(data[i, 1:col-1], u)
        if(curr_dist < min_dist)
        {
            min_dist <- curr_dist
            nearest <- i
        }
    }

    # returns a nearest neigbor or -1 if there are no neigbors
    return (data[nearest, col])
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
    for(i in 1:10)
    {
        answer <- nn(train, c(test[i, 1], test[i, 2]))
        if(answer == -1)
        {
            print('There are no neigbors')
            break
        }
        points(test[i, 1], test[i, 2], pch = 22, bg = colors[answer], asp = 1)
    }
}


main()


