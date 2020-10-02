<h1> 1NN </h1>
Эта программа представляет собой реализацию алгоритма одного ближайшего соседа (1NN) для задачи классификации на датасете Iris на языке R.
Принцип алгоритма состоит в том, чтобы используя гиоптезу компактности, определить класс объекта, в зависимости от других объектов 
в признаковом пространстве. Метрика Евклидова.

Код алгоритма на языке R:
```R
nn <- function(data, u)
{
    # This function gets the data and a point and returns
    # the class of the nearest one or -1 if there are no neigbors.
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
    return (data[nearest, col])
}
```
Результат программы отображается на графике, где круглые точки - это объекты из тренировочной выборки датасета Iris,
а квадратные точки - это объекты из тестововй выборки.
<img src='/img/plot.png'></img>

<h1> Поиск оптимального k для алгоритма kNN </h1>
Для поиска опитмального k используется алгоритм скользящего окна LOO (Leave-one-out). Для каждого k
находится ошибка его классификации. В качестве ответа выбирается k с минимальной ошибкой классификации.

Код алгоритма на языке R:
```R
find_loo <- function(data)
{
    # This function finds the best k for kNN algorithm.
    
    # dimensions 
    row <- dim(data)[1]  
    col <- dim(data)[2]
    loo <- matrix(0, row - 1, 1)  # matrix of errors for each k

    for(i in 1:row)
    {
        # take a data w/o ith element
        new_data <- data[-i, ]
        # sort by distances
        new_data <- sort_data_by_dist(new_data, data[i, 1:col-1])
        # building loo 
        for(k in 1:(row-1))
        {
            classes <- new_data[1:k, col]
            counts <- table(classes)
            answer <- names(which.max(counts))
            # check error
            if(data[i, 3] != answer)
                loo[k] = loo[k] + 1
        }
    }

    return (loo)
}
```

В качестве примера был выбран датасет Iris. График результата выполнения алгоритма LOO для каждого k в 
диапазоне от 1 до 149.

<img src='/img/LOO_plot.png'></img>
