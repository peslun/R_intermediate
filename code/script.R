# init ----

install.packages("renv")
# Initialize renv for the project
library(renv)
renv::init()
###say yes


# instructions ----

# Install required packages
renv::install(c("ggplot2", "dplyr", "tidyr"))

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Create a dataset with additional variables
set.seed(456)
data <- data.frame(
  x = rnorm(100),
  category = sample(c("A", "B", "C"), 100, replace = TRUE)
) %>% mutate(y = 2 * x + rnorm(100))

# Perform data manipulation using tidyverse
data_summary <- data %>%
  group_by(category) %>%
  summarise(
    mean_x = mean(x),
    sd_y = sd(y),
    n = n()
  )

# Print the summary table
print(data_summary)

# creation of a package ----
renv::install("devtools")
renv::snapshot()
library(usethis)
?create_package
create_package("~/app/rstudio/Rfirstpackage")
#everything else in Rfirstpackage

# sort ----
selection_sort <- function(arr) {
  n <- length(arr)
  for(i in 1:(n-1)){
    min_index <- i
    for(j in (i+1):n){
      if(arr[j] < arr[min_index]){
        min_index <- j
      }
    }
    if(i != min_index){
      temp <- arr[i]
      arr[i] <- arr[min_index]
      arr[min_index] <- temp
    }
  }
  return(arr)
}

sorted_array <- selection_sort(sample(letters))

# compare ----
## Bubble sort ----
bubble_sort <- function(arr) {
  n <- length(arr)
  for (i in 1:(n - 1)) {
    for (j in 1:(n - i)) {
      if (arr[j] > arr[j + 1]) {
        # Swap elements arr[j] and arr[j + 1]
        temp <- arr[j]
        arr[j] <- arr[j + 1]
        arr[j + 1] <- temp
      }
    }
  }
  return(arr)
}

## Insertion sort ----
insertion_sort <- function(arr) {
  n <- length(arr)
  for (i in 2:n) {
    key <- arr[i]
    j <- i - 1
    # Move elements of arr[1..i-1] that are greater than key
    # to one position ahead of their current position
    while (j >= 1 && arr[j] > key) {
      arr[j + 1] <- arr[j]
      j <- j - 1
    }
    arr[j + 1] <- key
  }
  return(arr)
}

## Quick sort ----
quick_sort <- function(arr) {   
  if (length(arr) <= 1) {
    return(arr)
  }
  # Choose a pivot element (middle of the array)  
  pivot <- arr[length(arr) %/% 2] 
  # Elements smaller than the pivot 
  left <- arr[arr < pivot]        
  # Elements equal to the pivot 
  middle <- arr[arr == pivot]     
  # Elements greater than the pivot
  right <- arr[arr > pivot]       
  return(c(quick_sort(left), middle, quick_sort(right)))
}

## comparisons ----
check_algorithm <- function(algorithm,arr,number){
  time <- list()
  for(a in 1:length(algorithm)){
    tmp_result <- c()
    for(b in 1:number){
      tmp_time <- Sys.time()
      algorithm[[a]](arr)
      time_diff <- Sys.time() - tmp_time
      tmp_result <- c(tmp_result, time_diff)
    }
    time[[a]] <- tmp_result
  }
  return(time)
}

check_algorithm(list(bubble_sort, insertion_sort, quick_sort), sample(1:1000), 10)

# review exam from christina ----
renv::install("remotes")
remotes::install_github("chrtsa/imagealgorithm@master")
library(imagealgorithm)
ls("package:imagealgorithm")
?filter_bitmap
filter_bitmap
filter_bw
filter_bitmap("~/edu/phd/swe/0.course/day5.exam/Test image-1.jpg")
filter_blue("~/edu/phd/swe/0.course/day5.exam/Test image-1.jpg")
filter_bw("~/edu/phd/swe/0.course/day5.exam/Test image-1.jpg")
