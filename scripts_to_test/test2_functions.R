insertion_sort_function <- function(A){
  for (j in 2:length(A)) {
    key = A[j] 
    # insert A[j] into sorted sequence A[1,...,j-1] 
    i = j - 1 
    while (i > 0 && A[i] > key) {
      A[(i + 1)] = A[i]
      i = i - 1 
    }
    A[(i + 1)] = key
  }
  A
} 

sampled_data=c(1,4,2,3,0,6,5,4)

#   ---                                  ----
# | sampled_data  |   <----c()---- |1,4,2,3,0,6,5,4 | 
#   ---                                  -----


A=insertion_sort_function(sampled_data)
#   ---                                         ----
# | A  |   <----insertion_sort_function---- |sampled_data | 
#   ---                                          -----

#color code for function,insertion_sort_function