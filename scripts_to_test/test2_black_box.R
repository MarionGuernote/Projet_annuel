#input list

A=c(1,4,2,3,0,6,5,4)


#   ---               --------------
# | A |   <----c()--- | 1,4,2,3,0,6,5,4 | 
#   ---               --------------


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
print(A)


# for loop
#                             --------------
#                  for < ---- | 2:length(A) | 
#                   ¦         -------------
#                   ▽
#   ---           -----         ----
# | key  |   <----| j |  <----- | A | 
#   ---           -----         -----
#                  ¦
#                  -1       
#                  ¦              
#                  ▽
#                -----
#                  i
#                ----
# while pretty complex to incorporate
## so if for and while make a black box

#   ---           -----         
# | A  |   <---- ¦black box¦  <----- |for&while| 
#   ---           -----         


key = A[j]