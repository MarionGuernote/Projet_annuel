# Create a vector filled with random normal values
u1 <- rnorm(30)
print("This loop calculates the square of the first 10 elements of vector u1")

# Initialize `usq`
usq <- 0

for(i in 1:10) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  usq[i] <- u1[i]*u1[i]
  print(usq[i])
}



#                 FOR 1:10
#                 ui[1]*u1[i]
#                 ---------
#                 |       |
#                 |       ---                    
#                 |--->|  usq  |
#                         ---                 
#
#
#


# Create a 30 x 30 matrix (of 30 rows and 30 columns)
mymat <- matrix(nrow=30, ncol=30)

#
#
#   ---                         ---    
# | mymat  |   <--matrix-----| 30,30 |  
#   ---                          ---   
#or
#   ---                        -----        
# | mymat  |   <--30,30-----| matrix_init | 
#   ---                         -----     
#to discuss in issue with terry (simple assignation)
#
#
#
# For each row and for each column, assign values based on position: product of two indexes
for(i in 1:dim(mymat)[1]) {
  for(j in 1:dim(mymat)[2]) {
    mymat[i,j] = i*j
  }
}

# Just show the upper left 10x10 chunk
mymat[1:10, 1:10]



#                 FOR i in dim(mymat)[1]
#                 ---------#                 
#                 FOR j in dim(mymat)[2]
#                 ---------
#                 |       |
#                 |                          ---                    
#                 |--->  |i*j|   ------>   |  usq  |
#                                            ---                 
#
#Pas mal de rajouter un noeud qui représente le calcul effectué


####exemple précedent
#                 FOR i in 1:10
#                 ---------
#                 |       |
#                 |                              ---                    
#                 |--->   |ui[1]*u1[i]| -----> | usq  |
#                                                 ---                 
#
#
#avec code couleur... 


