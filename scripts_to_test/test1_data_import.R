
################GEPHY INFORMATION################

# here we work on the lung data set in R
# we are doing a survival analysis which is very common in R and in biology in general
# 

#here are the info on the lung dataset copied from
# https://stat.ethz.ch/R-manual/R-devel/library/survival/html/lung.html
#plz have a quick look


#What we want to do here
  # How many patients are there ?
  # How many male - female?
  # how many deaths ?
  # Extract each column ?
  # preparing the data
  # Fit a cox model with no covariate, the real analysis
  # Extract some info from it


################script################

data(lung)

#   ---                   ----
# | lung |   <----data--- | R | or something 
#   ---                   ----

#calls dataset called lung already installed with R
#the function data is implemented in R see 
#help(data)

###number of patients or samples
nb_samples=dim(lung)[1]
# get used to the dim function as it is sometimes followed by [] to iterate over it
# see help(dim)  // calling dim(dataset) returns number of rows,column

#   ---                              ----
# | nb_samples |   <----dim [2]--- | lung | or something 
#   ---                              ----
 
###rename data to find gender 
#first extract gender// 2 ways to do it
gender=lung$sex #best
gender=lung[,5] 
#   -----              ---         ----
# | gender |   <----- ¦sex¦ <---- | lung | 
#   -----              ---          -----

#   ---                      ----
# | gender |   <----[,5]--- | lung | 
#   ---                      ----

# find indices with simple which test
males_indices=which(gender=='1')
female_indices=which(gender=='2')

#   ---                               ------
# | males_indices |   <----(==1)--- | gender | 
#   ---                                ----


# replace number by gender using indices
gender[males_indices]="Male"
gender[female_indices]="Female"

#   ---                       --------------
# | gender |   <----Male--- | males_indices | 
#   ---                       --------------

###finding how many deaths
deaths=sum(lung$status==2)

###extract each column into variables
inst=lung$inst
#   ---               ------
# | inst |   <---$--- | lung | 
#   ---               ------

time=lung$time
status=lung$status
age=lung$age
sex=lung$sex
ph.ecog=lung$ph.ecog
ph.karno=lung$ph.karno
pat.karno=lung$pat.karno
meal.cal=lung$meal.cal
wt.loss=lung$wt.loss

##first problem
###attach function does it automatically without telling you explicitly.
attach(lung)
#does all of what is above
#you could store all variable you find into a dictionnary so that if you encounter a attach 
# attach(lung) #will create every variable including age
#you should know by seeing for example 
#old=which(age>50) 
#that age is used for the first time (because 1: right of = sign and 2: not in dico) that you need to create a new node from 
#age=lung$age as the name of the variable is kept from the original column name


###extract each column into variables

old_indices=which(age>50) 
old=rep(0,nb_samples)   
old[old_indices]=1

####watch closely here, we are using an existing node nb_samples
## see help(rep)
# you sould have 
#   ---                           -------
# | old |   <---nb_samples----- | rep(0) | 
#   ---                           -------
#    ^
#    ¦
#    ¦                        ---
#     ------old_indices----- | 1 | 
#                             ---


### survival model
formula=Surv(lung$time,lung$status)

#   -----                      
# | formula |   <--surv-- ¦time¦  
#      ^-----------surv-- ¦status¦   


fit <- survfit(formula ~ old, data = data.frame(lung))
#   -----                      
# | fit |   <--survfit ~ old-- ¦formula¦  
#   -----                      

fit_table <- coxph(formula ~ old)
info=summary(fit_table)

#   -----                     ----            
# | info |   <--summary--- ¦fit_table¦ 
#   -----                     ----          


pval=info$sctest[3]
#   -----               ----            ----
# | pval |   <--[3]--- ¦sctest¦ <---- | info | 
#   -----               ----           -----


## graphical representation 
plot(fit, col = c("blue","red"),lty =c(1,2))
## see help(plot)

# here you should have a simple
#   -----                                                          ----
# | plot 1 |   <--- parameters (ex colors,linetype (lty)) ----- | fit | 
#   -----                                                         -----

# parameters not very important