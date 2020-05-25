
library(formatR)
library(RCy3)
library(stringr) 
library(tidyverse)
source("D:/Ordi/Documents/GitHub/Projet_annuel/SimpleAssignationCytoscape.R")

#take the access path of the script to treat 
scriptToGraph <- function(script) {

   formatR::tidy_file(script)
   p_data = parse(script)
   
   #parse script
   parsed = getParseData(p_data)
   
   #removing unwanted/empty/unknown lines 
   parsed = parsed[-c(which(parsed$token %in% c("expr", "equal_assign", "forcond"))), ]
   xy.list <- split(parsed, f = parsed$line1)
   
   #excluding system commands 
   to_remove = c("rm", "cat", "gc", "setwd")
   xy.list.sc = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) %in% to_remove)]
   xy.list = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) %in% to_remove) == F]
   
   
   #extracts the size of the longest line (use to create table results (token and content))
   lmax = 0
   for (i in 1:length(xy.list)) {
      l = length(xy.list[[i]]$token)
      if (l > lmax){
         lmax =l
      }
   }
   
   #extracting comments
   xy.list.comments = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 7), `[[`, 1) == "COMMENT")]
   xy.list = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 7), `[[`, 1) == "COMMENT") == F]
   
   
   #extracting packages imports
   xy.list.packages = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) == "library")]
   packages = as.character(lapply(lapply(xy.list.packages, `[[`, 9), `[[`, 3))
   xy.list = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) == "library") == F]
   l.packages=rep("library",length(packages))
   packages=cbind(l.packages,packages)

   
   
   #assignation of variable with an operation or a direct attribution
   ind_eq = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 7), `[[`, 1) == "SYMBOL")]   #retrieves the lines that start with a symbol
   ind_eq = ind_eq[as.logical(lapply(lapply(ind_eq, `[[`, 7), `[[`, 2) %in% c("EQ_ASSIGN",'LEFT_ASSIGN'))]  
   operationAssign = ind_eq[as.logical(lapply(lapply(ind_eq, `[[`, 7), `[[`, 3) %in% c("NUM_CONST", "STR_CONST"))]
   tmp_ind = match( operationAssign, xy.list)
   xy.list.withoutassi = xy.list
   xy.list.withoutassi[tmp_ind] <- NULL
   
   tmp_ind = which(lengths(lapply(xy.list.withoutassi, `[[`, 7)) == 3)
   ind_eq = which(lapply(lapply(xy.list.withoutassi[tmp_ind], `[[`, 7), `[[`, 2) %in% c("EQ_ASSIGN",'LEFT_ASSIGN'))
   simpleAssignation = xy.list.withoutassi[tmp_ind][ind_eq]
   xy.list.withoutassi[tmp_ind[ind_eq]] <- NULL
   
   simpleAssignation = c(simpleAssignation , operationAssign)
   
   
   
   tabsimpleAssignation = c()
   tokentabsimpleAssignation = c()
   for (i in 1:length(simpleAssignation)) {
      tabsimpleAssignation = rbind(tabsimpleAssignation, c( simpleAssignation[[i]]$text[c(1:lmax)] ) )
      tokentabsimpleAssignation = rbind(tokentabsimpleAssignation, c( simpleAssignation[[i]]$token[c(1:lmax)] ) )
   }
   
   
   SimpleAssignationCy(tabsimpleAssignation,tokentabsimpleAssignation) 
   
   
   
   token = c()
   for (i in 1:length(xy.list)) {
      #l = length(xy.list[[i]]$token)
      token = rbind(token, c( xy.list[[i]]$token[c(1:lmax)] ) )
   }   
   
   lineif = c()
   token[is.na(token)] <- 0
   nbop = 0
   nbcl = 0
   i = 1   
   while (i < nrow(token)) {
      l = data.frame(token[i,])
      
      if (l[1,] == "IF"){
         lineif = rbind(lineif, paste(toString(i), "begin", sep= ','))   
         
         for (n in (i+1):nrow(token)){
            l = data.frame(token[n,])
            
            if (l[1,] == "IF" | l[1,] == "FOR" ){
               nbop = nbop + 1
            }
            if (l[1,] == "'}'"){
               nbcl = nbcl + 1
            }
            if (nbcl == (nbop +1)) {
               lineif = rbind(lineif, paste(toString(n), "end", sep= ','))
               break         
            }  
            lineif = rbind(lineif, n)
            i = n
         }
         nbop = 0 
         nbcl = 0
      }
      i = i + 1
   }  
   
   
   contentif = c()
   tokenif = c()
   for (i in 1:nrow(lineif)){
      split = strsplit( lineif[i,] , ",")
      val =  as.integer(split[[1]][1])
      contentif = rbind(contentif, c( xy.list[[val]]$text[c(1:lmax)]) ) 
      tokenif = rbind(tokenif, c( xy.list[[val]]$token[c(1:lmax)]) ) 
   }
   
   
   
   
   
   linefor = c()
   nbop = 0
   nbcl = 0
   i = 1   
   while (i < nrow(token)) {
      l = data.frame(token[i,])
      
      if (l[1,] == "FOR"){
         linefor = rbind(linefor, paste(toString(i), "begin", sep= ','))   
         
         for (n in (i+1):nrow(token)){
            l = data.frame(token[n,])
            
            if (l[1,] == "IF" | l[1,] == "FOR" ){
               nbop = nbop + 1
            }
            if (l[1,] == "'}'"){
               nbcl = nbcl + 1
            }
            if (nbcl == (nbop +1)) {
               linefor = rbind(linefor, paste(toString(n), "end", sep= ','))
               break         
            }  
            linefor = rbind(linefor, n)
            i = n
         }
         nbop = 0 
         nbcl = 0
      }
      i = i + 1
   }  
   
   
   contentfor = c()
   tokenfor = c()
   for (i in 1:nrow(linefor)){
      split = strsplit( linefor[i,] , ",")
      val =  as.integer(split[[1]][1])
      contentfor = rbind(contentfor, c( xy.list[[val]]$text[c(1:lmax)]) ) 
      tokenfor = rbind(tokenfor, c( xy.list[[val]]$token[c(1:lmax)]) ) 
   }   
   
   
    
   
}

