#take the access path of the script to treat 
simpleAssignation <- function(script) {
   
   library(formatR)
   library(RCy3)
   library(stringr) 

   script = "test.R"
   
   formatR::tidy_file(script)
   p_data = parse(script)
   
   #parse script
   parsed = getParseData(p_data)
   
   #removing unwanted/empty/unknown lines 
   parsed = parsed[-c(which(parsed$token %in% c("expr", "equal_assign", "forcond"))), ]
   xy.list <- split(parsed, f = parsed$line1)
   
   #search line where functions are not ended
   # lmax = 0
   # for (i in 1:length(xy.list)) {
   #    l = length(xy.list[[i]]$token)
   #    if (l > lmax){
   #       lmax =l
   #    }
   # }
   # 
   # token = c()
   # for (i in 1:length(xy.list)) {
   #    #l = length(xy.list[[i]]$token)
   #    token = rbind(token, c( xy.list[[i]]$token[c(1:lmax)] ) )
   # }   
   # 
   # notok = c()
   # token[is.na(token)] <- 0
   # nbop = 0
   # nbcl = 0
   # for (i in 1:nrow(token)) {
   #    l = data.frame(token[35,])
   #    
   #    for (n in 1:nrow(l)) {
   #       if (l[n,] == "'('"){
   #          nbop = nbop + 1
   #       }
   #       if (l[n,] == "'('"){
   #          nbcl = nbcl + 1
   #       }
   #       
   #       if (nbop != nbcl){
   #          notok = rbind(notok, c(i))   
   #       }
   #       nbop = 0
   #       nbcl = 0
   #    }   
   # }   
   
   
   #extracting comments
   xy.list.comments = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 7), `[[`, 1) == "COMMENT")]
   xy.list = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 7), `[[`, 1) == "COMMENT") == F]
   
   #extracting packages imports
   xy.list.packages = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) == "library")]
   packages = as.character(lapply(lapply(xy.list.packages, `[[`, 9), `[[`, 3))
   xy.list = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) == "library") == F]
   l.packages=rep("library",length(packages))
   packages=cbind(l.packages,packages)
   
   #excluding system commands 
   to_remove = c("rm", "cat", "gc", "setwd")
   xy.list.sc = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) %in% to_remove)]
   xy.list = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) %in% to_remove) == F]
   
   
   #assignation of variable with a operation or a direct attribution
   ind_eq = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 7), `[[`, 1) == "SYMBOL")] #enl�ve les lignes qui commence pas par un symbol
   ind_eq = ind_eq[as.logical(lapply(lapply(ind_eq, `[[`, 7), `[[`, 2) %in% c("EQ_ASSIGN",'LEFT_ASSIGN'))]
   operationAssign = ind_eq[as.logical(lapply(lapply(ind_eq, `[[`, 7), `[[`, 3) %in% c("NUM_CONST", "STR_CONST"))]
   tmp_ind = match( operationAssign, xy.list)
   xy.list[tmp_ind] <- NULL
   
   
   tmp_ind = which(lengths(lapply(xy.list, `[[`, 7)) == 3)
   ind_eq = which(lapply(lapply(xy.list[tmp_ind], `[[`, 7), `[[`, 2) %in% c("EQ_ASSIGN",'LEFT_ASSIGN'))
   simpleAssignation = xy.list[tmp_ind][ind_eq]
   xy.list[tmp_ind[ind_eq]] <- NULL
   
   simpleAssignation = c(simpleAssignation , operationAssign)
   
   lmax = 0
   for (i in 1:length(simpleAssignation)) {
      l = length(simpleAssignation[[i]]$text)
      if (l > lmax){
         lmax =l
      }
   }
   
   tabsimpleAssignation = c()
   for (i in 1:length(simpleAssignation)) {
      tabsimpleAssignation = rbind(tabsimpleAssignation, c( simpleAssignation[[i]]$text[c(1:lmax)] ) )
   }
   
   
   
    # test = xy.list[lengths((lapply(xy.list, `[[`, 7))) ]
    # test2 = which(lapply(lapply(test, `[[`, 7), `[[`, 2) %in% c("EQ_ASSIGN",'LEFT_ASSIGN'))
    # test = test[test2]
    # tab = lapply(ind_eq, `[[`, 7)
    # tab2 = lapply(test, `[[`, 9) 
   
   
    ###############################################################
   #if 
   
   lmax = 0
   for (i in 1:length(xy.list)) {
      l = length(xy.list[[i]]$token)
      if (l > lmax){
         lmax =l
      }
   }
   
   token = c()
   for (i in 1:length(xy.list)) {
      #l = length(xy.list[[i]]$token)
      token = rbind(token, c( xy.list[[i]]$token[c(1:lmax)] ) )
   }   
   
   lineif = c()
   token[is.na(token)] <- 0
   nbop = 0
   nbcl = 0
   for (i in 1:nrow(token)) {
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
         }
         nbop = 0 
         nbcl = 0
      }
      
  
   }  
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
}