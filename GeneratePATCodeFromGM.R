##Author: Zhuoqun Yang
##Date: 20/8/2016
##Input: table of goal model
##Output: formal description known as CSP# which can be run and simulated within PAT

##library used: e1071
##function: permutations()

##Function FindNext is used to finding the linked node for leaf task nodes.
##Internal functions in R include:
##find index: which(gtree[,1]=='g1')
##pattern match: grepl(pattern="g",gtree[1,1])
##pattern substute: gsub(pattern="g", replacement="AS",x=...)

##install.packages("e1071")
library(e1071)

FindNext <- function(nd,tr){
  if(nd=="g0"){
    ##tl <- tr[which(tr[,1]==nd),8]
    tl <- "g0"
  } else if(tr[which(tr[,1]==nd),4]!=""){
    tl <- tr[which(tr[,1]==nd),4]
  } else if(tr[which(tr[,1]==nd),4]==""){
    nd <- tr[which(tr[,1]==nd),3]
    tl <- FindNext(nd,tr)
  }
  return(tl)
}
file.remove("C:/Users/Administrator/Documents/FormalDescription.txt")
##read goal model
gtree <- read.csv("C:/Users/Administrator/Documents/goalmodel2.csv",stringsAsFactors=F)
relationPattern <- gtree[,c(2,5,6)]
row <- nrow(gtree)
nodePass <- cbind(gtree[,1],0)
nodePass <- as.data.frame(nodePass)
nodePass[,2] <- as.numeric(nodePass[,2])
nodePass[,2] <- 0;

for (i in 1:row)
  ## goal and goal (111)
  if((relationPattern[i,1]==1)&(relationPattern[i,2]==1)&(relationPattern[i,3]==1)){
    des <- paste0(gtree$no[i],'=')
    des <- paste0(des,gtree[i,8])
    des <- gsub(pattern="g", replacement="AS",des)
    des <- paste0(des,';')
    print(des)
    write.table(des, file = "C:/Users/Administrator/Documents/FormalDescription.txt", row.names = F, col.names = F,quote = F,append = TRUE)
    nodePass[i,2] <- 1
  } else if((relationPattern[i,1]==1)&(relationPattern[i,2]==1)&(relationPattern[i,3]==2)){
    ## goal and task (112)
    des <- paste0(gtree$no[i],'=')
    des <- paste0(des,gtree[i,8])
    des <- gsub(pattern="g", replacement="AS",des)
    des <- gsub(pattern="t", replacement="T",des)
    des <- paste0(des,';')
    print(des)
    write.table(des, file = "C:/Users/Administrator/Documents/FormalDescription.txt", row.names = F, col.names = F,quote = F,append = TRUE)
    nodePass[i,2] <- 1
  } else if((relationPattern[i,1]==2)&(relationPattern[i,2]==1)&(relationPattern[i,3]==2)){
    ## task and task (212)
    des <- paste0(gtree$no[i],'=')
    des <- paste0(des,gtree[i,8])
    des <- gsub(pattern="t", replacement="T",des)
    des <- paste0(des,';')
    print(des)
    write.table(des, file = "C:/Users/Administrator/Documents/FormalDescription.txt", row.names = F, col.names = F,quote = F,append = TRUE)
    nodePass[i,2] <- 1
  } else if((relationPattern[i,1]==1)&(relationPattern[i,2]==2)&(relationPattern[i,3]==1)){
    ## goal or goal (121)
    des <- paste0(gtree$no[i],'=')
    for (j in 1:gtree[i,7])
      if (j==1)
      {des <- paste0(des,gtree[i,7+j])} else 
      {des <- paste0(des,'[]',gtree[i,7+j])}
    des <- gsub(pattern="g", replacement="AS",des)
    des <- paste0(des,';')
    print(des)
    write.table(des, file = "C:/Users/Administrator/Documents/FormalDescription.txt", row.names = F, col.names = F,quote = F,append = TRUE)
    nodePass[i,2] <- 1
  } else if((relationPattern[i,1]==1)&(relationPattern[i,2]==2)&(relationPattern[i,3]==2)){
    ## goal or task (122)
    des <- paste0(gtree$no[i],'=')
    for (j in 1:gtree[i,7])
      if (j==1)
      {des <- paste0(des,gtree[i,7+j])} else 
      {des <- paste0(des,'[]',gtree[i,7+j])}
    des <- gsub(pattern="g", replacement="AS",des)
    des <- gsub(pattern="t", replacement="T",des)
    des <- paste0(des,';')
    print(des)
    write.table(des, file = "C:/Users/Administrator/Documents/FormalDescription.txt", row.names = F, col.names = F,quote = F,append = TRUE)
    nodePass[i,2] <- 1
  } else if((relationPattern[i,1]==2)&(relationPattern[i,2]==2)&(relationPattern[i,3]==2)){
    ## task or task (222)
    des <- paste0(gtree$no[i],'=')
    for (j in 1:gtree[i,7])
      if (j==1)
      {des <- paste0(des,gtree[i,7+j])} else 
      {des <- paste0(des,'[]',gtree[i,7+j])}
    des <- gsub(pattern="t", replacement="T",des)
    des <- paste0(des,';')
    print(des)
    write.table(des, file = "C:/Users/Administrator/Documents/FormalDescription.txt", row.names = F, col.names = F,quote = F,append = TRUE)
    nodePass[i,2] <- 1
  } else if((relationPattern[i,1]==1)&(relationPattern[i,2]==3)&(relationPattern[i,3]==2)){
    ## goal and leaf task no sequence (112)
    node <-gtree$no[i]
    perm <- permutations(gtree[i,7])
    des <- paste0(gtree$no[i],'=')
    for(k in 1:nrow(perm)){
      if(k!=nrow(perm)){
        des <- paste0(des,gtree$no[i],k,'[]')
      } else{
        des <- paste0(des,gtree$no[i],k,';')
      }
    }
    des <- gsub(pattern="g", replacement="AS",des)
    print(des)
    write.table(des, file = "C:/Users/Administrator/Documents/FormalDescription.txt", row.names = F, col.names = F,quote = F,append = TRUE)
    for(p in 1:nrow(perm)){
      des <- paste0(gtree$no[i],p,'=')
      des <- gsub(pattern="g", replacement="AS",des)
      for (q in 1:gtree[i,7])
        des <- paste0(des,gtree[i,7+perm[p,q]],'->')
      tal <- FindNext(node, gtree)
      if(grepl(pattern="g",tal)){
        tal <- gsub(pattern="g", replacement="AS",tal)
      } else {
        tal <- toupper(tal)
      }
      des <- paste0(des,tal,';')
      print(des)
      write.table(des, file = "C:/Users/Administrator/Documents/FormalDescription.txt", row.names = F, col.names = F,quote = F,append = TRUE)
    }
    nodePass[i,2] <- 1
    for(j in 1:gtree[i,7])
      nodePass[which(nodePass[,1]==gtree[i,7+j]),2] <-1
  } else if((relationPattern[i,1]==2)&(relationPattern[i,2]==0)&(relationPattern[i,3]==0)){
      if(nodePass[i,2]==0)
      {node <-gtree$no[i]
      des <- paste0(toupper(gtree$no[i]),'=',gtree$no[i],'->')
      tal <- FindNext(node, gtree)
      if(grepl(pattern="g",tal)){
        tal <- gsub(pattern="g", replacement="AS",tal)
      } else {
        tal <- toupper(tal)
      }
      des <- paste0(des,tal)
      des <- paste0(des,';')
      print(des)
      write.table(des, file = "C:/Users/Administrator/Documents/FormalDescription.txt", row.names = F, col.names = F,quote = F,append = TRUE)}
      nodePass[i,2] <- 1
  }

