# ANODEV function from Richard Townsend, provided to me June 2018 

anodev.fn=function(x,rounding=4,test.in="F"){
  ## reformat Analysis of Deviance output
  if(test.in=="F"){
    ## F-test uses model MSE on all tests
    temp=anova(x,test=test.in)
    out=matrix(NA,nrow=dim(temp)[1]+1,ncol=dim(temp)[2]-1,
               dimnames=list(c("Totalc",attributes(temp)$row.names[-1],"Error"),
                             c(attributes(temp)$names[c(1,2)],"Mean Dev.",attributes(temp)$names[c(5,6)])))
    out[,1]=c(temp[1,3],temp[-1,1],temp[dim(temp)[1],3])
    out[,2]=c(temp[1,4],temp[-1,2],temp[dim(temp)[1],4])
    out[-1,3]=c((out[2:(dim(out)[1]-1),2]/out[2:(dim(out)[1]-1),1]),(out[dim(out)[1],2]/out[dim(out)[1],1]))
    out[2:(dim(out)[1]-1),4]=(out[2:(dim(out)[1]-1),3]/out[dim(out)[1],3])
    out[2:(dim(out)[1]-1),5]=1-pf(out[2:(dim(out)[1]-1),4],out[2:(dim(out)[1]-1),1],out[dim(out)[1],1])
    out[,-1]=round(out[,-1],rounding)
    out[is.na(out)]=""
  }
  if(test.in=="Chi"){
    ## Chi-sq test
    temp=anova(x,test=test.in)
    out=matrix(NA,nrow=dim(temp)[1]+1,ncol=dim(temp)[2]-1,
               dimnames=list(c("Totalc",attributes(temp)$row.names[-1],"Error"),
                             c(attributes(temp)$names[c(1,2)],"Mean Dev.",attributes(temp)$names[c(5)])))
    out[,1]=c(temp[1,3],temp[-1,1],temp[dim(temp)[1],3])
    out[,2]=c(temp[1,4],temp[-1,2],temp[dim(temp)[1],4])
    out[-1,3]=c((out[2:(dim(out)[1]-1),2]/out[2:(dim(out)[1]-1),1]),(out[dim(out)[1],2]/out[dim(out)[1],1]))
    out[2:(dim(out)[1]-1),4]=1-pchisq(out[2:(dim(out)[1]-1),3],out[2:(dim(out)[1]-1),1])
    out[,-1]=round(out[,-1],rounding)
    out[is.na(out)]=""
  }
  return(as.data.frame(out))
}
