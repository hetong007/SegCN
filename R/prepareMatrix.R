prepareMatrix=function(d,StrData)
{
    StrData=c(" ",StrData," ")
    n=length(StrData)
    phfix = lapply(1:(d+2),function(i)StrData[i:(n-d-2+i)])
    pmat=do.call(cbind,phfix)
    pmat = as.data.frame(pmat)
    pmat[,1] = match(pmat[,1],pmat[,1],0)
    pmat[,1] = as.numeric(pmat[,1])
    return(pmat)
}
