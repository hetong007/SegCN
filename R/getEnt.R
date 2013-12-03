getEnt=function(indi,indj,pmat,len)
{
    #pmat = match(pmat,pmat,0)
    #len=sapply(spl,length)
    ind=which(len>1)
    cnt=rep(1,length(indi))
    
    #cnt[ind]=lapply(spl[ind],function(spl)
    #    Tabulate(pmat[spl]))
    
    pmat = lapply(ind,function(ind)pmat[indi[ind]:indj[ind]])
    cnt[ind] = lapply(pmat,function(x){
                    y=which(x[-1] != x[-length(x)]);
                    c(y,length(x))-c(0,y)})
    
    len=sapply(cnt,length)
    ind=which(len>1)
    H=rep(0,length(cnt))
    
    sms = sapply(cnt[ind],sum)
    lcnt = unlist(cnt[ind])
    lcnt = lcnt*log(lcnt)
    lcnt = split(lcnt,rep(1:length(len[ind]),len[ind]))
    lsms = sapply(lcnt,sum)
    H[ind] = log(sms)-lsms/sms
    return(H)
}
