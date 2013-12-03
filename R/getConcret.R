getConcret=function(ansWord,ansFreq)
{
    len = (nchar(ansWord)+1)/2
    #wdl = strsplit(ansWord,' ')
    #len = sapply(wdl,length)
    
    n = length(ansWord)
    #ind = sort(ansWord)
    #ansWord = ansWord[ind]
    #ansFreq = ansFreq[ind]
    y = which(len[-1] != len[-length(len)])
    prun = c(y,length(len))-c(0,y)
    indj = cumsum(prun)
    indi = c(1,indj[-length(indj)]+1)
    
    #spl = split(1:n,len)
    ans = rep(0,n)
    
    for (l in 1:length(indj))
    {
        #ind = spl[[l]]
        ind = indi[l]:indj[l]
        if (len[ind[1]]==1)
            ans[ind] = 10000
        else
        {
            d = len[ind[1]]
            wdl = strsplit(ansWord[ind],' ')
            wds = do.call(rbind,wdl)
            lhs = wds[,1:(d-1),drop=FALSE]
            rhs = wds[,2:d,drop=FALSE]
            if (d>2)
            {
                for (i in 2:(d-1))
                    lhs[,i] = paste(lhs[,i-1],lhs[,i])
                for (i in (d-2):1)
                    rhs[,i] = paste(rhs[,i],rhs[,i+1])
            }
            mx = rep(0,length(ind))
            for (i in 1:(d-1))
            {
                lhs_ind = match(lhs[,i],ansWord,0)
                rhs_ind = match(rhs[,i],ansWord,0)
                lhs_ind = as.numeric(lhs_ind)
                rhs_ind = as.numeric(rhs_ind)
                f_ind = which(lhs_ind>0 & rhs_ind>0)
                tmp = rep(0,length(ind))
                tmp[f_ind] = ansFreq[lhs_ind[f_ind]]*
                    ansFreq[rhs_ind[f_ind]]
                mx = pmax(tmp,mx)
            }
            mx[which(mx==0)]=1
            ans[ind] = ansFreq[ind]/mx
        }
    }
    return(ans)
}