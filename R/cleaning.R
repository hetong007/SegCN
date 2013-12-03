cleaning=function(result,min.d=1,ent0=0,freq0=0,conc0=0)
{   
    phrases=result[[1]]
    freq=result[[2]]
    ent=result[[3]]
    conc=result[[4]]
    
    one_ind = which(nchar(phrases)==1)
    ind = which(freq>freq0 & ent>ent0 & conc>conc0)
    ind = union(ind,one_ind)
    
    phrases=phrases[ind]
    freq=freq[ind]
    ent=ent[ind]
    conc=conc[ind]
    
    ind=order(freq,decreasing=T)
    phrases=phrases[ind]
    ent=ent[ind]
    conc=conc[ind]
    freq=freq[ind]
    
    n=length(phrases)
    ind=NULL
    for (i in 1:n)
    {
        tmp=length(unlist(strsplit(phrases[i]," ")))
        if (tmp>=min.d)
            ind=c(ind,i)
    }
    phrases=phrases[ind]
    ent=ent[ind]
    conc=conc[ind]
    freq=freq[ind]
    
    return(list(Phrase=phrases,Freq=freq,Entropy=ent,Concret=conc))
}
