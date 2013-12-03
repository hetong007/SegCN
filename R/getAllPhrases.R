getAllPhrases=function(d,pmat)
{
    q=proc.time()
    if (d==1)
        Word = pmat[,2]
    else
    {
        cnms = paste('pmat[,',2:(d+1),']',sep='',collapse=',')
        Word = eval(parse(text=paste('paste(',cnms,')',sep='')))
    }
    m = nrow(pmat)
    #pmat[,1] = match(pmat[,1],pmat[,1],0)
    pmat[,2] = match(Word,Word,0)
    pmat[,3] = match(pmat[,d+2],pmat[,d+2],0)
    pmat = pmat[,1:3]
    for (i in 2:3)
        pmat[,i] = as.numeric(pmat[,i])
    pmat = pmat[order(pmat[,2],pmat[,1]),]
    
    y = which(pmat[-1,2] != pmat[-m,2])
    prun = c(y,m)-c(0,y)
    prun = cumsum(prun)
    indi = c(1,prun[-length(prun)]+1)
    indj = prun
    #spl = lapply(1:length(indi),function(x)indi[x]:indj[x])
    
    #len = sapply(spl,length)
    len = indj-indi+1
    Freq = len/nrow(pmat)
    ent1 = getEnt(indi,indj,pmat[,1],len)
    
    pmat = pmat[order(pmat[,2],pmat[,3]),]
    ent2 = getEnt(indi,indj,pmat[,3],len)
    Entropy = pmin(ent1,ent2)
    
    Word = Word[unique(pmat[,2])]
    cat(d,proc.time()-q,'\n')
    return(list(Word,Freq,Entropy))
}
