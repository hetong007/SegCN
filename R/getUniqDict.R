getUniqDict = function(dict,ent0=1,freq0=0,conc0=50)
{
    file_nms = dict[[1]]
    dict = dict[[2]]
    pool = lapply(dict,function(x)x[,1])
    pool = unlist(pool)
    pool = table(pool)
    nms = names(pool)
    ans = list()
    for (l in 1:length(dict))
    {
        ind = which(dict[[l]]$Concret>=conc0 & 
                    dict[[l]]$Entropy>=ent0 & 
                    dict[[l]]$Freq>=freq0)
        words = dict[[l]][ind,1]
        ind = which(nchar(words)>1)
        words = words[ind]
        ind = match(words,nms)
        ind_t = which(pool[ind]==1)
        ind = ind[ind_t]
        ind = match(nms[ind],dict[[l]][,1],0)
        tmp = dict[[l]][ind,]
        tmp = tmp[order(tmp$Concret),]
        ans[[l]] = tmp
    }
    return(list(file_nms,ans))
}
