getPathDict = function(path, ...)
{
    require(tmcn)
    pp=proc.time()
    
    nms = list.files(path)
    lsf = paste(path,nms,sep='')
    ans = list()
    for (l in 1:length(lsf))
    {
        p = proc.time()
        cat('\n beginning',nms[l],',',l,'/',length(lsf),'\n')
        lf = lsf[l]
        tmp = getDoc(lf)
        input=tmp[[1]]
        sentences=tmp[[2]]
        ws = SentenceSplit(input,sentences, ...)
        ans[[l]] = ws
        cat(nms[l],'finished in',as.vector((proc.time()-p))[3],'secs!\n\n')
    }
    cat('All finished in',as.vector((proc.time()-pp))[3],'secs!\n\n')
    return(list(nms,ans))
}
