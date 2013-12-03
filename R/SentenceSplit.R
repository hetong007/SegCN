SentenceSplit=function(txt,sentences=NULL,split=FALSE,
                       d0=4,ent0=0,freq0=0,conc0=1)
{
	prt=proc.time()
    require(compiler)

	res=list()
	pmat=prepareMatrix(d0,txt)
	show("pmat Done.")
	show(proc.time()-prt)
	prt=proc.time()

    res = lapply(1:d0,getAllPhrases,pmat)

    n=length(res)
    ansWord=NULL
    ansFreq=NULL
    ansEntropy=NULL
    for (i in 1:n)
    {
        ansWord=c(ansWord,res[[i]][[1]])
        ansFreq=c(ansFreq,res[[i]][[2]])
        ansEntropy=c(ansEntropy,res[[i]][[3]])
    }

	show("All Phrase Done")
	show(proc.time()-prt)
	prt=proc.time()
    
    ansConcrete = getConcret(ansWord,ansFreq)
    
	show("Concrete Done")
	show(proc.time()-prt)
	prt=proc.time()
    
    result=list(ansWord,ansFreq,ansEntropy,ansConcrete)
    result=cleaning(result,1,ent0,freq0,conc0)
	
    if (split && !is.null(sentences))
    {
    	wd=result[[1]]
    	freq=result[[2]]
        
        len = sapply(sentences,length)
        ind = which(len>1)
        uni_sent = unlist(sentences[-ind])
        uni_ind = match(uni_sent,wd,0)
        uni_freq = log(freq[uni_ind])
        
        tempfunc = cmpfun(Viterbi)
        vit = lapply(sentences[ind],tempfunc,wd,log(freq),d0)
        ansProb = lapply(vit,function(x)x[1])
        ansProb = sum(unlist(ansProb),uni_freq)
        ans = rep('',length(sentences))
        ans[ind] = lapply(vit,function(x)x[2])
        ans[-ind] = uni_sent
        
    	result=data.frame(Phrase=result[[1]],
    	                  Freq=result[[2]],
    	                  Entropy=result[[3]],
    	                  Concret=result[[4]])
    	result[,1]=unlist(lapply(strsplit(result[,1],' '),paste,collapse=''))
        rownames(result)=NULL
    	ans=unlist(lapply(strsplit(unlist(ans),' '),paste,collapse=''))
    	show("Viterbi done.")
    	show(proc.time()-prt)
    	prt=proc.time()
        return(list(result,ans,ansProb))
    }
	result=data.frame(Phrase=result[[1]],
	                  Freq=result[[2]],
	                  Entropy=result[[3]],
	                  Concret=result[[4]])
	result[,1]=unlist(lapply(strsplit(result[,1],' '),paste,collapse=''))
	rownames(result)=NULL
    return(result)
}
