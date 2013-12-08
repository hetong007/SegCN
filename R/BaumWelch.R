BaumWelch = function(txt,maxiter=100,leps=0.1,A=NULL,B=NULL,pie=NULL,trace=T)
{
    dict = unique(txt)
    txt = match(txt,dict)
    
    #Initialization
    N = 4
    M = length(dict)
    L = length(txt)
    
    if (is.null(A))
    {
        A = mat.or.vec(N,N)
        Azero = matrix(rep(TRUE,16),ncol=N)
        Azero[cbind(c(1,1,2,2,3,3,4,4),c(2,3,2,3,1,4,1,4))] = FALSE
        #A[!Azero] = 0.5
        A[!Azero] = runif(8)
        for (i in 1:N)
            A[i,] = A[i,]/sum(A[i,])
    }
    if (is.null(B))
    {
        B = matrix(runif(N*M),nrow=N)
        tmp = colSums(B)
        for (i in 1:N)
            B[i,] = B[i,]/tmp
    }
    if (is.null(pie))
    {
        pie = rep(0,N)
        pie[c(1,4)] = runif(2)
        pie = pie/sum(pie)
    }
    
    alpha = rep(N)
    scl_alpha = mat.or.vec(N,L)
    scl = rep(0,L)
    beta = rep(N)
    scl_beta = mat.or.vec(N,L)
    gamma = mat.or.vec(N,L)
    
    flag = FALSE
    iter_time = 1
    prob = -Inf
    
    while(!flag)
    {
        alpha = sum(pie*B[,txt[1]])
        scl[1] = 1/sum(alpha)
        scl_alpha[,1] = alpha*scl[1]
        for (t in 2:L)
        {
            alpha = (scl_alpha[,t-1] %*% A) * B[,txt[t]]
            scl[t] = 1/sum(alpha)
            scl_alpha[,t] = alpha*scl[t]
        }
        beta = c(0,0,1,1)
        scl_beta[,L] = beta
        for (t in (L-1):1)
        {
            beta = A %*% (B[,txt[t+1]]*scl_beta[,t+1])
            scl_beta[,t] = beta*scl[t+1]
        }
        
        gamma = scl_alpha*scl_beta
        invGamma = 1/colSums(gamma)
        for (i in 1:N)
            gamma[i,] = gamma[i,]*invGamma
        
        #Refresh
        #nprob = 0
        pie = gamma[,1]
        
        tA = mat.or.vec(N,N)
        for (t in 1:(L-1))
        {
            tmp = diag(scl_alpha[,t]) %*% A %*% 
                    diag(scl[t+1]*scl_beta[,t+1]*B[,txt[t+1]])
            #tmp[Azero] = 0
            s_tmp = sum(tmp)
            #nprob = nprob+log(s_tmp)
            tA = tA + tmp/s_tmp
        }
        invGamma = 1/rowSums(gamma)
        A = diag(invGamma) %*% tA
        
        for (t in 1:M)
        {
            ind = which(txt==t)
            B[,t] = rowSums(gamma[,ind,drop=FALSE])*invGamma
        }
        
        nprob = -sum(log(scl))
        if (trace)
        {
            cat(iter_time,nprob-prob,'\n')
            for (i in 1:N)
                cat(A[i,],'\n')
            cat(B[1,1:5],'\n')
            cat('\n')
        }
        #get Prob and judge for stop
        iter_time = iter_time+1
        delta = abs(nprob-prob)
        if (iter_time>maxiter || (iter_time>=20 && delta<leps))
            flag = TRUE
        prob = nprob
    }
    
    delta = mat.or.vec(N,L)
    phi = mat.or.vec(N,L)
    path = rep(0,L)
    lA = log(A)
    
    delta[,1] = log(pie*B[,txt[1]])
    phi[,1] = 0
    
    for (t in 2:L)
    {
        for (i in 1:N)
        {
            tmp = delta[,t-1]+lA[,i]
            delta[i,t] = max(tmp)
            phi[i,t] = which.max(tmp)
        }
        delta[,t] = delta[,t]+log(B[,txt[t]])
    }
    P = max(delta[,3:4])
    path[L] = which.max(delta[,3:4])+2
    
    ind = grep('^[\u4E00-\u9FA5\uF900-\uFA2D]*$',dict[txt])
    for (t in (L-1):1)
    {
        if (t %in% ind)
            path[t] = phi[path[t+1],t+1]
        else
            path[t] = 4
    }
    path = c('B','M','E','S')[path]
    
    #spl = NULL
    #for (t in 1:L)
    #{
    #    if (path[t]=='B')
    #        head = t
    #    if (path[t]=='E')
    #        spl = c(spl,paste(dict[txt[head:t]],collapse=''))
    #    if (path[t]=='S')
    #        spl = c(spl,dict[txt[t]])
    #}
    #return(list(A,B,pie,path,spl))
    return(list(A,B,pie,path))
}
