Viterbi = function(sent,wd,freq,d)
{
    sent = unlist(sent)
    n = length(sent)
    if (n==1)
    {
        ind = match(sent,wd,0)
        tprob = freq[ind]
        return(list(tprob,sent))
    }
    A_ind = matrix(rep(FALSE,n*n),ncol=n)
    A_wd = matrix(rep('',n*n),ncol=n)
    A = mat.or.vec(n,n)
    t_wd = NULL
    prob = rep(0,n)
    path = vector(mode='list',length=n)
    
    for (i in 1:n)
    {
        for (j in i:min(i+d-1,n))
        {
            A_ind[i,j] = TRUE
            A_wd[i,j] = paste(sent[i:j],collapse=' ')
        }
    }
    t_wd = A_wd[A_ind]
    t_freq = rep(-Inf,length(t_wd))
    ind = match(t_wd,wd,0)
    t_ind = which(ind>0)
    t_freq[t_ind] = freq[ind[t_ind]]
    A[A_ind] = t_freq
    
    path[[1]] = A_wd[1,1]
    prob[1] = A[1,1]
    for (i in 2:n)
    {
        j = max(1,i-d+1)
        tpath = A_wd[j,i]
        tprob = A[j,i]
        if (i>d)
        {
            prob[i] = prob[i-d]+A[j,i]
            path[[i]] = c(path[[i-d]],A_wd[j,i])
        }
        else
        {
            prob[i] = A[j,i]
            path[[i]] = A_wd[j,i]
        }
        for (j in max(2,i-d+2):i)
        {
            if (prob[j-1]+A[j,i]>prob[i])
            {
                prob[i] = prob[j-1]+A[j,i]
                path[[i]] = c(path[[j-1]],A_wd[j,i])
            }
        }
    }
    return(list(prob[n],path[[n]]))
}