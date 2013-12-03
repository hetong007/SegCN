Tabulate = function(bin)
{
    bin = match(bin,unique(bin),0)
    .Internal(tabulate(bin, max(bin)))
}
