getDocDict = function(file, ...)
{
    tmp = getDoc(file)
    input=tmp[[1]]
    sentences=tmp[[2]]
    ws = SentenceSplit(input,sentences, ...)
    return(ws)
}