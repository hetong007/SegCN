getDoc = function(file,punctuation=F)
{
    require(tmcn)
    doc = toUTF8(readLines(file))
    doc = unlist(strsplit(doc,'\\s+'))
    doc = paste(doc,collapse='')
    if (!punctuation)
    {
        doc = unlist(strsplit(doc,'\\s+'))
        doc = paste(doc,collapse='。')
        input = strsplit(doc,'')[[1]]
        input = input[grep('^[\u4E00-\u9FA5\uF900-\uFA2D]*$',input)]
        sentences = strsplit(doc,'[^\u4E00-\u9FA5\uF900-\uFA2D]')[[1]]
        sentences = sentences[sentences!='']
        sentences = strsplit(sentences,'')
    }
    else
    {
        input = unlist(strsplit(doc,''))
        input = input[input!='']
        #ind = grep('\\s+',input)
        #input = input[-ind]
        sentences = unlist(strsplit(doc,'[^\u4E00-\u9FA5\uF900-\uFA2D]'))
        sentences = sentences[sentences!='']
        sentences = strsplit(sentences,'')
        #ind = grep('\\s+',sentences)
        #sentences = sentences[-ind]
    }
    return(list(input,sentences))
}
