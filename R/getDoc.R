getDoc = function(file)
{
    require(tmcn)
    doc = readLines(file)
    doc = unlist(strsplit(toUTF8(doc),'\\s+'))
    doc = paste(doc,collapse='。')
    input = strsplit(doc,'')[[1]]
    input = input[grep('^[\u4E00-\u9FA5\uF900-\uFA2D]*$',input)]
    sentences = strsplit(doc,'[^\u4E00-\u9FA5\uF900-\uFA2D]')[[1]]
    sentences = sentences[sentences!='']
    sentences = strsplit(sentences,'')
    return(list(input,sentences))
}
