library(RWeka)
library(rJava)
library(tm)
library(devtools)
library(RCurl)
library(tm.plugin.webmining)
library(tm.plugin.sentiment)
library(XML)
library(corpus) ;library(corpora) ; library(corpustools)
library(BatchGetSymbols)
library(RTextTools)
library(e1071)
library(quantmod)
WebCorpus <- function(x, readerControl = list(reader = reader(x), language = "en"),
                      postFUN = x$postFUN, retryEmpty = TRUE, ...)
{
  stopifnot(inherits(x, "WebSource"))
  
  readerControl <- prepareReader(readerControl, reader(x))
  
  if (is.function(readerControl$init))
    readerControl$init()
  
  if (is.function(readerControl$exit))
    on.exit(readerControl$exit())
  
  tdl <- vector("list", length(x))
  counter <- 1
  while (!eoi(x)) {
    x <- stepNext(x)
    elem <- getElem(x)
    doc <- readerControl$reader(elem,
                                readerControl$language,
                                as.character(counter))
    tdl[[counter]] <- doc
    counter <- counter + 1
  }
  
  corpus <- structure(list(content = tdl,
                           meta = CorpusMeta(source = x, readerControl = readerControl, postFUN = postFUN),
                           dmeta = data.frame(row.names = seq_along(tdl))),
                      class = c("WebCorpus", "VCorpus", "Corpus"))
  if(retryEmpty){
    corpus <- getEmpty(corpus)
  }
  corpus
}
CorpusMeta <-
  function(..., meta = NULL)
  {
    if (is.null(meta))
      meta <- list(...)
    
    stopifnot(is.list(meta))
    
    structure(meta, class = "CorpusMeta")
  }
prepareReader <- 
  function(readerControl, reader = NULL, ...)
  {
    if (is.null(readerControl$reader))
      readerControl$reader <- reader
    if (inherits(readerControl$reader, "FunctionGenerator"))
      readerControl$reader <- readerControl$reader(...)
    if (is.null(readerControl$language))
      readerControl$language <- "en"
    readerControl
  }
`[.WebCorpus` <- function(x, i) {
  if (missing(i)) return(x)
  corpus <- NextMethod("[")
  class(corpus) <- c("WebCorpus", class(corpus))
  corpus
}
corpus.update <- function(x, ...){
  UseMethod("corpus.update", x)	
}
corpus.update.WebCorpus <- 
  function(x, fieldname = "id", retryempty = TRUE, verbose = FALSE, ...) {
    cm <- x$meta
    
    newsource <- source.update(cm$source)
    
    #WebCorpus
    newcorpus <- WebCorpus(newsource, readerControl = cm$MetaData$ReaderControl, 
                           retryEmpty = FALSE, ...)
    #intersect on ID
    id_old <- sapply(x, meta, fieldname)
    if(any(sapply(id_old, length) == 0))
      stop(paste("Not all elements in corpus to update have field '", fieldname, "' defined", sep = ""))
    
    id_new <- sapply(newcorpus, meta, fieldname)
    if(any(sapply(id_new, length) == 0))
      stop(paste("Not all elements in corpus to update have field '", fieldname, "' defined", sep = ""))
    
    newcorpus <- newcorpus[!id_new %in% id_old]
    
    if(length(newcorpus) > 0){
      if(!is.null(cm$postFUN)){
        newcorpus <- cm$postFUN(newcorpus)
      }
      corpus <- c(x, newcorpus)
      #attr(corpus, "CMetaData") <- CMetaData(x)
      class(corpus) <- c("WebCorpus", class(corpus))
    }else{
      corpus <- x
    }
    
    if(retryempty){
      corpus <- getEmpty(corpus)
    }
    
    if(verbose){
      cat(length(newcorpus), " corpus items added.\n")
    }
    
    corpus
  }
getEmpty <- function(x, ...){
  UseMethod("getEmpty", x)	
}
getEmpty.WebCorpus <- 
  function(x, nChar = 0, ...){
    cm <- x$meta
    noContent <- which(sapply(x, function(y){
      cy <- content(y)
      if(length(cy) == 0L) 0
      else nchar(content(y)) 
    }) <= nChar)
    if(length(noContent) > 0){
      corp_nocontent <- x[noContent]
      if(!is.null(cm$postFUN)){
        corp_nocontent <- cm$postFUN(corp_nocontent, ...)
      }
      # TODO: stupid construct because direct assignment of corpus does not work
      for(i in 1:length(noContent)){
        x[[noContent[i]]] <- corp_nocontent[[i]]
      }
    }
    x
  }
extract <- function(x, extractor, ...) UseMethod("extract", x)
extract.PlainTextDocument <- function(x, extractor = extractContentDOM, ...){
  content(x) <- tryCatch(extractor(x, ...),
                         error = function(e){
                           warning(e)
                           content(x)
                         })
  x
} 
extractHTMLStrip <-
  function(url, asText = TRUE, encoding, ...){
    if(missing(encoding)){
      encoding <- switch(.Platform$OS.type,
                         unix = "UTF-8",
                         windows = "latin1")
    }	
    
    if(url == ""){
      return("")
    }
    
    parseerror <- capture.output(tree <- htmlTreeParse(url, asText = asText, 
                                                       useInternalNodes = TRUE, encoding = encoding, ...))
    
    children <- xmlChildren(tree)
    children <- children[!sapply(children, function(x) 
      grepl("<!DOCTYPE", toString.XMLNode(x)))]
    childlen <- sapply(children, function(x) nchar(toString.XMLNode(x)))
    childidx <- max(which(childlen == max(childlen)))
    #childidx <- min(childidx, length(children))
    html <- children[[childidx]]
    val <- xmlValue(html)
    XML::free(tree)
    return(val)
  }
extractContentDOM <-
  function(url, threshold, asText = TRUE, ...){
    
    # FIXME: Hack because of roxygen2 bug (dot replaced by comma):
    if(missing(threshold)){
      threshold <- 0.5
    }
    
    if(url == ""){
      return("")
    }
    
    parseerror <- capture.output(tree <- htmlTreeParse(url, asText = asText, useInternalNodes = TRUE, ...))
    childlen <- sapply(xmlChildren(tree), function(x) nchar(toString.XMLNode(x)))
    childidx <- which(childlen == max(childlen))
    html <- xmlChildren(tree)[[childidx]]
    tags <- c("script" , "noscript", "style")
    htmlclean <- removeTags(html, tags)
    
    htmlannotated <- assignValues(htmlclean, FUN = calcDensity, threshold)
    content <- getMainText(htmlannotated, threshold)
    return(content)
  }
calcDensity <-
  function(xn, annotate = TRUE){
    textlen <- nchar( xmlValue(xn))
    treelen <- nchar(toString.XMLNode(xn))
    dens <- textlen / treelen
    if(annotate & inherits(xn, "XMLInternalElementNode")){
      addAttributes(xn, "dens" = dens, "textlen" = textlen, "treelen" = treelen)
    }
    return(c(dens, textlen, treelen))
  }
assignValues <-
  function(t, FUN, threshold, attribname = "attrib", recursive = TRUE, mintextlen = 10, ...){
    
    # FIXME: Hack because of roxygen2 bug (dot replaced by comma):
    if(missing(threshold)){
      threshold <- 0.5
    }
    
    dens <- xmlApply(t, FUN)
    dens <- do.call("rbind", dens)
    #dens <- as.data.frame(dens)
    
    
    if(!recursive){
      return(t)
    }
    lapply(t[(dens[,2] > mintextlen) & (dens[,1] < threshold)], assignValues, FUN, ...)
    return(t)
    
  }
getMainText <-
  function(xml, threshold){
    # FIXME: Hack because of roxygen2 bug (dot replaced by comma):
    if(missing(threshold)){
      threshold <- 0.5
    }
    
    textlen <- as.numeric( xpathSApply(xml, path = "//attribute::textlen"))
    dens <- as.numeric( xpathSApply(xml, path = "//attribute::dens"))
    
    textlen[dens < threshold] <- 0
    idxmaintext <- which(textlen == max(textlen))
    if(max(textlen) == 0){
      return("")
    }
    
    content <-  xpathSApply(xml, path = paste("//*[@textlen][@dens]",sep = ""))[[idxmaintext]]
    
    cleancontent <-  xmlValue(content)
    cleancontent <- trimWhiteSpaces(cleancontent)
    
    return(cleancontent)
  }
removeTags <-
  function(xmldoc, tags){
    #remove scripts tags
    xquery <- paste("//", tags, sep = "", collapse = " | ")
    scripts <-  getNodeSet(xmldoc, path = xquery)
    ret <- removeNodes(scripts , free = rep(FALSE, length(scripts)))
    removeTags <- xmldoc
  }
feedquery <-
  function(url, params){
    els <- lapply(names(params), function(n) {		
      paste(n, curlEscape(params[[n]]), sep = "=")
    })
    names(els) <- names(params)
    
    feeds <- ""
    for(i in names(els)){
      if(feeds[1] == ""){
        sep = ""
      }
      else{
        sep = "&"
      }
      feeds <- paste(feeds, els[[i]], sep = sep)
    }
    
    feeds <- paste(url, feeds, sep = "?")
    return(feeds)
  }
getLinkContent <- function(corpus, links = sapply(corpus, meta, "origin"),
                           timeout.request = 30, chunksize = 20, verbose = getOption("verbose"),
                           curlOpts = curlOptions(verbose = FALSE,
                                                  followlocation = TRUE, 
                                                  maxconnects = 5,
                                                  maxredirs = 20,
                                                  timeout = timeout.request,
                                                  connecttimeout = timeout.request,
                                                  ssl.verifyhost=FALSE,
                                                  ssl.verifypeer = FALSE,
                                                  useragent = "R", 
                                                  cookiejar = tempfile()),  
                           retry.empty = 3, 
                           sleep.time = 3, 
                           extractor = ArticleExtractor, 
                           .encoding = integer(),
                           ...){
  
  if(length(corpus) != length(links))
    stop("Corpus length not equal to links length\n")
  
  #content_urls <- unlist(sapply(content_parsed, linkreader))
  if(verbose){
    cat("Starting URL Download ...\n")
  }
  retries <- 0
  while(any(empty <- sapply(corpus, function(x) identical(content(x), character(0)))) & (retries <= retry.empty)){
    retries <- retries + 1
    emptycontent.ids <- which(empty)
    
    if(verbose){
      cat("Run ", retries, ", retrieving ", length(emptycontent.ids), " content items\n")
    }
    
    #for(cstart in seq(from = 1, to =  length(links), by = chunksize)){
    for(cstart in seq(from = 1, to =  length(emptycontent.ids), by = chunksize)){
      if(sleep.time > 0){
        if(verbose){
          cat("Sleeping ", sleep.time, " seconds...\n")
        }
        Sys.sleep(sleep.time)
      }
      
      cend <- min(cstart[1] + chunksize-1, length(emptycontent.ids))
      chunk.ids <- emptycontent.ids[cstart:cend]
      chunk <- links[chunk.ids]
      
      # TODO Enable chunk download
      content <- tryCatch({
        getURL(chunk, .opts = curlOpts, .encoding = .encoding, ...)
      },
      error=function(e){
        print(e)
        # TODO: Check if single retrieval part is really necessary
        cat("\nError on retrieval, single retrieval fallback... \n")
        content <- list()
        for(i in 1:length(chunk)){
          content[[i]] <- tryCatch({
            getURL(chunk[i], .opts = curlOpts, .encoding = .encoding, ...)
          },error = function(f) {
            print(f)
            ""})
        }
        #cat("Done\n")
        do.call(c, content)})
      
      
      # Extract Content
      extract <- sapply(content, extractor)
      
      # Put Content Into Corpus
      for(i in 1:length(chunk.ids)){
        cid <- chunk.ids[i]
        content(corpus[[cid]]) <- extract[i]
        
      }
      if(verbose){
        progress <- floor(cend/length(links)*100)
        cat(paste(progress, "% (",cend,"/",length(emptycontent.ids), ") ", Sys.time(), "\n",sep = ""))
      }
    }
  }
  corpus
}
parse <- function(..., asText = TRUE, type = c("XML", "HTML", "JSON")){
  parsetype <- match.arg(type)
  encoding <- switch(.Platform$OS.type,
                     unix = "UTF-8",
                     windows = "latin1")
  parser <- switch(parsetype,
                   XML = xmlInternalTreeParse,
                   HTML = htmlTreeParse,
                   JSON = fromJSON)
  parser(..., encoding = encoding, asText = asText)
}
xml_content <- function(doc, spec) {
  type <- spec[[1]]
  fun <- switch(type,
                node = XML::xmlValue,
                attribute = identity)
  
  if (identical(type, "unevaluated"))
    spec[[2]]
  else if (identical(type, "function") && is.function(spec[[2]]))
    spec[[2]](doc)
  else
    as.character(sapply(XML::getNodeSet(doc, spec[[2]]), fun))
}
readWeb <- FunctionGenerator(function(spec, doc, parser, contentparser, freeFUN = NULL) {
  
  parser <- parser
  contentparser <- contentparser
  freeFUN <- freeFUN
  spec <- spec
  doc <- doc
  
  function(elem, language, id) {
    tree <- parser(elem$content)
    
    ###Set Content
    content(doc) <- if ("content" %in% names(spec)){
      content <- contentparser(tree, spec[["content"]])
    }
    else{
      character(0)
    }		
    
    for (n in setdiff(names(spec), "content")){
      meta(doc, n) <- contentparser(tree, spec[[n]])
    }
    
    if(!is.null(freeFUN)){
      freeFUN(tree)
    }
    doc
  }
})
readWebXML <- function(...){
  parser <- function(x){
    #XML::xmlInternalTreeParse(x, asText = TRUE)
    parse(x, type = "XML")
  } 
  contentparser <- xml_content
  freeFUN <- free
  readWeb(parser = parser, contentparser = contentparser, freeFUN = freeFUN, ...)
}
readWebHTML <- function(...){
  #parser <- function(x) XML::htmlTreeParse(x, asText = TRUE, useInternalNodes = TRUE)
  parser <- function(x) parse(x, type = "HTML", useInternalNodes = TRUE)
  contentparser <- function(x, cspec) xml_content(x, cspec)
  freeFUN <- free
  readWeb(parser = parser, contentparser = contentparser, freeFUN = freeFUN, ...)
}
readWebJSON <- function(...){
  parser <- function(x) identity(x)
  contentparser <- function(x, cspec) json_content(x, cspec)
  freeFUN <- rm
  readWeb(parser = parser, contentparser = contentparser, freeFUN = freeFUN, ...)
}
json_content <- 
  function (doc, spec) 
  {
    type <- spec[[1]]
    fun <- switch(type, field = identity, node = identity)
    if (identical(type, "unevaluated")) 
      spec[[2]]
    else if (identical(type, "function") && is.function(spec[[2]])) 
      spec[[2]](doc)
    else{
      as.character(sapply(doc[[spec[[2]]]], 
                          fun))
    } 
  }
readGoogle <- readWebXML(spec = list(
  heading = list("node", "//title"),
  datetimestamp = list("function", function(node){
    loc <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    val <- sapply(getNodeSet(node, "//pubDate"), xmlValue)
    time <- strptime(val,format = "%a, %d %b %Y %H:%M:%S",tz = "GMT")
    Sys.setlocale("LC_TIME", loc)
    time
  }),
  origin = list("node", "//link"),
  description = list("function", function(node){
    val <- sapply(getNodeSet(node, "//item/description"), xmlValue)
    extractHTMLStrip(sprintf("<html>%s</html>", val), asText = T)
  }),
  id = list("node",  "//guid")),
  doc = PlainTextDocument())
readYahooInplay <- readWebHTML(spec = list(
  heading = list("node", "//b[1]"),
  id = list("node", "//b[1]"),
  content = list("node", "//p"),
  datetimestamp = list("function", function(node){
    val <- unlist(getNodeSet(node, "//b[1]", fun = xmlValue))
    substr(val, 1, regexpr("\\s", val)-1)
  }),
  ticker  = list("node", "//p/b/a")),
  doc = PlainTextDocument())
readYahoo <- readWebXML(spec = list(
  heading = list("node", "//title"),
  datetimestamp = list("function", function(node){
    loc <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    val <- sapply(getNodeSet(node, "//pubDate"), xmlValue)
    time <- strptime(val,format = "%a, %d %b %Y %H:%M:%S",tz = "GMT")
    Sys.setlocale("LC_TIME", loc)
    time
  }),
  origin = list("node", "//link"),
  description = list("node", "//item/description"),
  id = list("node",  "//guid")),
  doc = PlainTextDocument())
readYahooHTML <- readWebHTML(spec = list(
  heading = list("node", "//div[@class='compTitle']/h3[@class='title']/a"),
  datetimestamp = list("function", function(node){
    loc <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    val <- sapply(getNodeSet(node, "//span[@class='tri fc-2nd ml-10']"), xmlValue)
    time <- strptime(val, format = "%b %d %H:%M %p",tz = "GMT")
    Sys.setlocale("LC_TIME", loc)
    time
  }),
  origin = list("attribute", "//div[@class='compTitle']/h3[@class='title']/a/@href"),
  author = list("node", "//span[@class='cite']"),
  description = list("node", "//div[@class='compText']/p"),
  id = list("attribute", "//div[@class='compTitle']/h3[@class='title']/a/@href")),
  doc = PlainTextDocument())
WebSource <- function(feedurls, class = "WebXMLSource", reader, parser, encoding = "UTF-8",
                      curlOpts = curlOptions(
                        followlocation = TRUE, 
                        maxconnects = 5,
                        maxredirs = 20,
                        timeout = 30,
                        connecttimeout = 30,
                        ssl.verifyhost = FALSE,
                        ssl.verifypeer = FALSE), 
                      postFUN = NULL, retrieveFeedURL = TRUE, ...){
  
  content_raw <- NULL
  if(retrieveFeedURL) {
    content_raw <- getURL(feedurls, .opts = curlOpts)
  } else {
    content_raw <- feedurls
  }
  content_raw <- content_raw[sapply(content_raw, nchar) > 0]
  content_parsed <- unlist(lapply(content_raw, parser), recursive = FALSE)
  structure(list(encoding = encoding, length = length(content_parsed), names = NA_character_,
                 position = 0, reader = reader, content = content_parsed, feedurls = feedurls,
                 parser = parser, curlOpts = curlOpts, postFUN = postFUN, retrieveFeedURL = retrieveFeedURL, ...), 
            class = unique(c(class, "WebSource", "SimpleSource")))
}
source.update <- function(x){
  UseMethod("source.update", x)	
}
source.update.WebXMLSource <- 
  source.update.WebHTMLSource <- 
  source.update.WebJSONSource <- 
  function(x) {
    content_raw <- NULL
    if(x$retrieveFeedURL) {
      content_raw <- getURL(x$feedurls, .opts = x$curlOpts)
    } else {
      content_raw <- x$feedurls
    }
    content_raw <- content_raw[sapply(content_raw, nchar) > 0]
    
    content_parsed <- unlist(lapply(content_raw, x$parser), recursive = FALSE)
    x$content <- content_parsed
    x$position <- 0
    x
  }
GoogleFinanceSource <- function(query, params = 
                                  list( 	hl= 'en', 
                                         q=query, 
                                         ie='utf-8', 
                                         start = 0, 
                                         num = 20, 
                                         output='rss'),...){
  feed <- "http://www.google.com/finance/company_news"
  parser <- function(cr){
    tree <- parse(cr, type = "XML", asText = FALSE)
    
    xpathSApply(tree, path = "//item")
  }
  fq <- feedquery(feed, params)
  ws <- WebSource(feedurls = fq, class = "WebXMLSource", parser = parser, reader = readGoogle, 
                  postFUN = getLinkContent, retrieveFeedURL = FALSE,...)
  ws
  
}
GoogleNewsSource <- function(query, params = 
                               list(	hl= 'en', 
                                     q = query, 
                                     ie='utf-8', 
                                     num = 30, 
                                     output='rss'), ...){
  feed <- "http://news.google.com/news"
  fq <- feedquery(feed, params)
  parser <- function(cr){
    tree <- parse(cr, type = "XML", asText = TRUE)
    nodes <- xpathSApply(tree, path = "//item")
    xmlns1 <- lapply(nodes, newXMLNamespace, "http://purl.org/dc/elements/1.1/", "dc")
    nodes
  }
  ws <- WebSource(feedurls = fq, class = "WebXMLSource", parser = parser, reader = readGoogle,
                  postFUN = getLinkContent, retrieveFeedURL = TRUE, ...)
  ws
}
YahooNewsSource <- function(query, params = 
                              list(	p= query), ...){
  feed <- "https://news.search.yahoo.com/search"
  fq <- feedquery(feed, params)
  parser <- function(cr){
    tree <- parse(cr, type = "HTML", useInternalNodes = TRUE)
    xpathSApply(tree, path = "//div[contains(@class, 'NewsArticle')]")
  }
  ws <- WebSource(feedurls = fq, class = "WebXMLSource", parser = parser, reader = readYahooHTML, 
                  postFUN = getLinkContent, ...)
  ws
}
YahooInplaySource <- function(...){
  url <- "http://finance.yahoo.com/marketupdate/inplay"
  parser <- function(cr){
    tree <- parse(cr, useInternalNodes = T, type = "HTML")
    xp_expr = "//div[@class= 'body yom-art-content clearfix']/p"
    paragraphs = xpathSApply(tree, xp_expr)
  }
  
  ws <- WebSource(feedurls = url, class = "WebHTMLSource", parser = parser, reader = readYahooInplay, ...)
  ws
}
getElem.WebXMLSource <- 
  getElem.WebHTMLSource <- function(x) {
    list(content = saveXML(x$content[[x$position]]), linkcontent = NULL, uri = NULL)
  }
getElem.WebJSONSource <- function(x) {
  list(content = x$content[[x$position]], linkcontent = NULL, uri = NULL)
}
encloseHTML <- function(x) UseMethod("encloseHTML", x)
encloseHTML.PlainTextDocument <- function(x){
  content(x) <- sprintf("<html>%s</html>", x)
  x
} 
removeNonASCII <- function(x, fields = c("Content", "Heading", "Description"), from = "UTF-8", to = "ASCII//TRANSLIT")
  UseMethod("removeNonASCII", x)
removeNonASCII.PlainTextDocument <- function(x, fields = c("Content", "Heading", "Description"), from = "UTF-8", to = "ASCII//TRANSLIT"){
  if("Content" %in% fields){
    content(x) <- iconv(x, from, to)
  }
  for(fn in setdiff(fields, "Content")){
    meta(x, fn) <- iconv(meta(x, fn), from, to)
  }
  x
}
trimWhiteSpaces <-
  function(txt){
    txt <- sub("\\s+", "", txt, perl = TRUE)
    txt <- sub("\\s+$", "", txt, perl = TRUE)
    txt <- gsub("\\s\\s+", " ", txt, perl = TRUE)
    return(txt)
  }
removePunctuation<-
  function(x, preserve_intra_word_dashes = FALSE)
    {
      if (!preserve_intra_word_dashes)
        gsub("[[:punct:]]+", "", x)
      else {
        # Assume there are no ASCII 1 characters.
        x <- gsub("(\\w)-(\\w)", "\\1\1\\2", x)
        x <- gsub("[[:punct:]]+", "", x)
        gsub("\1", "-", x, fixed = TRUE)
      }
    }
 lower <- function (df){
   names(df) <- tolower(names(df))
   df
  } 
