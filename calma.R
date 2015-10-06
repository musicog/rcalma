library("RCurl")
library("SPARQL")
library("rrdf")
library("ggplot2")
setwd(tempdir())
endpoint = "http://etree.linkedmusic.org/sparql"

etreeTrackQuery <- "
PREFIX etree:<http://etree.linkedmusic.org/vocab/>
PREFIX mo:<http://purl.org/ontology/mo/>
PREFIX event:<http://purl.org/NET/c4dm/event.owl#>
PREFIX skos:<http://www.w3.org/2004/02/skos/core#>
PREFIX timeline:<http://purl.org/NET/c4dm/timeline.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX calma:<http://calma.linkedmusic.org/vocab/>
SELECT ?track ?calma
{
    BIND(<http://etree.linkedmusic.org/performance/alo2004-07-16> as ?performance) .
    ?performance event:hasSubEvent ?track .
    ?track calma:data ?calma .
}
ORDER BY ?calma
"
calmaFileQuery <- "
prefix prov: <http://www.w3.org/ns/prov#> 
select distinct ?file where { 
   ?file prov:wasAssociatedWith <http://vamp-plugins.org/rdf/plugins/qm-vamp-plugins#qm-tempotracker_output_tempo>
}
ORDER BY ?file"

calmaFeatureQuery <- "
PREFIX af: <http://purl.org/ontology/af/>
PREFIX mo: <http://purl.org/ontology/mo/>
PREFIX event: <http://purl.org/NET/c4dm/event.owl#>
PREFIX tl: <http://purl.org/NET/c4dm/timeline.owl#> 

select ?event ?feature where { 
  ?file a mo:AudioFile .
  ?event a af:Tempo ;
         af:feature ?feature .
}
ORDER BY ?file"


graphify <- function(files, remote=TRUE) { 
  if(remote) { 
    files <- gsub('<|>', '', files) # get rid of uri decoration if any
    data <- gsub('\\"', '"', getURL(files)) # un-escape quote marks
    files = gsub("http://calma.linkedmusic.org/data/", "", files)
    files = gsub("/", "--", files)
  }
  else { # got sent a local directory
    files <- paste0(files, "/", list.files(files))
  }
  
  g = new.rdf()
  for (f in 1:length(files)) { 
    if(remote){ # need to write to our local tmp folder
      track <- files[f]
      track <- regmatches(track, gregexpr(pattern = "track_[^/]*", track))[[1]]
      cat(x=data[f], file=files[f], append=FALSE)
    }
    thisg = load.rdf(filename = files[f], format="TURTLE")
    g = combine.rdf(g, load.rdf(filename = files[f], format="TURTLE"))
  }
  return(g)
}

untarBlobs <- function(blobURIs) { 
  tmp = tempfile(tmpdir = tempdir())
  blobFiles <- basename(blobURIs)
  for (f in 1:length(blobFiles)) {
    download.file(blobURIs[f], blobFiles[f])
    untar(blobFiles[f], compressed = 'gzip', exdir = tmp)
    file = gsub(".tar.gz", "", blobFiles[f])
    file = gsub(".tar.bz2", "", file)
    file <- paste0(tmp,  "/", file)
  }
  files <- paste0(tmp, "/", list.files(tmp))
  print("---------------------")
  for (f in 1:length(files)) { 
      contextualized <- gsub("<#>", paste0("<", files[f], "#>"), readLines(files[f]))
      cat(x = contextualized, file=files[f], append = FALSE)
    
    }
  return(tmp)
}

result <- SPARQL(endpoint, etreeTrackQuery)$results
# chop off the <'s and >'s 
calma <- substr(result$calma, 2, nchar(result$calma))
calma <- substr(calma, 1, nchar(calma)-1)
# add trailing /
calma <- paste0(calma, "/")
files <- paste0(calma, "analyses.ttl")
etreeCalma <- cbind(result, files)
g <- graphify(files)
featurefiles <- sparql.rdf(g, calmaFileQuery)
g <- graphify(featurefiles)
etreeCalma <- cbind(etreeCalma, featurefiles)
names(etreeCalma) <- c("etree", "calma", "analysis.ttl", "feature")
etreeCalma$etree <- factor(etreeCalma$etree)
etreeCalma$calma<- factor(etreeCalma$calma)

query = "select distinct ?blob where { ?s <http://calma.linkedmusic.org/vocab/feature_blob> ?blob }"
blobFileURIs <- sparql.rdf(g, query)
blobFileDir <- untarBlobs(blobFileURIs)
featureGraph <- graphify(blobFileDir, remote=FALSE)
summarize.rdf(featureGraph)

featureData <- as.data.frame(sparql.rdf(featureGraph, calmaFeatureQuery))
featureData$track <- factor((gsub(".*/([^/#]+)#.*", "\\1", as.character(featureData$event),fixed=FALSE, perl=TRUE)))
featureData$eventNum <- as.numeric(gsub(".*event_(\\d+)", "\\1", featureData$event,fixed=FALSE, perl=TRUE))
featureData$feature <- as.numeric(as.character(featureData$feature))
featureData <- unique(featureData[order(featureData$track, featureData$eventNum),])
ggplot(featureData, aes(eventNum, feature)) + geom_line(aes(color=track)) + facet_wrap(~track) + theme_bw() 










