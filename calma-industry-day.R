library("RCurl")
library("SPARQL")
library("rrdf")
library("ggplot2")
library("plyr")
library("dplyr")
library("magrittr")
library("stringr")
library("readr")
library("httr")
library("svglite")
# if the following is true, override the etreeTrackQuery and use the sample instead
injectSample <- FALSE
setwd(tempdir())
#following line: see http://www.bramschoenmakers.nl/en/node/726
#options( java.parameters = "-Xmx3g" )
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
  BIND(<http://etree.linkedmusic.org/artist/422faf80-4aac-012f-19e9-00254bd44c28> as ?artist) .
  ?artist mo:performed ?performance .
  ?performance event:hasSubEvent ?track .
  ?track calma:data ?calma ;
  skos:prefLabel \"Today\" .
}
ORDER BY ?calma
LIMIT 100
"

whatFeaturesQuery <- "
prefix prov: <http://www.w3.org/ns/prov#>
select distinct ?feature where {
?file prov:wasAssociatedWith ?feature
}
ORDER BY ?feature"

calmaFeatureQuery <- "
PREFIX af: <http://purl.org/ontology/af/>
PREFIX mo: <http://purl.org/ontology/mo/>
PREFIX event: <http://purl.org/NET/c4dm/event.owl#>
PREFIX tl: <http://purl.org/NET/c4dm/timeline.owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

select distinct ?event ?feature ?key ?onsetTime ?audioFile where {
?event a af:KeyChange ;
event:time ?time ;
af:feature ?feature ;
rdfs:label ?key .
?time tl:at ?onsetTime ;
  tl:onTimeLine ?tl .
?sig a mo:Signal ; 
  mo:time ?sigTime .
?sigTime tl:onTimeLine ?tl .
?audioFile a mo:AudioFile ; 
  mo:encodes ?sig .
}
ORDER BY ?event"

audioFileMetadataQuery <- "
PREFIX etree: <http://etree.linkedmusic.org/vocab/>
PREFIX af: <http://purl.org/ontology/af/>
PREFIX mo: <http://purl.org/ontology/mo/>
PREFIX event: <http://purl.org/NET/c4dm/event.owl#>
PREFIX tl: <http://purl.org/NET/c4dm/timeline.owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

select ?etree ?label ?audioFile WHERE {
  ?etree etree:audio ?audioFile ;
    etree:isSubEventOf ?performance .
  ?performance etree:date ?date ;
    event:place ?place .
  ?place etree:name ?name ; 
    etree:location ?location .
  BIND(CONCAT(?date, ' ', ?name, ' @ ', ?location) as ?label) .
  VALUES ?audioFile { AUDIOFILE } .
}"

graphify <- function(files, remote=TRUE) {
  if(remote) {
    files <- gsub('<|>', '', files) # get rid of uri decoration if any
    data <- gsub('\\"', '"', getURL(files)) # un-escape quote marks
   # files = gsub("http://calma.linkedmusic.org/data/", "", files)
    files = gsub("http://calma.linkedmusic.org/data/../", "", files)
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
    summarize.rdf(g)
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

queryFeatureFiles <- function(g, feature) {
  calmaFileQuery <- paste0("
                           prefix prov: <http://www.w3.org/ns/prov#>
                           select distinct ?file where {
                           ?file prov:wasAssociatedWith <", feature, ">
                           }
                           ORDER BY ?file")
  files <- sparql.rdf(g, calmaFileQuery)
  return (files)
}

calculateEventDurations <- function(featureData) {
  calcPerTrack <- function(trackData) {
    for(i in 1:nrow(trackData)) {
      trackData[i, "duration"] <- trackData[i+1, "onsetTime"] - trackData[i, "onsetTime"]
      # FIXME for last event, calculate based on track duration
    }
    return(trackData)
  }
  result <- data.frame()
  tracks <- unique(featureData$track)
  for (t in 1:length(tracks)){
    result <- rbind(result, calcPerTrack(featureData[featureData$track == tracks[t], ]))
  }
  return (result)
}


if(injectSample) {
  sample_files <- getSampleTracks()
  sample_files$calmatrack <- str_replace_all(sample_files$calma, '(<http://calma.linkedmusic.org/data/|>)', '')
  sample_files$newloc = substr(sample_files$calmatrack, start = 7, stop=8)
  files <- paste0("http://eeboo.oerc.ox.ac.uk/sample_calma_data/", sample_files$newloc, "/", sample_files$tracks, sample_files$calmatrack, "/analyses.ttl")
  sample_files$files <- files
  sample_files$artist <- factor(sample_files$artist)
  sample_files$trackname <- factor(sample_files$trackname)
  by_artist_song_features <- sample_files %>% 
  group_by(artist) %>%
  select(artist)
  grand_output <- dlply(sample_files, "artist", getFeatureData)
  
} else {
  result <- SPARQL(endpoint, etreeTrackQuery)$results
  # chop off the <'s and >'s
  calma <- substr(result$calma, 2, nchar(result$calma))
  calma <- substr(calma, 1, nchar(calma)-1)
  # add trailing /
  calma <- paste0(calma, "/")
  files <- paste0(calma, "analyses.ttl")
  etreeCalma <- cbind(result, files)
}





g <- graphify(files, remote=TRUE)

# What features are available for these files?
features <- sparql.rdf(g, whatFeaturesQuery)

#featurefiles <- queryFeatureFiles(g, features[18])
featurefiles <- queryFeatureFiles(g, features[9])
g <- graphify(featurefiles)
etreeCalma <- cbind(etreeCalma, featurefiles)
names(etreeCalma) <- c("etree", "calma", "analysis.ttl", "feature")
etreeCalma$etree <- factor(etreeCalma$etree)
etreeCalma$calma<- factor(etreeCalma$calma)

query = "select distinct ?blob where {
?s <http://calma.linkedmusic.org/vocab/feature_blob> ?blob
}
ORDER BY ?blob"
blobFileURIs <- sparql.rdf(g, query)
blobFileDir <- untarBlobs(blobFileURIs)
featureGraph <- graphify(blobFileDir, remote=FALSE)
summarize.rdf(featureGraph)

featureData <- as.data.frame(sparql.rdf(featureGraph, calmaFeatureQuery))
featureData$track <- factor(str_remove(
  (gsub(".*/([^/#]+).*$", "\\1", as.character(featureData$event),fixed=FALSE, perl=TRUE)),
  "_vamp_qm-vamp-plugins_qm-keydetector_key.n3"))
#featureData$track <- factor((gsub(".*/([^/#]+)\\.wav.*$", "\\1", as.character(featureData$event),fixed=FALSE, perl=TRUE)))
featureData$eventNum <- as.numeric(gsub(".*event_(\\d+)", "\\1", as.character(featureData$event),fixed=FALSE, perl=TRUE))
featureData$feature <- as.numeric(as.character(featureData$feature))
featureData$onsetTime <- as.numeric(gsub("[PTS]", "", as.character(featureData$onsetTime), fixed=FALSE, perl=TRUE))
featureData <- unique(featureData[order(featureData$track, featureData$eventNum),])
featureData <- calculateEventDurations(featureData)

audioFileMetadataQuery <- str_replace(audioFileMetadataQuery, "AUDIOFILE", paste0( "<", unique(featureData$audioFile), ">", collapse=" "))
# slight inconvenience -- our parameterised query is likely to long for a GET
# so we need ot do the following line to POST our query to the virtuoso endpoint
audioFileMetadata <- read_csv(POST(url=endpoint, encode = c("form"), body=list(query = audioFileMetadataQuery), accept("text/csv"))$content)
featureData <- featureData %>% join(audioFileMetadata)
                                                  


featureData <- select(featureData, key, track, duration, etree, audioFile, label) %>%
  group_by(track, key, etree, audioFile, label) %>%
  filter(key != "N") %>% # remove NA's
  summarise(key_duration = sum(duration, na.rm=TRUE))

featureData <- inner_join(featureData, select(featureData, track, key_duration) %>%
                            group_by(track) %>%
                            summarise(track_duration = sum(key_duration, na.rm=TRUE)))


corpusScore <- group_by(featureData, key) %>%
  summarise(key_corpus_duration = sum(key_duration)) %>%
  ungroup() %>%
  mutate(total_corpus_duration = sum(key_corpus_duration)) %>%
  mutate(key_corpus_proportion = key_corpus_duration / total_corpus_duration) %>%
  arrange(desc(key_corpus_proportion)) %>%
  mutate(key_corpus_rank = seq_along(key_corpus_proportion))

# normalise corpus proportions
maxProp <- max(corpusScore$key_corpus_proportion)
corpusScore$normalised_key_corpus_prop <- corpusScore$key_corpus_proportion / maxProp

#keyScore <- group_by(featureData, track) %>%
#  arrange(desc(key_duration)) %>%
#  mutate(key_rank = seq_along(key_duration)) %>%
#  mutate(key_duration_weighted = (key_duration / (track_duration*(key_rank)))) %>%
#  summarise(keyScore = sum(key_duration_weighted))

keyScore <- inner_join(featureData, corpusScore, by=c("key")) %>%
  mutate(track_key_score = (key_duration / track_duration) * key_corpus_proportion) %>%
  mutate(normalised_track_key_score = (key_duration / track_duration) * normalised_key_corpus_prop) %>%
  group_by(etree) %>%
  summarise(key_score = sum(track_key_score), normalised_key_score = sum(normalised_track_key_score)) 

# order tracks by key score
keyScore <- keyScore[order(desc(keyScore$key_score)),]
#featureData$etree <- factor(featureData$etree, levels = keyScore$etree)
featureData$etree <- factor(featureData$etree)
keyScore$etree <- factor(keyScore$etree)


# add font size relative to duration in key per track
featureData <- featureData %>% 
    group_by(etree, key) %>% 
    mutate(fontSize = min(6, 10*(key_duration/track_duration))) 

# arrange according to key score
featureDataKeyScore <- inner_join(featureData, keyScore) %>%
    ungroup(etree, key) %>%
    mutate(etree = reorder(etree, desc(normalised_key_score)))

etreeLabels <- featureDataKeyScore %>% 
    ungroup(key) %>% 
    select(etree, label, normalised_key_score) %>% 
    unique %>%
    mutate(label = str_replace(label, " @ ", "\n"))

p <- ggplot(featureDataKeyScore, aes(key, key_duration)) + 
  geom_bar(data=featureDataKeyScore, stat="identity") + facet_wrap(~etree) + theme_bw() +
  #geom_text(data=keymappings, aes(feature, 0, label=key, angle=90), color="#aaaaaa", hjust=0, size=2) +
  geom_text(data=featureDataKeyScore, aes(key, key_duration + 5, label=key, size=5+fontSize), color="#aaaaaa") +
  geom_text(data=keyScore, aes(5, 200, label=round(normalised_key_score, digits = 2)), color="red") + 
  geom_label(data=etreeLabels, aes(25, 150, label=label), hjust=1, lineheight=.8, size=2.5, alpha=.3, color="red") + 
  labs(x="Feature (Key). Normalised key typicality score in red.", y = "Total duration (seconds)") + scale_y_continuous(breaks=seq(1,300,50)) +
  theme(text = element_text(size = 10), axis.text.x = element_blank(), axis.ticks.x = element_blank(), strip.text = element_blank()) +
  guides(size = FALSE)

ggsave(file="foo.svg", plot=p, width=20, height=12)
