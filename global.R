library(shiny)
library(genomation)
library(GenomicRanges)
library(GenomicFeatures)
library(seqplots)
library(IdeoViz)
library(Gviz)
library(RColorBrewer)

#set maximum file size
options(shiny.maxRequestSize=100*1024^2) #100Mb

#setup a color pallete choice on main dashboard or per tool?
cols<-brewer.pal(9,"Set1")

#available genomes
genomes<-c("sacCer3","hg19","mm9")