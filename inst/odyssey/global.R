library(shiny)
library(shinyFiles)
library(genomation)
library(GenomicRanges)
library(GenomicFeatures)
library(seqplots)
library(IdeoViz)
library(Gviz)
library(ggbio)
library(biovizBase)
library(RColorBrewer)
library(htmltools)

#set maximum file size
options(shiny.maxRequestSize=100*1024^2) #100Mb

#setup a color pallete choice on main dashboard or per tool?
cols<-brewer.pal(9,"Set1")

#available genomes
genomes<-c("sacCer3","hg19","mm9")

#chrs_global<-list()
#chrs_global[["sacCer3"]]=c(Seqinfo(genome="sacCer3")@seqnames)
#chrs_global[["hg19"]]=c(Seqinfo(genome="hg19")@seqnames)
#chrs_global[["mm9"]]=c(Seqinfo(genome="mm9")@seqnames)


#library(biomaRt)
#ensembl67 <- useMart(host='may2012.archive.ensembl.org',
#                     biomart='ENSEMBL_MART_ENSEMBL') #uses ensemble67 build
#ensembl67 <- useDataset("mmusculus_gene_ensembl", mart=ensembl67)
