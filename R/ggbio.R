
library(ggbio)
library(GenomicRanges)

data(hg19IdeogramCyto, package = "biovizBase")
autoplot(hg19IdeogramCyto, layout = "karyogram", cytoband = TRUE)
hg19 <- keepSeqlevels(hg19IdeogramCyto, paste0("chr", c(1:22, "X", "Y")))
autoplot(hg19, layout = "karyogram", cytoband = FALSE, aes(fill = gieStain)) +
  scale_fill_giemsa()
genes <- genomation::readBed("inst/odyssey/test_data/Ensembl.GRCh37.74.edited.pc_genes.bed")
data(hg19Ideogram, package = "biovizBase")
seqlengths(genes) <- seqlengths(hg19Ideogram)[names(seqlengths(genes))]
genes <- keepSeqlevels(genes, paste0("chr", c(1:22, "X")))
autoplot(genes, layout = "karyogram",color="dodger blue")


p.ideo <- Ideogram(genome = "hg19")
p.ideo
## special highlights instead of zoomin!
p.ideo + xlim(GRanges("chr2", IRanges(1, 1e8)))


library(Homo.sapiens)
##
data(genesymbol, package = "biovizBase")
wh <- genesymbol[c("BRCA1", "NBR1")]
wh <- range(wh, ignore.strand = TRUE)
p.txdb <- autoplot(Homo.sapiens, which  = wh)
p.txdb
autoplot(Homo.sapiens, which  = wh, label.color = "black", color = "brown",
         fill = "brown")

autoplot(Homo.sapiens, which  = wh, gap.geom = "chevron")

columns(Homo.sapiens)
autoplot(Homo.sapiens, which  = wh, columns = c("TXNAME", "GO"), names.expr = "TXNAME::GO")

library(EnsDb.Hsapiens.v75)
ensdb <- EnsDb.Hsapiens.v75
autoplot(ensdb, GenenameFilter("PHKG2"))
gr <- GRanges(seqnames=16, IRanges(30768000, 30770000), strand="*")
autoplot(ensdb, GRangesFilter(gr, "overlapping"), names.expr="gene_name")

library(BSgenome.Hsapiens.UCSC.hg19)
bg <- BSgenome.Hsapiens.UCSC.hg19
p.bg <- autoplot(bg, which = wh)
## no geom
p.bg
## segment
p.txdb + zoom(1/10)
## rectangle
p.txdb + zoom(1/1000)
## text
p.txdb + zoom(1/2500)


fl.bam <- system.file("extdata", "wg-brca1.sorted.bam", package  = "biovizBase")
wh <- keepSeqlevels(wh, "chr17")
autoplot(fl.bam, which = wh)
fl.bam <- system.file("extdata", "wg-brca1.sorted.bam", package  = "biovizBase")
wh <- keepSeqlevels(wh, "chr17")
autoplot(fl.bam, which = resize(wh, width = width(wh)/10), geom = "gapped.pair")
library(BSgenome.Hsapiens.UCSC.hg19)
bg <- BSgenome.Hsapiens.UCSC.hg19
p.mis <- autoplot(fl.bam, bsgenome = bg, which = wh, stat = "mismatch")
p.mis
autoplot(fl.bam, method = "estimate")
