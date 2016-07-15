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

shinyServer(function(input, output,session) {
  
  get_files<-function(pattern){
    dir<-input$dir
    file_list<-list.files(dir,include.dirs = T,recursive=T,pattern=pattern,full.names = T)
  }
  
  bedGRList<-reactive({
    bedGRList<-list()
    if(length(input$bedFiles>0)){
      for(i in 1:length(input$bedFiles)){
        b<-readBed(input$bedFiles[i])  ##Maybe switch to read generic???
        n<-basename(input$bedFiles[i])
        bedGRList[[n]]<-b
      }
    }
    if(length(input$rdsFiles>0)){
      for(i in 1:length(input$rdsFiles)){
        gr<-readRDS(input$rdsFiles[i])
        n<-basename(input$rdsFiles[i])
        bedGRList[[n]]<-gr
      }
    }
    bedGRList
  })
  
  observeEvent(input$list_dir, {
    output$inFiles <- renderUI({
      tagList(  
        checkboxGroupInput('bedFiles', 'Select bed files (.bed):',get_files(pattern="*.bed")),
        checkboxGroupInput('rdsFiles', 'Select R files (.rds):',get_files(pattern="*.rds")),
        checkboxGroupInput('bwFiles', 'Select bigWig files (.bw):',get_files(pattern="*.bw"))
      )
    })
  })
  
  ##data table
  output$bedFiles<-renderUI({
    if(is.null(input$bedFiles) & is.null(input$rdsFiles)){
      return(NULL)
    }
    tagList(
      radioButtons("bedIn","Select bed file",choices = names(bedGRList()))
    )
  })
  
  rCode<-reactive({
    gr<-bedGRList()[[input$bedIn]]
    if(input$apply_code){
      withProgress(message="Applying operation...",value=0,{
        a<-strsplit(input$code,"@")
        for(i in 1:length(a[[1]])){
          eval(parse(text=a[[1]][i]))
        }
      })
    }
    if(length(gr) == 0){
      return(NULL)
    }
    gr
  })
  
  output$table<-renderDataTable({
    if(is.null(input$bedFiles) & is.null(input$rdsFiles)){
      return(NULL)
    }
    gr<-rCode() #add this function for changing gr ##Need button?
    t<-as.data.frame(gr)
    t
  },options = list(bSortClasses = TRUE,aLengthMenu = c(5,10,20,50,100), iDisplayLength = 5)
  )
  
  observeEvent(input$save, {
    gr<-rCode()
    name<-paste0(input$dir,"/",input$save_name,".rds")
    saveRDS(gr,name)
    files<-c(input$rdsFiles,name)
    updateCheckboxGroupInput(session,inputId="rdsFiles",choices=files,selected=files)
    session$sendCustomMessage(type = 'testmessage',
                              message = 'File saved')
  })
  
  #ideoViz
  output$ideoViz_controls<-renderUI({
    tagList(
      selectInput("ideo_genome","Select genome:",choices=genomes),
      checkboxGroupInput("ideo_bwIn","Select bw files",choices = input$bwFiles)
    )
  })
  
  output$ideoViz_plot<-renderPlot({
    ideo <- getIdeo(input$ideo_genome)
    plotOnIdeo(chrom=seqlevels(binned_multiSeries), # which chrom to plot?
               ideoTable=ideo, # ideogram name
               values_GR=binned_multiSeries, # data goes here
               value_cols=colnames(mcols(binned_multiSeries)), # col to plot
               col=brewer.pal(n=5, "Spectral"), # colours
               val_range=c(0,10), # set y-axis range
               ylab="",
               plot_title="")
  })
  
  #Gviz
  output$gviz_controls<-renderUI({
    tagList(
      selectInput("gviz_genome","Select genome:",choices=genomes),
      textInput("gviz_chr","Chromosome:","chrII"),
      numericInput("gviz_start","Region start:",1000),
      numericInput("gviz_end","Region start:",2000),
      selectInput("gviz_bedIn","Select interval files",choices = names(bedGRList()),multiple = T),
      selectInput("gviz_bwIn","Select bw files",choices = input$bwFiles,multiple=T)
    )
  })
  
  output$gviz_plot<-renderPlot({
    gtrack <- GenomeAxisTrack(fontsize=15)
    biomTrack <- BiomartGeneRegionTrack(genome = input$gviz_genome,
                                        chromosome = input$gviz_chr, start = input$gviz_start, end = input$gviz_end,
                                        name = "ENSEMBL")
    tracks<-list(gtrack,biomTrack)
    #itrack <- IdeogramTrack(chromosome=input$gviz_chr ,genome=input$gviz_genome)
    blist <- bedGRList()
    if(length(input$gviz_bedIn>0)){
      for(i in 1:length(input$gviz_bedIn)){
        btrack<-AnnotationTrack(blist[[basename(input$gviz_bedIn[i])]],name=basename((input$gviz_bedIn[i])))
        tracks<-c(tracks,btrack)
      }
    }
    if(length(input$gviz_bedIn>0)){
      for(i in 1:length(input$gviz_bwIn)){
        bwtrack<-DataTrack(range=input$gviz_bwIn[i],genome=input$gviz_genome,name=basename(input$gviz_bwIn[i]),fontsize=15,type="hist")
        tracks<-c(tracks,bwtrack)
      }
    }
    #DataTrack(range=cgcf,genome="mm9",name="mCG %",fontsize=15,type="mountain",col.mountain="blue",fill.mountain=c("green","grey"),ylim=c(0,100))
    plotTracks(tracks)
  })    
  
  ##SeqPlots
  output$seqplots_controls<-renderUI({
    tagList(
      selectInput("seqplots_genome","Select genome:",choices=genomes),
      selectInput("seqplots_bedIn","Select bed files",choices = names(bedGRList()),multiple = T),
      selectInput("seqplots_bwIn","Select bw files",choices = input$bwFiles,multiple=T),
      checkboxInput("seqplots_motif",label = "Add sequence motif",value = F),
      conditionalPanel(
        condition = "input.seqplots_motif == true",
        textInput("seqplots_motifIn","Sequence motif:",""),
        numericInput("seqplots_window","Window size",value = 200),
        checkboxInput("seqplots_revcomp","Include reverse complement",value = T)
      )
    )
  })
  
  getPlotSet<-reactive({
    m<-MotifSetup()
    tracks<-NULL
    if(input$seqplots_motif  & input$seqplots_motifIn!=""){
      m$addMotif(input$seqplots_motifIn,window=input$seqplots_window,heatmap=T,revcomp=input$seqplots_revcomp,genome=input$seqplots_genome) ##parse strsplit for multiple motifs
    }
    for(i in 1:length(input$seqplots_bwIn)){
      m$addBigWig(input$seqplots_bwIn[i])
    }
    plotset<-getPlotSetArray(tracks=m,features=bedGRList()[input$seqplots_bedIn],refgenome = input$seqplots_genome,
                             bin = input$seqplots_bin,rm0 = F,ignore_strand = F,
                             xmin = input$seqplots_xmin,xmax = input$seqplots_xmax,xanchored = input$seqplots_anchored,
                             type = input$seqplots_type,add_heatmap = T,stat = input$seqplots_stat)
    return(plotset)
  })
  
  output$seqplots_plot<-renderPlot({
    if(length(input$seqplots_bedIn)==0 | length(input$seqplots_bwIn)==0 & input$seqplots_motif==F){
      return(NULL)
    }
    else{
      plotset<-getPlotSet()
      if(input$seqplots_output=="profile"){
        plotAverage(plotset)
      }
      else{
        plotHeatmap(plotset)
      }
    }
  })
    
})    



