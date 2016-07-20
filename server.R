
shinyServer(function(input, output,session) {
  
  get_files<-function(pattern){
    dir<-input$dir
    file_list<-list.files(dir,include.dirs = T,recursive=T,pattern=pattern,full.names = T)
  }
  
  bedGRList<-reactive({
    bedGRList<-list()
    if(length(input$bedFiles>0)){
      for(i in 1:length(input$bedFiles)){
        #b<-readGeneric(input$bedFiles[i],keep.all.metadata = T,header = T)  ##Maybe switch to read generic???
        b<-readBed(input$bedFiles[i])
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
    withProgress(message="Loading...",value=0,{
    output$inFiles <- renderUI({
      tagList(  
        checkboxGroupInput('bedFiles', 'Select bed files (.bed):',get_files(pattern="*.bed")),
        checkboxGroupInput('rdsFiles', 'Select R files (.rds):',get_files(pattern="*.rds")),
        checkboxGroupInput('bwFiles', 'Select bigWig files (.bw):',get_files(pattern="*.bw"))
      )
    })
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
  ###Add labels, change colours, better labelling of features,search by symbol
  output$gviz_controls<-renderUI({
    tagList(
      selectInput("gviz_genome","Select genome:",choices=genomes),
      selectInput("gviz_select","Select region by:",choices=c("Coordinates","Gene ID")),
      conditionalPanel(condition="input.gviz_select == 'Coordinates'",
        selectInput("gviz_chr","Chromosome:",choices=Seqinfo(genome=input$gviz_genome)@seqnames),
        numericInput("gviz_start","Region start:",1000),
        numericInput("gviz_end","Region end:",2000)
      ),
      conditionalPanel(condition="input.gviz_select == 'Gene ID'",
                       textInput("gviz_gene","Ensembl gene ID:","YBL087C"),
                       numericInput("gviz_left","Extend left:",0),
                       numericInput("gviz_right","Extend right:",0)
      ),
      selectInput("gviz_bedIn","Select interval files",choices = names(bedGRList()),multiple = T),
      selectInput("gviz_bwIn","Select bw files",choices = input$bwFiles,multiple=T)
    )
  })
  
  output$gviz_plot<-renderPlot({
    gtrack <- GenomeAxisTrack(fontsize=15)
    ideoTrack <- IdeogramTrack(genome =input$gviz_genome)
    if(input$gviz_select=="Coordinates"){
      biomTrack <- BiomartGeneRegionTrack(genome = input$gviz_genome,
                                          chromosome = input$gviz_chr, start = input$gviz_start, end = input$gviz_end,
                                          name = "ENSEMBL",col.line = NULL, col = NULL,showId=T,just.group="above")
    }
    else{
      biomTrack <- BiomartGeneRegionTrack(genome = input$gviz_genome,
                                          gene=input$gviz_gene,name = "ENSEMBL",col.line = NULL, 
                                          col = NULL,showId=T,just.group="above")
    }
    tracks<-list(ideoTrack,gtrack,biomTrack)
    #itrack <- IdeogramTrack(chromosome=input$gviz_chr ,genome=input$gviz_genome)
    blist <- bedGRList()
    if(length(input$gviz_bedIn>0)){
      for(i in 1:length(input$gviz_bedIn)){
        btrack<-AnnotationTrack(blist[[basename(input$gviz_bedIn[i])]],name=basename((input$gviz_bedIn[i])),
                                showFeatureId=T,showOverplotting=T,just.group="above")
        tracks<-c(tracks,btrack)
      }
    }
    if(length(input$gviz_bwIn>0)){
      for(i in 1:length(input$gviz_bwIn)){
        bwtrack<-DataTrack(range=input$gviz_bwIn[i],genome=input$gviz_genome,name=basename(input$gviz_bwIn[i]),
                           fontsize=15,type="hist")
        tracks<-c(tracks,bwtrack)
      }
    }
    #DataTrack(range=cgcf,genome="mm9",name="mCG %",fontsize=15,type="mountain",col.mountain="blue",fill.mountain=c("green","grey"),ylim=c(0,100))
    if(input$gviz_select=="Coordinates"){
      plotTracks(tracks,from = input$gviz_start, to=input$gviz_end,chromosome = biomTrack@chromosome)
    }
    else{
      plotTracks(tracks,from = biomTrack@start-input$gviz_left, to=biomTrack@end+input$gviz_right,chromosome = biomTrack@chromosome)
    }
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
                             bin = input$seqplots_bin,rm0 = input$seqplots_rm,ignore_strand = input$seqplots_ignorestrand,
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
        ylim=NULL
        labels=NULL
        if(input$seqplots_manual & !is.null(input$seqplots_ylim_min) & !is.null(input$seqplots_ylim_max)){
          ylim=c(input$seqplots_ylim_min,input$seqplots_ylim_max)
        }
        if(input$seqplots_setlabels){
          labels=unlist(strsplit(input$seqplots_labels,split = ","))
        }
        plotAverage(plotset,keepratio = input$seqplots_keepratio,
                    #ylim =c(input$seqplots_ylim_min,input$seqplots_ylim_max),
                    ylim =ylim,
                    main = input$seqplots_main,xlab = input$seqplots_xlab,
                    ylab =input$seqplots_ylab,plotScale = input$seqplots_scale,
                    error.estimates = input$seqplots_error,legend_pos = input$seqplots_leg,
                    ln.v = input$seqplots_vl,ln.h = input$seqplots_hl,
                    pointsize = input$seqplots_point,colvec = cols,labels=labels
                    ) ##need to add labels
      }
      else{
        plotHeatmap(plotset)
      }
    }
  })
    
})    



