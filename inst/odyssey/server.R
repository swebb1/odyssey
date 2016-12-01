
shinyServer(function(input, output,session) {

  ##list of file labels (short names for display)
  values <- reactiveValues(label_list=list())
  
  ##Get the input directory
  #roots=c(home = '/homes/')
  home_dir<-Sys.getenv("HOME")
  roots=c(home= home_dir,wd=".")
  shinyDirChoose(input, 'directory',roots=roots)
  input_dir <- reactive({
    id<-""
    if(!is.null(input$directory)){
      id<-parseDirPath(roots, input$directory)
    }
    return(id)
  })
  output$path <- renderText(input_dir())
  
  ##Get files function from input directory
  get_files<-function(pattern){
    dir<-input_dir()
    file_list<-list.files(dir,include.dirs = T,recursive=input$recursive,pattern=pattern,full.names = T)
  }
  
  ##Create list of bed files or GR objects saved as .rds files
  bedGRList<-reactive({
    bedGRList<-list()
    if(length(input$bedFiles>0)){
      for(i in 1:length(input$bedFiles)){
        #b<-readGeneric(input$bedFiles[i],keep.all.metadata = T,header = F,strand = 6)  ##Maybe switch to read generic???
        b<-readBed(input$bedFiles[i])
        if(is.null(values$label_list[[input$bedFiles[i]]])){values$label_list[[input$bedFiles[i]]]<-basename(input$bedFiles[i])}
        n<-values$label_list[[input$bedFiles[i]]]
        bedGRList[[n]]<-b
      }
    }
    if(length(input$rdsFiles>0)){
      for(i in 1:length(input$rdsFiles)){
        gr<-readRDS(input$rdsFiles[i])
        if(is.null(values$label_list[[input$rdsFiles[i]]])){values$label_list[[input$rdsFiles[i]]]<-basename(input$rdsFiles[i])}
        n<-values$label_list[[input$rdsFiles[i]]]
        bedGRList[[n]]<-gr
      }
    }
    bedGRList
  })
  
  ##Create list of bigWig files
  bwList<-reactive({
    bwList<-list()
    if(length(input$bwFiles>0)){
      for(i in 1:length(input$bwFiles)){
        b<-input$bwFiles[i]
        if(is.null(values$label_list[[input$bwFiles[i]]])){values$label_list[[input$bwFiles[i]]]<-basename(input$bwFiles[i])}
        n<-values$label_list[[input$bwFiles[i]]]
        bwList[[n]]<-b
      }
    }
    bwList
  })
  
  ##Output list of files under input directory
  get_input_files<-eventReactive(input$list_dir, {
      uilist<-tagList(  
        checkboxGroupInput('bedFiles', 'Select bed files (.bed):',get_files(pattern="*.bed$")),
        checkboxGroupInput('rdsFiles', 'Select R files (.rds):',get_files(pattern="*.gr.rds$")),
        checkboxGroupInput('bwFiles', 'Select bigWig files (.bw):',get_files(pattern="*.bw$"))
      )
      return(uilist)
  })
  
  ##Set ui for selecting files
  output$inFiles <- renderUI({
    withProgress(message="Loading...",value=0,{
      get_input_files()
    })
  })
  
  ##Save labels for filenames
  observeEvent(input$saveLabels,{ 
    if(!is.null(input$bedFiles)){
      for(i in 1:length(input$bedFiles)){
        eval(parse(text=paste0("values$label_list[['",input$bedFiles[i],"']]<-'",eval(parse(text=paste0("input$bedLabel",i))),"'")))
      }
    }
    if(!is.null(input$rdsFiles)){
      for(i in 1:length(input$rdsFiles)){
        eval(parse(text=paste0("values$label_list[['",input$rdsFiles[i],"']]<-'",eval(parse(text=paste0("input$rdsLabel",i))),"'")))
      }
    }
    if(!is.null(input$bwFiles)){
      for(i in 1:length(input$bwFiles)){
        eval(parse(text=paste0("values$label_list[['",input$bwFiles[i],"']]<-'",eval(parse(text=paste0("input$bwLabel",i))),"'")))
      }
    }
  })
  
  ##function to generate label names
  lname<-function(n){
    if(!is.null(values$label_list[[n]])){
      return(values$label_list[[n]])
    }
    else{
      return(basename(n))
    }
  }
  
  ##Create the labeling UI
  output$labels<-renderUI({
    if(is.null(input$bedFiles) & is.null(input$rdsFiles) & is.null(input$bwFiles)){
      return(NULL)
    }
    llist<-tagList()
    if(!is.null(input$bedFiles)){
      for(i in 1:length(input$bedFiles)){
        label<-textInput(paste0("bedLabel",i),label = input$bedFiles[i],value = lname(input$bedFiles[i]))
        llist[[i]]<-label
      }
    }
    if(!is.null(input$rdsFiles)){
      for(i in 1:length(input$rdsFiles)){
        label<-textInput(paste0("rdsLabel",i),label = input$rdsFiles[i],value = lname(input$rdsFiles[i]))
        llist[[length(input$bedFiles)+i]]<-label
      }
    }
    if(!is.null(input$bwFiles)){
      for(i in 1:length(input$bwFiles)){
        label<-textInput(paste0("bwLabel",i),label = input$bwFiles[i],value = lname(input$bwFiles[i]))
        llist[[length(input$bedFiles)+length(input$rdsFiles)+i]]<-label
      }
    }
    llist
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
  
  ##R code to apply to selected bed/GR object
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
  
  ##Render table of GR/Bed regions
  output$table<-renderDataTable({
    if(is.null(input$bedFiles) & is.null(input$rdsFiles)){
      return(NULL)
    }
    gr<-rCode() #add this function for changing gr ##Need button?
    t<-as.data.frame(gr)
    t
  },options = list(bSortClasses = TRUE,aLengthMenu = c(5,10,20,50,100), iDisplayLength = 5)
  )
  
  ##Save the displayed table as a granges .rds object
  observeEvent(input$save, {
    gr<-rCode()
    name<-paste0(input_dir(),"/",input$save_name,".gr.rds")
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
        textInput("gviz_chr","Chromosome:","chrII"),
        numericInput("gviz_start","Region start:",1000),
        numericInput("gviz_end","Region end:",2000)
      ),
      conditionalPanel(condition="input.gviz_select == 'Gene ID'",
                       textInput("gviz_gene","Ensembl gene ID:","YBL087C"),
                       numericInput("gviz_left","Extend left:",0),
                       numericInput("gviz_right","Extend right:",0)
      ),
      selectInput("gviz_bedIn","Select interval files",choices = names(bedGRList()),multiple = T),
      selectInput("gviz_bwIn","Select bw files",choices = names(bwList()),multiple=T)
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
    bw_list<- bwList()
    if(length(input$gviz_bedIn>0)){
      for(i in 1:length(input$gviz_bedIn)){
        btrack<-AnnotationTrack(blist[[basename(input$gviz_bedIn[i])]],name=basename((input$gviz_bedIn[i])),
                                showFeatureId=T,showOverplotting=T,just.group="above")
        tracks<-c(tracks,btrack)
      }
    }
    if(length(input$gviz_bwIn>0)){
      for(i in 1:length(input$gviz_bwIn)){
        bwtrack<-DataTrack(range=bw_list[[input$gviz_bwIn[i]]],genome=input$gviz_genome,name=basename(input$gviz_bwIn[i]),
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
      selectInput("seqplots_bwIn","Select bw files",choices = names(bwList()),multiple=T),
      checkboxInput("seqplots_motif",label = "Add sequence motifs",value = F),
      conditionalPanel(
        condition = "input.seqplots_motif == true",
        textInput("seqplots_motifIn","Sequence motifs:",""),
        helpText("Separate multiple motifs with a comma."),
        numericInput("seqplots_window","Window size",value = 200),
        checkboxInput("seqplots_revcomp","Include reverse complement",value = T)
      )
      )
  })
  
  getPlotSet<-eventReactive(input$seqplots_plot,{
    withProgress(message="Calculating...",value=0,{
    m<-MotifSetup()
    tracks<-NULL
    if(input$seqplots_motif  & input$seqplots_motifIn!=""){
      mlist<-unlist(strsplit(input$seqplots_motifIn,split = ","))
      for(i in 1:length(mlist)){
        m$addMotif(mlist[i],window=input$seqplots_window,heatmap=T,revcomp=input$seqplots_revcomp,genome=input$seqplots_genome) ##parse strsplit for multiple motifs
      }
    }
    if(!is.null(input$seqplots_bwIn)){
      for(i in 1:length(input$seqplots_bwIn)){
        m$addBigWig(bwList()[[input$seqplots_bwIn[i]]])
      }
    }
    seqplots_type<-switch(input$seqplots_type,"Start of feature"="pf","Midpoint"="mf","End of feature"="ef","Anchor feature"="af")
    plotset<-getPlotSetArray(tracks=m,features=bedGRList()[input$seqplots_bedIn],refgenome = input$seqplots_genome,
                             bin = input$seqplots_bin,rm0 = input$seqplots_rm,ignore_strand = input$seqplots_ignorestrand,
                             xmin = input$seqplots_xmin,xmax = input$seqplots_xmax,xanchored = input$seqplots_anchored,
                             type = seqplots_type,add_heatmap = T,stat = input$seqplots_stat)
    return(plotset)
    })
  })
  
  labels<-eventReactive(input$seqplots_plot,{
    labels=NULL
    if(input$seqplots_setlabels){
      labels=unlist(strsplit(input$seqplots_labels,split = ","))
    }
    else{
      if(input$seqplots_motif  & input$seqplots_motifIn!=""){
        mlist<-unlist(strsplit(input$seqplots_motifIn,split = ","))
        for(j in 1:length(mlist)){
          for(i in 1:length(input$seqplots_bedIn)){
            labels<-append(labels,paste0(mlist[j],"@",input$seqplots_bedIn[i]))
          }
        }
      }
      for(j in 1:length(input$seqplots_bwIn)){
        for(i in 1:length(input$seqplots_bedIn)){
          labels<-append(labels,paste0(input$seqplots_bwIn[j],"@",input$seqplots_bedIn[i]))
        }
      }
    }
    return(labels)
  })
  
  seqplots_plot<-function(){
    if(is.null(getPlotSet())){#(length(input$seqplots_bedIn)==0 | length(input$seqplots_bwIn)==0 & input$seqplots_motif==F){
      return(NULL)
    }
    else{
      plotset<-getPlotSet()
      if(input$seqplots_output=="profile"){
        ylim=NULL
        labels=labels()
        if(input$seqplots_manual & !is.null(input$seqplots_ylim_min) & !is.null(input$seqplots_ylim_max)){
          ylim=c(input$seqplots_ylim_min,input$seqplots_ylim_max)
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
        #plotHeatmap(plotset,labels = labels()) #need to fix labels
      }
    }
  }
  
  output$seqplots_plot<-renderPlot({
    seqplots_plot()
  })
  
  output$seqlink <- downloadHandler(
    filename = function(){paste0(input$seqplotName,".",input$sftype)},
    content = function(file) {
      if(input$sftype=="png"){
        png(file,height = 600,width=900)
      }
      else{
        pdf(file,height = 6,width=9)
      }
      seqplots_plot()
      dev.off()
    }
  )
  
  
})    



