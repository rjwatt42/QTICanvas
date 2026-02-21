chooseOne<-function(var) {
  use<-ceiling(runif(1)*length(var))
  return(var[use])
}
formatP<-function(p,digits=3) {
  pstr<-format(p,digits=digits)
  # remove leading "0"
  pstr<-substr(pstr,2,10)
  pstr[p<0.001]<-'<.001'
  return(pstr)
}

makeAssessment<-function(title="Assessment",questionText=questionText,n_questions=50,n_choices=4,hypothesisAll=hypothesisAll) {
  quizTitle=title
  
  
  # make the folders
  rootFolder<-paste0('./',quizTitle,'/')
  if (!dir.exists(rootFolder))  dir.create(rootFolder)
  
  mainFolder<-paste0(rootFolder,'/',quizTitle,'/')
  if (!dir.exists(mainFolder))  dir.create(mainFolder)
  
  webFolder<-paste0(rootFolder,
                    'web_resources','/')
  if (!dir.exists(webFolder))  dir.create(webFolder)
  webFolder<-paste0(webFolder,
                    'Data files for Assessments','/')
  if (!dir.exists(webFolder))  dir.create(webFolder)
  
  dataFolder<-paste0(webFolder,
                     quizTitle,'/')
  if (!dir.exists(dataFolder))  dir.create(dataFolder)
  # done
  
  # for links
  dataLink<-paste0('Data files for Assessments','/',quizTitle,'/')
  type_answers=c('r','t','F','chi','U','T','rho')
  df_answers=c('(xx)','(xx)','(yy,xx)','(yy,n=xx)')
  
  n1<-names(hypothesisAll)
  nH<-n1[is.element(n1,names(braw.def$hypothesis))]
  nE<-n1[is.element(n1,names(braw.def$hypothesis$effect))]
  nD<-n1[is.element(n1,names(braw.def$design))]
  
  if (is.null(hypothesisAll$rIV)) hypothesisAll$rIV<-0.3
  if (is.null(hypothesisAll$sN)) hypothesisAll$sN<-50
  
  allQuestions<-array(list(),n_questions)
  for (qi in 1:n_questions) {
    hypothesis<-braw.def$hypothesis
    design<-braw.def$design
    design$sDataFormat<-"wide"
    for (fi in n1) {
      switch(fi,
             "IV"=hypothesis$IV<-getVariable(chooseOne(hypothesisAll$IV)),
             "IV2"=hypothesis$IV2<-getVariable(chooseOne(hypothesisAll$IV2)),
             "DV"=hypothesis$DV<-getVariable(chooseOne(hypothesisAll$DV)),
             "rIV"=hypothesis$effect$rIV<-chooseOne(hypothesisAll$rIV),
             "rIV2"=hypothesis$effect$rIV2<-chooseOne(hypothesisAll$rIV2),
             "rIVIV2"=hypothesis$effect$rIVIV2<-chooseOne(hypothesisAll$rIVIV2),
             "rIVIV2DV"=hypothesis$effect$rIVIV2DV<-chooseOne(hypothesisAll$rIVIV2DV),
             "sN"=design$sN<-chooseOne(hypothesisAll$sN),
             "sIV1Use"=design$sIV1Use<-chooseOne(hypothesisAll$sIV1Use),
             "sIV2Use"=design$sIV2Use<-chooseOne(hypothesisAll$sIV2Use),
             "sOutliers"=design$sOutliers<-chooseOne(hypothesisAll$sOutliers)
      )
    }
    # make a sample
    sample<-doSingle(hypothesis=hypothesis,design=design)
    # save the sample to a data file
    dataName<-paste0('Data3_', format(qi),'.xlsx')
    if (is.null(hypothesis$IV2)) {
      data<-data.frame(sample$participant,sample$iv,sample$dv)
      names(data)<-c("Participant",hypothesis$IV$name,hypothesis$DV$name)
    } else {
      data<-data.frame(sample$participant,sample$iv,sample$iv2,sample$dv)
      names(data)<-c("Participant",hypothesis$IV$name,hypothesis$IV2$name,hypothesis$DV$name)
    }
    
    writexl::write_xlsx(data,
                        paste0(dataFolder,dataName))
    
    # now set up the question
    fileRoot<-'https://canvas.stir.ac.uk/courses/19281/file_contents/$IMS-CC-FILEBASE$/'
    
    questionTextThis<-gsub('\\*\\*([a-zA-Z0-9_.]*)\\*\\*',
                       paste0(
                       '<a class="instructure_file_link instructure_scribd_file inline_disabled" ',
                       'title="Data" ',
                       'href="',dataLink,dataName,
                       '?canvas_=1&amp;amp;canvas_qs_wrap=1" target="_blank">',
                       'here',
                       '</a> '),
                       questionText)
    questionType<-'multiple_dropdowns_question'
    
    # here we get all possible answers
    questionAnswers<-list(
      testname = sample$test_name,
      df       = sample$df,
      testval  = format(sample$test_val,digits=3),
      pval     = formatP(sample$pIV,digits=3),
      rval     = format(sample$rIV),
      dval     = format(sample$rIV),
      dvmean    = format(sample$dv.mn),
      dvsd    = format(sample$dv.sd),
      power    = format(sample$wFull),
      n80      = format(sample$wFulln80),
    )
    
    df<-df_answers[ceiling(runif(n_choices-1,0,length(df_answers)))]
    for (i in 1:length(df)) {
      df[i]<-gsub('yy',ceiling(runif(1)*4),gsub('xx',ceiling(runif(1)*100),df[i]))
    }
    questionFoils<-list(
      testname=setdiff(type_answers,questionAnswers$test)[ceiling(runif(n_choices-1,0,length(df_answers)))],
      df=df,
      testval=format(runif(n_choices-1,0,3),digits=3),
      pval=formatP(runif(n_choices-1,0,0.5),digits=3),
      rval=format(runif(n_choices-1,-0.8,0.8),digits=3),
      dval=format(runif(n_choices-1,-1.2,1.2),digits=3),
      dvmean=format(sample$dv.mn+runif(n_choices-1,-2,2)*sample$dv.sd,digits=3),
      dvsd=format(runif(n_choices-1,0.2,2)*sample$dv.sd,digits=3),
      power=format(runif(n_choices-1,0,0.95),digits=3),
      n80= format(runif(n_choices-1,10,200))
    )
    
    question<-list(
      questionType=questionType,
      questionText=questionTextThis,
      questionAnswers=questionAnswers,
      questionFoils=questionFoils,
      dataLink=paste0(dataLink,dataName)
    )
    
    allQuestions[[qi]]<-question
  }
  
  quiz<-list(title=quizTitle,
             questions=allQuestions,
             questions2answer=5
  )
  
  qti_build(quiz)
  
  zip(paste0('./',quiz$title,'.zip'),paste0('./',quiz$title,'/'))
}
