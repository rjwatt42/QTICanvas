#' @import BrawPackage
#' @import stringr

chooseOne<-function(var) {
  use<-ceiling(runif(1)*length(var))
  return(var[use])
}
chooseBetween<-function(var) {
  if (is.list(var))  return(runif(1,var$min,var$max))
  else return(chooseOne(var))
}
formatP<-function(p,digits=3) {
  pstr<-format(p,digits=digits)
  # remove leading "0"
  pstr<-substr(pstr,2,10)
  pstr[p<0.001]<-'<.001'
  return(pstr)
}

#' @export
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
             "rIV"=hypothesis$effect$rIV<-chooseBetween(hypothesisAll$rIV),
             "rIV2"=hypothesis$effect$rIV2<-chooseBetween(hypothesisAll$rIV2),
             "rIVIV2"=hypothesis$effect$rIVIV2<-chooseBetween(hypothesisAll$rIVIV2),
             "rIVIV2DV"=hypothesis$effect$rIVIV2DV<-chooseBetween(hypothesisAll$rIVIV2DV),
             "sN"=design$sN<-chooseBetween(hypothesisAll$sN),
             "sIV1Use"=design$sIV1Use<-chooseOne(hypothesisAll$sIV1Use),
             "sIV2Use"=design$sIV2Use<-chooseOne(hypothesisAll$sIV2Use),
             "sDataFormat"=design$sDataFormat<-chooseOne(hypothesisAll$sDataFormat),
             "sOutliers"=design$sOutliers<-chooseBetween(hypothesisAll$sOutliers)
      )
      if (is.null(hypothesis$IV2))
        while(hypothesis$DV$name==hypothesis$IV$name) 
          hypothesis$DV<-getVariable(chooseOne(hypothesisAll$DV))
      else {
        while(hypothesis$IV$name==hypothesis$IV2$name) 
          hypothesis$IV2<-getVariable(chooseOne(hypothesisAll$IV2))
        while(hypothesis$DV$name==hypothesis$IV$name || hypothesis$DV$name==hypothesis$IV2$name) 
          hypothesis$DV<-getVariable(chooseOne(hypothesisAll$DV))
      }
    }
    # make a sample
    sample<-doSingle(hypothesis=hypothesis,design=design)
    # save the sample to a data file
    dataName<-paste0('Data_', format(qi),'.xlsx')
    if (is.null(hypothesis$IV2)) {
      data<-data.frame(sample$participant,sample$iv,sample$dv)
      names(data)<-c("Participant",hypothesis$IV$name,hypothesis$DV$name)
    } else {
      data<-data.frame(sample$participant,sample$iv,sample$iv2,sample$dv)
      names(data)<-c("Participant",hypothesis$IV$name,hypothesis$IV2$name,hypothesis$DV$name)
    }
    writexl::write_xlsx(data,
                        paste0(dataFolder,dataName))
    if (sample$test_name=="t") sample$test_val<-abs(sample$test_val)
    
    # now set up the question
    questionTextThis<-questionText
    questionTextThis<-gsub('\\*\\*IVname\\*\\*',hypothesis$IV$name,questionTextThis)
    questionTextThis<-gsub('\\*\\*DVname\\*\\*',hypothesis$DV$name,questionTextThis)
    questionTextThis<-gsub('\\*\\*IVcase1\\*\\*',hypothesis$IV$cases[1],questionTextThis)
    questionTextThis<-gsub('\\*\\*IVcase2\\*\\*',hypothesis$IV$cases[2],questionTextThis)
    questionTextThis<-gsub('\\*\\*IVcase3\\*\\*',hypothesis$IV$cases[3],questionTextThis)
    questionTextThis<-gsub('\\*\\*DVcase1\\*\\*',hypothesis$DV$cases[1],questionTextThis)
    questionTextThis<-gsub('\\*\\*DVcase2\\*\\*',hypothesis$DV$cases[2],questionTextThis)
    questionTextThis<-gsub('\\*\\*DVcase3\\*\\*',hypothesis$DV$cases[3],questionTextThis)
    questionTextThis<-gsub('\\*\\*datafile\\*\\*',
                       paste0(
                       '<a class="instructure_file_link instructure_scribd_file inline_disabled" ',
                       'title="Data" ',
                       'href="',dataLink,dataName,
                       '?canvas_=1&amp;amp;canvas_qs_wrap=1" target="_blank">',
                       'file=',dataName,
                       '</a> '),
                       questionTextThis)
    
    questionType<-'multiple_dropdowns_question'
    
    # here we get all possible answers
    questionAnswers<-list(
      ivtype    = sample$hypothesis$IV$type,
      dvtype    = sample$hypothesis$DV$type,
      dvmean    = format(sample$dv.mn,digits=3),
      dvsd      = format(sample$dv.sd,digits=3),
      ivmean    = format(sample$iv.mn,digits=3),
      ivsd      = format(sample$iv.sd,digits=3),
      gp1mean   = format(mean(sample$dv[sample$iv==1]),digits=3),
      gp1sd     = format(sd(sample$dv[sample$iv==1]),digits=3),
      gp2mean   = format(mean(sample$dv[sample$iv==2]),digits=3),
      gp2sd     = format(sd(sample$dv[sample$iv==2]),digits=3),
      gp3mean   = format(mean(sample$dv[sample$iv==3]),digits=3),
      gp3sd     = format(sd(sample$dv[sample$iv==3]),digits=3),
      testname = sample$test_name,
      df       = sample$df,
      testval  = format(sample$test_val,digits=3),
      pval     = formatP(sample$pIV,digits=3),
      rval     = format(sample$rIV,digits=3),
      dval     = format(sample$rIV,digits=3),
      power    = format(sample$wFull,digits=3),
      n80      = format(sample$wFulln80,digits=3)
    )
    
    df<-df_answers[ceiling(runif(n_choices-1,0,length(df_answers)))]
    for (i in 1:length(df)) {
      df[i]<-gsub('yy',ceiling(runif(1)*4),gsub('xx',ceiling(runif(1)*100),df[i]))
    }
    testnames<-pracma::randperm(setdiff(type_answers,questionAnswers$testname))
    
    questionFoils<-list(
      ivtype    = setdiff(c("Interval","Ordinal","Categorical"),questionAnswers$ivtype),
      dvtype    = setdiff(c("Interval","Ordinal","Categorical"),questionAnswers$dvtype),
      dvmean    = format(runif(n_choices-1,-2,2)*sample$hypothesis$DV$sd+sample$hypothesis$DV$mu,digits=3),
      dvsd      = format(runif(n_choices-1,0,2)*sample$hypothesis$DV$sd,digits=3),
      ivmean    = format(runif(n_choices-1,-2,2)*sample$hypothesis$IV$sd+sample$hypothesis$IV$mu,digits=3),
      ivsd      = format(runif(n_choices-1,0,2)*sample$hypothesis$IV$sd,digits=3),
      gp1mean    = format(runif(n_choices-1,-2,2)*sample$hypothesis$DV$sd+sample$hypothesis$DV$mu,digits=3),
      gp1sd      = format(runif(n_choices-1,0,2)*sample$hypothesis$DV$sd,digits=3),
      gp2mean    = format(runif(n_choices-1,-2,2)*sample$hypothesis$DV$sd+sample$hypothesis$DV$mu,digits=3),
      gp2sd      = format(runif(n_choices-1,0,2)*sample$hypothesis$DV$sd,digits=3),
      gp3mean    = format(runif(n_choices-1,-2,2)*sample$hypothesis$DV$sd+sample$hypothesis$DV$mu,digits=3),
      gp3sd      = format(runif(n_choices-1,0,2)*sample$hypothesis$DV$sd,digits=3),
      testname=testnames[1:(n_choices-1)],
      df=df,
      testval=format(runif(n_choices-1,0,3),digits=3),
      pval=formatP(runif(n_choices-1,0,0.5),digits=3),
      rval=format(runif(n_choices-1,-0.8,0.8),digits=3),
      dval=format(runif(n_choices-1,-1.2,1.2),digits=3),
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
