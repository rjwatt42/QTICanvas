formatDF<-function(df) {
  n<-lapply(df,nchar)
  dfstr<-substr(df,rep(2,length(df)),unlist(n)-1)
  return(dfstr)
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
  type_answers=c('r','t','F','chi')
  df_answers=c('xx','xx','yy,xx','yy,n=xx')
  
  allQuestions<-array(list(),n_questions)
  for (qi in 1:n_questions) {
    # choose one of the possible hypotheses
    h_use<-ceiling(runif(1)*length(hypothesisAll))
    hypothesis<-hypothesisAll[[h_use]]
    # make a sample
    sample<-doSingle(hypothesis=hypothesis)
    # save the sample to a data file
    dataName<-paste0('Data3_', format(qi),'.xlsx')
    writexl::write_xlsx(data.frame(sample$participant,sample$iv,sample$dv),
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
    questionAnswers<-list(
      test    = sample$test_name,
      df      = formatDF(sample$df),
      testval = format(sample$test_val,digits=3),
      pval    = formatP(sample$pIV,digits=3)
    )
    
    df<-df_answers[ceiling(runif(n_choices-1,0,length(df_answers)))]
    for (i in 1:length(df)) {
      df[i]<-gsub('yy',ceiling(runif(1)*4),gsub('xx',ceiling(runif(1)*100),df[i]))
    }
    questionFoils<-list(
      test=setdiff(type_answers,questionAnswers$test),
      df=formatDF(df),
      testval=format(runif(n_choices-1,0,3),digits=3),
      pval=formatP(runif(n_choices-1,0,0.5),digits=3)
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
