library(BrawPackage)


  hypothesisAll<-list(IV="?",
                      DV="?Interval",
                      rIV=c(0.4,0,-0.4),
                      sN=c(50,100,200)
  )
  
  questionText<-paste0(
    'Download the data file from: **datafile**',
    'Analyse it to find the APA statement: [testname][df]=[testval], p=[pval]'
  )

  makeAssessment(title="Online Assessment 0",
                 questionText=questionText,
                 n_questions=50,n_choices=4,
                 hypothesisAll=hypothesisAll)
  
  
###########################


