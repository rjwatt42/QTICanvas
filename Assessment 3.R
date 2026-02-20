library(BrawPackage)

  hypothesisAll<-array(list(),4)
  hypothesisAll[[1]]<-makeHypothesis()
  hypothesisAll[[2]]<-makeHypothesis()
  hypothesisAll[[3]]<-makeHypothesis()
  hypothesisAll[[4]]<-makeHypothesis()
  
  questionText<-paste0(
    'Download the data file from: ',
    '**datafile**',
    'Analyse it to find the APA statement:',
    '[test]([df])=[testval], p=[pval]'
  )
  
  makeAssessment(title="Online Assessment test2",
                 questionText=questionText,
                 n_questions=50,n_choices=4,
                 hypothesisAll=hypothesisAll)
  
  
###########################


