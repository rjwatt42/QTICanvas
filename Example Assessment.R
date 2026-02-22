library(BrawPackage)
library(QTICanvas)

BrawPackage::BrawOpts()

  hypothesisAll<-list(IV="?Int?CatN",
                      DV="?Int",
                      rIV=c(0.4,-0.4),
                      sN=c(50,200)
  )
  
  questionText<-paste0(
    'Download the data file from: **datafile**',
    'Analyse it to find the APA statement: [testname][df]=[testval], p=[pval]'
  )

  QTICanvas::makeAssessment(title="Test2",
                 questionText=questionText,
                 n_questions=50,n_choices=4,
                 hypothesisAll=hypothesisAll)
  
  
###########################


