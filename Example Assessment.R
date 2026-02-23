library(BrawPackage)
library(QTICanvas)

BrawPackage::BrawOpts()

 # in these the values can be 
 #  a single value - which is then used
 #  a set if values c(a,b,c) - each data set has one randomly chosen 
 #  a list of values list(min=0,max=1) - each data set has a random value
  hypothesisAll<-list(IV=c("?Int","?CatN"),         # any interval or categorical variables
                      IV2=NULL,                     # no second IV
                      DV="?Int",                    # any interval variable
                      rIV=list(min=-0.4,max=0.4),   # any value between these two
                      rIV2=0,                          # effect of second IV on DV
                      rIVIV2=0,                        # covariation between IV and IV2
                      rIVIV2DV=0,                      # interaction between IV and IV2
                      sN=list(min=50,max=200),      # any sample size between these two
                      sIV1Use="Between",            # Between/Within for IV
                      sIV2Use="Between"             # Between/Within for IV2
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


