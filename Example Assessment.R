library(BrawPackage)
library(QTICanvas)

BrawPackage::BrawOpts()

##################################
# define the population that the sample comes from
# define the design

# in these the values can be 
#  a single value - which is then used
#  a set if values c(a,b,c) - each data set has one randomly chosen 
#  a list of values list(min=0,max=1) - each data set has a random value
# all are optional

# variables are defined by their type
#.  ?Int - any interval variable
#.  ?Ord - any ordinal variable
#.  ?Cat - any categorical variable
#.  ?Cat2 - any 2-cases categorical variable
#.  ?Cat3 - any 3-cases categorical variable

  hypothesisAll<-list(IV=c("?Int","?CatN"),         # any interval or categorical variables
                      IV2=NULL,                     # no second IV
                      DV="?Int",                    # any interval variable
                      rIV=list(min=-0.4,max=0.4),   # effect of IV on DV
                      rIV2=0,                          # effect of second IV on DV
                      rIVIV2=0,                        # covariation between IV and IV2
                      rIVIV2DV=0,                      # interaction between IV and IV2
                      sN=list(min=50,max=200),      # any sample size between these two
                      sIV1Use="Between",            # Between/Within for IV
                      sIV2Use="Between",            # Between/Within for IV2
                      sDataFormat="wide"            # wide/long for within data
  )
  
################################  
# define the question  
  # text **xxx** gets replaced by its live value
  # fields for answers are enclosed in [ ]
  questionText<-paste(
    '<p>Download the data file from: **datafile**</p>',
    '<p>The hypothesis is **DVname**(DV) <big>←</big> **IVname**(IV)</p>',
    '<p>Analyse it to find the <b>APA statement</b>: [testname][df]=[testval], p=[pval]</p>',
    sep=''
  )
  # **xxx** can be any of:
    # **datafile**
    # **IVname**
    # **DVname**
    # **IVcase1**
    # **IVcase2**
    # **IVcase3**
    # **DVcase1**
    # **DVcase2**
    # **DVcase3**

  # [ ] fields can be any of:
    # dvmean   mean of DV
    # dvsd     sd of DV
    # ivmean   mean of IV
    # ivsd     sd of IV
    # gp1mean  mean of DV[IV==1]
    # gp1sd    sd of DV[IV==1]
    # gp2mean  mean of DV[IV==2]
    # gp2sd    sd of DV[IV==2]
    # gp3mean  mean of DV[IV==3]
    # gp3sd    sd of DV[IV==3]
    # testname eg r t F chi etc
    # df       eg (40) (2,90) (2,n=42)
    # testval  test statistic value
    # pval     p-value
    # rval     sample effect suze (normalized)
    # dval     sample Cohen's d
    # power    post-hoc power (meaningless really...)
    # n80      sample size to give 80% power
  
  QTICanvas::makeAssessment(title="Test1",
                 questionText=questionText,
                 n_questions=50,n_choices=4,
                 hypothesisAll=hypothesisAll)
  
  
###########################


