
qti_header<-function(qi,qtype,answer_idents,item_ident) {
  header=c(
    '        <itemmetadata>',
    '          <qtimetadata>',
    '            <qtimetadatafield>',
    '              <fieldlabel>question_type</fieldlabel>',
    paste0('              <fieldentry>', qtype, '</fieldentry>'),
    '            </qtimetadatafield>',
    '            <qtimetadatafield>',
    '              <fieldlabel>points_possible</fieldlabel>',
    '              <fieldentry>1.0</fieldentry>',
    '            </qtimetadatafield>',
    '            <qtimetadatafield>',
    '              <fieldlabel>original_answer_ids</fieldlabel>',
    paste0('              <fieldentry>', paste0(format(answer_idents),collapse=','), '</fieldentry>'),
    '            </qtimetadatafield>',
    '            <qtimetadatafield>',
    '              <fieldlabel>assessment_question_identifierref</fieldlabel>',
    paste0('              <fieldentry>', item_ident, 'b', format(qi), '</fieldentry>'),
    '            </qtimetadatafield>',
    '          </qtimetadata>',
    '        </itemmetadata>'
  )
  return(header)
}

qti_scoring<-function(maxvalue,keywords,answer_correct_idents) {
  scoring=c(
    '        <resprocessing>',
    '          <outcomes>',
    paste0('            <decvar maxvalue="', maxvalue, '" minvalue="0" varname="SCORE" vartype="Decimal"/>'),
    '          </outcomes>'
  )
  for (j in 1:length(keywords)) {
    scoring=c(scoring, 
                   paste0(
                     '          <respcondition continue="No">',
                     '            <conditionvar>',
                     paste0('              <varequal respident="response_', keywords[j], '">', format(answer_correct_idents[j]), '</varequal>'),
                     '            </conditionvar>',
                     '            <setvar action="Add" varname="SCORE">1</setvar>',
                     '          </respcondition>'
                   )
    );
  }
  scoring=c(scoring,'        </resprocessing>')
  
  return(scoring)
}

qti_presentation<-function(question_qti,answers_qti) {
  presentation=c(
    '<presentation>',
    '<material>',
    '<mattext texttype="text/html">',
    '<div>',
    question_qti,
    '</div>',
    '</mattext>',
    '</material>'
  )
  presentation<-c(presentation,
    paste0(answers_qti,collapse='\n')
  )
  presentation=c(presentation,
    '</presentation>'
  )
  return(presentation)
}



make_question_item<-function(question,qi,item_ident) {
  
  question_qti=paste0('<p>',question$questionText, '</p>')
  
  keywords=str_extract_all(question$questionText,'\\[[a-zA-Z0-9_]*\\]')
  keywords=substr(keywords[[1]],rep(2,4),sapply(keywords[[1]],nchar)-1)
  
  nkeys=length(keywords)
  nchoices=1+length(question$questionFoils[[1]])
  answer_idents=10000+qi*100+(1:(nchoices*nkeys))

  nc=1
  answers_qti=c()
  answer_correct_idents<-rep('',nkeys)
  for (key in 1:nkeys) {
    # the keyword (remove optional underscores)
    thiskey<-gsub('_','',keywords[key])
    this_answer_qti= paste0('<response_lid ident="response_', thiskey, '">')
    this_answer_qti=c(this_answer_qti,
                           '<material>',
                           paste0('<mattext>', keywords[key], '</mattext>'),
                           '</material>'
    )
    
    # the choices
    this_answer_qti=c(this_answer_qti,'<render_choice>')
    # correct answer first
    this_answer_qti=c(
      this_answer_qti,
      paste0('<response_label ident="', format(answer_idents[nc]), '">'),
      '<material>',
      paste0('<mattext texttype="text/plain">', question$questionAnswers[[thiskey]], '</mattext>'),
      '</material>',
      '</response_label>'
    )
    answer_correct_idents[key]=answer_idents[nc]
    nc<-nc+1
    # then the wrong answers
    for (i in 1:length(question$questionFoils[[thiskey]])) {
      this_answer_qti=c(
        this_answer_qti,
        paste0('<response_label ident="', format(answer_idents[nc]), '">'),
        '<material>',
        paste0('<mattext texttype="text/plain">', question$questionFoils[[thiskey]][i], '</mattext>'),
        '</material>',
        '</response_label>'
      )
      nc=nc+1;
    }
    this_answer_qti=c(
      this_answer_qti,
      '</render_choice>',
      '</response_lid>'
    )
    answers_qti=c(answers_qti,this_answer_qti)
  }
  
  header=qti_header(qi,question$questionType,answer_idents,item_ident)
  presentation=qti_presentation(question_qti,answers_qti)
  scoring=qti_scoring(nkeys,keywords,answer_correct_idents)
  
  qti_item=c(
    paste0('      <item ident="', item_ident, 'a', format(qi), '" title="Q', format(qi), '">'),
    header,
    paste0(presentation,collapse='\n'),
    scoring,
    '      </item>'
  )
  
  return(qti_item)
}

make_questions<-function(quiz) {
  
  group_questions_start=c(
    paste0('      <section ident="', quiz$title, 'G" title="GroupQuestions', quiz$title, '" >'),
    '        <selection_ordering>',
    '          <selection>',
    paste0('            <selection_number>', format(quiz$questions2answer), '</selection_number>'),
    '            <selection_extension>',
    '              <points_per_item>1.0</points_per_item>',
    '            </selection_extension>',
    '          </selection>',
    '        </selection_ordering>'
  )
  group_questions_end=c(
    '      </section>'
  )
  
  n_questions<-length(quiz$questions)
  group_questions_body=c()
  for (qi in 1:n_questions) {
    item_ident=paste0('g',format(qi))
    qti_item=make_question_item(quiz$questions[[qi]],qi,item_ident)
    group_questions_body=c(group_questions_body,qti_item)
  }
  
  group_questions<-c(
    group_questions_start,
    group_questions_body,
    group_questions_end
  )
  
  return(group_questions)
}

make_assessment<-function(quiz) {
  
  assessment_start=c(
    paste0('  <assessment ident="', quiz$title, '" title="', quiz$title, '">'),
    '    <qtimetadata>',
    '      <qtimetadatafield>',
    '        <fieldlabel>cc_maxattempts</fieldlabel>',
    '        <fieldentry>1</fieldentry>',
    '      </qtimetadatafield>',
    '    </qtimetadata>',
    '    <section ident="root_section">'
  )
  assessment_end=c(
    '</section>',
    '  </assessment>'
  )
  
  assessment<-c(
    assessment_start,
    make_questions(quiz),
    assessment_end
  )
  
  qti_start<-c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<questestinterop xmlns="http://www.imsglobal.org/xsd/ims_qtiasiv1p2" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.imsglobal.org/xsd/ims_qtiasiv1p2 http://www.imsglobal.org/xsd/ims_qtiasiv1p2p1.xsd">'
  )
  
  qti_end<-    '</questestinterop>'
  
  qti<-c(
    qti_start,
    assessment,
    qti_end
  )
  
  return(qti)
}

make_meta<-function(quiz) {
  qti_meta=c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    paste0('<quiz identifier="', quiz$title, '" xmlns="http://canvas.instructure.com/xsd/cccv1p0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://canvas.instructure.com/xsd/cccv1p0 https://canvas.instructure.com/xsd/cccv1p0.xsd">'),
    paste0('  <title>', quiz$title, '</title>'),
    '  <description></description>',
    '  <shuffle_answers>true</shuffle_answers>',
    '  <scoring_policy>keep_highest</scoring_policy>',
    '  <hide_results></hide_results>',
    '  <quiz_type>assignment</quiz_type>',
    '  <show_correct_answers>false</show_correct_answers>',
    '  <one_question_at_a_time>true</one_question_at_a_time>',
    '  <cant_go_back>false</cant_go_back>',
    '  <one_time_results>false</one_time_results>',
    '  <show_correct_answers_last_attempt>false</show_correct_answers_last_attempt>',
    '</quiz>'
  )
  return(qti_meta)
}

make_manifest<-function(quiz) {
  files<-c()
  for (i in 1:length(quiz$questions)) 
    files<-c(files,c('		<resource ',
                     paste0('				identifier="',quiz$title,format(i),'"',' type="webcontent" ',
                            'href="web_resources/',quiz$questions[[i]]$dataLink,'"',
                            '>'),
                     paste0('				<file href="web_resources/', quiz$questions[[i]]$dataLink, '"/>'),
                     '		</resource>'
    )
    )
  
  manifest=c(
    '<?xml version="1.0" encoding="utf-8"?>',
    paste0('<manifest identifier="MANIFEST-', quiz$title, '" xmlns="http://www.imsglobal.org/xsd/imsccv1p1/imscp_v1p1" xmlns:imsmd="http://www.imsglobal.org/xsd/imsmd_v1p2" xmlns:lom="http://ltsc.ieee.org/xsd/imsccv1p1/LOM/resource" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.imsglobal.org/xsd/imsccv1p1/imscp_v1p1 http://www.imsglobal.org/xsd/imscp_v1p1.xsd http://ltsc.ieee.org/xsd/imsccv1p1/LOM/resource http://www.imsglobal.org/profile/cc/ccv1p1/LOM/ccv1p1_lomresource_v1p0.xsd http://www.imsglobal.org/xsd/imsmd_v1p2 http://www.imsglobal.org/xsd/imsmd_v1p2p2.xsd">'),
    '	<metadata>',
    '		<schema>IMS Content</schema>',
    '		<schemaversion>1.1.3</schemaversion>',
    '		<imsmd:lom>',
    '			<imsmd:general>',
    '				<imsmd:title>',
    '					<imsmd:langstring xml:lang="en-US"/>',
    '				</imsmd:title>',
    '			</imsmd:general>',
    '		</imsmd:lom>',
    '	</metadata>',
    '	<organizations/>',
    '	<resources>',
    paste0('		<resource href="', quiz$title, '/', quiz$title, '.xml" identifier="', quiz$title, '" type="imsqti_xmlv1p2">'),
    paste0('			<file href="', quiz$title, '/', quiz$title, '.xml"/>'),
    '      <dependency identifierref="RESOURCE2"/>',
    '		</resource>',
    paste0('       <resource href="', quiz$title, '/', 'assessment_meta.xml" identifier="RESOURCE2" type="associatedcontent/imscc_xmlv1p1/learning-application-resource">'),
    paste0('           <file href="', quiz$title, '/', 'assessment_meta.xml"/>'),
    '		</resource>',
    files,
    '	</resources>',
    '</manifest>'
  )
  
  return(manifest)  
}

qti_build<-function(quiz) {
  folder<-paste0('./',quiz$title,'/',quiz$title,'/')
  
  qti<-make_assessment(quiz)
  qti<-paste(qti,collapse='\n')
  folder<-paste0('./',quiz$title,'/',quiz$title,'/')
  filename<-paste0(quiz$title,'.xml')
  file1<-file(paste0(folder,filename))
  write(qti,file1)
  close(file1)
  # write to paste0(folder,filename)
  print(paste0(folder,filename))
  
  qti_meta<-make_meta(quiz)
  qti_meta<-paste(qti_meta,collapse='\n')
  folder<-paste0('./',quiz$title,'/',quiz$title,'/')
  filename<-paste0('assessment_meta.xml')
  file1<-file(paste0(folder,filename))
  write(qti_meta,file1)
  close(file1)
  # write to paste0(folder,filename)
  print(paste0(folder,filename))
  
  qti_manifest<-make_manifest(quiz)
  qti_manifest<-paste(qti_manifest,collapse='\n')
  folder<-paste0('./',quiz$title,'/')
  filename<-paste0('imsmanifest.xml')
  file1<-file(paste0(folder,filename))
  write(qti_manifest,file1)
  close(file1)
  # write to paste0(folder,filename)
  print(paste0(folder,filename))
  
  
}
