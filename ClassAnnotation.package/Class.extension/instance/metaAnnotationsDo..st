*ClassAnnotation
metaAnnotationsDo: aBlock
	^ClassMetaAnnotation findAllAnnotationsFor: self andDo: aBlock