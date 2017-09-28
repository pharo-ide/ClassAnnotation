*ClassAnnotation
metaAnnotationsDo: aBlock
	^ClassAnnotation findAllAnnotationsFor: self andDo: aBlock