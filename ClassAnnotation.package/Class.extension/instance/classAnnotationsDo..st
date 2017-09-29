*ClassAnnotation
classAnnotationsDo: aBlock
	^ClassAnnotation findAllAnnotationsFor: self andDo: aBlock