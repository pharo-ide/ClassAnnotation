*ClassAnnotation
classAnnotationAt: selector
	^self classAnnotations 
		detect: [ :each | each declarationSelector = selector ]