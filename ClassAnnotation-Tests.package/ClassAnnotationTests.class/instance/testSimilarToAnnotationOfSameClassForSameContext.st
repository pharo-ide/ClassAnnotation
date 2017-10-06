tests
testSimilarToAnnotationOfSameClassForSameContext
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 for: AnnotationUserContextExample withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample1 for: AnnotationUserContextExample withAnnotatedClass: ClassWithSingleAnnotation.
	
	self assert: (ann1 isSimilarTo: ann2)