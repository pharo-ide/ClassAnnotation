tests
testNotSimilarToAnnotationOfSameClassButForDifferentContext
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample1 for: AnnotationUserContextExample withAnnotatedClass: ClassWithSingleAnnotation.
	
	self deny: (ann1 isSimilarTo: ann2)