tests
testSimilarToAnnotationOfSameClassWhenBothDoNotDefinedWithContext
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	self assert: (ann1 isSimilarTo: ann2)