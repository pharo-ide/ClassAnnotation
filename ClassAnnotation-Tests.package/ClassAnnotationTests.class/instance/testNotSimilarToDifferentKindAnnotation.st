tests
testNotSimilarToDifferentKindAnnotation
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample2 withAnnotatedClass: ClassWithSingleAnnotation.
	
	self deny: (ann1 isSimilarTo: ann2)