tests
testNotSimilarToAnnotationOfDifferentClass
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample1 withAnnotatedClass: SubclassWithOnlyInheritedAnnotation.
	
	self deny: (ann1 isSimilarTo: ann2)