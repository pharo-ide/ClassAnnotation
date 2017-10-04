tests
testComparisonOfTwoAnnotationsWithDifferentClass
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	ann1 declarationSelector: #selector1.
	
	ann2 := ClassAnnotationExample1 withAnnotatedClass: SubclassWithOnlyInheritedAnnotation.
	ann2 declarationSelector: #selector1.
	
	self deny: ann1 = ann2