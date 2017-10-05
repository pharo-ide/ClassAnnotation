tests
testComparisonOfTwoAnnotationsWithDifferentSelector
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithThreeAnnotations.
	ann1 declarationSelector: #selector1.
	
	ann2 := ClassAnnotationExample1 withAnnotatedClass: ClassWithThreeAnnotations.
	ann2 declarationSelector: #selector2.
	
	self deny: ann1 = ann2