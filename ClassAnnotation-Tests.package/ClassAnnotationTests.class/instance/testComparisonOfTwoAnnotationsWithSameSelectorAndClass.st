tests
testComparisonOfTwoAnnotationsWithSameSelectorAndClass
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithThreeAnnotations.
	ann1 declarationSelector: #selector1.
	
	ann2 := ClassAnnotationExample1 withAnnotatedClass: ClassWithThreeAnnotations.
	ann2 declarationSelector: #selector1.
	
	self assert: ann1 equals: ann2.
	self assert: ann1 hash equals: ann2 hash