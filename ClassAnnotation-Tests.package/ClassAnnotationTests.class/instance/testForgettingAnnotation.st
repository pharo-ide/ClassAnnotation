tests
testForgettingAnnotation
	| annotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	
	ClassAnnotation registry forgetAnnotation: annotation.
	
	self assert: ClassWithSingleAnnotation classAnnotations isEmpty