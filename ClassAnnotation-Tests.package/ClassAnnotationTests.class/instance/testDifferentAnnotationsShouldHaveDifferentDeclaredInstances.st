tests
testDifferentAnnotationsShouldHaveDifferentDeclaredInstances

	| annotations1 annotations2 |
	annotations1 := ClassAnnotationExample1 declaredInstances.
	annotations2 := ClassAnnotationExample2 declaredInstances.
	
	self deny: annotations1 = annotations2