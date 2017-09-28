tests
testDifferentAnnotationsShouldHaveDifferentDeclaredInstances

	| annotations1 annotations2 |
	annotations1 := ClassAnnotationExample1 registeredInstances.
	annotations2 := ClassAnnotationExample2 registeredInstances.
	
	self deny: annotations1 = annotations2