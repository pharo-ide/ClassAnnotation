tests
testDifferentAnnotationsShouldHaveDifferentDeclaredInstances

	| annotations1 annotations2 |
	annotations1 := ClassMetaAnnotationExample1 declaredInstances.
	annotations2 := ClassMetaAnnotationExample2 declaredInstances.
	
	self deny: annotations1 = annotations2