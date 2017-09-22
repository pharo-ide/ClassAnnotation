tests
testDeclaredInstancesShouldBeCached

	| annotations |
	annotations := ClassMetaAnnotationExample1 declaredInstances.
	
	self assert: ClassMetaAnnotationExample1 declaredInstances == annotations