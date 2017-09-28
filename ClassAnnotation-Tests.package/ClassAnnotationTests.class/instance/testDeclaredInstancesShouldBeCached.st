tests
testDeclaredInstancesShouldBeCached

	| annotations |
	annotations := ClassAnnotationExample1 declaredInstances.
	
	self assert: ClassAnnotationExample1 declaredInstances == annotations