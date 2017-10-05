tests
testAreCached

	| annotations |
	annotations := ClassAnnotationExample1 registeredInstances.
	
	self assert: ClassAnnotationExample1 registeredInstances == annotations