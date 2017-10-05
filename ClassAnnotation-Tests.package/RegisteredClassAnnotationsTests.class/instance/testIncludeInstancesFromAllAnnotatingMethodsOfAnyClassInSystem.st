tests
testIncludeInstancesFromAllAnnotatingMethodsOfAnyClassInSystem

	| annotations |
	annotations := ClassAnnotationExample1 registeredInstances.
	
	self assert: annotations size equals: 2.
	self assert: (annotations collect: #class as: Set) equals: {ClassAnnotationExample1} asSet