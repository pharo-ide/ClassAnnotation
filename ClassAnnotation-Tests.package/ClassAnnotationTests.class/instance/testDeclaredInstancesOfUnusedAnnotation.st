tests
testDeclaredInstancesOfUnusedAnnotation

	| actual |
	actual := ClassAnnotationUnusedExample registeredInstances.
	
	self assert: actual isEmpty