tests
testAreEmptyForUnusedAnnotation

	| actual |
	actual := ClassAnnotationUnusedExample registeredInstances.
	
	self assert: actual isEmpty