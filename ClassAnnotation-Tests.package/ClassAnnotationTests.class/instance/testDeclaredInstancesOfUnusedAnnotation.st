tests
testDeclaredInstancesOfUnusedAnnotation

	| actual |
	actual := ClassAnnotationUnusedExample declaredInstances.
	
	self assert: actual isEmpty