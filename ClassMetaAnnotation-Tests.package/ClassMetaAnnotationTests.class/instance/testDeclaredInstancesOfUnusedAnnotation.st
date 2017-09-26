tests
testDeclaredInstancesOfUnusedAnnotation

	| actual |
	actual := ClassMetaAnnotationUnusedExample declaredInstances.
	
	self assert: actual isEmpty