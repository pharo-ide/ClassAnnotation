tests
testGettingAllDeclaredInstancesForConcreteAnnotation

	| annotations |
	annotations := ClassMetaAnnotationExample1 declaredInstances.
	
	self assert: annotations size equals: 2.
	self assert: (annotations collect: #class as: Set) equals: {ClassMetaAnnotationExample1} asSet