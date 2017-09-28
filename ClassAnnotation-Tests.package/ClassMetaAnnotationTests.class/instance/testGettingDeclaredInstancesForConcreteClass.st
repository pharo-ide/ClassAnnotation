tests
testGettingDeclaredInstancesForConcreteClass

	| annotations |
	annotations := ClassMetaAnnotationExample1 declaredInstancesFor: ClassWithThreeMetaAnnotations.
	
	self assert: annotations size equals: 2.
	self assert: (annotations collect: #annotatedClass as: Set) equals: {ClassWithThreeMetaAnnotations} asSet