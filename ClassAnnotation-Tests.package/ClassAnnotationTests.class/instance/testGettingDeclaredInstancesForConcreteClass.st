tests
testGettingDeclaredInstancesForConcreteClass

	| annotations |
	annotations := ClassAnnotationExample1 declaredInstancesFor: ClassWithThreeAnnotations.
	
	self assert: annotations size equals: 2.
	self assert: (annotations collect: #annotatedClass as: Set) equals: {ClassWithThreeAnnotations} asSet