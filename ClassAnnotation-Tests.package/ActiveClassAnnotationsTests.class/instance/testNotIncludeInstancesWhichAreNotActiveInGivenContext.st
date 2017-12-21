tests
testNotIncludeInstancesWhichAreNotActiveInGivenContext

	| actual context |
	context := AnnotationUserExample new.
	context isSimulatedActiveAnnotation: false. 
	
	actual := ContextualClassAnnotationExample1 activeInstancesInContext: context.
	
	self assert: actual isEmpty