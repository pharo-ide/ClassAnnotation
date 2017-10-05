tests
testNotIncludeInstancesWhichAreNotActiveInGivenContext

	| actual context |
	context := AnnotationUserContextExample new.
	context isSimulatedActiveAnnotation: false. 
	
	actual := ContextualClassAnnotationExample1 activeInstancesInContext: context.
	
	self assert: actual isEmpty