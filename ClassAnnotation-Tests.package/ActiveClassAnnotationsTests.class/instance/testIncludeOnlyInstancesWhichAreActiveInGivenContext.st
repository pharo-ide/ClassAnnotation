tests
testIncludeOnlyInstancesWhichAreActiveInGivenContext

	| actual context expected |
	context := AnnotationUserContextExample new.
	context isSimulatedActiveAnnotation: true. 
	
	actual := ContextualClassAnnotationExample1 activeInstancesInContext: context.
	
	expected := ContextualClassAnnotationExample1 visibleInstancesInContext: context.	
	self assert: actual asSet equals: expected asSet