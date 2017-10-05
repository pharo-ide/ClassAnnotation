tests
testNotIncludeInstancesWithoutContext

	| annotations context |
	context := AnnotationUserContextExample new.
	annotations := ClassAnnotationExample1 visibleInstancesInContext: context.
	
	self assert: annotations isEmpty