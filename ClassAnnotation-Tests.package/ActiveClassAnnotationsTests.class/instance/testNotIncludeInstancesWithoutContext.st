tests
testNotIncludeInstancesWithoutContext

	| annotations context |
	context := AnnotationUserContextExample new.
	annotations := ClassAnnotationExample1 activeInstancesInContext: context.
	
	self assert: annotations isEmpty