tests
testNotIncludeInstancesWithoutContext

	| annotations context |
	context := AnnotationUserExample new.
	annotations := ClassAnnotationExample1 visibleInstancesInContext: context.
	
	self assert: annotations isEmpty