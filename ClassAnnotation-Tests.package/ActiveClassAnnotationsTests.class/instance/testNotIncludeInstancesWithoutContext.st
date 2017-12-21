tests
testNotIncludeInstancesWithoutContext

	| annotations context |
	context := AnnotationUserExample new.
	annotations := ClassAnnotationExample1 activeInstancesInContext: context.
	
	self assert: annotations isEmpty