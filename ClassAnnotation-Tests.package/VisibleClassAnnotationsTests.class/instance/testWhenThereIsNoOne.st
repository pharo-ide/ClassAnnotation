tests
testWhenThereIsNoOne

	| annotations context |
	context := Object new. "context can be anything"
	
	annotations := ContextualClassAnnotationExample1 visibleInstancesInContext: context.
	
	self assert: annotations isEmpty