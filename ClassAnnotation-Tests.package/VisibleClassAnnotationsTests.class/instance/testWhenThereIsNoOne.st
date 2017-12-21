tests
testWhenThereIsNoOne

	| annotations annotationUser |
	annotationUser := Object new. "context can be anything"
	
	annotations := ContextualClassAnnotationExample1 visibleInstancesInContext: annotationUser.
	
	self assert: annotations isEmpty