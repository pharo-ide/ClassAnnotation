tests
testNotIncludeInstancesWithoutContext

	| annotations annotationUser |
	annotationUser := AnnotationUserExample new.
	annotations := ClassAnnotationExample1 visibleInstancesInContext: annotationUser.
	
	self assert: annotations isEmpty