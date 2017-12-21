tests
testNotIncludeInstancesWithoutContext

	| annotations annotationUser |
	annotationUser := AnnotationUserExample new.
	annotations := ClassAnnotationExample1 activeInstancesInContext: annotationUser.
	
	self assert: annotations isEmpty