tests
testIncludeOnlyInstancesWhichAreActiveForGivenUser

	| actual annotationUser expected |
	annotationUser := AnnotationUserExample new.
	annotationUser isSimulatedActiveAnnotation: true. 
	
	actual := ContextualClassAnnotationExample1 activeInstancesInContext: annotationUser.
	
	expected := ContextualClassAnnotationExample1 visibleInstancesInContext: annotationUser.	
	self assert: actual asSet equals: expected asSet