tests
testNotIncludeInstancesWhichAreNotActiveForGivenUser

	| actual annotationUser |
	annotationUser := AnnotationUserExample new.
	annotationUser isSimulatedActiveAnnotation: false. 
	
	actual := ContextualClassAnnotationExample1 activeInstancesInContext: annotationUser.
	
	self assert: actual isEmpty