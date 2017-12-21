tests
testIncludeOnlyInstancesCreatedForGivenUser

	| annotations annotationUser declaredContexts |
	annotationUser := AnnotationUserExample new.
	annotations := ContextualClassAnnotationExample1 visibleInstancesInContext: annotationUser.
	
	self assert: annotations size equals: 2.
	declaredContexts := annotations collect: [:each | each activeContext annotationUserClass].
	self assert: declaredContexts asSet equals: {AnnotationUserExample} asSet