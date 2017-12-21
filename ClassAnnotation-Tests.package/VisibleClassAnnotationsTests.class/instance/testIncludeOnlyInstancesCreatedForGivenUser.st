tests
testIncludeOnlyInstancesCreatedForGivenUser

	| annotations context declaredContexts |
	context := AnnotationUserExample new.
	annotations := ContextualClassAnnotationExample1 visibleInstancesInContext: context.
	
	self assert: annotations size equals: 2.
	declaredContexts := annotations collect: [:each | each activeContext annotationUserClass].
	self assert: declaredContexts asSet equals: {AnnotationUserExample} asSet