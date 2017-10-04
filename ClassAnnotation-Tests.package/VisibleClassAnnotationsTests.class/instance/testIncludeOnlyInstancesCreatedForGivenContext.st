tests
testIncludeOnlyInstancesCreatedForGivenContext

	| annotations context declaredContexts |
	context := AnnotationUserContextExample new.
	annotations := ContextualClassAnnotationExample1 visibleInstancesInContext: context.
	
	self assert: annotations size equals: 2.
	declaredContexts := annotations collect: [:each | each activeContext expectedContextClass].
	self assert: declaredContexts asSet equals: {AnnotationUserContextExample} asSet