tests
testIncludeInstancesDeclaredForSuperclassOfGivenContext

	| annotations context declaredContext |
	context := AnnotationUserContextExample new.
	annotations := ContextualClassAnnotationExample2 visibleInstancesInContext: context.
	
	self assert: annotations size equals: 1.
	declaredContext := annotations anyOne activeContext userContextClass.
	self assert: declaredContext equals: BaseAnnotationUserContextExample