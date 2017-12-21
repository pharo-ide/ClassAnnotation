tests
testIncludeInstancesDeclaredForSuperclassOfGivenUser

	| annotations context declaredContext |
	context := AnnotationUserExample new.
	annotations := ContextualClassAnnotationExample2 visibleInstancesInContext: context.
	
	self assert: annotations size equals: 1.
	declaredContext := annotations anyOne activeContext annotationUserClass.
	self assert: declaredContext equals: BaseAnnotationUserExample