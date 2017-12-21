tests
testIncludeInstancesDeclaredForSuperclassOfGivenUser

	| annotations annotationUser declaredContext |
	annotationUser := AnnotationUserExample new.
	annotations := ContextualClassAnnotationExample2 visibleInstancesInContext: annotationUser.
	
	self assert: annotations size equals: 1.
	declaredContext := annotations anyOne activeContext annotationUserClass.
	self assert: declaredContext equals: BaseAnnotationUserExample