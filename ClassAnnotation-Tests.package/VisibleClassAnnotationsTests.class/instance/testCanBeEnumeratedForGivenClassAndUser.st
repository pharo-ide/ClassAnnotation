tests
testCanBeEnumeratedForGivenClassAndUser

	| annotationUser actual expected |
	annotationUser := AnnotationUserExample new.
	actual := OrderedCollection new.
	ContextualClassAnnotationExample1 
		visibleInstancesFor: ClassWithAnnotationsDefinedForUserClass inContext: annotationUser 
		do: [:each | actual add: each].
	expected := ContextualClassAnnotationExample1 visibleInstancesInContext: annotationUser.

	self assert: actual asSet equals: expected asSet