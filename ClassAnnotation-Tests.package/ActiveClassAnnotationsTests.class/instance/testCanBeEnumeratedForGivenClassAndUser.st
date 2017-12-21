tests
testCanBeEnumeratedForGivenClassAndUser

	| annotationUser actual expected |
	annotationUser := AnnotationUserExample new.
	actual := OrderedCollection new.
	ContextualClassAnnotationExample1 
		activeInstancesFor: ClassWithAnnotationsDefinedForUserClass inContext: annotationUser 
		do: [:each | actual add: each].
	expected := ContextualClassAnnotationExample1 activeInstancesInContext: annotationUser.

	self assert: actual asSet equals: expected asSet