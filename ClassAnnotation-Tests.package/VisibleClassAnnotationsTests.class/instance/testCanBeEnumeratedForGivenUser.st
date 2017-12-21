tests
testCanBeEnumeratedForGivenUser

	| annotationUser actual expected |
	annotationUser := AnnotationUserExample new.
	actual := OrderedCollection new.
	ContextualClassAnnotationExample1 visibleInstancesInContext: annotationUser do: [:each | 
		actual add: each].
	expected := ContextualClassAnnotationExample1 visibleInstancesInContext: annotationUser.

	self assert: actual asSet equals: expected asSet.
