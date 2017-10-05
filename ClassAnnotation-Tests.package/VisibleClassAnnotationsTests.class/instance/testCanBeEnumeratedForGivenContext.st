tests
testCanBeEnumeratedForGivenContext

	| context actual expected |
	context := AnnotationUserContextExample new.
	actual := OrderedCollection new.
	ContextualClassAnnotationExample1 visibleInstancesInContext: context do: [:each | 
		actual add: each].
	expected := ContextualClassAnnotationExample1 visibleInstancesInContext: context.

	self assert: actual asSet equals: expected asSet.
