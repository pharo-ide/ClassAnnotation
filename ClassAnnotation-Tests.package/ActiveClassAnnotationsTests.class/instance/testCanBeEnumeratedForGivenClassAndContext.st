tests
testCanBeEnumeratedForGivenClassAndContext

	| context actual expected |
	context := AnnotationUserContextExample new.
	actual := OrderedCollection new.
	ContextualClassAnnotationExample1 
		activeInstancesFor: ClassWithAnnotationsDefinedForContext inContext: context 
		do: [:each | actual add: each].
	expected := ContextualClassAnnotationExample1 activeInstancesInContext: context.

	self assert: actual asSet equals: expected asSet