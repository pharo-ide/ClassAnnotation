tests
testGettingAnnotationsOfClassWithoutAnnotations

	| annotations |
	annotations := ClassWithoutAnnotations classAnnotations.
	
	self assert: annotations isEmpty