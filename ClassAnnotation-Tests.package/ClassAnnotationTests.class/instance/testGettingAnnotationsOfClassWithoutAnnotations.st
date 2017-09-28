tests
testGettingAnnotationsOfClassWithoutAnnotations

	| annotations |
	annotations := ClassWithoutAnnotations metaAnnotations.
	
	self assert: annotations isEmpty