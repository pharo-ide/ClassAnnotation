tests
testGettingAnnotationsOfClassWithoutAnnotations

	| annotations |
	annotations := ClassWithoutMetaAnnotations metaAnnotations.
	
	self assert: annotations isEmpty