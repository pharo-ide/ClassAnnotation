tests
testWhenClassHasNoAnnotations

	| annotations |
	annotations := ClassWithoutAnnotations classAnnotations.
	
	self assert: annotations isEmpty