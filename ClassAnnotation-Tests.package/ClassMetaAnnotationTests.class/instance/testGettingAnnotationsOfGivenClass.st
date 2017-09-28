tests
testGettingAnnotationsOfGivenClass

	| annotations |
	annotations := ClassWithThreeMetaAnnotations metaAnnotations.
	
	self assert: annotations size equals: 3.
	annotations do: [ :each | 
		self assert: each annotatedClass equals: ClassWithThreeMetaAnnotations]