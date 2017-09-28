tests
testGettingAnnotationsOfGivenClass

	| annotations |
	annotations := ClassWithThreeAnnotations metaAnnotations.
	
	self assert: annotations size equals: 3.
	annotations do: [ :each | 
		self assert: each annotatedClass equals: ClassWithThreeAnnotations]