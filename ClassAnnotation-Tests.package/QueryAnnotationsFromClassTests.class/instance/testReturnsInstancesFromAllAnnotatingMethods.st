tests
testReturnsInstancesFromAllAnnotatingMethods

	| annotations |
	annotations := ClassWithThreeAnnotations classAnnotations.
	
	self assert: annotations size equals: 3.
	annotations do: [ :each | 
		self assert: each annotatedClass equals: ClassWithThreeAnnotations]