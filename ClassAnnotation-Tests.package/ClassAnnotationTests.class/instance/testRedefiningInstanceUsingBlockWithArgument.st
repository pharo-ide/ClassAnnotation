tests
testRedefiningInstanceUsingBlockWithArgument
	| newAnnotation |
	
	ClassWithSingleAnnotation classAnnotations anyOne
		redefineBy: [:annotation | annotation priority: -1000 ].
		
	newAnnotation := ClassWithSingleAnnotation classAnnotations anyOne.
	self assert: newAnnotation priority equals: -1000.
	self assert: newAnnotation isRedefined