tests
testRedefiningInstance
	| annotation newAnnotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	
	annotation redefineBy: [ annotation priority: -1000 ].
	self assert: annotation priority equals: -1000.
	self assert: annotation isRedefined.
	
	ClassAnnotation resetCache.
	newAnnotation := ClassWithSingleAnnotation classAnnotations anyOne.
	self assert: newAnnotation priority equals: -1000.
	self assert: newAnnotation isRedefined