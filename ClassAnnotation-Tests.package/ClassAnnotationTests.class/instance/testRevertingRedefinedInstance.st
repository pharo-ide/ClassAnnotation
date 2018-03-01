tests
testRevertingRedefinedInstance
	| annotation revertedAnnotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].
	annotation revertRedefinedInstance.
	
	revertedAnnotation := ClassWithSingleAnnotation classAnnotations anyOne.
	self assert: revertedAnnotation priority equals: 0.
	self deny: revertedAnnotation isRedefined.
	self deny: revertedAnnotation == annotation