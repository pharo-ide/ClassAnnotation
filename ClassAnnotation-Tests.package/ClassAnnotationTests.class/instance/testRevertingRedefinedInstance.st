tests
testRevertingRedefinedInstance
	| annotation revertedAnnotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].
	revertedAnnotation := annotation revertRedefinedInstance.

	self deny: revertedAnnotation == annotation.	
	self assert: revertedAnnotation == ClassWithSingleAnnotation classAnnotations anyOne.
	self assert: revertedAnnotation priority equals: 0.
	self deny: revertedAnnotation isRedefined