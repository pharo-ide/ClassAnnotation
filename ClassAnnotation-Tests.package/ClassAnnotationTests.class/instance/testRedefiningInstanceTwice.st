tests
testRedefiningInstanceTwice
	| annotation newAnnotation reverted |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	
	annotation redefineBy: [ annotation priority: -1000 ].
	annotation redefineBy: [ annotation priority: -2000 ].
	
	ClassAnnotation resetCache.
	newAnnotation := ClassWithSingleAnnotation classAnnotations anyOne.
	self assert: newAnnotation priority equals: -2000.
	newAnnotation revertRedefinedInstance.
	reverted := ClassWithSingleAnnotation classAnnotations anyOne.
	self assert: reverted priority equals: 0.