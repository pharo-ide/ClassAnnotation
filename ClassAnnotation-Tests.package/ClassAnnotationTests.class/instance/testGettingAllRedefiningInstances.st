tests
testGettingAllRedefiningInstances
	| annotation allRedefining |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].

	allRedefining := annotation class redefiningInstances.	
	self assert: allRedefining size equals: 1.
	self assert: allRedefining anyOne priority equals: -1000.
	self assert: allRedefining anyOne == annotation