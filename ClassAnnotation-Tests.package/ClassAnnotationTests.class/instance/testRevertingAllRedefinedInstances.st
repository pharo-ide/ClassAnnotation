tests
testRevertingAllRedefinedInstances
	| annotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation class revertRedefinedInstances.
	
	self assert: annotation class redefinedInstances isEmpty.
	self assert: annotation class redefiningInstances isEmpty