tests
testGettingAllRedefinedInstances
	| annotation allRedefined |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].

	allRedefined := annotation class redefinedInstances.	
	self assert: allRedefined size equals: 1.
	self assert: allRedefined anyOne priority equals: 0