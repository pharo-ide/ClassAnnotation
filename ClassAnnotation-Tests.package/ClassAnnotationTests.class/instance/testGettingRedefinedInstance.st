tests
testGettingRedefinedInstance
	| annotation redefinedInstance |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].
	
	redefinedInstance := annotation redefinedInstance.
	self deny: redefinedInstance == annotation.
	self assert: redefinedInstance priority equals: 0.
	self assert: redefinedInstance isRedefined