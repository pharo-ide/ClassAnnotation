tests
testGettingRedefiningInstance
	| annotation actual |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].
	
	actual := annotation copy redefiningInstance.
	
	self assert: actual == annotation