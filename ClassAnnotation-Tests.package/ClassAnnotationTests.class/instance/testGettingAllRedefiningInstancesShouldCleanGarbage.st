tests
testGettingAllRedefiningInstancesShouldCleanGarbage
	| annotation allRedefined |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].
	
	ClassAnnotation registry forgetAnnotation: annotation.
	
	allRedefined := annotation class redefiningInstances.	
	self assert: allRedefined isEmpty