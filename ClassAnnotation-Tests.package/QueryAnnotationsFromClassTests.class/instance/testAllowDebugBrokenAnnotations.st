tests
testAllowDebugBrokenAnnotations

	| annotations ann |
	annotations := ClassWithBrokenAnnotatingMethod classAnnotations.	
	self assert: annotations size equals: 1.
	
	ann := annotations anyOne.
	self assert: ann class equals: BrokenClassAnnotation.

	[ann debug.
	self assert: false description: 'should fail'] 
		on: Error do: [ :err | 
	self assert: err messageText equals: ClassWithBrokenAnnotatingMethod brokenAnnotationMessage  ]