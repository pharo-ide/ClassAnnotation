tests
testAllowDebugForbiddenBrokenAnnotations

	| annotations ann |
	annotations := ClassWithForbiddenAnnotationExample classAnnotations.	
	self assert: annotations size equals: 2. "onw forbidden and one broken"
	
	ann := (annotations reject: #isForbidden) anyOne.
	self assert: ann class equals: BrokenClassAnnotation.

	[ann debug.
	self assert: false description: 'should fail'] 
		on: Error do: [ :err |  ]