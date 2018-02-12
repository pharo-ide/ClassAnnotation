tests
testWhenAnnotationFailsCheckForForbiddenProperty

	| annotations ann |
	[ ClassAnnotationExampleWithFailedForbiddenCheck new isForbidden.
	self assert: false description: 'should fail'  ] ifError: [ ].

	annotations := ClassWithForbiddenAnnotationExample classAnnotations.	
	self assert: annotations size equals: 2. "one forbidden and one broken"
	
	ann := (annotations reject: #isForbidden) anyOne.
	
	self assert: ann class equals: BrokenClassAnnotation.
	self assert: ann annotatedClass equals: ClassWithForbiddenAnnotationExample.
	self assert: ann declarationSelector equals: #failedForbiddenAnnotation
