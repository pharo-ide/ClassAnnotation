tests
testWhenAnnotationFailsCheckForForbiddenProperty

	| annotations ann |
	[ ClassAnnotationExampleWithFailedForbiddenCheck new isForbidden.
	self assert: false description: 'should fail'  ] ifError: [ ].

	annotations := ClassWithForbiddenAnnotationExample classAnnotations.	
	self assert: annotations size equals: 1.
	
	ann := annotations anyOne.
	
	self assert: ann class equals: BrokenClassAnnotation.
	self assert: ann annotatedClass equals: ClassWithForbiddenAnnotationExample.
	self assert: ann declarationSelector equals: #failedForbiddenAnnotation
