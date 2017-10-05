tests
testWhenDeclaringMethodNotReturnAnnotation

	| annotations ann |
	annotations := ClassWithBadAnnotatingMethod classAnnotations.	
	self assert: annotations size equals: 1.
	
	ann := annotations anyOne.
	
	self assert: ann class equals: BrokenClassAnnotation.
	self assert: ann annotatedClass equals: ClassWithBadAnnotatingMethod.
	self assert: ann declarationSelector equals: #badAnnotation