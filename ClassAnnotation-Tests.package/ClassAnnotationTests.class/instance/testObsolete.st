tests
testObsolete
	| annotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	self deny: annotation isObsolete.
	
	ClassAnnotation registry forgetAnnotation: annotation.
	
	self assert: annotation isObsolete