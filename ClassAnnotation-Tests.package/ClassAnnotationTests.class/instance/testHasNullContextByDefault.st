tests
testHasNullContextByDefault
	
	| ann |
	ann := ClassAnnotationExample1 new.
	
	self assert: ann activeContext equals: NullAnnotationContext instance