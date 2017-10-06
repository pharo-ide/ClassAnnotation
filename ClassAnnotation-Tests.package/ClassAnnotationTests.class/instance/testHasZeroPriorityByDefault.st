tests
testHasZeroPriorityByDefault
	
	| ann |
	ann := ClassAnnotationExample1 new.
	
	self assert: ann priority equals: 0