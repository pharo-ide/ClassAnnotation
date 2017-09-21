tests
testCollectingDeclarationsForConcreteUse

	| uses |
	uses := ObjectusFirstStubUse declaredInstances.
	
	self assert: uses size equals: 2.
	self assert: uses first class equals: ObjectusFirstStubUse.
	self assert: uses second class equals: ObjectusFirstStubUse.