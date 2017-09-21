tests
testCollectedDeclarationsShouldBeCached

	| uses |
	uses := ObjectusFirstStubUse declaredInstances.
	
	self assert: ObjectusFirstStubUse declaredInstances == uses