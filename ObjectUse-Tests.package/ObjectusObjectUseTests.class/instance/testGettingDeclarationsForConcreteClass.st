tests
testGettingDeclarationsForConcreteClass

	| uses |
	uses := ObjectusFirstStubUse declaredInstancesFor: ObjectusReusableObject1.
	
	self assert: uses size equals: 1.
	self assert: uses first objectClass equals: ObjectusReusableObject1