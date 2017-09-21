tests
testObjectsShouldInheritDeclaredUses

	| uses |
	uses := ObjectusSecondStubUse declaredInstancesFor: ObjectusSubclass3.
	
	self assert: (uses anySatisfy: [ :each | each name = #object1Use ])