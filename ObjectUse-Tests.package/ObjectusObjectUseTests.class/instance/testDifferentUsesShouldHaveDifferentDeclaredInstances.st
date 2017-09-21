tests
testDifferentUsesShouldHaveDifferentDeclaredInstances

	| uses1 uses2 |
	uses1 := ObjectusFirstStubUse declaredInstances.
	uses2 := ObjectusSecondStubUse declaredInstances.
	
	self deny: uses1 = uses2