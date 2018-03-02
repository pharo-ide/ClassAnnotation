redefining
redefinedInstances
	self cleanRedefinedGarbage.
	
	^redefinedInstances ifNil: [ #() ]