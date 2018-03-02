redefining
revertRedefinedInstanceIfAbsent: absentBlock

	self class revertRedefinedInstanceFor: self.
	^annotatedClass classAnnotations 
		detect: [ :each | each = self ] ifNone: absentBlock.