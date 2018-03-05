system changes
handleOldMethodChange: aMethodModified
	
	self resetIf: [ self doesMethodAffectAnnotations: aMethodModified oldMethod]