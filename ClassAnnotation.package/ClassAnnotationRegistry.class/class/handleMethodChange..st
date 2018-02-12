system changes
handleMethodChange: aMethodAdded
	
	self resetIf: [ self doesMethodAffectAnnotations: aMethodAdded methodAffected]