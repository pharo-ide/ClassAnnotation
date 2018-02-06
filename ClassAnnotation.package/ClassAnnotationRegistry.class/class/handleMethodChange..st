system subscription
handleMethodChange: aMethodAdded
	
	self resetIf: [ self doesMethodAffectAnnotations: aMethodAdded methodAffected]