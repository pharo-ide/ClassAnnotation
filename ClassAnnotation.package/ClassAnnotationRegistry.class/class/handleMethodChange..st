system changes
handleMethodChange: aMethodAnnouncement
	
	self resetIf: [ self doesMethodAffectAnnotations: aMethodAnnouncement methodAffected]