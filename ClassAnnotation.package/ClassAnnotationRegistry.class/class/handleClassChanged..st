system subscription
handleClassChanged: aClassAnnouncement

	self resetIf: [ default includesAnnotationsFor: aClassAnnouncement classAffected]