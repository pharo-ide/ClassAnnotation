system changes
handleClassChanged: aClassAnnouncement

	self resetIf: [ default includesAnnotationsFor: aClassAnnouncement classAffected]