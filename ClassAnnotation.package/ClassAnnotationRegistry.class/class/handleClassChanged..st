system changes
handleClassChanged: aClassAnnouncement

	self resetIf: [ 
		(default includesAnnotationsFor: aClassAnnouncement classAffected)
			or: [default includesAnnotationsInheritedBy: aClassAnnouncement classAffected]]