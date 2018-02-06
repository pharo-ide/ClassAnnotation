system changes
ensureSystemSubscription

	SystemAnnouncer uniqueInstance unsubscribe: self.
	
	SystemAnnouncer uniqueInstance weak
		when: ClassRemoved, ClassAdded, ClassModificationApplied send: #handleClassChanged: to: self;
		when: MethodRemoved, MethodAdded, MethodModified send: #handleMethodChange: to: self