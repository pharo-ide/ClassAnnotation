accessing
registeredInstances
	"it will return all my instances which were registered"
	^self registry at: self ifAbsent: #()