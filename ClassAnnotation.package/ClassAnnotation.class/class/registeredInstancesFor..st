queries-general
registeredInstancesFor: aClass 
	"it will return all my registered instances which are annotating given aClass"
	
	^(self registry at: self ifAbsent: [ ^#() ])
		select: [ :each | each annotates: aClass ]