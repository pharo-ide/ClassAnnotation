queries-general
registeredInstancesFor: aClass 
	"it will return all my registered instances which are annotating given aClass"
	
	^self registeredInstances
		select: [ :each | each annotates: aClass ]