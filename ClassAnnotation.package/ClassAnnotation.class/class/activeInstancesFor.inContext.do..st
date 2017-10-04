queries-contextual
activeInstancesFor: aClass inContext: aContext do: aBlock
	"it will enumerate all my registered instances active in given aContext 
	which are annotating given aClass"
	
	self registeredInstances 
		select: [ :each | 
			(each annotates: aClass) and: [each isActiveInContext: aContext] ]
		thenDo: aBlock