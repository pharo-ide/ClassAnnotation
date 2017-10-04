accessing
visibleInstancesFor: aClass inContext: aContext do: aBlock
	"it will enumerate all my registered instances visible in given aContext 
	which are annotating given aClass"
	
	self registeredInstances 
		select: [ :each | 
			(each annotates: aClass) and: [each isVisibleInContext: aContext] ]
		thenDo: aBlock