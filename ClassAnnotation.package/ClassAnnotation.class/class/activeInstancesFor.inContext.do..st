queries-contextual
activeInstancesFor: aClass inContext: anAnnotationUser do: aBlock
	"it will enumerate all my registered instances active for given anAnnotationUser 
	which are annotating given aClass"
	
	self registeredInstances 
		select: [ :each | 
			(each annotates: aClass) and: [each isActiveInContext: anAnnotationUser] ]
		thenDo: aBlock