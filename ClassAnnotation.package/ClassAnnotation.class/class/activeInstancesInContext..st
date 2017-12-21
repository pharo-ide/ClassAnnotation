queries-contextual
activeInstancesInContext: anAnnotationUser
	"it will return all my registered instances which should be active for given anAnnotationUser"
	
	^self registeredInstances 
		select: [ :each | each isActiveInContext: anAnnotationUser ]