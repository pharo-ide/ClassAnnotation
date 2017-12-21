queries-contextual
visibleInstancesInContext: anAnnotationUser
	"it will return all my registered instances which should be visible for given anAnnotationUser"
	
	^self registeredInstances 
		select: [ :each | each isVisibleInContext: anAnnotationUser ]