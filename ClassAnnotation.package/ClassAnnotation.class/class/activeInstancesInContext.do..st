queries-contextual
activeInstancesInContext: anAnnotationUser do: aBlock
	"it will enumerate all my registered instances which should be active for given anAnnotationUser"
	
	self registeredInstances 
		select: [ :each | each isActiveInContext: anAnnotationUser ]
		thenDo: aBlock