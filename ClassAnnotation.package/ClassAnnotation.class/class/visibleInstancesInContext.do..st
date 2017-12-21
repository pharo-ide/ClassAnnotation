queries-contextual
visibleInstancesInContext: anAnnotationUser do: aBlock
	"it will enumerate all my registered instances which should be visible for given anAnnotationUser"
	
	self registeredInstances 
		select: [ :each | each isVisibleInContext: anAnnotationUser ]
		thenDo: aBlock