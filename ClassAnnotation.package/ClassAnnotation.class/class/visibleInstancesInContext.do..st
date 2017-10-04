accessing
visibleInstancesInContext: aContext do: aBlock
	"it will enumerate all my registered instances which should be visible in given context"
	
	self registeredInstances 
		select: [ :each | each isVisibleInContext: aContext ]
		thenDo: aBlock