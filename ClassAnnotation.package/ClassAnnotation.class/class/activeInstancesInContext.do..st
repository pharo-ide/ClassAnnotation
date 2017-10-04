queries-contextual
activeInstancesInContext: aContext do: aBlock
	"it will enumerate all my registered instances which should be active in given context"
	
	self registeredInstances 
		select: [ :each | each isActiveInContext: aContext ]
		thenDo: aBlock