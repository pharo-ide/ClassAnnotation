queries-contextual
activeInstancesInContext: aContext
	"it will return all my registered instances which should be active in given context"
	
	^self registeredInstances 
		select: [ :each | each isActiveInContext: aContext ]