accessing
visibleInstancesInContext: aContext
	"it will return all my registered instances which should be visible in given context"
	
	^self registeredInstances 
		select: [ :each | each isVisibleInContext: aContext ]