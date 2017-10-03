accessing
registeredInstancesInContext: aContext do: aBlock
	"it will enumerate all my instances which were registered for given context"
	
	^self registeredInstances 
		select: [ :each | each isDeclaredForContext: aContext ]
		thenDo: aBlock