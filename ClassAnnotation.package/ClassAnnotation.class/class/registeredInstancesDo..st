accessing
registeredInstancesDo: aBlock
	"it will enumerate all my instances which were registered"
	
	^self registeredInstances do: aBlock