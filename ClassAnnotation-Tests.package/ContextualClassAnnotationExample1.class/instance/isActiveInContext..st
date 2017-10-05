testing
isActiveInContext: aContext
	^(super isActiveInContext: aContext)
		and: [ aContext isSimulatedActiveAnnotation ]