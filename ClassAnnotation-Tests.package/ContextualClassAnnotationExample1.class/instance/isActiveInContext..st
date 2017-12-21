testing
isActiveInContext: anAnnotationUser
	^(super isActiveInContext: anAnnotationUser)
		and: [ anAnnotationUser isSimulatedActiveAnnotation ]