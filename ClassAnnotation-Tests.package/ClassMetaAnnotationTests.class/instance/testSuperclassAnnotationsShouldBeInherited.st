tests
testSuperclassAnnotationsShouldBeInherited

	| annotations |
	annotations := SubclassWithOnlyInheritedMetaAnnotation metaAnnotations.
	
	self assert: annotations size equals: 1.
	self assert: annotations first annotatedClass equals: SubclassWithOnlyInheritedMetaAnnotation