tests
testSuperclassAnnotationsShouldBeInherited

	| annotations |
	annotations := SubclassWithOnlyInheritedAnnotation metaAnnotations.
	
	self assert: annotations size equals: 1.
	self assert: annotations first annotatedClass equals: SubclassWithOnlyInheritedAnnotation