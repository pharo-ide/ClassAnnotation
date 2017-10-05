tests
testWhenSuperclassIsAnnotatedToo

	| annotations |
	annotations := SubclassWithOnlyInheritedAnnotation classAnnotations.
	
	self assert: annotations size equals: 1.
	self assert: annotations first annotatedClass equals: SubclassWithOnlyInheritedAnnotation