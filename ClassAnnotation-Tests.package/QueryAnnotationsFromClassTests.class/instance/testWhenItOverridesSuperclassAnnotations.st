tests
testWhenItOverridesSuperclassAnnotations

	| annotations |
	annotations := SubclassWithOverridenAnnotation classAnnotations.
	
	self assert: annotations size equals: 1.
	self assert: annotations first annotatedClass equals: SubclassWithOverridenAnnotation.
	self assert: annotations first class equals: ClassAnnotationExample2.