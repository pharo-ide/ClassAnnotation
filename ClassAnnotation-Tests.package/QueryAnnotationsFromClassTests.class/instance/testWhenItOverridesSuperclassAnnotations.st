tests
testWhenItOverridesSuperclassAnnotations

	| annotations |
	annotations := SubclassWithOverridenAnnotation classAnnotations.
	
	self assert: annotations size equals: 1.
	self assert: annotations anyOne annotatedClass equals: SubclassWithOverridenAnnotation.
	self assert: annotations anyOne class equals: ClassAnnotationExample2.