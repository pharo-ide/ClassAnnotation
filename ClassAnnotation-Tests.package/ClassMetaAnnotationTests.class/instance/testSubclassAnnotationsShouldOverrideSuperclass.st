tests
testSubclassAnnotationsShouldOverrideSuperclass

	| annotations |
	annotations := SubclassWithOverridenMetaAnnotation metaAnnotations.
	
	self assert: annotations size equals: 1.
	self assert: annotations first annotatedClass equals: SubclassWithOverridenMetaAnnotation.
	self assert: annotations first class equals: ClassMetaAnnotationExample2.