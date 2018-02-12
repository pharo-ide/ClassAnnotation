tests
testGettingFreshInstance
	| ann actual |
	ann := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	ann declarationSelector: #annotationExample. 
	
	actual := ann freshInstance.
	
	self assert: actual equals: ClassWithSingleAnnotation annotationExample