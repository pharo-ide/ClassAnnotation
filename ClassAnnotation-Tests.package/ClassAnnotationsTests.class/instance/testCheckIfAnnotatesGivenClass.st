tests
testCheckIfAnnotatesGivenClass
	| ann|
	ann := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	self assert: (ann annotates: ClassWithSingleAnnotation).
	self deny: (ann annotates: ClassWithThreeAnnotations)