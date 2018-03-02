tests
testGettingSingleAnnotationUsingSelector
	| expected actual |
	
	expected := ClassWithThreeAnnotations classAnnotations 
		detect: [ :each | each declarationSelector = #annotationExample2 ].
	actual := ClassWithThreeAnnotations classAnnotationAt: #annotationExample2.
	
	self assert: actual == expected