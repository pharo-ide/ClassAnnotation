tests
testCreationByCommaWithMultipleCompatipleContexts
	
	| context1 context |
	context1 := Object asAnnotationContext.
	
	context := context1 , Array, String.
	
	self assert: context class equals: CompositeAnnotationContext.
	self 
		assert: context parts 
		equals: {context1. Array asAnnotationContext. String asAnnotationContext}