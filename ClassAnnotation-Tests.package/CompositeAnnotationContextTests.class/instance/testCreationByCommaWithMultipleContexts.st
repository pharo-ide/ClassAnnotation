tests
testCreationByCommaWithMultipleContexts
	
	| context1 context2 context context3 |
	context1 := Object asAnnotationContext.
	context2 := Array asAnnotationContext.
	context3 := String asAnnotationContext.
	
	context := context1 , context2, context3.
	
	self assert: context class equals: CompositeAnnotationContext.
	self assert: context parts equals: {context1. context2. context3}.