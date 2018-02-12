tests
testCreationByComma
	
	| context1 context2 context |
	context1 := Object asAnnotationContext.
	context2 := Array asAnnotationContext.
	
	context := context1 , context2.
	
	self assert: context class equals: CompositeAnnotationContext.
	self assert: context parts equals: {context1. context2}.