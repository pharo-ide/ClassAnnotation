tests
testCreationByCommaWithCompatibleArgument
	
	| context1 context |
	context1 := Object asAnnotationContext.
	
	context := context1 , Array.
	
	self assert: context class equals: CompositeAnnotationContext.
	self assert: context parts equals: {context1. Array asAnnotationContext}.