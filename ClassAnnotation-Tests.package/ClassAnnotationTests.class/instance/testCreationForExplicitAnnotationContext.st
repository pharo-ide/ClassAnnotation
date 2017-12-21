tests
testCreationForExplicitAnnotationContext
	| ann context |
	context := SimpleAnnotationContext of: AnnotationUserExample.
	ann := ClassAnnotationExample1 for: context.
	
	self assert: ann class equals: ClassAnnotationExample1.
	self assert: ann activeContext equals: context.