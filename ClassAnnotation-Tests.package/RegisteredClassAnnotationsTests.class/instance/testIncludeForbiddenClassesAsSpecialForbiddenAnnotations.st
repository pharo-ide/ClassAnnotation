tests
testIncludeForbiddenClassesAsSpecialForbiddenAnnotations
	| allForbidden forbidden actualAnnotation |
	
	allForbidden := ForbiddenClassAnnotation registeredInstancesFor: ClassWithForbiddenAnnotationExample.
	forbidden := allForbidden detect: [:each | each declarationSelector = #forbiddenAnnotation].
	actualAnnotation := forbidden actualAnnotation.
	self assert: actualAnnotation class equals: ForbiddenClassAnnotationExample