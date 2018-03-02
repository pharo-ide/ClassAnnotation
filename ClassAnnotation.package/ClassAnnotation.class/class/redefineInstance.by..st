redefining
redefineInstance: aClassAnnotation by: aBlock
	| redefined |
	redefinedInstances ifNil: [ redefinedInstances := Dictionary new ].
	
	redefined := redefinedInstances removeKey: aClassAnnotation ifAbsent: [ aClassAnnotation copy ].
	redefinedInstances at: aClassAnnotation put: redefined.
	aBlock cull: aClassAnnotation