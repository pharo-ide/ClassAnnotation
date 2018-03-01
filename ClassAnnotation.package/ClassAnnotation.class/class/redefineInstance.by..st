redefining
redefineInstance: aClassAnnotation by: aBlock
	redefinedInstances ifNil: [ redefinedInstances := Dictionary new ].
	
	redefinedInstances removeKey: aClassAnnotation ifAbsent: [  ].
	redefinedInstances at: aClassAnnotation put: aClassAnnotation copy.
	aBlock value