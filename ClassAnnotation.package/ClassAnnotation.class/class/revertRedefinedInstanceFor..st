redefining
revertRedefinedInstanceFor: aClassAnnotation
	redefinedInstances ifNil: [ ^self].
	
	redefinedInstances removeKey: aClassAnnotation ifAbsent: [  ].
	self resetCache