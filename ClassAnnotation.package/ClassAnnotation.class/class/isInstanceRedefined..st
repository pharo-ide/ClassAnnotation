testing
isInstanceRedefined: aClassAnnotation

	redefinedInstances ifNil: [ ^false ].
	
	^redefinedInstances includesKey: aClassAnnotation	