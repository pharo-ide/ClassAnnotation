redefining
updateRedefinedInstance: aClassAnnotation
	"It updates actual redefined instance of annotation.
	It sets given annotation as new value.
	Both redefined and redefining annotations are equal. 
	So dictionary value can be used as a key"
	
	redefinedInstances ifNil: [ ^self error: 'Given annotation is not redefined!' ].
	
	redefinedInstances 
		at: aClassAnnotation ifAbsent: [ self error: 'Given annotation is not redefined!' ].
	
	redefinedInstances at: aClassAnnotation put: aClassAnnotation