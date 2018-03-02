redefining
redefinedInstanceFor: aClassAnnotation
	"It returnes the annotation instance which is redefined by given annotation"	
	redefinedInstances ifNil: [ ^self error: 'Given annotation is not redefined!' ].
	
	^redefinedInstances 
		at: aClassAnnotation ifAbsent: [ self error: 'Given annotation is not redefined!' ]