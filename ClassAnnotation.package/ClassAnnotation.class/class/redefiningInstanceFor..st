redefining
redefiningInstanceFor: aClassAnnotation
	"It returnes the annotation instance which redefines given default collected annotation"
	
	redefinedInstances ifNil: [ ^self error: 'Given annotation is not redefined!' ].
	
	^redefinedInstances 
		at: aClassAnnotation ifAbsent: [ self error: 'Given annotation is not redefined!' ]