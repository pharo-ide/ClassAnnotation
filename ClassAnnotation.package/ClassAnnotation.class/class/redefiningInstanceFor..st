redefining
redefiningInstanceFor: aClassAnnotation
	"It returnes the annotation instance which redefines given default collected annotation"
	
	| assoc |
	redefinedInstances ifNil: [ ^self error: 'Given annotation is not redefined!' ].
	
	assoc := redefinedInstances 
		associationAt: aClassAnnotation ifAbsent: [ self error: 'Given annotation is not redefined!' ].
	^assoc key