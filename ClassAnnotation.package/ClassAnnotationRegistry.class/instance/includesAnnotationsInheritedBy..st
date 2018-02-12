testing
includesAnnotationsInheritedBy: aClass

	aClass superclass ifNil: [ ^false ].
	
	^self includesAnnotationsFor: aClass superclass