system changes
doesMethodAffectAnnotations: aMethod
	
	(self doesMethodDefineAnnotation: aMethod) ifTrue: [ ^true ].

	aMethod methodClass superclass ifNotNil: [ :parentClass | 
		"check if there is overridden method which defines annotation"
		(parentClass lookupSelector: aMethod selector) ifNotNil: [:overriddenMethod |
			(self doesMethodDefineAnnotation: overriddenMethod) ifTrue: [^true]].
	].	

	^false