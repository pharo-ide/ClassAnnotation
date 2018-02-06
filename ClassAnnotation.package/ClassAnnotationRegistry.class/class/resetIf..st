accessing
resetIf: conditionBlock
	default ifNil: [ ^self ].
	
	conditionBlock ifTrue: [ self reset ]