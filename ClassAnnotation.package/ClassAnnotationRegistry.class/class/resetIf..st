accessing
resetIf: conditionBlock
	default ifNil: [ ^self ].
	
	conditionBlock value ifTrue: [ self reset ]