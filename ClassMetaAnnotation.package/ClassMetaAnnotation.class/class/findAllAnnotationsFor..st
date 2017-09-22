accessing
findAllAnnotationsFor: aClass 
	| result |
	result := OrderedCollection new.
	
	self registry do: [ :instances | 
		result addAll: (instances select: [ :each | each annotates: aClass ]) ].
	
	^result