queries-general
findAllAnnotationsFor: aClass 
	"It will lookup all kind of annotations which annotate given class"
	| result |
	result := OrderedCollection new.
	
	self registry do: [ :instances | 
		result addAll: (instances select: [ :each | each annotates: aClass ]) ].
	
	^result