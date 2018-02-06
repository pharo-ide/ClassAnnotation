pragma collecting
collectAnnotationPragmas

	| result |
	result := OrderedCollection new.
	
	self environment allClassesDo: [ :each | 
		self annotationPragmasIn: each classSide do: [:pragma | result add: pragma]].
	self environment allTraitsDo: [ :each | 
		self annotationPragmasIn: each classSide do: [:pragma | result add: pragma]].
	
	^result