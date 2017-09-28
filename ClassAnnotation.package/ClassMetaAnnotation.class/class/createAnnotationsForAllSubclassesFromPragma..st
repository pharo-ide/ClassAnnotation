private
createAnnotationsForAllSubclassesFromPragma: aPragma
	
	| declaringClass annotation result |
	result := OrderedCollection new.
	declaringClass := aPragma methodClass instanceSide.
	declaringClass withAllSubclassesDo: [ :annotatedClass | 
		annotation := self createAnnotationFor: annotatedClass fromPragma: aPragma. 
		result add: annotation].
	^result