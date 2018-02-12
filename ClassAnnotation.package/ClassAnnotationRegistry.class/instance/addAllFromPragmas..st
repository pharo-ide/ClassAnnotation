accessing
addAllFromPragmas: pragmas

	| collectedAnnotations |
	pragmas do: [ :pragma |
		collectedAnnotations := self createAnnotationsForAllSubclassesFromPragma: pragma.
		collectedAnnotations do: [ :each | self addAnnotation: each]]