tests
testIncludeCopiesForEachSubclassOfOriginDeclaration

	| annotations |
	annotations := ClassAnnotationExample3 registeredInstances.
	
	self assert: (annotations anySatisfy: [ :each | each annotatedClass = SubclassWithOnlyInheritedAnnotation ])