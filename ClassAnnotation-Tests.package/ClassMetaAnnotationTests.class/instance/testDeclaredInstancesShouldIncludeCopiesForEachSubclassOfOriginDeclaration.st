tests
testDeclaredInstancesShouldIncludeCopiesForEachSubclassOfOriginDeclaration

	| annotations |
	annotations := ClassMetaAnnotationExample3 declaredInstances.
	
	self assert: (annotations anySatisfy: [ :each | each annotatedClass = SubclassWithOnlyInheritedMetaAnnotation ])