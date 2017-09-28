tests
testDeclaredInstancesShouldIncludeCopiesForEachSubclassOfOriginDeclaration

	| annotations |
	annotations := ClassAnnotationExample3 declaredInstances.
	
	self assert: (annotations anySatisfy: [ :each | each annotatedClass = SubclassWithOnlyInheritedAnnotation ])