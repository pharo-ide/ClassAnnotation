testing
includesAnnotation: aClassAnnotation

	| allClassAnnotations |
	allClassAnnotations := annotatedClasses at: aClassAnnotation annotatedClass ifAbsent: [ ^false ].
	^allClassAnnotations includes: aClassAnnotation