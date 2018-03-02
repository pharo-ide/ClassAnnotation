accessing
forgetAnnotation: aClassAnnotation
	"This test was created for tests to emulate that annotation not exist anymore"
	| container |

	container := annotations at: aClassAnnotation class ifAbsent: [ ^self ].
	container remove: aClassAnnotation ifAbsent: [ ^self ].
	
	(annotatedClasses at: aClassAnnotation annotatedClass) remove: aClassAnnotation