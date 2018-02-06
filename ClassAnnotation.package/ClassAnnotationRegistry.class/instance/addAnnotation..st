accessing
addAnnotation: aClassAnnotation
	| container |
	container := annotations at: aClassAnnotation class ifAbsentPut: [ 
		aClassAnnotation class createContainerForRegistry ].
	(container includes: aClassAnnotation) ifTrue: [ ^self ].
	container add: aClassAnnotation.

	container := annotatedClasses 
		at: aClassAnnotation annotatedClass ifAbsentPut: [ IdentitySet new].
	container add: aClassAnnotation	