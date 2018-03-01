accessing
addAnnotation: aClassAnnotation
	| container actualAnnotation |
	actualAnnotation := aClassAnnotation isRedefined 
		ifTrue: [ aClassAnnotation redefiningInstance] ifFalse: [ aClassAnnotation].
	container := annotations at: actualAnnotation class ifAbsentPut: [ 
		aClassAnnotation class createContainerForRegistry ].
	(container includes: actualAnnotation) ifTrue: [ ^self ].
	container add: actualAnnotation.

	container := annotatedClasses 
		at: actualAnnotation annotatedClass ifAbsentPut: [ Set new].
	container add: actualAnnotation	