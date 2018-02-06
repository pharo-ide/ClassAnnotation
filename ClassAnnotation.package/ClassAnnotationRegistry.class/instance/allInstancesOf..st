queries
allInstancesOf: anAnnotationClass

	^annotations at: anAnnotationClass ifAbsent: [ #() ]