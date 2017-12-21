queries-contextual
visibleInstancesFor: aClass inContext: anAnnotationUser do: aBlock
	"it will enumerate all my registered instances visible for given anAnnotationUser 
	which are annotating given aClass"
	
	self registeredInstances 
		select: [ :each | 
			(each annotates: aClass) and: [each isVisibleInContext: anAnnotationUser] ]
		thenDo: aBlock