redefining
redefineInstance: aClassAnnotation by: aBlock
	"In case when instance is already redefined 
	the dictionary will just put new instance into existing key which is riginal annotation instance.
	Otherwise origin annotation instance (the copy) will be saved in the key.
	Annotations use annotated class and selector as identity properties"
	redefinedInstances ifNil: [ redefinedInstances := Dictionary new ].
	
	redefinedInstances at: aClassAnnotation copy put: aClassAnnotation.
	aBlock value