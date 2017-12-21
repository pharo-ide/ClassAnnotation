testing
isVisibleInContext: anAnnotationUser
	"Annotation is only visible for users which are described by activeContext"
	
	^activeContext describes: anAnnotationUser