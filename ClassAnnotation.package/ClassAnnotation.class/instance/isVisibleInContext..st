testing
isVisibleInContext: aContext
	"Annotation is only visible in context which is described by activeContext"
	
	^activeContext describes: aContext