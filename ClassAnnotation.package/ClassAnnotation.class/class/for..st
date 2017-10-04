instance creation
for: anAnnotationContextOrCompatible
	"For example argument can be a class which represents the context"
	^ self new 
		activeContext: anAnnotationContextOrCompatible asAnnotationContext