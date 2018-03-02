running
tearDown
	"Some tests redefine following annotation. 
	Here we clear redefining state which forces cache reset"
	ClassAnnotationExample3 revertRedefinedInstances.
	"ClassAnnotationExample3 = ClassWithSingleAnnotation classAnnotations anyOne class"
	
	super tearDown.