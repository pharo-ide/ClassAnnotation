running
tearDown
	"Some tests redefine following annotation. 
	Here we clear redefining state which forces cache reset"
	| redefiningAnnotation |
	redefiningAnnotation := ClassWithSingleAnnotation classAnnotations anyOne.
	redefiningAnnotation class resetRedefiningInstances.
	
	super tearDown.