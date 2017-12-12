testing
describes: someContextObject
	"Following indirection provides extra flexibility for the users 
	to define special kind of simple application contexts
	and in same time to be able use simple classes to declare annotations.
	By default objects just performs isKindOf: check with my userContextClass"
	
	^someContextObject implementsSimpleAnnotationContext: userContextClass