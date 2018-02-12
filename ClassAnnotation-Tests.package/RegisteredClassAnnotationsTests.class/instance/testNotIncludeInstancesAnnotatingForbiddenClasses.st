tests
testNotIncludeInstancesAnnotatingForbiddenClasses
	"In that test the annotation define canAnnotateClass: with constant false answer.
	So any annotating methods which define it should never register their instances"
	| annotations |
	annotations := ForbiddenClassAnnotationExample registeredInstances.
	
	self assert: annotations isEmpty