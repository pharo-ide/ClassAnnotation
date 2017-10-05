tests
testNotIncludeInstancesAnnotatingForbiddenClasses
	"In that test the annotation define canAnnotateClass: with constant false answer.
	So any annotating methods which define it should be never register their instances"
	| annotations broken |
	annotations := ForbiddenClassAnnotationExample registeredInstances.
	
	self assert: annotations isEmpty.
	broken := BrokenClassAnnotation registeredInstancesFor: ClassWithForbiddenAnnotationExample.
	broken 
		detect: [:each | each declarationSelector = #forbiddenAnnotation]
		ifFound: [ :ann | self assert: false description: 'forbidden annotation should not be broken' ]