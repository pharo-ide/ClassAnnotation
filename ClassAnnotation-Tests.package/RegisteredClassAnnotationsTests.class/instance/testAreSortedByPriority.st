tests
testAreSortedByPriority

	| annotations expected |
	annotations := ClassAnnotationExampleWithPriority registeredInstances.
	
	self assert: annotations size equals: 3.
	
	expected := annotations sorted: #priority descending.
	self assert: annotations asArray equals: expected asArray