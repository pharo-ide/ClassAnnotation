tests
testDeclaredInstancesEnumeration

	| enumerated |
	enumerated := OrderedCollection new.
	ClassAnnotationExample1 registeredInstancesDo: [:each | enumerated add: each].
	
	self assert: enumerated asArray equals: ClassAnnotationExample1 registeredInstances asArray